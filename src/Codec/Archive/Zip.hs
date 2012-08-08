{- | Sink file to the archive:

@
import           Data.Time (getCurrentTime)
import           System.Environment (getArgs)
import           System.FilePath (takeFileName)
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import           Codec.Archive.Zip

main = do
    filePath:_ <- getArgs
    time <- getCurrentTime
    withArchive \"some.zip\" $ do
        sink <- getSink (takeFileName filePath) time
        runResourceT $ CB.sourceFile filePath $$ sink
@

Source first file from the archive:

@
import           System.Environment (getArgs)
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import           Codec.Archive.Zip

main = do
    archivePath:_ <- getArgs
    withArchive archivePath $ do
        fileName:_ <- fileNames
        source     <- getSource fileName
        runResourceT $ source $$ CB.sinkFile fileName
@

List files in the zip archive:

@
import System.Environment (getArgs)
import Codec.Archive.Zip

main = do
    archivePath:_ <- getArgs
    names <- withArchive archivePath fileNames
    mapM_ putStrLn names
@


Add files to the archive:

@
import Control.Monad (filterM)
import System.Directory (doesFileExist, getDirectoryContents)
import System.Environment (getArgs)
import Codec.Archive.Zip

main = do
    dirPath:_ <- getArgs
    paths     <- getDirectoryContents dirPath
    filePaths <- filterM doesFileExist paths
    withArchive \"some.zip\" $ addFiles filePaths
@

Extract all files from the archive:

@
import System.Environment (getArgs)
import Codec.Archive.Zip

main = do
    dirPath:_ <- getArgs
    withArchive \"some.zip\" $ do
        names <- fileNames
        extractFiles names dirPath
@

-}

module Codec.Archive.Zip
    ( -- * Archive monad
      Archive
    , withArchive

    -- * Operations
    , getComment
    , setComment
    , fileNames

    -- * Conduit interface
    , getSource
    , getSink

    -- * High level functions
    , addFiles
    , extractFiles
    ) where

import           Prelude hiding (readFile, zip)
import           Control.Applicative
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.List (find)
import           Data.Time
import           Data.Word
import           System.Directory
import           System.FilePath
import           System.IO hiding (readFile)

import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Util as CU
import           Data.Conduit.Zlib

import           Codec.Archive.Zip.Internal
import           Codec.Archive.Zip.Util


------------------------------------------------------------------------------
-- Archive monad
type Archive a = StateT Zip IO a


data Zip = Zip
    { zipFilePath               :: FilePath
    , zipFileHeaders            :: [FileHeader]
    , zipCentralDirectoryOffset :: Int
    , zipComment                :: ByteString
    } deriving (Show)


withArchive :: FilePath -> Archive a -> IO a
withArchive path ar = do
    zip <- ifM (doesFileExist path)
               (readZip path)
               (return $ emptyZip path)

    evalStateT ar zip


readZip :: FilePath -> IO Zip
readZip f =
    withFile f ReadMode $ \h -> do
        e  <- readEnd h
        cd <- readCentralDirectory h e
        return $ Zip { zipFilePath    = f
                     , zipFileHeaders = cdFileHeaders cd
                     , zipCentralDirectoryOffset =
                         endCentralDirectoryOffset e
                     , zipComment     = endZipComment e
                     }


emptyZip :: FilePath -> Zip
emptyZip f = Zip { zipFilePath               = f
                 , zipFileHeaders            = []
                 , zipCentralDirectoryOffset = 0
                 , zipComment                = B.empty
                 }


------------------------------------------------------------------------------
-- Operations
fileNames :: Archive [FilePath]
fileNames = gets $ map fhFileName . zipFileHeaders


getComment :: Archive ByteString
getComment = gets zipComment


setComment :: ByteString -> Archive ()
setComment comment = modify $ \zip ->  zip { zipComment = comment }


------------------------------------------------------------------------------
-- Conduit interface
getSource :: MonadResource m => FilePath -> Archive (Source m ByteString)
getSource f = gets $ \zip -> sourceFile zip f


getSink :: MonadResource m
        => FilePath -> UTCTime -> Archive (Sink ByteString m ())
getSink f time = gets $ \zip -> sinkFile zip f time


sourceFile :: MonadResource m => Zip -> FilePath -> Source m ByteString
sourceFile zip f = do
    source $= CB.isolate (fromIntegral $ fhCompressedSize fileHeader)
           $= decomp
  where
    source = CB.sourceIOHandle $ do
        h      <- openFile (zipFilePath zip) ReadMode
        offset <- calculateFileDataOffset h fileHeader
        hSeek h AbsoluteSeek offset
        return h

    fileHeader =
        maybe (error "No such file.")
              id
              $ find (\fh -> f == fhFileName fh) $ zipFileHeaders zip

    decomp =
        case fhCompressionMethod fileHeader of
          NoCompression -> CL.map id
          Deflate       -> decompress $ WindowBits (-15)


sinkFile :: MonadResource m
         => Zip -> FilePath -> UTCTime -> Sink ByteString m ()
sinkFile zip f time = do
    h  <- liftIO $ openFile (zipFilePath zip) WriteMode
    fh <- liftIO $ appendLocalFileHeader h zip f time
    dd <- sinkData h
    liftIO $ do
        writeDataDescriptor' h dd offset
        let zip' = updateZip zip fh dd
        writeFinish h zip'
        hClose h
  where
    offset = fromIntegral $ zipCentralDirectoryOffset zip  -- FIXME: old offset!


------------------------------------------------------------------------------
-- High level functions
addFiles :: [FilePath] -> Archive ()
addFiles fs = do
    zip <- get
    zip' <- liftIO $ withFile (zipFilePath zip) ReadWriteMode $ \h -> do
        zip' <- foldM (addFile h) zip fs
        writeFinish h zip'
        return zip'
    put zip'


extractFiles :: [FilePath] -> FilePath -> Archive ()
extractFiles fs dir = do
    zip <- get
    liftIO $ forM_ fs $ \fileName -> do
        createDirectoryIfMissing True $ dir </> takeDirectory fileName
        runResourceT $ sourceFile zip fileName $$ CB.sinkFile (dir </> fileName)


------------------------------------------------------------------------------
-- Low level functions

-- | Appends file to the 'Zip'.
addFile :: Handle -> Zip -> FilePath -> IO Zip
addFile h zip f = do
    m  <- clockTimeToUTCTime <$> getModificationTime f
    fh <- appendLocalFileHeader h zip (dropDrive f) m
    dd <- runResourceT $ CB.sourceFile f $$ sinkData h
    writeDataDescriptor' h dd offset
    return $ updateZip zip fh dd
  where
    offset = fromIntegral $ zipCentralDirectoryOffset zip  -- FIXME: old offset!


appendLocalFileHeader :: Handle -> Zip -> FilePath -> UTCTime -> IO FileHeader
appendLocalFileHeader h zip f time = do
    hSeek h AbsoluteSeek offset
    writeLocalFileHeader h fh
    return fh
  where
    offset = fromIntegral $ zipCentralDirectoryOffset zip
    fh     = mkFileHeader f time (fromIntegral offset)


mkFileHeader :: FilePath -> UTCTime -> Word32 -> FileHeader
mkFileHeader f lastModified relativeOffset =
    FileHeader { fhBitFlag                = 2  -- max compression + data descriptor
               , fhCompressionMethod      = Deflate
               , fhLastModified           = lastModified
               , fhCRC32                  = 0
               , fhCompressedSize         = 0
               , fhUncompressedSize       = 0
               , fhInternalFileAttributes = 0
               , fhExternalFileAttributes = 0
               , fhRelativeOffset         = relativeOffset
               , fhFileName               = f
               , fhExtraField             = B.empty
               , fhFileComment            = B.empty
               }


sinkData :: MonadResource m => Handle -> Sink ByteString m DataDescriptor
sinkData h = do
    ((uncompressedSize, crc32), compressedSize) <-
        CU.zipSinks sizeCrc32Sink
                    compressSink
    return $ DataDescriptor
               { ddCRC32            = crc32
               , ddCompressedSize   = fromIntegral compressedSize
               , ddUncompressedSize = fromIntegral uncompressedSize
               }
  where
    compressSink :: MonadResource m => Sink ByteString m Int
    compressSink = compress 6 (WindowBits (-15)) =$ sizeDataSink

    sizeCrc32Sink :: MonadResource m => Sink ByteString m (Int, Word32)
    sizeCrc32Sink =  CU.zipSinks sizeSink crc32Sink

    sizeDataSink :: MonadResource m => Sink ByteString m Int
    sizeDataSink  = fst <$> CU.zipSinks sizeSink (CB.sinkHandle h)


writeDataDescriptor' :: Handle -> DataDescriptor -> Integer -> IO ()
writeDataDescriptor' h dd offset = do
    old <- hTell h
    hSeek h AbsoluteSeek $ offset + 4 + 2 + 2 + 2 + 2 + 2
    writeDataDescriptor h dd
    hSeek h AbsoluteSeek old


updateZip :: Zip -> FileHeader -> DataDescriptor -> Zip
updateZip zip fh dd =
    zip { zipFileHeaders = (zipFileHeaders zip)
                           ++ [ fh { fhCRC32            = ddCRC32 dd
                                   , fhCompressedSize   = ddCompressedSize dd
                                   , fhUncompressedSize = ddUncompressedSize dd
                                   } ]
       , zipCentralDirectoryOffset = (zipCentralDirectoryOffset zip) + (fromIntegral $ localFileHeaderLength fh + ddCompressedSize dd) -- the last is datadescriptor size
       }


writeFinish :: Handle -> Zip -> IO ()
writeFinish h zip = do
    writeCentralDirectory h $ CentralDirectory (zipFileHeaders zip)  -- FIXME: CentralDirectory?
    writeEnd h
             (length $ zipFileHeaders zip)
             (sum $ map fileHeaderLength $ zipFileHeaders zip)
             (zipCentralDirectoryOffset zip)
