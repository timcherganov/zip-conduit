module Codec.Archive.Zip
    ( Archive
    , readArchive
    , emptyArchive
    , fileNames
    , sourceFile
    , sinkFile
    , addFiles
    , extractFiles
    ) where

import           Prelude hiding (readFile)
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
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit.Zlib

import           Codec.Archive.Zip.Internal
import           Codec.Archive.Zip.Util


data Archive = Archive
    { archiveFilePath               :: FilePath
    , archiveFileHeaders            :: [FileHeader]
    , archiveCentralDirectoryOffset :: Int
    , archiveComment                :: ByteString
    } deriving (Show)


readArchive :: FilePath -> IO Archive
readArchive f =
    withFile f ReadMode $ \h -> do
        e  <- readEnd h
        cd <- readCentralDirectory h e
        return $ Archive
                   { archiveFilePath    = f
                   , archiveFileHeaders = cdFileHeaders cd
                   , archiveCentralDirectoryOffset =
                       endCentralDirectoryOffset e
                   , archiveComment     = endZipComment e
                   }


emptyArchive :: FilePath -> Archive
emptyArchive f = Archive
                   { archiveFilePath               = f
                   , archiveFileHeaders            = []
                   , archiveCentralDirectoryOffset = 0
                   , archiveComment                = B.empty
                   }


fileNames :: Archive -> [FilePath]
fileNames = (map fhFileName) . archiveFileHeaders


-- getComment :: Archive -> ByteString
-- getComment = archiveComment


-- setComment :: Archive -> ByteString -> Archive
-- setComment ar comment = ar { archiveComment = comment }


------------------------------------------------------------------------------
sourceFile :: MonadResource m => Archive -> FilePath -> Source m ByteString
sourceFile ar f =
    source $= CB.isolate (fromIntegral $ fhCompressedSize fileHeader)
           $= decomp
  where
    source = CB.sourceIOHandle $ do
        h <- openFile (archiveFilePath ar) ReadMode
        offset <- calculateFileDataOffset h fileHeader
        hSeek h AbsoluteSeek offset
        return h

    fileHeader =
        maybe (error "No such file.")
              id
              $ find (\fh -> f == fhFileName fh) $ archiveFileHeaders ar

    decomp =
        case fhCompressionMethod fileHeader of
          NoCompression -> CL.map id
          Deflate       -> decompress $ WindowBits (-15)


sinkFile :: MonadResource m
         => Archive -> FilePath -> UTCTime -> Sink ByteString m Archive
sinkFile ar f time = do
    h  <- liftIO $ openFile (archiveFilePath ar) WriteMode
    fh <- liftIO $ writeStart h ar f time
    dd <- sinkData h
    liftIO $ writeDataDescriptor' h dd offset
    let ar' = updateArchive ar fh dd
    liftIO $ writeFinish h ar'
    liftIO $ hClose h
    return ar'
  where
    offset = fromIntegral $ archiveCentralDirectoryOffset ar  -- FIXME: old offset!


------------------------------------------------------------------------------
addFiles :: Archive -> [FilePath] -> IO Archive
addFiles ar fs = do
    withFile (archiveFilePath ar) ReadWriteMode $ \h -> do
        ar' <- foldM (addFile h) ar fs
        writeFinish h ar'
        return ar'


extractFiles :: Archive -> [FilePath] -> FilePath -> IO ()
extractFiles ar fs dir = do
    forM_ fs $ \fileName -> do
        print $ "\n" ++ fileName
        createDirectoryIfMissing True $ dir </> takeDirectory fileName
        runResourceT $ sourceFile ar fileName $$ CB.sinkFile (dir </> fileName)


------------------------------------------------------------------------------
addFile :: Handle -> Archive -> FilePath -> IO Archive
addFile h ar f = do
    m  <- clockTimeToUTCTime <$> getModificationTime f
    fh <- writeStart h ar (dropDrive f) m
    dd <- runResourceT $ CB.sourceFile f $$ sinkData h
    writeDataDescriptor' h dd offset
    return $ updateArchive ar fh dd
  where
    offset = fromIntegral $ archiveCentralDirectoryOffset ar  -- FIXME: old offset!


-- | Appends 'LocalFileHeader' to the 'Archive'.
writeStart :: Handle -> Archive -> FilePath -> UTCTime -> IO FileHeader
writeStart h ar f time = do
    hSeek h AbsoluteSeek offset
    writeLocalFileHeader h fh
    return fh
  where
    offset = fromIntegral $ archiveCentralDirectoryOffset ar
    fh = mkFileHeader f time (fromIntegral offset)


mkFileHeader :: FilePath -> UTCTime -> Word32 -> FileHeader
mkFileHeader f lastModified relativeOffset =
    FileHeader
      { fhBitFlag                = 2  -- max compression + data descriptor
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
        CL.zipSinks sizeCrc32Sink
                    sizeCompressSink
    return $ DataDescriptor
               { ddCRC32            = crc32
               , ddCompressedSize   = fromIntegral compressedSize
               , ddUncompressedSize = fromIntegral uncompressedSize
               }
  where
    sizeCrc32Sink :: MonadResource m => Sink ByteString m (Int, Word32)
    sizeCrc32Sink = CL.zipSinks sizeSink crc32Sink

    sizeDataSink :: MonadResource m => Sink ByteString m Int
    sizeDataSink  = fst <$> CL.zipSinks sizeSink (CB.sinkHandle h)

    sizeCompressSink :: MonadResource m => Sink ByteString m Int
    sizeCompressSink = compress 6 (WindowBits (-15)) =$ sizeDataSink


writeDataDescriptor' :: Handle -> DataDescriptor -> Integer -> IO ()
writeDataDescriptor' h dd offset = do
    old <- hTell h
    hSeek h AbsoluteSeek $ offset + 4 + 2 + 2 + 2 + 2 + 2
    writeDataDescriptor h dd
    hSeek h AbsoluteSeek old


updateArchive :: Archive -> FileHeader -> DataDescriptor -> Archive
updateArchive ar fh dd =
    ar { archiveFileHeaders = (archiveFileHeaders ar)
                           ++ [ fh { fhCRC32            = ddCRC32 dd
                                   , fhCompressedSize   = ddCompressedSize dd
                                   , fhUncompressedSize = ddUncompressedSize dd
                                   } ]
       , archiveCentralDirectoryOffset = (archiveCentralDirectoryOffset ar) + (fromIntegral $ localFileHeaderLength fh + ddCompressedSize dd) -- the last is datadescriptor size
       }


writeFinish :: Handle -> Archive -> IO ()
writeFinish h ar = do
    writeCentralDirectory h $ CentralDirectory (archiveFileHeaders ar)  -- FIXME: CentralDirectory?
    writeEnd h
             (length $ archiveFileHeaders ar)
             (sum $ map fileHeaderLength $ archiveFileHeaders ar)
             (archiveCentralDirectoryOffset ar)
