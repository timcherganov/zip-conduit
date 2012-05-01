module Codec.Archive.Zip
    ( Archive
    , readArchive
    , fileNames
    , sourceFile
    ) where

import           Prelude hiding (readFile)
import           Data.ByteString (ByteString)
import           Data.List (find)
import           System.IO hiding (readFile)

import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import           Data.Conduit.Lazy
import           Data.Conduit.Zlib

import           Codec.Archive.Zip.Internal


data Archive = Archive
    { archiveFilePath    :: FilePath
    , archiveFileHeaders :: [FileHeader]
    , archiveComment     :: ByteString
    } deriving (Show)


readArchive :: FilePath -> IO Archive
readArchive f =
    withFile f ReadMode $ \h -> do
        e  <- readEnd h
        cd <- readCentralDirectory h e
        return $ Archive
                   { archiveFilePath    = f
                   , archiveFileHeaders = cdFileHeaders cd
                   , archiveComment     = endZipComment e
                   }


fileNames :: Archive -> [FilePath]
fileNames = (map fhFileName) . archiveFileHeaders


sourceFile :: MonadResource m => Archive -> FilePath -> Source m ByteString
sourceFile ar f =
    source $= (decompress $ WindowBits (-15))
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
