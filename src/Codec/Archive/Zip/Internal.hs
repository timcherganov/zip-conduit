module Codec.Archive.Zip.Internal where

import           Prelude hiding (readFile)
import           Control.Applicative hiding (many)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Serialize hiding (get)
import           System.IO hiding (readFile)

import           Control.Monad.Error
import           Data.ByteString.UTF8 (toString)

import           Codec.Archive.Zip.Util


calculateFileDataOffset :: Handle -> FileHeader -> IO Integer
calculateFileDataOffset h fh = do
    lfhLength <- readLocalFileHeaderLength h fh
    return . fromIntegral $ fhRelativeOffset fh + lfhLength


------------------------------------------------------------------------------
-- Local file header:
--
-- local file header signature     4 bytes  (0x04034b50)
-- version needed to extract       2 bytes
-- general purpose bit flag        2 bytes
-- compression method              2 bytes
-- last mod file time              2 bytes
-- last mod file date              2 bytes
-- crc-32                          4 bytes
-- compressed size                 4 bytes
-- uncompressed size               4 bytes
-- file name length                2 bytes
-- extra field length              2 bytes
--
-- file name (variable size)
-- extra field (variable size)

localFileHeaderConstantLength :: Int
localFileHeaderConstantLength = 30


readLocalFileHeaderLength :: Handle -> FileHeader -> IO Int
readLocalFileHeaderLength h header = do
    runGet' getLocalFileHeaderLength <$> hGetLocalFileHeader h header


-- Gets length of the local file header, i.e. sum of lengths of its
-- constant and variable parts.
getLocalFileHeaderLength :: Get Int
getLocalFileHeaderLength = do
    signature 0x04034b50
    skip $ 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4
    fileNameLength    <- fromIntegral <$> getWord16le
    extraFieldLength  <- fromIntegral <$> getWord16le

    return $ localFileHeaderConstantLength
           + fileNameLength
           + extraFieldLength


-- Gets constant part of the local file header.
hGetLocalFileHeader :: Handle -> FileHeader -> IO ByteString
hGetLocalFileHeader h fh = do
    hSeek h AbsoluteSeek offset
    B.hGet h localFileHeaderConstantLength
  where
    offset = fromIntegral $ fhRelativeOffset fh


------------------------------------------------------------------------------
-- Central directory structure:
--
-- [file header 1]
-- .
-- .
-- .
-- [file header n]
-- [digital signature]

data CentralDirectory = CentralDirectory
    { cdFileHeaders      :: [FileHeader]
    , cdDigitalSignature :: Maybe ByteString
    } deriving (Show)


readCentralDirectory :: Handle -> End -> IO CentralDirectory
readCentralDirectory h e = do
    runGet' getCentralDirectory <$> hGetCentralDirectory h e


getCentralDirectory :: Get CentralDirectory
getCentralDirectory = do
    headers <- many . maybeEmpty $ getFileHeader
    return $ CentralDirectory
               { cdFileHeaders      = headers
               , cdDigitalSignature = Nothing
               }


hGetCentralDirectory :: Handle -> End -> IO ByteString
hGetCentralDirectory h e = do
    hSeek h AbsoluteSeek $ fromIntegral offset
    B.hGet h size
  where
    size   = endCentralDirectorySize e
    offset = endCentralDirectoryOffset e


------------------------------------------------------------------------------
-- File header:
--
-- central file header signature   4 bytes  (0x02014b50)
-- version made by                 2 bytes
-- version needed to extract       2 bytes
-- general purpose bit flag        2 bytes
-- compression method              2 bytes
-- last mod file time              2 bytes
-- last mod file date              2 bytes
-- crc-32                          4 bytes
-- compressed size                 4 bytes
-- uncompressed size               4 bytes
-- file name length                2 bytes
-- extra field length              2 bytes
-- file comment length             2 bytes
-- disk number start               2 bytes
-- internal file attributes        2 bytes
-- external file attributes        4 bytes
-- relative offset of local header 4 bytes

-- file name (variable size)
-- extra field (variable size)
-- file comment (variable size)

data FileHeader = FileHeader
    { fhCompressedSize   :: Int
    , fhRelativeOffset   :: Int
    , fhFileName         :: FilePath
    } deriving (Show)


getFileHeader :: Get FileHeader
getFileHeader = do
    signature 0x02014b50
    skip 2
    versionNeededToExtract <- getWord16le
    unless (versionNeededToExtract <= 20) $
        fail "This archive requires zip >= 2.0 to extract."
    skip $ 2 + 2 + 2 + 2 + 4
    compressedSize    <- fromIntegral <$> getWord32le
    skip 4
    fileNameLength    <- fromIntegral <$> getWord16le
    extraFieldLength  <- fromIntegral <$> getWord16le
    fileCommentLength <- fromIntegral <$> getWord16le
    skip $ 2 + 2 + 4
    relativeOffset    <- fromIntegral <$> getWord32le
    fileName          <- getByteString fileNameLength
    skip $ extraFieldLength + fileCommentLength
    return $ FileHeader
               { fhCompressedSize   = compressedSize
               , fhRelativeOffset   = relativeOffset
               , fhFileName         = toString fileName
               }


------------------------------------------------------------------------------
-- End of central directory record:
--
-- end of central dir signature    4 bytes  (0x06054b50)
-- number of this disk             2 bytes
-- number of the disk with the
-- start of the central directory  2 bytes
-- total number of entries in the
-- central directory on this disk  2 bytes
-- total number of entries in
-- the central directory           2 bytes
-- size of the central directory   4 bytes
-- offset of start of central
-- directory with respect to
-- the starting disk number        4 bytes
-- .ZIP file comment length        2 bytes
-- .ZIP file comment       (variable size)

data End = End
    { endCentralDirectorySize   :: Int
    , endCentralDirectoryOffset :: Int
    , endZipComment             :: ByteString
    } deriving (Show)


readEnd :: Handle -> IO End
readEnd h =
    runGet' getEnd <$> hGetEnd h


getEnd :: Get End
getEnd = do
   skip $ 2 + 2 + 2 + 2
   size          <- fromIntegral <$> getWord32le
   offset        <- fromIntegral <$> getWord32le
   commentLength <- fromIntegral <$> getWord16le
   comment       <- getByteString commentLength
   return $ End
              { endCentralDirectorySize   = size
              , endCentralDirectoryOffset = offset
              , endZipComment             = comment
              }


-- TODO: find a better way to find the end of central dir signature
hGetEnd :: Handle -> IO ByteString
hGetEnd h = do
    hSeek h SeekFromEnd (-4)
    loop
  where
    loop = do
        s <- B.hGet h 4

        if s == B.pack (reverse [0x06, 0x05, 0x4b, 0x50])
          then get
          else next

    get = do
        size   <- hFileSize h
        offset <- hTell h
        B.hGet h $ fromIntegral (size - offset)

    next = do
        pos <- hTell h
        if pos < 1000
          then return B.empty
          else do
              hSeek h RelativeSeek (-5)
              loop
