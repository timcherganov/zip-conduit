{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           System.Directory
import           System.FilePath
import           System.IO

import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Time
import           System.IO.Temp
import           Test.Framework
import           Test.Framework.Providers.HUnit
--import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit hiding (Test, path)
--import           Test.QuickCheck

import           Codec.Archive.Zip


main :: IO ()
main = defaultMain tests


tests :: [Test]
tests =
    [ testGroup "cases"
                [ testCase "conduit" assertConduit
                , testCase "files  " assertFiles
                ]
    ]


assertConduit :: Assertion
assertConduit =
    withSystemTempDirectory "zip-conduit" $ \dir -> do
        let archivePath = dir </> archiveName

        archive archivePath fileName content
        result <- unarchive archivePath fileName

        assertEqual "" content result
  where
    archiveName = "test.zip"
    fileName    = "test.txt"
    content     = "some test text really long texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt"


assertFiles :: Assertion
assertFiles = do
    withSystemTempDirectory "zip-conduit" $ \dir -> do
        let ar = emptyArchive $ dir </> archiveName

        filePaths <- putFiles dir filesInfo

        ar' <- addFiles ar filePaths
        print ar'
        ar'' <- readArchive $ dir </> archiveName
        print ar''


        extractFiles ar' (fileNames ar') dir

        result <- getFiles dir

        assertEqual "" filesInfo result
  where
    archiveName  = "test.zip"
    filesInfo = [ ("test1.txt", "some test text")
               , ("test2.txt", "some another test text")
               , ("test3.txt", "one more time")
               ]

    putFiles :: FilePath -> [(FilePath, ByteString)] -> IO [FilePath]
    putFiles dir fileInfo =
        forM fileInfo $ \(name, content) -> do
            let path = dir </> name
            withFile path WriteMode $ \h -> do
                B.hPut h content
                return path

    getFiles :: FilePath -> IO [(FilePath, ByteString)]
    getFiles dir = do
        let path = dir </> dropDrive dir
        dirContents <- getDirectoryContents path
        let resultFiles = map (path </>) $ filter (`notElem` [".", ".."]) dirContents
        forM resultFiles $ \file -> do
            content <- withFile file ReadMode B.hGetContents
            return (takeFileName file, content)


-- | Creates new archive at 'archivePath' and puts there file with
-- 'content'.
archive :: FilePath -> FilePath -> ByteString -> IO Archive
archive archivePath fileName content = do
    time <- getCurrentTime
    runResourceT $ CL.sourceList [content] $$ sinkFile ar fileName time
  where
    ar = emptyArchive archivePath


-- | Gets content from 'fileName' in archive at 'arcihvePath'.
unarchive :: FilePath -> FilePath -> IO ByteString
unarchive archivePath fileName = do
    ar <- readArchive archivePath
    runResourceT $ sourceFile ar fileName $$ CL.fold B.append ""


------------------------------------------------------------------------------
-- instance Arbitrary Archive where
--     arbitrary =
--         return $ Archive
--                    { archiveFilePath               = arbitrary
--                    , archiveFileHeaders            = arbitrary
--                    , archiveCentralDirectoryOffset = arbitrary
--                    , archiveComment                = arbitrary
--                    }

-- instance Arbitrary FileHeader where
--     arbitrary =
--         return $ FileHeader
--                    {

--prop_FileNames = 

-- t = quickCheck (prop_idempotent :: [Int] -> Bool)
-- t' = verboseCheck (prop_idempotent :: [Int] -> Bool)

-- prop_idempotent xs = qsort (qsort xs) == qsort xs

-- prop_minimum :: Ord a => [a] -> Property
-- prop_minimum xs = not (null xs) ==> head (qsort xs) == minimum xs


-- qsort :: Ord a => [a] -> [a]
-- qsort [] = []
-- qsort (x:xs) =
--     qsort lhs ++ [x] ++ qsort rhs
--   where
--     lhs = filter (< x) xs
--     rhs = filter (>= x) xs
