{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Time
import           System.Directory
import           System.FilePath
import           System.IO

import           Data.Conduit
import qualified Data.Conduit.List as CL
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
    content     = "some not really long test text"


assertFiles :: Assertion
assertFiles =
    withSystemTempDirectory "zip-conduit" $ \dir -> do
        -- create files
        filePaths <- putFiles dir filesInfo

        -- archive and unarchive
        let ar = emptyArchive $ dir </> archiveName
        ar' <- addFiles ar filePaths
        extractFiles ar' (fileNames ar') dir

        -- read unarchived files
        result <- getFiles dir

        -- compare
        assertEqual "" filesInfo result
  where
    archiveName  = "test.zip"
    filesInfo = [ ("test1.txt", "some test text")
                , ("test2.txt", "some another test text")
                , ("test3.txt", "one more")
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
