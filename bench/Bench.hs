{-# LANGUAGE PackageImports #-}

-- cabal-dev bench --benchmark-option="-o bench.html"

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as B
import           Data.Word
import           System.FilePath
import           System.IO

import           Criterion.Config
import           Criterion.Main
import           Criterion.Monad
import           System.IO.Temp
import           System.Random

import qualified Codec.Archive.LibZip as L
import qualified "zip-archive" Codec.Archive.Zip as A
import           "zip-conduit" Codec.Archive.Zip
--import           Codec.Archive.Zip


main :: IO ()
main = do
    let sizes = [1024*1024, 10*1024*1024]    -- ^ sizes of files for benchmarking

    withSystemTempDirectory "zip-conduit" $ \dir ->
        defaultMainWith myConfig
                        (prepareFiles dir sizes)
                        (prepareBench dir $ map show sizes)


myConfig :: Config
myConfig = defaultConfig {
             cfgPerformGC = ljust True  -- ^ always GC between runs
           }


-- | Prepares benchmarks.
prepareBench :: FilePath     -- ^ the path to the directory with files
             -> [FilePath]   -- ^ file names
             -> [Benchmark]
prepareBench dir names =
    [ bgroup "zip-conduit" $
             map (\name -> bench name $ zipConduit dir name)
                 names
    , bgroup "zip-archive" $
             map (\name -> bench name $ zipArchive dir name)
                 names
    , bgroup "libZip" $
             map (\name -> bench name $ libZip dir name)
                 names
    ]


-- | Creates source files for archiving. File name is the size of
-- this file in bytes.
prepareFiles :: FilePath      -- ^ the path to the directory for files
             -> [Int]         -- ^ sizes of files to create
             -> Criterion ()
prepareFiles dir sizes = liftIO $
    forM_ sizes $ \s -> createFile (dir </> show s) s


-- | Creates a file of specified length with random content.
createFile :: FilePath -> Int -> IO ()
createFile path size =
    withFile path WriteMode $ \h -> do
        g <- getStdGen
        B.hPut h $ B.pack $ take size (randoms g :: [Word8])


------------------------------------------------------------------------------
-- Create zip archive with three different packages.

zipConduit :: FilePath -> FilePath -> IO ()
zipConduit dir name =
    withTempDirectory dir "zip-conduit" $ \tmpDir -> do
        let ar = emptyArchive $ tmpDir </> name <.> "zip"
        addFiles ar [dir </> name]
        return ()


zipArchive :: FilePath -> FilePath -> IO ()
zipArchive dir name =
    withTempDirectory dir "zip-conduit" $ \tmpDir -> do
        ar' <- A.addFilesToArchive [] ar [dir </> name]
        withFile (tmpDir </> name <.> "zip") WriteMode $ \h ->
            B.hPut h $ A.fromArchive ar'
  where
    ar = A.emptyArchive


libZip :: FilePath -> FilePath -> IO ()
libZip dir name =
    withTempDirectory dir "zip-conduit" $ \tmpDir -> do
        L.withArchive [L.CreateFlag] (tmpDir </> name <.> "zip") $ do
           zs <- L.sourceFile (dir </> name) 0 0
           L.addFile (dir </> name) zs
        return ()
