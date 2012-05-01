module Codec.Archive.Zip.Util where

import Control.Applicative hiding (many)
import Data.ByteString (ByteString)
import Data.Word (Word32)

import Data.Serialize.Get


many :: Monad m => m (Maybe a) -> m [a]
many p = do
  r <- p
  case r of
       Just x  -> many p >>= return . (x:)
       Nothing -> return []


maybeEmpty :: Get a -> Get (Maybe a)
maybeEmpty p = do
    e <- isEmpty
    if e
      then return Nothing
      else Just <$> p


runGet' :: Get a -> ByteString -> a
runGet' g b =
    either error id $ runGet g b


signature :: Word32 -> Get ()
signature sig = do
    s <- lookAhead getWord32le
    if s == sig
      then skip 4
      else fail "Wrong signature."
