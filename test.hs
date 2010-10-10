{-# OPTIONS_GHC -F -pgmF htfpp #-}

import System.Environment ( getArgs )
import Test.Framework

import Codec.Zlib
import Codec.Compression.Zlib
import qualified Codec.Compression.GZip as Gzip
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Control.Monad (foldM)
import System.IO.Unsafe (unsafePerformIO)

test_license_single_deflate = do
    raw <- S.readFile "LICENSE"
    def <- initDeflate $ WindowBits 31
    gziped <- withDeflateInput def raw $ go id
    gziped' <- finishDeflate def $ go gziped
    let raw' = L.fromChunks [raw]
    assertEqual raw' $ Gzip.decompress $ L.fromChunks $ gziped' []
  where
    go front x = do
        y <- x
        case y of
            Nothing -> return front
            Just z -> go (front . (:) z) x

test_license_single_inflate = do
    gziped <- S.readFile "LICENSE.gz"
    inf <- initInflate $ WindowBits 31
    ungziped <- withInflateInput inf gziped $ go id
    final <- finishInflate inf
    raw <- S.readFile "LICENSE"
    assertEqual raw $ S.concat $ ungziped [final]
  where
    go front x = do
        y <- x
        case y of
            Nothing -> return front
            Just z -> go (front . (:) z) x

test_license_multi_deflate = do
    raw <- S.readFile "LICENSE"
    def <- initDeflate $ WindowBits 31
    gziped <- foldM (go' def) id $ map S.singleton $ S.unpack raw
    gziped' <- finishDeflate def $ go gziped
    let raw' = L.fromChunks [raw]
    assertEqual raw' $ Gzip.decompress $ L.fromChunks $ gziped' []
  where
    go' inf front bs = withDeflateInput inf bs $ go front
    go front x = do
        y <- x
        case y of
            Nothing -> return front
            Just z -> go (front . (:) z) x

test_license_multi_inflate = do
    gziped <- S.readFile "LICENSE.gz"
    let gziped' = map S.singleton $ S.unpack gziped
    inf <- initInflate $ WindowBits 31
    ungziped' <- foldM (go' inf) id gziped'
    raw <- S.readFile "LICENSE"
    final <- finishInflate inf
    assertEqual raw $ S.concat $ ungziped' [final]
  where
    go' inf front bs = withInflateInput inf bs $ go front
    go front x = do
        y <- x
        case y of
            Nothing -> return front
            Just z -> go (front . (:) z) x

instance Arbitrary L.ByteString where
    arbitrary = L.fromChunks `fmap` arbitrary
instance Arbitrary S.ByteString where
    arbitrary = S.pack `fmap` arbitrary

prop_lbs_zlib_inflate lbs = unsafePerformIO $ do
    let glbs = compress lbs
    inf <- initInflate defaultWindowBits
    inflated <- foldM (go' inf) id $ L.toChunks glbs
    final <- finishInflate inf
    return $ lbs == L.fromChunks (inflated [final])
  where
    go' inf front bs = withInflateInput inf bs $ go front
    go front x = do
        y <- x
        case y of
            Nothing -> return front
            Just z -> go (front . (:) z) x

prop_lbs_zlib_deflate lbs = unsafePerformIO $ do
    def <- initDeflate defaultWindowBits
    deflated <- foldM (go' def) id $ L.toChunks lbs
    deflated' <- finishDeflate def $ go deflated
    return $ lbs == decompress (L.fromChunks (deflated' []))
  where
    go' inf front bs = withDeflateInput inf bs $ go front
    go front x = do
        y <- x
        case y of
            Nothing -> return front
            Just z -> go (front . (:) z) x

main = do
    args <- getArgs
    runTestWithArgs args allHTFTests
