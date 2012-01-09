{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import System.Environment ( getArgs )
import Test.Framework

import Codec.Zlib
import Codec.Compression.Zlib
import qualified Codec.Compression.GZip as Gzip
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.UTF8 as SU8
import qualified Data.ByteString.Lazy as L
import Control.Monad (foldM)
import System.IO.Unsafe (unsafePerformIO)

decompress' :: L.ByteString -> L.ByteString
decompress' gziped = unsafePerformIO $ do
    inf <- initInflate defaultWindowBits
    ungziped <- foldM (go' inf) id $ L.toChunks gziped
    final <- finishInflate inf
    return $ L.fromChunks $ ungziped [final]
  where
    go' inf front bs = withInflateInput inf bs $ go front
    go front x = do
        y <- x
        case y of
            Nothing -> return front
            Just z -> go (front . (:) z) x

prop_decompress' :: L.ByteString -> Bool
prop_decompress' lbs = lbs == decompress' (compress lbs)

compress' :: L.ByteString -> L.ByteString
compress' raw = unsafePerformIO $ do
    def <- initDeflate 7 defaultWindowBits
    gziped <- foldM (go' def) id $ L.toChunks raw
    gziped' <- finishDeflate def $ go gziped
    return $ L.fromChunks $ gziped' []
  where
    go' def front bs = withDeflateInput def bs $ go front
    go front x = do
        y <- x
        case y of
            Nothing -> return front
            Just z -> go (front . (:) z) x

prop_compress' :: L.ByteString -> Bool
prop_compress' lbs = lbs == decompress (compress' lbs)

license :: S.ByteString
license = S8.filter (/= '\r') $ unsafePerformIO $ S.readFile "LICENSE"

exampleDict = SU8.fromString "INITIALDICTIONARY"

deflateWithDict :: SU8.ByteString -> L.ByteString -> L.ByteString
deflateWithDict dict raw = unsafePerformIO $ do
    def <- initDeflateWithDictionary 7 dict $ WindowBits 15
    compressed <- foldM (go' def) id $ L.toChunks raw
    compressed' <- finishDeflate def $ go compressed
    return $ L.fromChunks $ compressed' []
  where
    go' def front bs = withDeflateInput def bs $ go front
    go front x = do
        y <- x
        case y of
            Nothing -> return front
            Just z -> go (front . (:) z) x

inflateWithDict :: SU8.ByteString -> L.ByteString -> L.ByteString
inflateWithDict dict compressed = unsafePerformIO $ do
    inf <- initInflateWithDictionary (WindowBits 15) dict
    decompressed <- foldM (go' inf) id $ L.toChunks compressed
    final <- finishInflate inf
    return $ L.fromChunks $ decompressed [final]
  where
    go' inf front bs = withInflateInput inf bs $ go front
    go front x = do
        y <- x
        case y of
            Nothing -> return front
            Just z -> go (front . (:) z) x

prop_inflate_deflate_with_dictionary :: L.ByteString -> Bool
prop_inflate_deflate_with_dictionary bs =
    bs == (inflateWithDict exampleDict . deflateWithDict exampleDict) bs

test_license_single_deflate :: Assertion
test_license_single_deflate = do
    def <- initDeflate 8 $ WindowBits 31
    gziped <- withDeflateInput def license $ go id
    gziped' <- finishDeflate def $ go gziped
    let raw' = L.fromChunks [license]
    assertEqual raw' $ Gzip.decompress $ L.fromChunks $ gziped' []
  where
    go front x = do
        y <- x
        case y of
            Nothing -> return front
            Just z -> go (front . (:) z) x

test_fail_deflate_inflate_different_dict :: Assertion
test_fail_deflate_inflate_different_dict = do
    raw <- L.readFile "LICENSE"
    deflated <- return $ deflateWithDict exampleDict raw
    inflated <- return $ inflateWithDict (SU8.drop 1 exampleDict) deflated
    assertBool $ L.null inflated

test_license_single_inflate :: Assertion
test_license_single_inflate = do
    gziped <- S.readFile "LICENSE.gz"
    inf <- initInflate $ WindowBits 31
    ungziped <- withInflateInput inf gziped $ go id
    final <- finishInflate inf
    assertEqual license $ S.concat $ ungziped [final]
  where
    go front x = do
        y <- x
        case y of
            Nothing -> return front
            Just z -> go (front . (:) z) x

test_license_multi_deflate :: Assertion
test_license_multi_deflate = do
    def <- initDeflate 5 $ WindowBits 31
    gziped <- foldM (go' def) id $ map S.singleton $ S.unpack license
    gziped' <- finishDeflate def $ go gziped
    let raw' = L.fromChunks [license]
    assertEqual raw' $ Gzip.decompress $ L.fromChunks $ gziped' []
  where
    go' inf front bs = withDeflateInput inf bs $ go front
    go front x = do
        y <- x
        case y of
            Nothing -> return front
            Just z -> go (front . (:) z) x

test_license_multi_inflate :: Assertion
test_license_multi_inflate = do
    gziped <- S.readFile "LICENSE.gz"
    let gziped' = map S.singleton $ S.unpack gziped
    inf <- initInflate $ WindowBits 31
    ungziped' <- foldM (go' inf) id gziped'
    final <- finishInflate inf
    assertEqual license $ S.concat $ ungziped' [final]
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

prop_lbs_zlib_inflate :: L.ByteString -> Bool
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

prop_lbs_zlib_deflate :: L.ByteString -> Bool
prop_lbs_zlib_deflate lbs = unsafePerformIO $ do
    def <- initDeflate 7 defaultWindowBits
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
