{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..))
import Test.HUnit

import Codec.Zlib
import Codec.Compression.Zlib
import qualified Codec.Compression.GZip as Gzip
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
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

instance Arbitrary L.ByteString where
    arbitrary = L.fromChunks `fmap` arbitrary
instance Arbitrary S.ByteString where
    arbitrary = S.pack `fmap` arbitrary

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

license :: S.ByteString
license = S8.filter (/= '\r') $ unsafePerformIO $ S.readFile "LICENSE"

exampleDict :: S.ByteString
exampleDict = "INITIALDICTIONARY"

deflateWithDict :: S.ByteString -> L.ByteString -> L.ByteString
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

inflateWithDict :: S.ByteString -> L.ByteString -> L.ByteString
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

main :: IO ()
main = hspecX $ do
    describe "inflate/deflate" $ do
        prop "decompress'" $ \lbs -> lbs == decompress' (compress lbs)
        prop "compress'" $ \lbs -> lbs == decompress (compress' lbs)

        prop "with dictionary" $ \bs ->
            bs ==
            (inflateWithDict exampleDict . deflateWithDict exampleDict) bs
        it "different dict" $ do
            raw <- L.readFile "LICENSE"
            deflated <- return $ deflateWithDict exampleDict raw
            inflated <- return $ inflateWithDict (S.drop 1 exampleDict) deflated
            assertBool "is null" $ L.null inflated

    describe "license" $ do
        it "single deflate" $ do
            let go front x = do
                    y <- x
                    case y of
                        Nothing -> return front
                        Just z -> go (front . (:) z) x
            def <- initDeflate 8 $ WindowBits 31
            gziped <- withDeflateInput def license $ go id
            gziped' <- finishDeflate def $ go gziped
            let raw' = L.fromChunks [license]
            raw' @?= Gzip.decompress (L.fromChunks $ gziped' [])

        it "single inflate" $ do
            let go front x = do
                    y <- x
                    case y of
                        Nothing -> return front
                        Just z -> go (front . (:) z) x
            gziped <- S.readFile "LICENSE.gz"
            inf <- initInflate $ WindowBits 31
            ungziped <- withInflateInput inf gziped $ go id
            final <- finishInflate inf
            license @?= (S.concat $ ungziped [final])

        it "multi deflate" $ do
            let go' inf front bs = withDeflateInput inf bs $ go front
                go front x = do
                    y <- x
                    case y of
                        Nothing -> return front
                        Just z -> go (front . (:) z) x
            def <- initDeflate 5 $ WindowBits 31
            gziped <- foldM (go' def) id $ map S.singleton $ S.unpack license
            gziped' <- finishDeflate def $ go gziped
            let raw' = L.fromChunks [license]
            raw' @?= (Gzip.decompress $ L.fromChunks $ gziped' [])

        it "multi inflate" $ do
            let go' inf front bs = withInflateInput inf bs $ go front
                go front x = do
                    y <- x
                    case y of
                        Nothing -> return front
                        Just z -> go (front . (:) z) x
            gziped <- S.readFile "LICENSE.gz"
            let gziped' = map S.singleton $ S.unpack gziped
            inf <- initInflate $ WindowBits 31
            ungziped' <- foldM (go' inf) id gziped'
            final <- finishInflate inf
            license @?= (S.concat $ ungziped' [final])

    describe "lbs zlib" $ do
        prop "inflate" $ \lbs -> unsafePerformIO $ do
            let glbs = compress lbs
                go' inf front bs = withInflateInput inf bs $ go front
                go front x = do
                    y <- x
                    case y of
                        Nothing -> return front
                        Just z -> go (front . (:) z) x
            inf <- initInflate defaultWindowBits
            inflated <- foldM (go' inf) id $ L.toChunks glbs
            final <- finishInflate inf
            return $ lbs == L.fromChunks (inflated [final])
        prop "deflate" $ \lbs -> unsafePerformIO $ do
            let go' inf front bs = withDeflateInput inf bs $ go front
                go front x = do
                    y <- x
                    case y of
                        Nothing -> return front
                        Just z -> go (front . (:) z) x
            def <- initDeflate 7 defaultWindowBits
            deflated <- foldM (go' def) id $ L.toChunks lbs
            deflated' <- finishDeflate def $ go deflated
            return $ lbs == decompress (L.fromChunks (deflated' []))
