{-# OPTIONS_GHC -F -pgmF htfpp #-}

import System.Environment ( getArgs )
import Test.Framework

import Codec.Zlib
import Codec.Compression.Zlib
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Control.Monad (foldM)
import System.IO.Unsafe (unsafePerformIO)

test_license_single = do
    gziped <- S.readFile "LICENSE.gz"
    inf <- initInflate $ WindowBits 31
    ungziped <- withInflateInput inf gziped $ go id
    raw <- S.readFile "LICENSE"
    assertEqual raw ungziped
  where
    go front x = do
        y <- x
        case y of
            Nothing -> return $ S.concat $ front []
            Just z -> go (front . (:) z) x

test_license_multi = do
    gziped <- S.readFile "LICENSE.gz"
    let gziped' = map S.singleton $ S.unpack gziped
    inf <- initInflate $ WindowBits 31
    ungziped' <- foldM (go' inf) id gziped'
    raw <- S.readFile "LICENSE"
    assertEqual raw $ S.concat $ ungziped' []
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

prop_lbs_zlib lbs = unsafePerformIO $ do
    let glbs = compress lbs
    inf <- initInflate defaultWindowBits
    inflated <- foldM (go' inf) id $ L.toChunks glbs
    return $ lbs == L.fromChunks (inflated [])
  where
    go' inf front bs = withInflateInput inf bs $ go front
    go front x = do
        y <- x
        case y of
            Nothing -> return front
            Just z -> go (front . (:) z) x

main = do
    args <- getArgs
    runTestWithArgs args allHTFTests
