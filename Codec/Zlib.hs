{-# LANGUAGE DeriveDataTypeable #-}
-- | This is a middle-level wrapper around the zlib C API. It allows you to
-- work fully with bytestrings and not touch the FFI at all, but is still
-- low-level enough to allow you to implement high-level abstractions such as
-- enumerators. Significantly, it does not use lazy IO.
--
-- You'll probably need to reference the docs a bit to understand the
-- WindowBits parameters below, but a basic rule of thumb is 15 is for zlib
-- compression, and 31 for gzip compression.
--
-- A simple streaming compressor in pseudo-code would look like:
--
-- > def <- initDeflate ...
-- > withDeflateInput def rawContent1 sendCompressedData
-- > withDeflateInput def rawContent2 sendCompressedData
-- > ...
-- > finishDeflate def sendCompressedData
--
-- You can see a more complete example is available in the included
-- file-test.hs.
module Codec.Zlib
    ( -- * Inflate
      Inflate
    , initInflate
    , initInflateWithDictionary
    , withInflateInput
    , finishInflate
    , flushInflate
      -- * Deflate
    , Deflate
    , initDeflate
    , initDeflateWithDictionary
    , withDeflateInput
    , finishDeflate
    , flushDeflate
      -- * Data types
    , WindowBits (..)
    , defaultWindowBits
    , ZlibException (..)
    ) where

import Codec.Zlib.Lowlevel
import Foreign.ForeignPtr
import Foreign.C.Types
import Data.ByteString.Unsafe
import Codec.Compression.Zlib (WindowBits(WindowBits), defaultWindowBits)
import qualified Data.ByteString as S
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Control.Monad (when)
import Data.Typeable (Typeable)
import Control.Exception (Exception, throwIO)

type ZStreamPair = (ForeignPtr ZStreamStruct, ForeignPtr CChar)

-- | The state of an inflation (eg, decompression) process. All allocated
-- memory is automatically reclaimed by the garbage collector.
-- Also can contain the inflation dictionary that is used for decompression.
newtype Inflate = Inflate (ZStreamPair, Maybe S.ByteString)

-- | The state of a deflation (eg, compression) process. All allocated memory
-- is automatically reclaimed by the garbage collector.
newtype Deflate = Deflate ZStreamPair

-- | Exception that can be thrown from the FFI code. The parameter is the
-- numerical error code from the zlib library. Quoting the zlib.h file
-- directly:
--
-- * #define Z_OK            0
--
-- * #define Z_STREAM_END    1
--
-- * #define Z_NEED_DICT     2
--
-- * #define Z_ERRNO        (-1)
--
-- * #define Z_STREAM_ERROR (-2)
--
-- * #define Z_DATA_ERROR   (-3)
--
-- * #define Z_MEM_ERROR    (-4)
--
-- * #define Z_BUF_ERROR    (-5)
--
-- * #define Z_VERSION_ERROR (-6)

data ZlibException = ZlibException Int
    deriving (Show, Typeable)
instance Exception ZlibException

-- | Some constants for the error codes, used internally
zNeedDict :: CInt
zNeedDict = 2

zBufError :: CInt
zBufError = -5

-- | Initialize an inflation process with the given 'WindowBits'. You will need
-- to call 'withInflateInput' to feed compressed data to this and
-- 'finishInflate' to extract the final chunk of decompressed data.
initInflate :: WindowBits -> IO Inflate
initInflate w = do
    zstr <- zstreamNew
    inflateInit2 zstr w
    fzstr <- newForeignPtr c_free_z_stream_inflate zstr
    fbuff <- mallocForeignPtrBytes defaultChunkSize
    withForeignPtr fbuff $ \buff ->
        c_set_avail_out zstr buff $ fromIntegral defaultChunkSize
    return $ Inflate ((fzstr, fbuff), Nothing)

-- | Initialize an inflation process with the given 'WindowBits'. 
-- Unlike initInflate a dictionary for inflation is set which must
-- match the one set during compression.
initInflateWithDictionary :: WindowBits -> S.ByteString -> IO Inflate
initInflateWithDictionary w bs = do
    zstr <- zstreamNew
    inflateInit2 zstr w
    fzstr <- newForeignPtr c_free_z_stream_inflate zstr
    fbuff <- mallocForeignPtrBytes defaultChunkSize

    withForeignPtr fbuff $ \buff ->
        c_set_avail_out zstr buff $ fromIntegral defaultChunkSize
    return $ Inflate ((fzstr, fbuff), Just bs)

-- | Initialize a deflation process with the given compression level and
-- 'WindowBits'. You will need to call 'withDeflateInput' to feed uncompressed
-- data to this and 'finishDeflate' to extract the final chunks of compressed
-- data.
initDeflate :: Int -- ^ Compression level
            -> WindowBits -> IO Deflate
initDeflate level w = do
    zstr <- zstreamNew
    deflateInit2 zstr level w 8 StrategyDefault
    fzstr <- newForeignPtr c_free_z_stream_deflate zstr
    fbuff <- mallocForeignPtrBytes defaultChunkSize
    withForeignPtr fbuff $ \buff ->
        c_set_avail_out zstr buff $ fromIntegral defaultChunkSize
    return $ Deflate (fzstr, fbuff)

-- | Initialize an deflation process with the given compression level and
-- 'WindowBits'.
-- Unlike initDeflate a dictionary for deflation is set.
initDeflateWithDictionary :: Int -- ^ Compression level
			  -> S.ByteString -- ^ Deflate dictionary
			  -> WindowBits -> IO Deflate
initDeflateWithDictionary level bs w = do
    zstr <- zstreamNew
    deflateInit2 zstr level w 8 StrategyDefault
    fzstr <- newForeignPtr c_free_z_stream_deflate zstr
    fbuff <- mallocForeignPtrBytes defaultChunkSize

    unsafeUseAsCStringLen bs $ \(cstr, len) -> do
        c_call_deflate_set_dictionary zstr cstr $ fromIntegral len

    withForeignPtr fbuff $ \buff ->
        c_set_avail_out zstr buff $ fromIntegral defaultChunkSize
    return $ Deflate (fzstr, fbuff)

-- | Feed the given 'S.ByteString' to the inflater. This function takes a
-- function argument which takes a \"popper\". A popper is an IO action that
-- will return the next bit of inflated data, returning 'Nothing' when there is
-- no more data to be popped.
--
-- Note that this function automatically buffers the output to
-- 'defaultChunkSize', and therefore you won't get any data from the popper
-- until that much decompressed data is available. After you have fed all of
-- the compressed data to this function, you can extract your final chunk of
-- decompressed data using 'finishInflate'.
withInflateInput
    :: Inflate -> S.ByteString -> (IO (Maybe S.ByteString) -> IO a)
    -> IO a
withInflateInput (Inflate ((fzstr, fbuff), inflateDictionary)) bs f =
    withForeignPtr fzstr $ \zstr ->
        unsafeUseAsCStringLen bs $ \(cstr, len) -> do
            c_set_avail_in zstr cstr $ fromIntegral len
            f $ drain fbuff zstr inflate False
  where
    inflate zstr = do
	res <- c_call_inflate_noflush zstr
	if (res == zNeedDict)
	    then maybe (throwIO $ ZlibException $ fromIntegral zNeedDict) -- no dictionary supplied so throw error
		       (\dict -> (unsafeUseAsCStringLen dict $ \(cstr, len) -> do
				    c_call_inflate_set_dictionary zstr cstr $ fromIntegral len
				    c_call_inflate_noflush zstr))
		       inflateDictionary
	    else return res

drain :: ForeignPtr CChar -> ZStream' -> (ZStream' -> IO CInt) -> Bool
      -> IO (Maybe S.ByteString)
drain fbuff zstr func isFinish = do
    a <- c_get_avail_in zstr
    if a == 0 && not isFinish
        then return Nothing
        else withForeignPtr fbuff $ \buff -> do
            res <- func zstr
            when (res < 0 && res /= zBufError)
                $ throwIO $ ZlibException $ fromIntegral res
            avail <- c_get_avail_out zstr
            let size = defaultChunkSize - fromIntegral avail
            let toOutput = avail == 0 || (isFinish && size /= 0)
            if toOutput
                then do
                    bs <- S.packCStringLen (buff, size)
                    c_set_avail_out zstr buff
                        $ fromIntegral defaultChunkSize
                    return $ Just bs
                else return Nothing


-- | As explained in 'withInflateInput', inflation buffers your decompressed
-- data. After you call 'withInflateInput' with your last chunk of compressed
-- data, you will likely have some data still sitting in the buffer. This
-- function will return it to you.
finishInflate :: Inflate -> IO S.ByteString
finishInflate (Inflate ((fzstr, fbuff), _)) =
    withForeignPtr fzstr $ \zstr ->
        withForeignPtr fbuff $ \buff -> do
            avail <- c_get_avail_out zstr
            let size = defaultChunkSize - fromIntegral avail
            bs <- S.packCStringLen (buff, size)
            c_set_avail_out zstr buff
                $ fromIntegral defaultChunkSize
            return bs

-- | Flush the inflation buffer. Useful for interactive application.
--
-- This is actually a synonym for 'finishInflate'. It is provided for its more
-- semantic name.
--
-- Since 0.0.3
flushInflate :: Inflate -> IO S.ByteString
flushInflate = finishInflate

-- | Feed the given 'S.ByteString' to the deflater. This function takes a
-- function argument which takes a \"popper\". A popper is an IO action that
-- will return the next bit of deflated data, returning 'Nothing' when there is
-- no more data to be popped.
--
-- Note that this function automatically buffers the output to
-- 'defaultChunkSize', and therefore you won't get any data from the popper
-- until that much compressed data is available. After you have fed all of the
-- decompressed data to this function, you can extract your final chunks of
-- compressed data using 'finishDeflate'.
withDeflateInput
    :: Deflate -> S.ByteString -> (IO (Maybe S.ByteString) -> IO a) -> IO a
withDeflateInput (Deflate (fzstr, fbuff)) bs f =
    withForeignPtr fzstr $ \zstr ->
        unsafeUseAsCStringLen bs $ \(cstr, len) -> do
            c_set_avail_in zstr cstr $ fromIntegral len
            f $ drain fbuff zstr c_call_deflate_noflush False

-- | As explained in 'withDeflateInput', deflation buffers your compressed
-- data. After you call 'withDeflateInput' with your last chunk of decompressed
-- data, we need to flush the rest of the data waiting to be deflated. This
-- function takes a function parameter which accepts a \"popper\", just like
-- 'withDeflateInput'.
finishDeflate :: Deflate -> (IO (Maybe S.ByteString) -> IO a) -> IO a
finishDeflate (Deflate (fzstr, fbuff)) f =
    withForeignPtr fzstr $ \zstr ->
        f $ drain fbuff zstr c_call_deflate_finish True

-- | Flush the deflation buffer. Useful for interactive application.
--
-- Internally this passes Z_SYNC_FLUSH to the zlib library.
--
-- Since 0.0.3
flushDeflate :: Deflate -> (IO (Maybe S.ByteString) -> IO a) -> IO a
flushDeflate (Deflate (fzstr, fbuff)) f =
    withForeignPtr fzstr $ \zstr ->
        f $ drain fbuff zstr c_call_deflate_flush True
