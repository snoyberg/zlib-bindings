{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
module Codec.Zlib
    ( -- * Data types
      Inflate
    , Deflate
    , WindowBits (..)
      -- * Inflate
    , initInflate
    , withInflateInput
    , finishInflate
      -- * Deflate
    , initDeflate
    , withDeflateInput
    , finishDeflate
    ) where

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (allocaBytes)
import Data.ByteString.Unsafe
import Codec.Compression.Zlib (WindowBits (..))
import qualified Data.ByteString as S
import Data.ByteString.Unsafe
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Control.Monad (when)

data ZStreamStruct
type ZStream' = Ptr ZStreamStruct
type ZStreamPair = (ForeignPtr ZStreamStruct, ForeignPtr CChar)
newtype Inflate = Inflate ZStreamPair
newtype Deflate = Deflate ZStreamPair

foreign import ccall unsafe "create_z_stream_inflate"
    c_create_z_stream_inflate :: CInt -> IO ZStream'

foreign import ccall unsafe "&free_z_stream_inflate"
    c_free_z_stream_inflate :: FunPtr (ZStream' -> IO ())

initInflate :: WindowBits -> IO Inflate
initInflate w = do
    let w' = case w of
                DefaultWindowBits -> 15
                WindowBits i -> fromIntegral i
    zstr <- c_create_z_stream_inflate w'
    fzstr <- newForeignPtr c_free_z_stream_inflate zstr
    fbuff <- mallocForeignPtrBytes defaultChunkSize
    withForeignPtr fbuff $ \buff ->
        c_set_avail_out zstr buff $ fromIntegral defaultChunkSize
    return $ Inflate (fzstr, fbuff)

foreign import ccall unsafe "create_z_stream_deflate"
    c_create_z_stream_deflate :: CInt -> IO ZStream'

foreign import ccall unsafe "&free_z_stream_deflate"
    c_free_z_stream_deflate :: FunPtr (ZStream' -> IO ())

initDeflate :: WindowBits -> IO Deflate
initDeflate w = do
    let w' = case w of
                DefaultWindowBits -> 15
                WindowBits i -> fromIntegral i
    zstr <- c_create_z_stream_deflate w'
    fzstr <- newForeignPtr c_free_z_stream_deflate zstr
    fbuff <- mallocForeignPtrBytes defaultChunkSize
    withForeignPtr fbuff $ \buff ->
        c_set_avail_out zstr buff $ fromIntegral defaultChunkSize
    return $ Deflate (fzstr, fbuff)

foreign import ccall unsafe "set_avail_in"
    c_set_avail_in :: ZStream' -> Ptr CChar -> CUInt -> IO ()

foreign import ccall unsafe "set_avail_out"
    c_set_avail_out :: ZStream' -> Ptr CChar -> CUInt -> IO ()

foreign import ccall unsafe "get_avail_out"
    c_get_avail_out :: ZStream' -> IO CUInt

foreign import ccall unsafe "get_avail_in"
    c_get_avail_in :: ZStream' -> IO CUInt

foreign import ccall unsafe "call_inflate_noflush"
    c_call_inflate_noflush :: ZStream' -> IO CInt

withInflateInput
    :: Inflate -> S.ByteString -> (IO (Maybe S.ByteString) -> IO a)
    -> IO a
withInflateInput (Inflate (fzstr, fbuff)) bs f =
    withForeignPtr fzstr $ \zstr ->
        unsafeUseAsCStringLen bs $ \(cstr, len) -> do
            c_set_avail_in zstr cstr $ fromIntegral len
            f $ drain fbuff zstr c_call_inflate_noflush False

drain fbuff zstr func isFinish = do
    a <- c_get_avail_in zstr
    if a == 0 && not isFinish
        then return Nothing
        else withForeignPtr fbuff $ \buff -> do
            res <- func zstr
            when (res < 0 && res /= (-5)) $ error -- FIXME
                $ "zlib: Error in underlying stream: " ++ show res
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

finishInflate :: Inflate -> IO S.ByteString
finishInflate (Inflate (fzstr, fbuff)) =
    withForeignPtr fzstr $ \zstr ->
        withForeignPtr fbuff $ \buff -> do
            avail <- c_get_avail_out zstr
            let size = defaultChunkSize - fromIntegral avail
            S.packCStringLen (buff, size)

foreign import ccall unsafe "call_deflate_noflush"
    c_call_deflate_noflush :: ZStream' -> IO CInt

withDeflateInput
    :: Deflate -> S.ByteString -> (IO (Maybe S.ByteString) -> IO a) -> IO a
withDeflateInput (Deflate (fzstr, fbuff)) bs f =
    withForeignPtr fzstr $ \zstr ->
        unsafeUseAsCStringLen bs $ \(cstr, len) -> do
            c_set_avail_in zstr cstr $ fromIntegral len
            f $ drain fbuff zstr c_call_deflate_noflush False

foreign import ccall unsafe "call_deflate_finish"
    c_call_deflate_finish :: ZStream' -> IO CInt

finishDeflate :: Deflate -> (IO (Maybe S.ByteString) -> IO a) -> IO a
finishDeflate (Deflate (fzstr, fbuff)) f =
    withForeignPtr fzstr $ \zstr ->
        f $ drain fbuff zstr c_call_deflate_finish True
