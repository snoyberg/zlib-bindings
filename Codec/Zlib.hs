{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
module Codec.Zlib
    ( -- * Data types
      ZStream
      -- * Inflate
    , initInflate
    , withInflateInput
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
type ZStream = ForeignPtr ZStreamStruct

foreign import ccall unsafe "create_z_stream_inflate"
    c_create_z_stream_inflate :: CInt -> IO ZStream'

foreign import ccall unsafe "&free_z_stream_inflate"
    c_free_z_stream_inflate :: FunPtr (ZStream' -> IO ())

initInflate :: WindowBits -> IO ZStream
initInflate w = do
    let w' = case w of
                DefaultWindowBits -> 15
                WindowBits i -> fromIntegral i
    zstr <- c_create_z_stream_inflate w'
    newForeignPtr c_free_z_stream_inflate zstr

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
    :: ZStream -> S.ByteString -> (IO (Maybe S.ByteString) -> IO a)
    -> IO a
withInflateInput fzstr bs f =
    withForeignPtr fzstr $ \zstr ->
        unsafeUseAsCStringLen bs $ \(cstr, len) -> do
            c_set_avail_in zstr cstr $ fromIntegral len
            f $ drain zstr
  where
    drain zstr = do
        a <- c_get_avail_in zstr
        if a == 0
            then return Nothing
            else allocaBytes defaultChunkSize $ \buff -> do
                c_set_avail_out zstr buff $ fromIntegral defaultChunkSize
                res <- c_call_inflate_noflush zstr
                when (res < 0 && res /= (-5)) $ error -- FIXME
                    $ "zlib: Error in underlying stream: " ++ show res
                avail <- c_get_avail_out zstr
                let size = defaultChunkSize - fromIntegral avail
                if size == 0
                    then return Nothing
                    else do
                        bs <- S.packCStringLen (buff, size)
                        return $ Just bs
