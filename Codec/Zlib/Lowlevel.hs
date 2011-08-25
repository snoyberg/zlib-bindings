{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
module Codec.Zlib.Lowlevel
    ( ZStreamPair
    , ZStream'
    , c_create_z_stream_inflate
    , c_free_z_stream_inflate
    , c_create_z_stream_deflate
    , c_free_z_stream_deflate
    , c_set_avail_in
    , c_set_avail_out
    , c_get_avail_out
    , c_get_avail_in
    , c_call_inflate_noflush
    , c_call_deflate_noflush
    , c_call_deflate_finish
    ) where

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr

import Data.ByteString.Unsafe

data ZStreamStruct
type ZStream' = Ptr ZStreamStruct
type ZStreamPair = (ForeignPtr ZStreamStruct, ForeignPtr CChar)

foreign import ccall unsafe "create_z_stream_inflate"
    c_create_z_stream_inflate :: CInt -> IO ZStream'

foreign import ccall unsafe "&free_z_stream_inflate"
    c_free_z_stream_inflate :: FunPtr (ZStream' -> IO ())

foreign import ccall unsafe "create_z_stream_deflate"
    c_create_z_stream_deflate :: CInt -> CInt -> IO ZStream'

foreign import ccall unsafe "&free_z_stream_deflate"
    c_free_z_stream_deflate :: FunPtr (ZStream' -> IO ())

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

foreign import ccall unsafe "call_deflate_noflush"
    c_call_deflate_noflush :: ZStream' -> IO CInt

foreign import ccall unsafe "call_deflate_finish"
    c_call_deflate_finish :: ZStream' -> IO CInt
