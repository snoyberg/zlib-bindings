import Codec.Zlib
import System.Environment (getArgs)
import System.IO
import qualified Data.ByteString as S
import Control.Monad (unless)

main = do
    [action, inFile, outFile] <- getArgs
    withFile inFile ReadMode $ \inH ->
        withFile outFile WriteMode $ \outH ->
            case action of
                "inflate" -> inflate inH outH
                "deflate" -> deflate inH outH

inflate inH outH = do
    inf <- initInflate $ WindowBits 31
    go inf
    bs <- finishInflate inf
    S.hPutStr outH bs
  where
    go inf = do
        bs <- S.hGet inH 1024
        unless (S.null bs) $ do
            withInflateInput inf bs $ writer outH
            go inf

deflate inH outH = do
    def <- initDeflate 7 $ WindowBits 31
    go def
    finishDeflate def $ writer outH
  where
    go def = do
        bs <- S.hGet inH 1024
        unless (S.null bs) $ do
            withDeflateInput def bs $ writer outH
            go def

writer outH pop = do
    mbs <- pop
    case mbs of
        Nothing -> return ()
        Just bs -> do
            S.hPutStr outH bs
            writer outH pop
