{-# LANGUAGE BangPatterns #-}

import Control.Concurrent (forkIO)
import Control.Monad (forever, when)
import qualified Data.ByteString as B
import Data.ByteString.Internal (toForeignPtr)
import Foreign
import Network.Socket (Socket, accept)
import Network.Socket.ByteString (recv)

import ListenSock
import Http
import Send

main :: IO ()
main = do
    listenSock <- getPort >>= startListenSock
    forever $ do
        (sock, _) <- accept listenSock
        forkIO $ worker sock

worker :: Socket -> IO ()
worker sock = do
    let (replyFPtr,_,_) = toForeignPtr reply
    withForeignPtr replyFPtr $ serve sock

serve :: Socket -> Ptr Word8 -> IO ()
serve sock replyPtr = loop expectedRequestLength
  where
    loop :: Int -> IO ()
    loop !left
      | left < 0  = error "ACK" -- maybe need to handle this properly
      | left == 0 = do
          sendAll sock replyPtr replyLen
          loop expectedRequestLength
      | otherwise = do
          bs <- recv sock left
          let len = B.length bs
          when (len /= 0) $ loop (left - len)
