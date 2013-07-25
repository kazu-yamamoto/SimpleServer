{-# LANGUAGE BangPatterns #-}

import Control.Concurrent (forkIO)
import Control.Monad (forever, when)
import Data.ByteString.Internal (toForeignPtr)
import Foreign
import Network.Socket (Socket, accept)

import ListenSock
import Http
import Send
import Recv

main :: IO ()
main = do
    listenSock <- getPort >>= startListenSock
    forever $ do
        (sock, _) <- accept listenSock
        forkIO $ worker sock

worker :: Socket -> IO ()
worker sock = do
    recvBuffer <- mallocForeignPtrBytes recvBufferSize
    let (replyFPtr,_,_) = toForeignPtr reply
    withForeignPtr replyFPtr $
        withForeignPtr recvBuffer . serve sock

serve :: Socket -> Ptr Word8 -> Ptr Word8 -> IO ()
serve sock replyPtr recvPtr = loop expectedRequestLength
  where
    loop :: Int -> IO ()
    loop !left
      | left < 0  = error "ACK" -- maybe need to handle this properly
      | left == 0 = do
          sendAll sock replyPtr replyLen
          loop expectedRequestLength
      | otherwise = do
          len <- recv sock recvPtr left
          when (len /= 0) $ loop (left - len)

recvBufferSize :: Int
recvBufferSize = expectedRequestLength + 100 -- 100 extra bytes for good measure.
