{-# LANGUAGE BangPatterns #-}

import Control.Concurrent (forkIO)
import Control.Monad (forever, when)
import qualified Data.ByteString as B
import Network.Socket (Socket, accept)
import Network.Socket.ByteString (recv, sendAll)

import ListenSock
import Http

main :: IO ()
main = do
    listenSock <- getPort >>= startListenSock
    forever $ do
        (sock, _) <- accept listenSock
        forkIO $ worker sock

worker :: Socket -> IO ()
worker sock = loop expectedRequestLength
  where
    loop :: Int -> IO ()
    loop !left
      | left < 0  = error "ACK" -- maybe need to handle this properly
      | left == 0 = do
          sendAll sock reply
          loop expectedRequestLength
      | otherwise = do
          bs <- recv sock requestSize
          let len = B.length bs
          when (len /= 0) $ loop (left - len)

requestSize :: Int
requestSize = 2000

