module ListenSock where

import Control.Applicative
import Network.Socket
import System.Environment (getArgs)

getPort :: IO String
getPort = head <$> getArgs

startListenSock :: String -> IO Socket
startListenSock portNumber = do
    addrinfos  <- getAddrInfo
                  (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                  Nothing
                  (Just portNumber)
    let serveraddr = head addrinfos
    listenSock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bindSocket listenSock $ addrAddress serveraddr
    setSocketOption listenSock ReuseAddr 1
    listen listenSock listenQueueLength
    return listenSock
  where
    listenQueueLength :: Int
    listenQueueLength = 8192


