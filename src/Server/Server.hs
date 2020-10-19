{-# LANGUAGE OverloadedStrings #-}
module Server.Server where

import           Control.Concurrent         (forkFinally)
import Control.ExceptT ( ExceptT(runExceptT) )           
import           Control.Exception          ()
import qualified Control.Exception          as E
import           Control.Monad              (forever, void)
import           Data.Attoparsec.ByteString (parseOnly)
import Data.Response ( err2Resp, getResponse )
import Network.Socket
    ( ServiceName,
      Socket,
      setCloseOnExecIfNeeded,
      defaultHints,
      getAddrInfo,
      withSocketsDo,
      setSocketOption,
      gracefulClose,
      accept,
      bind,
      listen,
      socket,
      close,
      withFdSocket,
      AddrInfo(addrFamily, addrSocketType, addrProtocol, addrAddress),
      SocketOption(ReuseAddr),
      SocketType(Stream) )
import           Network.Socket.ByteString  (recv, sendAll)
import           Server.Parser              (request)
import Server.Type ( HTTPVersion(HTTP10) )
import           Utils.ByteString           (toByteStr)

test :: IO ()
test = putStrLn "running on http://localhost:4000/" >> runTCPServer "4000" server
  where server s = do
          msg <- recv s 1024
          case parseOnly (runExceptT request) msg of
            Right either_req ->
              case either_req of
                Right req -> do
                  resp <- getResponse "/home/nutr1t07/blog/public/" req
                  sendAll s (toByteStr resp)
                  print $ toByteStr resp
                Left err -> sendAll s (toByteStr $ err2Resp HTTP10 err)
            err -> print err

runTCPServer :: ServiceName -> (Socket -> IO a) -> IO a
runTCPServer port server = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close loop
  where
    resolve = head <$> getAddrInfo (Just defaultHints { addrSocketType = Stream } ) Nothing (Just port)
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock
    loop sock = forever $ do
      (conn, _peer) <- accept sock
      void $ forkFinally (server conn) (const $ gracefulClose conn 5000)