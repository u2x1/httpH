{-# LANGUAGE OverloadedStrings #-}
module Server.Server where

import           Control.Concurrent         (forkFinally)
import           Control.ExceptT            (ExceptT (ExceptT, runExceptT),
                                             MonadTrans (lift))
import           Control.Exception          ()
import qualified Control.Exception          as E
import           Control.Monad              (forever, void)
import           Data.Attoparsec.ByteString as BAS (parseOnly)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BSC
import           Data.Response              (getFileContent, getFileType)
import           Data.Time.Format           (defaultTimeLocale, formatTime)
import           Network.Socket
import           Network.Socket.ByteString  (recv, sendAll)
import           Server.Handler             (getHeaderHandler)
import           Server.Parser              (request)
import           Server.Type
import           System.Directory           (getModificationTime)
import           Utils.ByteString           (toByteStr)
import           Utils.Misc                 (takeWhileEnd)

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
                Left err -> sendAll s (toByteStr $ err2Resp HTTP10 err)
            err -> print err

getResponse :: FilePath -> Request -> IO Response
getResponse root req = getOut $ runExceptT $ do
  let filePath = let x = root <> (BSC.unpack $ req_uri req) in
                    if last x == '/'
                      then x <> "index.html"
                      else if elem '.' $ takeWhileEnd (/= '/') x
                        then x
                        else x <> "/index.html"
      ver = req_version req
  body <- getFileContent filePath
  let foldIt = (foldr (fmap . (=<<)) return)
                  :: [Response -> ExceptT Error IO Response] -> (Response -> ExceptT Error IO Response)
  let f = runExceptT . (foldIt $ getHeaderHandler req <$> (req_headers req))
  initResp <- do
        date <- lift $ BSC.pack . formatTime defaultTimeLocale "%a, %_d %b %Y %H:%M:%S GMT" <$> getModificationTime filePath
        let fileType = getFileType filePath
            fileLen = BS.length body
        return $ Response ver 200 "OK"
         [Header Date date, Header ContentType fileType, Header ContentLength (BSC.pack $ show fileLen)] body
  ExceptT $ f initResp

  where getOut mx = do
          x <- mx
          case x of
            Right a   -> return a
            Left  err -> return $ err2Resp HTTP10 err

err2Resp :: HTTPVersion -> Error -> Response
err2Resp ver (Error code reason)
  | code < 600 = cr8Resp code reason
  | code >= 600 =  cr8Resp 500 "INTERNAL ERROR"
   where
    cr8Resp code reason = Response ver code reason hdrs (toByteStr code <> " " <> reason)
    hdrs = [Header ContentType "text/plain; charset=utf-8"]

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
