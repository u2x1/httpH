{-# LANGUAGE OverloadedStrings #-}
module Data.Response where

import Control.ExceptT ( ExceptT(..), MonadTrans(lift) )   
import           Control.Exception (SomeException, catch)
import           Data.ByteString   (ByteString)
import qualified Data.ByteString   as BS
import qualified Data.ByteString.Char8   as BSC
import Server.Type
    ( Error(..),
      HeaderName(..),
      Header(..),
      HTTPVersion(HTTP10, HTTP09),
      Response(Response),
      Request(Request, req_version, req_headers, req_uri) )
import System.Directory
    ( doesFileExist,
      getModificationTime,
      getPermissions,
      Permissions(readable) )  
import Utils.ByteString ( ToByteString(toByteStr) )  
import Utils.Misc ( takeWhileEnd, toGMTTime )       
import Server.Handler ( getHeaderHandler )
import           Data.Time                  (getCurrentTime)

getResponse :: FilePath -> Request -> IO Response
getResponse root req@Request{req_version=ver}=
   case ver of
     HTTP09 -> getSimpleResponse root req
     _      -> getFullResponse root req

getFullResponse :: [Char] -> Request -> IO Response
getFullResponse root req@Request{req_headers=hdrs} = eitherErr2Resp $ runExceptT $ do
  
  initResp <- do
        let filePath = getFilePath $ root <> (BSC.unpack $ req_uri req)
        body <- getFileContent filePath
        date <- lift $ toGMTTime <$> getCurrentTime
        modifyTime <- lift $ toGMTTime <$> getModificationTime filePath
        let fileType = getFileType filePath
            fileLen = toByteStr $ BS.length body
        return $ Response HTTP10 200 "OK"
         [ Header LastModified  modifyTime
         , Header Date          date
         , Header ContentType   fileType
         , Header ContentLength fileLen] body
  ExceptT $ getHandler hdrs initResp

getSimpleResponse :: [Char] -> Request -> IO Response
getSimpleResponse root Request{req_uri=uri}  = eitherErr2Resp $ runExceptT $ do
    let filePath = getFilePath $ root <> (BSC.unpack uri)
    body <- getFileContent filePath
    return $ Response HTTP09 200 "" [] body

getHandler :: [Header] -> (Response -> IO (Either Error Response))
getHandler hdrs = runExceptT . (foldIt $ getHeaderHandler <$> hdrs)
  where foldIt = foldr (fmap . (=<<)) return

eitherErr2Resp :: Monad m => m (Either Error Response) -> m Response
eitherErr2Resp mx = do
          x <- mx
          case x of
            Right a   -> return a
            Left  err -> return $ err2Resp HTTP10 err


getFilePath :: FilePath -> FilePath
getFilePath x =if last x == '/'
                 then x <> "index.html"
                 else if elem '.' $ takeWhileEnd (/= '/') x
                   then x
                   else x <> "/index.html"

getFileType :: String -> ByteString
getFileType path = case takeWhileEnd (/= '.') path of
                     "html" -> "text/html"
                     "css"  -> "text/css"
                     "jpg"  -> "image/jpeg"
                     "png"  -> "image/png"
                     "gif"  -> "image/gif"
                     "tiff" -> "image/tiff"
                     "js"   -> "application/javascript"
                     _      -> "text/plain"

getFileContent :: FilePath -> ExceptT Error IO ByteString
getFileContent path = ExceptT $ do
  exist <- doesFileExist path
  if not exist
    then return (Left (Error 404 ""))
    else do
      permission <- getPermissions path
      if not (readable permission)
        then return (Left (Error 403 ""))
        else catch (Right <$> BS.readFile path)
        (\e -> return $ Left (Error 500 $ toByteStr (e:: SomeException)))

err2Resp :: HTTPVersion -> Error -> Response
err2Resp ver err@(Error code reason)
  | code < 600 = cr8Resp code reason
  | code >= 600 =  cr8Resp 500 ""
   where
    cr8Resp code reason = Response ver code reason hdrs (toByteStr err)
    hdrs = [Header ContentType "text/html; charset=utf-8"]