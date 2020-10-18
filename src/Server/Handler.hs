{-# LANGUAGE OverloadedStrings #-}
module Server.Handler where

import           Codec.Compression.GZip (compress)
import           Control.ExceptT        (ExceptT, except)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import           Server.Type
import           Utils.Misc             (parseGMTTime)

-- A initial response should have these headers included: Date, Content-Type
getHeaderHandler :: Request -> Header -> Response -> ExceptT Error IO Response

getHeaderHandler _ (Header IfModifiedSince date) resp = do
  let guard x = case x of
                 Just y -> return y
                 _      -> except (Error 1002 "WRONG DATE FORMAT")
  resp_date <- guard $ parseGMTTime =<< (lookupHeader Date $ resp_headers resp)
  req_date <- guard $ parseGMTTime date

  if req_date < resp_date
    then return resp
    else except (Error 304 "NOT MODIFIED")

-- enable gzip compression as default
getHeaderHandler _ (Header AcceptEncoding encoding) resp =
  if BS.null $ snd $ BS.breakSubstring "gzip" encoding
    then return  resp
    else do
      let retBody = BL.toStrict $ compress (BL.fromStrict $ resp_body resp)
      let headers = Header ContentEncoding "gzip" : (resp_headers resp)
      return resp { resp_body= retBody, resp_headers = headers}

getHeaderHandler _ _ resp = return resp


lookupHeader :: HeaderName -> [Header] -> Maybe ByteString
lookupHeader _ [] = Nothing
lookupHeader hdrName ((Header name val):hdrs) = if name == hdrName
  then Just val
  else lookupHeader hdrName hdrs
