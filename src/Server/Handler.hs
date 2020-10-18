{-# LANGUAGE OverloadedStrings #-}
module Server.Handler where

import           Codec.Compression.GZip
import           Control.ExceptT
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import           Server.Type
import           Utils.Misc


-- A initial response should have these headers included: Date, Content-Type
getHeaderHandler :: Request -> Header -> Response -> ExceptT Error IO Response

getHeaderHandler req (Header IfModifiedSince date) resp = do
  resp_date <- case parseGMTTime =<< (lookup Date $ toTuples $ resp_headers resp) of
                 Just x -> return x
                 _      -> except (Error 1002 "WRONG DATE FORMAT")

  req_date <- case parseGMTTime =<< (lookup IfModifiedSince $ toTuples $ req_headers req) of
                 Just x -> return x
                 _      -> except (Error 1002 "WRONG DATE FORMAT")
  if req_date < resp_date
    then return resp
    else except (Error 304 "NOT MODIFIED")

-- enable gzip compression as default
getHeaderHandler req (Header AcceptEncoding encoding) resp =
  if BS.null $ snd $ BS.breakSubstring "gzip" encoding
    then return  resp
    else do
      let retBody = BL.toStrict $ compress (BL.fromStrict $ resp_body resp)
      let headers = Header ContentEncoding "gzip" : (resp_headers resp)
      return resp { resp_body= retBody, resp_headers = headers}


getHeaderHandler _ _ resp = return resp
