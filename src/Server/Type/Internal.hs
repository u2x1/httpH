{-# LANGUAGE OverloadedStrings #-}
module Server.Type.Internal where

import           Data.ByteString (ByteString)

data Request = Request{
    req_method  :: RequestMethod
  , req_uri     :: ByteString
  , req_version :: HTTPVersion
  , req_headers :: [Header]
  , req_body    :: ByteString
} deriving (Show)

data Response = Response {
    resp_version     :: HTTPVersion
  , resp_status_code :: Integer
  , resp_reason      :: ByteString
  , resp_headers     :: [Header]
  , resp_body        :: ByteString
}

data HTTPVersion = HTTP09
                 | HTTP10
                 | HTTP11
                 | HTTP20
  deriving (Show, Eq)


data RequestMethod = GET
                  --  | POST
                  --  | HEAD
  deriving (Show, Eq)

data Header = Header HeaderName ByteString
  deriving (Show)

data HeaderName =
                -- General header
                Date | Connection | CacheControl
                -- Request header
                | Accept  | AcceptLanguage  | AcceptEncoding
                | Referer | Cookie          | UserAgent
                | Host    | IfModifiedSince | IfNoneMatch

                -- Entity header
                | ContentLength | ContentEncoding | ContentLanguage
                | ContentType   | LastModified

                | UnknownHeader -- save this to ignore unknown headers instead of throwing exceptions
  deriving (Show, Eq)

data Error = Error Integer ByteString
  deriving (Show)