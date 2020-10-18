{-# LANGUAGE OverloadedStrings #-}
module Server.Type where

import           Data.ByteString
import           Data.ByteString       as BS
import           Data.ByteString.Char8 as BSC
import           Data.Tuple

data Error = Error Int ByteString
  deriving (Show)

data Response = Response {
    resp_line    :: StatusLine
  , resp_headers :: [Header]
  , resp_body    :: ByteString
}
resp2BS :: Response -> ByteString
resp2BS (Response (StatusLine ver (StatusCode code) reason) headers body) =
  ver2BS ver <> " " <> BSC.pack (show code) <> " " <> reason
    <> (BS.concat $ ("\r\n" <>) . hdr2BS <$> headers)
    <> "\r\n\r\n" <> body

ver2BS :: HTTPVersion -> ByteString
ver2BS HTTP09 = "HTTP/0.9"
ver2BS HTTP10 = "HTTP/1.0"
ver2BS HTTP11 = "HTTP/1.1"
ver2BS HTTP20 = "HTTP/2"

hdr2BS :: Header -> ByteString
hdr2BS (Header hdr val) = case lookup hdr $ swap <$> headerMap of
  Just x -> x <> ": " <> val
  _      -> ""
headerMap :: [(ByteString, HeaderName)]
headerMap = [
                     ("date"              , Date          )
                   , ("connection"        , Connection    )
                   , ("cache-control"     , CacheControl)

                   , ("accept"            , Accept)
                   , ("accept-language"   , AcceptLanguage)
                   , ("accept-encoding"   , AcceptEncoding)
                   , ("referer"           , Referer)
                   , ("cookie"            , Cookie)
                   , ("user-agent"        , UserAgent)
                   , ("if-modified-since" , IfModifiedSince)
                   , ("if-none-match"     , IfNoneMatch)
                   , ("host"              , Host)

                   , ("content-length"    , ContentLength)
                   , ("content-encoding"  , ContentEncoding)
                   , ("content-language"  , ContentLanguage)
                   , ("content-type"      , ContentType)
                   ]

data Request = Request{
    req_line    :: RequestLine
  , req_headers :: [Header]
  , req_body    :: ByteString
} deriving (Show)

data StatusLine = StatusLine {
    resp_version :: HTTPVersion
  , status_code  :: StatusCode
  , resp_reason  :: ByteString
} deriving (Show)

data RequestLine = RequestLine {
    method      :: RequestMethod
  , uri         :: ByteString
  , req_version :: HTTPVersion
} deriving (Show)

data StatusCode = StatusCode Int
  deriving (Show)

data HTTPVersion = HTTP09 | HTTP10 | HTTP11 | HTTP20
  deriving (Show)

data RequestMethod = GET | POST | HEAD | NullMethod
  deriving (Show)

data Header = Header HeaderName ByteString
  deriving (Show)

data HeaderName =
                -- General header
                Date | Connection | CacheControl
                -- Request header
                | Accept | AcceptLanguage | AcceptEncoding | Referer | Cookie | UserAgent
                | IfModifiedSince | IfNoneMatch | Host

                -- Entity header
                | ContentLength | ContentEncoding | ContentLanguage | ContentType

                | UnknownHeader
  deriving (Show, Eq)

toTuples :: [Header] -> [(HeaderName, ByteString)]
toTuples = fmap (\(Header name val) -> (name, val))
