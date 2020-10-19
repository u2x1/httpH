{-# LANGUAGE OverloadedStrings #-}
module Server.Type ( HeaderName(..)
                   , Header(..)
                   , HTTPVersion(..)
                   , Response(..)
                   , Request(..)
                   , RequestMethod(..)
                   , Error(..)
                   ) where

import           Data.ByteString      (ByteString)
import Server.Type.Internal
    ( Error(..),
      HeaderName(..),
      Header(..),
      RequestMethod(..),
      HTTPVersion(..),
      Response(..),
      RequestMethod(..),
      Request(..) )
import Utils.ByteString ( ToByteString(..), FromByteString(..), toLower)

import Data.Tuple

instance FromByteString RequestMethod where
  fromByteStr bStr = case bStr of
    "GET" -> Just GET
    _ -> Nothing

instance FromByteString HTTPVersion where
  fromByteStr bStr = lookup bStr $ swap <$> versionMap

instance FromByteString HeaderName where
  fromByteStr bStr = lookup (toLower bStr) $ swap <$> headerMap

instance ToByteString Header where
  toByteStr (Header hdr val) = case lookup hdr $ headerMap of
                                 Just x -> x <> ": " <> val
                                 _      -> ""
instance ToByteString Response where
  toByteStr (Response HTTP09 _ _ _ body) = body
  toByteStr (Response ver code reason headers body) =
    version' <> " " <> statusCode' <> " " <> reason
      <> headers'
      <> "\r\n\r\n" <> body
    where
      version' = toByteStr ver
      statusCode' = toByteStr code
      headers' = mconcat (("\r\n" <>) . toByteStr <$> headers)

instance ToByteString HTTPVersion where
  toByteStr ver = case lookup ver versionMap of
                    Just x -> x
                    _      -> ""

instance ToByteString Error where
  toByteStr (Error code note) =
    let html h1 = "<html>\
               \<h1>" <> toByteStr code <> " " <> h1 <> "</h1>\
               \<p>" <> note <> "</p>\
               \<p><a href=\"https://github.com/Nutr1t07/httpH\">httpH</a> 0.0.0.1</p>\
               \</html>" in
    case lookup code errorMap of
      Just x -> html x
      _      -> html ("UNKNOWN ERROR")

headerMap :: [(HeaderName, ByteString)]
headerMap = [ (Date            , "date"              )
            , (Connection      , "connection"        )
            , (CacheControl    , "cache-control"     )
                                                 
            , (Accept          , "accept"            )
            , (AcceptLanguage  , "accept-language"   )
            , (AcceptEncoding  , "accept-encoding"   )
            , (Referer         , "referer"           )
            , (Cookie          , "cookie"            )
            , (UserAgent       , "user-agent"        )
            , (IfModifiedSince , "if-modified-since" )
            , (LastModified    , "last-modified" )
            , (IfNoneMatch     , "if-none-match"     )
            , (Host            , "host"              )
                                                    
            , (ContentLength   , "content-length"    )
            , (ContentEncoding , "content-encoding"  )
            , (ContentLanguage , "content-language"  )
            , (ContentType     , "content-type"      )
            ]

versionMap :: [(HTTPVersion, ByteString)]
versionMap = [ (HTTP09, "HTTP/0.9")
             , (HTTP10, "HTTP/1.0")
             , (HTTP11, "HTTP/1.1")
             , (HTTP20, "HTTP/2"  )]

errorMap :: [(Integer, ByteString)]
errorMap = [ (303   , "NOT MODIFIED"             )
           , (403   , "FORBIDDEN"                )
           , (404   , "NOT FOUND"                )
           , (500   , "INTERNAL ERROR"           )
           , (501   , "NOT IMPLEMENTED"          )
           , (1001  , "UNSAFE URI"               )
           , (1002  , "INCORRECT DATE FORMAT"    )
           ]
