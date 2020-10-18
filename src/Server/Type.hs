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
import           Data.Tuple           (swap)
import           Server.Type.Internal
import           Utils.ByteString

instance ToByteString Header where
  toByteStr (Header hdr val) = case lookup hdr $ swap <$> headerMap of
                                 Just x -> x <> ": " <> val
                                 _      -> ""
instance ToByteString Response where
  toByteStr (Response ver code reason headers body) =
    version' <> " " <> statusCode' <> " " <> reason
      <> headers'
      <> "\r\n\r\n" <> body
    where
      version' = toByteStr ver
      statusCode' = toByteStr code
      headers' = mconcat (("\r\n" <>) . toByteStr <$> headers)

instance ToByteString HTTPVersion where
  toByteStr ver = case lookup ver $ swap <$> versionMap of
                    Just x -> x
                    _      -> ""

headerMap :: [(ByteString, HeaderName)]
headerMap = [ ("date"              , Date              )
            , ("connection"        , Connection        )
            , ("cache-control"     , CacheControl      )

            , ("accept"            , Accept            )
            , ("accept-language"   , AcceptLanguage    )
            , ("accept-encoding"   , AcceptEncoding    )
            , ("referer"           , Referer           )
            , ("cookie"            , Cookie            )
            , ("user-agent"        , UserAgent         )
            , ("if-modified-since" , IfModifiedSince   )
            , ("if-none-match"     , IfNoneMatch       )
            , ("host"              , Host              )

            , ("content-length"    , ContentLength     )
            , ("content-encoding"  , ContentEncoding   )
            , ("content-language"  , ContentLanguage   )
            , ("content-type"      , ContentType       )
            ]

versionMap :: [(ByteString, HTTPVersion)]
versionMap = [ ("HTTP/0.9", HTTP09)
             , ("HTTP/1.0", HTTP10)
             , ("HTTP/1.1", HTTP11)
             , ("HTTP/2"  , HTTP20)]
