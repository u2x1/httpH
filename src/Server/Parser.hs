{-# LANGUAGE OverloadedStrings #-}
module Server.Parser where

import           Control.Applicative        ((<|>))
import           Control.ExceptT
import           Data.Attoparsec.ByteString
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.Char                  (ord)
import           Data.Word                  (Word8)
import           Server.Type
import           Utils.ByteString

type ParserEx = ExceptT Error Parser

request :: ParserEx Request
request = do
  reqLine' <- reqLine
  headers' <- headers
  reqBody' <- lift takeByteString
  return (Request reqLine' headers' reqBody')

reqLine :: ParserEx RequestLine
reqLine = do
  method' <- ExceptT $ getMethod <$> takeTill (== sp) <* takeTill (/= sp)
  uri' <- ExceptT $ checkUri <$> takeTill (== sp) <* takeTill (/= sp)
  version' <- ExceptT $ getVersion <$> takeTill (== sp) <* takeTill (/= sp)
  return (RequestLine method' uri' version')
  where
    getMethod str = case str of
              "GET"  -> Right GET
              "POST" -> Right POST
              "HEAD" -> Right HEAD
              _      -> Left (Error 501 "METHOD NOT IMPLEMENT")

    getVersion str = case str of
               "HTTP/0.9" -> Right HTTP09
               "HTTP/1.0" -> Right HTTP10
               "HTTP/1.1" -> Right HTTP11
               "HTTP/2"   -> Right HTTP20
               _          -> Right HTTP10

    prefixWithDot bstr = if (not $ BS.null bstr)
                            then BS.head bstr == 46
                            else False
    checkUri str = if any prefixWithDot (BS.split 47 str)
                      then Left (Error 1001 "UNSAFE URI")
                      else Right str

lf :: Word8
lf = 10
cr :: Word8
cr = 13
sp :: Word8
sp = 32

headers :: ParserEx [Header]
headers = lift $ many' $ do
  name <- getHeaderType . BS.pack <$> manyTill anyWord8 (string ": ")
  val <- takeTill (\x -> x== lf || x == cr) <* (many' ((word8 cr *> word8 lf) <|> word8 lf))
  return (Header name val)
  where
    getHeaderType str = case toLower str of
                      "date"              -> Date
                      "connection"        -> Connection
                      "cache-control"     -> CacheControl

                      "accept"            -> Accept
                      "accept-language"   -> AcceptLanguage
                      "accept-encoding"   -> AcceptEncoding
                      "referer"           -> Referer
                      "cookie"            -> Cookie
                      "user-agent"        -> UserAgent
                      "if-modified-since" -> IfModifiedSince
                      "if-none-match"     -> IfNoneMatch
                      "host"              -> Host

                      "content-length"    -> ContentLength
                      "content-encoding"  -> ContentEncoding
                      "content-language"  -> ContentLanguage
                      _                   -> UnknownHeader
