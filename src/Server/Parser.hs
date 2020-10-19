{-# LANGUAGE OverloadedStrings #-}
module Server.Parser where

import           Control.Applicative        ((<|>), some)
import Control.ExceptT ( ExceptT(..), MonadTrans(lift) )          
import Data.Attoparsec.ByteString
    ( Parser,
      anyWord8,
      string,
      takeByteString,
      takeTill,
      word8,
      many',
      manyTill )
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.Word                  (Word8)
import Server.Type
    ( Error(..),
      HeaderName(..),
      Header(..),
      RequestMethod(..),
      HTTPVersion(..),
      Request(Request) )
import Utils.ByteString ( FromByteString(fromByteStr) )

type ParserEx = ExceptT Error Parser

request :: ParserEx Request
request = ExceptT $ runExceptT fullRequest <|> runExceptT simpleRequest

fullRequest :: ParserEx Request
fullRequest = do
  (method', uri', version') <- reqLine
  headers' <- headers
  reqBody' <- lift takeByteString
  return (Request method' uri' version' headers' reqBody')

simpleRequest  ::  ParserEx Request
simpleRequest = do
  _ <- lift (string "GET" *> some (word8 sp))
  uri' <- ExceptT $ checkUri <$> takeByteString
  return  (Request GET uri' HTTP09 [] "")

reqLine :: ParserEx (RequestMethod, ByteString, HTTPVersion)
reqLine = do
  method' <- ExceptT $ getMethod <$> takeTill (== sp) <* takeTill (/= sp)
  uri' <- ExceptT $ checkUri <$> takeTill (== sp) <* takeTill (/= sp)
  version' <- ExceptT $ getVersion <$> takeTill (== sp) <* takeTill (/= sp)
  return (method', uri', version')
  where
    getMethod str = case fromByteStr str of
      Just x -> Right x
      _      -> Left (Error 501 "METHOD NOT IMPLEMENT")

    getVersion str = Right $ case fromByteStr str of
      Just x -> x
      _      -> HTTP10


checkUri :: ByteString -> Either Error ByteString
checkUri str = if any prefixWithDot (BS.split 47 str)
                  then Left (Error 1001 "UNSAFE URI")
                  else Right str
  where
    prefixWithDot bstr = if (not $ BS.null bstr)
                            then BS.head bstr == 46
                            else False


headers :: ParserEx [Header]
headers = lift $ many' $ do
  name <- getHeaderType . BS.pack <$> manyTill anyWord8 (string ": ")
  val <- takeTill (\x -> x== lf || x == cr) <* (many' ((word8 cr *> word8 lf) <|> word8 lf))
  return (Header name val)
  where
    getHeaderType str = case fromByteStr str of
      Just x -> x
      _      -> UnknownHeader

lf :: Word8
cr :: Word8
sp :: Word8
lf = 10       -- '\n'
cr = 13       -- '\r'
sp = 32       -- ' '
