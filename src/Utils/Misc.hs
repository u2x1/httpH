{-# LANGUAGE OverloadedStrings #-}
module Utils.Misc where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BSC
import           Data.Time.Clock       (UTCTime)
import Data.Time.Format
    ( formatTime, defaultTimeLocale, rfc822DateFormat, parseTimeM )

parseGMTTime :: ByteString -> Maybe UTCTime
parseGMTTime str = parseTimeM True defaultTimeLocale rfc822DateFormat (BSC.unpack str)

toGMTTime :: UTCTime -> ByteString
toGMTTime = BSC.pack . formatTime defaultTimeLocale "%a, %_d %b %Y %H:%M:%S GMT"

takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd f xs = reverse $ go f (reverse xs)
  where
        go g (y:ys) = if g y then y : (go g ys) else []
        go _ []     = []
