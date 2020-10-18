module Utils.Misc where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.Time.Clock       (UTCTime)
import           Data.Time.Format      (defaultTimeLocale, parseTimeM,
                                        rfc822DateFormat)

parseGMTTime :: ByteString -> Maybe UTCTime
parseGMTTime str = parseTimeM True defaultTimeLocale rfc822DateFormat (BC.unpack str)

takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd f xs = reverse $ go f (reverse xs)
  where
        go g (y:ys) = if g y then y : (go g ys) else []
        go _ []     = []
