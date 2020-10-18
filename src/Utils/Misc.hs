module Utils.Misc where

import           Control.ExceptT
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import           Data.Time.Clock
import           Data.Time.Format
import           Server.Type

parseGMTTime :: ByteString -> Maybe UTCTime
parseGMTTime str = parseTimeM True defaultTimeLocale rfc822DateFormat (BC.unpack str)

takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd f xs = reverse $ go f (reverse xs)
  where
        go g (y:ys) = if g y then y : (go g ys) else []
        go _ []     = []

foldIt :: [Response -> ExceptT Error IO Response] -> (Response -> ExceptT Error IO Response)
foldIt (f:fs) = (\x -> f x >>= foldIt fs)
foldIt []     = return
