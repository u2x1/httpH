module Utils.ByteString where

import           Control.Exception     (SomeException)
import           Data.Bits             ((.|.))
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC

class ToByteString a where
  toByteStr :: a -> ByteString

instance ToByteString Integer where
  toByteStr = BSC.pack . show

instance ToByteString SomeException where
  toByteStr = BSC.pack . show

toLower :: ByteString -> ByteString
toLower =
  BS.map (.|. 0x20)
