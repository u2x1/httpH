module Utils.ByteString where

import           Data.Bits       ((.|.))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS


toLower :: ByteString -> ByteString
toLower =
  BS.map (.|. 0x20)
