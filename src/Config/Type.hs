{-# LANGUAGE DeriveGeneric #-}
module Config.Type where

import           Data.Aeson
import           GHC.Generics

data Config = Config {
    site_root :: FilePath
} deriving (Generic)
instance FromJSON Config
