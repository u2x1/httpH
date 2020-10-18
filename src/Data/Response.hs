{-# LANGUAGE OverloadedStrings #-}
module Data.Response where

import           Control.ExceptT   (ExceptT (ExceptT))
import           Control.Exception (SomeException, catch)
import           Data.ByteString   (ByteString)
import qualified Data.ByteString   as BS
import           Server.Type       (Error (..))
import           System.Directory  (Permissions (readable), doesFileExist,
                                    getPermissions)
import           Utils.ByteString  (ToByteString (toByteStr))
import           Utils.Misc        (takeWhileEnd)

getFileType :: String -> ByteString
getFileType path = case takeWhileEnd (/= '.') path of
                     "html" -> "text/html"
                     "css"  -> "text/css"
                     "jpg"  -> "image/jpeg"
                     "png"  -> "image/png"
                     "gif"  -> "image/gif"
                     "tiff" -> "image/tiff"
                     "js"   -> "application/javascript"
                     _      -> "text/plain"

getFileContent :: FilePath -> ExceptT Error IO ByteString
getFileContent path = ExceptT $ do
  exist <- doesFileExist path
  if not exist
    then return (Left (Error 404 "NOT FOUND"))
    else do
      permission <- getPermissions path
      if not (readable permission)
        then return (Left (Error 403 "FORBIDDEN"))
        else catch (Right <$> BS.readFile path)
        (\e -> return $ Left (Error 500 $ toByteStr (e:: SomeException)))
