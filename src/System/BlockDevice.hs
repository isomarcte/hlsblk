module System.BlockDevice where

import Control.Applicative (Applicative(..))
import Control.Monad (return)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Maybe (maybe)
import Data.Monoid ((<>), mempty)
import Data.String (String)
import Prelude (FilePath)
import System.IO (IO)
import System.Process.Text (readProcessWithExitCode)
import Text.Show (Show(..))

import qualified Data.Aeson as DA
import qualified Data.BlockDevice as DB
import qualified Data.HashMap.Strict as DHS
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE

listBlockDevices' :: IO (Either String [DB.BlockDevice])
listBlockDevices' = do
  (_, out, _) <-
    readProcessWithExitCode lsblkPath ["-J", "-a", "-O", "-b"] mempty
  return $ do
    value <- DA.eitherDecodeStrict' $ DTE.encodeUtf8 out
    value' <- unwrap value
    case DA.fromJSON value' of
      DA.Error err -> Left err
      DA.Success a -> Right a

lsblkPath :: FilePath
lsblkPath = "/bin/lsblk"

unwrap :: DA.Value -> Either String DA.Value
unwrap (DA.Object o) =
  let key = "blockdevices"
   in maybe
        (Left $ "Unable to find key: " <> DT.unpack key)
        pure
        (DHS.lookup key o)
unwrap other = Left $ "Expected JSON Object, got: " <> show other
