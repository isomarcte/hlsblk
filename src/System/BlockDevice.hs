module System.BlockDevice
  ( defaultFlags
  , listBlockDevices
  , listBlockDevices'
  , Error(..)
  , errorMessage
  , errorMessage'
  ) where

import Control.Monad (Monad(return))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Except (Except(..), ExceptT(..), throwE)
import Data.Bool (otherwise)
import Data.Either (Either(..), either)
import Data.Eq (Eq(..))
import Data.Function (($), (.))
import Data.Int (Int)
import Data.Maybe (maybe)
import Data.Monoid ((<>), mempty)
import Data.String (String)
import Prelude (FilePath)
import System.Exit (ExitCode(..))
import System.IO (IO)
import System.Process.Text (readProcessWithExitCode)
import Text.Show (Show(..))

import qualified Data.Aeson as DA
import qualified Data.BlockDevice as DB
import qualified Data.HashMap.Strict as DHS
import qualified Data.Set as DS
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE

defaultFlags :: DS.Set DT.Text
defaultFlags = DS.fromList ["-J", "-a", "-O", "-b"]

listBlockDevices ::
     (MonadIO m) => DS.Set DT.Text -> ExceptT Error m [DB.BlockDevice]
listBlockDevices flags =
  let flags' = DS.toList . DS.map DT.unpack $ DS.insert jsonFlag flags
   in do (ec, out, err) <-
           liftIO $ readProcessWithExitCode lsblkPath flags' mempty
         guardExitCode ec err
         value <- stdoutToJson out
         value' <- unwrap value
         jsonToBlockDevice value'

listBlockDevices' :: DS.Set DT.Text -> IO (Either Error [DB.BlockDevice])
listBlockDevices' flags =
  runExceptT $ catchError (listBlockDevices flags) throwError

data Error where
  UnexpectedJsonValue :: DA.Value -> Error
  InvalidBlockDeviceValue :: DA.Value -> String -> Error
  StdOutIsNotValidJson :: DT.Text -> String -> Error
  NonzeroExitCode :: Int -> DT.Text -> Error

deriving instance Show Error

deriving instance Eq Error

errorMessage :: Error -> DT.Text
errorMessage UnexpectedJsonValue {} =
  "Unexpected initial JSON value. lsblk is expected to emit a single object, with a single key \"blockdevices\", the value of which is a JSON array of blockdevice values."
errorMessage (NonzeroExitCode ec err) =
  "lsblk exited with a non-zero exit code: " <> (DT.pack . show $ ec) <>
  stdErrMessageOrNothing err
errorMessage (InvalidBlockDeviceValue value err) =
  "The value found in the output JSON for \"blockdevice\" was malformed\n" <>
  "Found: " <>
  DT.pack (show value) <>
  "\n" <>
  "Aeson Error: " <>
  DT.pack err
errorMessage (StdOutIsNotValidJson stdout err) =
  "lsblk did not output valid JSON to stdout.\n" <> "Aeson Error: " <>
  DT.pack err <>
  "\n" <>
  "StdOut: " <>
  stdout

errorMessage' :: Error -> String
errorMessage' = DT.unpack . errorMessage

guardExitCode :: (Monad m) => ExitCode -> DT.Text -> ExceptT Error m ()
guardExitCode (ExitFailure i) err = throwE $ NonzeroExitCode i err
guardExitCode _ _ = return ()

stdoutToJson :: DT.Text -> Except Error DA.Value
stdoutToJson out =
  either
    (throwError . StdOutIsNotValidJson out)
    return
    (DA.eitherDecodeStrict' $ DTE.encodeUtf8 out)

jsonToBlockDevice :: DA.Value -> Except Error [DB.BlockDevice]
jsonToBlockDevice v =
  case DA.fromJSON v of
    DA.Error err -> throwError $ InvalidBlockDeviceValue v err
    DA.Success bd -> return bd

jsonFlag :: DT.Text
jsonFlag = "-J"

stdErrMessageOrNothing :: DT.Text -> DT.Text
stdErrMessageOrNothing err
  | DT.length err == 0 = ""
  | otherwise = "\nStdErr: " <> err

lsblkPath :: FilePath
lsblkPath = "/bin/lsblk"

unwrap :: DA.Value -> Except Error DA.Value
unwrap v@(DA.Object o) =
  let key = "blockdevices"
   in maybe (throwError $ UnexpectedJsonValue v) return (DHS.lookup key o)
unwrap other = throwError $ UnexpectedJsonValue other
