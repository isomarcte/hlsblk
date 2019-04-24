module Data.BlockDevice.Internal.JSON
  ( remapKeysFromJson
  , remapKeysToJson
  ) where

import Data.Eq (Eq(..))
import Data.Function (($), (.))
import Data.Functor (Functor(..))
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Ord (Ord(..))

import qualified Data.Aeson as DA
import qualified Data.HashMap.Strict as DHS
import qualified Data.Map.Strict as DMS
import qualified Data.Text as DT

deepmap :: (DA.Value -> DA.Value) -> DA.Value -> DA.Value
deepmap f (DA.Object hm) = f . DA.Object $ fmap (deepmap f) hm
deepmap f (DA.Array v) = f . DA.Array $ fmap (deepmap f) v
deepmap f other = f other

remapKeysJson' :: (DT.Text -> DT.Text) -> DA.Value -> DA.Value
remapKeysJson' f (DA.Object hm) =
  DA.Object $ DHS.foldlWithKey' (folder f) DHS.empty hm
  where
    folder ::
         (Eq a, Hashable a)
      => (a -> a)
      -> DHS.HashMap a b
      -> a
      -> b
      -> DHS.HashMap a b
    folder f' acc k v = DHS.insert (f' k) v acc
remapKeysJson' _ other = other

remapKeyFromJson' :: DT.Text -> DT.Text
remapKeyFromJson' k =
  let mapLookup = DMS.lookup k fromJsonKeyRemappings
      fallbackTransform = DT.replace "-" "_" k
   in fromMaybe fallbackTransform mapLookup

remapKeysFromJson' :: DA.Value -> DA.Value
remapKeysFromJson' = remapKeysJson' remapKeyFromJson'

remapKeysFromJson :: DA.Value -> DA.Value
remapKeysFromJson = deepmap remapKeysFromJson'

remapKeyToJson' :: DT.Text -> DT.Text
remapKeyToJson' k =
  let mapLookup = DMS.lookup k toJsonKeyRemappings
      fallbackTransform = DT.replace "_" "-" k
   in fromMaybe fallbackTransform mapLookup

remapKeysToJson' :: DA.Value -> DA.Value
remapKeysToJson' = remapKeysJson' remapKeyToJson'

remapKeysToJson :: DA.Value -> DA.Value
remapKeysToJson = deepmap remapKeysToJson'

mapSwap :: (Ord b) => DMS.Map a b -> DMS.Map b a
mapSwap = DMS.foldlWithKey' folder DMS.empty
  where
    folder :: (Ord v) => DMS.Map v k -> k -> v -> DMS.Map v k
    folder acc k v = DMS.insert v k acc

fromJsonKeyRemappings :: DMS.Map DT.Text DT.Text
fromJsonKeyRemappings =
  DMS.fromList [("type", "type'"), ("maj:min", "majmin"), ("fsuse%", "fsuse")]

toJsonKeyRemappings :: DMS.Map DT.Text DT.Text
toJsonKeyRemappings = mapSwap fromJsonKeyRemappings
