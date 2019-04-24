module Data.BlockDevice
  ( BlockDevice(..)
  , Byte(..)
  , Percentage(..)
  , DeviceName(..)
  , KernelDeviceName(..)
  , DevicePath
  , MajorDeviceNumber(..)
  , MinorDeviceNumber(..)
  , AvailableFileSystemSpace(..)
  , FileSystemSize(..)
  , FileSystemType(..)
  , FileSystemUsedSpace(..)
  , FileSystemPercentageInUse(..)
  , MountPoint(..)
  , Label(..)
  , PartitionTableIdentifier(..)
  , PartitionTableType(..)
  , PartitionType(..)
  , PartitionLabel(..)
  , PartitionUUID(..)
  , PartitionFlags(..)
  , DeviceReadAhead(..)
  , DeviceIsReadOnly(..)
  , DeviceIsRemovable(..)
  , DeviceIsHotPluggable(..)
  , DeviceModel(..)
  , SerialNumber(..)
  , DeviceSize(..)
  , DeviceState(..)
  , DeviceOwner(..)
  , DeviceGroup(..)
  , DeviceMode(..)
  , AlignmentOffset(..)
  , MinimumIOSize(..)
  , OptimalIOSize(..)
  , PhysicalSectorSize(..)
  , LogicalSectorSize(..)
  , IsRotationalDevice(..)
  , IOScheduler(..)
  , RequestQueueSize(..)
  , DeviceType(..)
  , DiscardAlignmentOffset(..)
  , DiscardGranularity(..)
  , DiscardMax(..)
  , DoesDiscardZeroData(..)
  , WriteSameMaxBytes(..)
  , UniqueStorageIdentifier(..)
  , AddsRandomness(..)
  , ParentKernelDeviceName(..)
  , SCSIHost(..)
  , SCSIChannel(..)
  , SCSITarget(..)
  , SCSILun(..)
  , SCSIHostChannelTargetLun(..)
  , SubSystem(..)
  , SubSystems(..)
  , DeviceRevision(..)
  , DeviceVendor(..)
  , ZoneModel(..)
  , DeviceNumber(..)
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad.Fail (MonadFail(..), fail)
import Data.Aeson.Types (typeMismatch)
import Data.BlockDevice.Internal.JSON (remapKeysFromJson, remapKeysToJson)
import Data.Bool (Bool(..))
import Data.Either (Either(..), either)
import Data.Eq (Eq(..))
import Data.Function (($), (.))
import Data.Functor (Functor(..))
import Data.Maybe (Maybe(..))
import Data.Ord (Ord(..))
import Data.Semigroup ((<>))
import Data.String (IsString(..))
import Data.UUID (UUID)
import Data.Word (Word)
import GHC.Generics (Generic)
import Prelude (Bounded, Enum, Integral(..), Num, Real)
import Text.Read (Read)
import Text.Show (Show(..))

import qualified Data.Aeson as DA
import qualified Data.Attoparsec.Text as DAT
import qualified Data.BlockDevice.Internal.Parsers as DBIP
import qualified Data.Text as DT

data BlockDevice where
  BlockDevice
    :: { name :: Maybe DeviceName
       , kname :: Maybe KernelDeviceName
       , path :: Maybe DevicePath
       , majmin :: Maybe DeviceNumber
       , fsavail :: Maybe AvailableFileSystemSpace
       , fssize :: Maybe FileSystemSize
       , fstype :: Maybe FileSystemType
       , fsused :: Maybe FileSystemUsedSpace
       , fsuse :: Maybe FileSystemPercentageInUse
       , mountpoint :: Maybe MountPoint
       , label :: Maybe Label
       , uuid :: Maybe FileSystemUUID
       , ptuuid :: Maybe PartitionTableIdentifier
       , pttype :: Maybe PartitionTableType
       , parttype :: Maybe PartitionType
       , partlabel :: Maybe PartitionLabel
       , partuuid :: Maybe PartitionUUID
       , partflags :: Maybe PartitionFlags
       , ra :: Maybe DeviceReadAhead
       , ro :: Maybe DeviceIsReadOnly
       , rm :: Maybe DeviceIsRemovable
       , hotplug :: Maybe DeviceIsHotPluggable
       , model :: Maybe DeviceModel
       , serial :: Maybe SerialNumber
       , size :: Maybe DeviceSize
       , state :: Maybe DeviceState
       , owner :: Maybe DeviceOwner
       , group :: Maybe DeviceGroup
       , mode :: Maybe DeviceMode
       , alignment :: Maybe AlignmentOffset
       , min_io :: Maybe MinimumIOSize
       , opt_io :: Maybe OptimalIOSize
       , phy_sec :: Maybe PhysicalSectorSize
       , log_sec :: Maybe LogicalSectorSize
       , rota :: Maybe IsRotationalDevice
       , sched :: Maybe IOScheduler
       , rq_size :: Maybe RequestQueueSize
       , type' :: Maybe DeviceType
       , disc_aln :: Maybe DiscardAlignmentOffset
       , disc_gran :: Maybe DiscardGranularity
       , disc_max :: Maybe DiscardMax
       , disc_zero :: Maybe DoesDiscardZeroData
       , wsame :: Maybe WriteSameMaxBytes
       , wwn :: Maybe UniqueStorageIdentifier
       , rand :: Maybe AddsRandomness
       , pkname :: Maybe ParentKernelDeviceName
       , hctl :: Maybe SCSIHostChannelTargetLun
       , tran :: Maybe DeviceTransportType
       , subsystems :: Maybe SubSystems
       , rev :: Maybe DeviceRevision
       , vendor :: Maybe DeviceVendor
       , zoned :: Maybe ZoneModel
       , children :: Maybe [BlockDevice]}
    -> BlockDevice

deriving instance Show BlockDevice

deriving instance Generic BlockDevice

deriving instance Eq BlockDevice

deriving instance Ord BlockDevice

instance DA.FromJSON BlockDevice where
  parseJSON v@(DA.Object _) =
    DA.genericParseJSON DA.defaultOptions (remapKeysFromJson v)
  parseJSON v = typeMismatch "Object" v

instance DA.ToJSON BlockDevice where
  toJSON = remapKeysToJson . DA.genericToJSON DA.defaultOptions

newtype Byte =
  Byte Word
  deriving ( Show
           , Eq
           , Ord
           , Read
           , Enum
           , Num
           , Real
           , Bounded
           , Integral
           , DA.FromJSON
           , DA.ToJSON
           )

newtype Percentage =
  Percentage Word
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

newtype DeviceName =
  DeviceName DT.Text
  deriving (Show, Eq, Ord, IsString, DA.FromJSON, DA.ToJSON, Generic)

newtype KernelDeviceName =
  KernelDeviceName DT.Text
  deriving (Show, Eq, Ord, IsString, DA.FromJSON, DA.ToJSON, Generic)

newtype DevicePath =
  DevicePath DT.Text
  deriving (Show, Eq, Ord, IsString, DA.FromJSON, DA.ToJSON, Generic)

newtype MajorDeviceNumber =
  MajorDeviceNumber Word
  deriving (Show, Eq, Ord, Read, Enum, Num, Real, Bounded, Integral, Generic)

newtype MinorDeviceNumber =
  MinorDeviceNumber Word
  deriving (Show, Eq, Ord, Read, Enum, Num, Real, Bounded, Integral, Generic)

newtype AvailableFileSystemSpace =
  AvailableFileSystemSpace Byte
  deriving (Show, Eq, Ord, Read, Enum, Num, Real, Bounded, Integral, Generic)

instance DA.FromJSON AvailableFileSystemSpace where
  parseJSON =
    fmap (AvailableFileSystemSpace . Byte) . DBIP.parseIntegralStringAsAeson

instance DA.ToJSON AvailableFileSystemSpace where
  toJSON (AvailableFileSystemSpace (Byte w)) = DA.String . DT.pack . show $ w

newtype FileSystemSize =
  FileSystemSize Byte
  deriving (Show, Eq, Ord, Read, Enum, Num, Real, Bounded, Integral, Generic)

instance DA.FromJSON FileSystemSize where
  parseJSON = fmap (FileSystemSize . Byte) . DBIP.parseIntegralStringAsAeson

instance DA.ToJSON FileSystemSize where
  toJSON (FileSystemSize (Byte w)) = DA.String . DT.pack . show $ w

newtype FileSystemType =
  FileSystemType DT.Text
  deriving (Show, Eq, Ord, IsString, DA.FromJSON, DA.ToJSON, Generic)

newtype FileSystemUsedSpace =
  FileSystemUsedSpace Byte
  deriving (Show, Eq, Ord, Read, Enum, Num, Real, Bounded, Integral, Generic)

instance DA.FromJSON FileSystemUsedSpace where
  parseJSON =
    fmap (FileSystemUsedSpace . Byte) . DBIP.parseIntegralStringAsAeson

instance DA.ToJSON FileSystemUsedSpace where
  toJSON (FileSystemUsedSpace (Byte w)) = DA.String . DT.pack . show $ w

newtype FileSystemPercentageInUse =
  FileSystemPercentageInUse Percentage
  deriving (Show, Eq, Ord, Generic)

instance DA.FromJSON FileSystemPercentageInUse where
  parseJSON (DA.String t) =
    case DAT.parseOnly (DBIP.parseIntegralWithSuffix "%") t of
      Right i -> pure $ FileSystemPercentageInUse (Percentage i)
      Left err -> fail err
  parseJSON other = typeMismatch "String" other

instance DA.ToJSON FileSystemPercentageInUse where
  toJSON (FileSystemPercentageInUse (Percentage i)) =
    DA.String $ (DT.pack . show $ i) <> "%"

newtype MountPoint =
  MountPoint DT.Text
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

newtype Label =
  Label DT.Text
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

newtype PartitionTableIdentifier =
  PartitionTableIdentifier DT.Text
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

newtype PartitionTableType =
  PartitionTableType DT.Text
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

newtype PartitionType =
  PartitionType UUID
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

newtype PartitionLabel =
  PartitionLabel DT.Text
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

newtype FileSystemUUID =
  FileSystemUUID DT.Text
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

newtype PartitionUUID =
  PartitionUUID UUID
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

newtype PartitionFlags =
  PartitionFlags DT.Text
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

newtype DeviceReadAhead =
  DeviceReadAhead Byte
  deriving ( Show
           , Eq
           , Ord
           , Read
           , Enum
           , Num
           , Real
           , Bounded
           , Integral
           , DA.FromJSON
           , DA.ToJSON
           , Generic
           )

newtype DeviceIsReadOnly =
  DeviceIsReadOnly Bool
  deriving (Show, Eq, Ord, Read, Enum, Bounded, DA.FromJSON, DA.ToJSON, Generic)

newtype DeviceIsRemovable =
  DeviceIsRemovable Bool
  deriving (Show, Eq, Ord, Read, Enum, Bounded, DA.FromJSON, DA.ToJSON, Generic)

newtype DeviceIsHotPluggable =
  DeviceIsHotPluggable Bool
  deriving (Show, Eq, Ord, Read, Enum, Bounded, DA.FromJSON, DA.ToJSON, Generic)

newtype DeviceModel =
  DeviceModel DT.Text
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

newtype SerialNumber =
  SerialNumber DT.Text
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

newtype DeviceSize =
  DeviceSize Byte
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

newtype DeviceState =
  DeviceState DT.Text
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

newtype DeviceOwner =
  DeviceOwner DT.Text
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

newtype DeviceGroup =
  DeviceGroup DT.Text
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

newtype DeviceMode =
  DeviceMode DT.Text
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

newtype AlignmentOffset =
  AlignmentOffset Byte
  deriving ( Show
           , Eq
           , Ord
           , Read
           , Enum
           , Num
           , Real
           , Bounded
           , Integral
           , DA.FromJSON
           , DA.ToJSON
           , Generic
           )

newtype MinimumIOSize =
  MinimumIOSize Byte
  deriving ( Show
           , Eq
           , Ord
           , Read
           , Enum
           , Num
           , Real
           , Bounded
           , Integral
           , DA.FromJSON
           , DA.ToJSON
           , Generic
           )

newtype OptimalIOSize =
  OptimalIOSize Byte
  deriving ( Show
           , Eq
           , Ord
           , Read
           , Enum
           , Num
           , Real
           , Bounded
           , Integral
           , DA.FromJSON
           , DA.ToJSON
           , Generic
           )

newtype PhysicalSectorSize =
  PhysicalSectorSize Byte
  deriving ( Show
           , Eq
           , Ord
           , Read
           , Enum
           , Num
           , Real
           , Bounded
           , Integral
           , DA.FromJSON
           , DA.ToJSON
           , Generic
           )

newtype LogicalSectorSize =
  LogicalSectorSize Byte
  deriving ( Show
           , Eq
           , Ord
           , Read
           , Enum
           , Num
           , Real
           , Bounded
           , Integral
           , DA.FromJSON
           , DA.ToJSON
           , Generic
           )

newtype IsRotationalDevice =
  IsRotationalDevice Bool
  deriving (Show, Eq, Ord, Read, Enum, Bounded, DA.FromJSON, DA.ToJSON, Generic)

newtype IOScheduler =
  IOScheduler DT.Text
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

newtype RequestQueueSize =
  RequestQueueSize Byte
  deriving ( Show
           , Eq
           , Ord
           , Read
           , Enum
           , Num
           , Real
           , Bounded
           , Integral
           , DA.FromJSON
           , DA.ToJSON
           , Generic
           )

newtype DeviceType =
  DeviceType DT.Text
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

newtype DiscardAlignmentOffset =
  DiscardAlignmentOffset Byte
  deriving ( Show
           , Eq
           , Ord
           , Read
           , Enum
           , Num
           , Real
           , Bounded
           , Integral
           , DA.FromJSON
           , DA.ToJSON
           , Generic
           )

newtype DiscardGranularity =
  DiscardGranularity Byte
  deriving ( Show
           , Eq
           , Ord
           , Read
           , Enum
           , Num
           , Real
           , Bounded
           , Integral
           , DA.FromJSON
           , DA.ToJSON
           , Generic
           )

newtype DiscardMax =
  DiscardMax Byte
  deriving ( Show
           , Eq
           , Ord
           , Read
           , Enum
           , Num
           , Real
           , Bounded
           , Integral
           , DA.FromJSON
           , DA.ToJSON
           , Generic
           )

newtype DoesDiscardZeroData =
  DoesDiscardZeroData Bool
  deriving (Show, Eq, Ord, Enum, Bounded, DA.ToJSON, DA.FromJSON, Generic)

newtype WriteSameMaxBytes =
  WriteSameMaxBytes Byte
  deriving ( Show
           , Eq
           , Ord
           , Read
           , Enum
           , Num
           , Real
           , Bounded
           , Integral
           , DA.FromJSON
           , DA.ToJSON
           , Generic
           )

newtype UniqueStorageIdentifier =
  UniqueStorageIdentifier DT.Text
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

newtype AddsRandomness =
  AddsRandomness Bool
  deriving (Show, Eq, Ord, Enum, Bounded, DA.ToJSON, DA.FromJSON, Generic)

newtype ParentKernelDeviceName =
  ParentKernelDeviceName DT.Text
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

newtype SCSIHost =
  SCSIHost Word
  deriving (Show, Eq, Ord, Read, Enum, Num, Real, Bounded, Integral, Generic)

newtype SCSIChannel =
  SCSIChannel Word
  deriving (Show, Eq, Ord, Read, Enum, Num, Real, Bounded, Integral, Generic)

newtype SCSITarget =
  SCSITarget Word
  deriving (Show, Eq, Ord, Read, Enum, Num, Real, Bounded, Integral, Generic)

newtype SCSILun =
  SCSILun Word
  deriving (Show, Eq, Ord, Read, Enum, Num, Real, Bounded, Integral, Generic)

data SCSIHostChannelTargetLun where
  SCSIHostChannelTargetLun
    :: { scsiHost :: SCSIHost
       , scsiChannel :: SCSIChannel
       , scsiTarget :: SCSITarget
       , scsiLun :: SCSILun}
    -> SCSIHostChannelTargetLun

deriving instance Eq SCSIHostChannelTargetLun

deriving instance Ord SCSIHostChannelTargetLun

deriving instance Show SCSIHostChannelTargetLun

deriving instance Generic SCSIHostChannelTargetLun

instance DA.FromJSON SCSIHostChannelTargetLun where
  parseJSON (DA.String s) =
    case DAT.parseOnly DBIP.parseNonEmptyColonSeparatedIntegrals s of
      Right [h, c, t, l] ->
        pure $
        SCSIHostChannelTargetLun
          (SCSIHost h)
          (SCSIChannel c)
          (SCSITarget t)
          (SCSILun l)
      Right xs ->
        fail $
        "SCSIHostChannelTargetLun must be exactly 4 : separated values: " <>
        show xs
      Left err -> fail err
  parseJSON other = typeMismatch "String" other

instance DA.ToJSON SCSIHostChannelTargetLun where
  toJSON (SCSIHostChannelTargetLun (SCSIHost h) (SCSIChannel c) (SCSITarget t) (SCSILun l)) =
    let f = DT.pack . show
        h' = f h
        c' = f c
        t' = f t
        l' = f l
        sep = ":"
     in DA.String $ h' <> sep <> c' <> sep <> t' <> sep <> l'

newtype DeviceTransportType =
  DeviceTransportType DT.Text
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

newtype SubSystem =
  SubSystem DT.Text
  deriving (Show, Eq, Ord, Generic)

newtype SubSystems =
  SubSystems [SubSystem]
  deriving (Show, Eq, Ord, Generic)

instance DA.FromJSON SubSystems where
  parseJSON (DA.String t) =
    either fail (pure . SubSystems . fmap SubSystem) $
    DAT.parseOnly DBIP.parseColonSeparatedText t
  parseJSON other = typeMismatch "String" other

instance DA.ToJSON SubSystems where
  toJSON (SubSystems xs) = DA.String . DT.intercalate ":" $ fmap unwrap xs
    where
      unwrap :: SubSystem -> DT.Text
      unwrap (SubSystem t) = t

newtype DeviceRevision =
  DeviceRevision DT.Text
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

newtype DeviceVendor =
  DeviceVendor DT.Text
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

newtype ZoneModel =
  ZoneModel DT.Text
  deriving (Show, Eq, Ord, DA.FromJSON, DA.ToJSON, Generic)

data DeviceNumber where
  DeviceNumber
    :: { major :: MajorDeviceNumber
       , minor :: MinorDeviceNumber}
    -> DeviceNumber

deriving instance Show DeviceNumber

deriving instance Eq DeviceNumber

deriving instance Ord DeviceNumber

deriving instance Generic DeviceNumber

instance DA.FromJSON DeviceNumber where
  parseJSON (DA.String t) =
    case DAT.parseOnly DBIP.parseNonEmptyColonSeparatedIntegrals t of
      Right [major, minor] ->
        pure $ DeviceNumber (MajorDeviceNumber major) (MinorDeviceNumber minor)
      Right xs ->
        fail $
        "DeviceNumber values must be exactly 2 : separated values: " <> show xs
      Left err -> fail err
  parseJSON other = typeMismatch "String" other

instance DA.ToJSON DeviceNumber where
  toJSON (DeviceNumber (MajorDeviceNumber major) (MinorDeviceNumber minor)) =
    let f = DT.pack . show
        major' = f major
        minor' = f minor
     in DA.String $ major' <> ":" <> minor'
