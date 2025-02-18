module Types.TransactionMetadata
  ( GeneralTransactionMetadata(..)
  , TransactionMetadatum(..)
  , TransactionMetadatumLabel(..)
  ) where

import Prelude

import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map (empty) as Map
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Helpers (appendRightMap, showWithParens)
import Types.ByteArray (ByteArray)
import Types.Int (Int) as Int

newtype GeneralTransactionMetadata =
  GeneralTransactionMetadata
    (Map TransactionMetadatumLabel TransactionMetadatum)

derive instance Newtype GeneralTransactionMetadata _

derive newtype instance Eq GeneralTransactionMetadata
derive instance Generic GeneralTransactionMetadata _

instance Show GeneralTransactionMetadata where
  show = genericShow

-- This Semigroup instance simply takes the Last value for duplicate keys
-- to avoid a Semigroup instance for TransactionMetadatum.
-- Do we want to avoid a Semigroup instance for TransactionMetadatum? Recursion
-- is fine but how to combine Text with Bytes for example? One would have to take
-- precedence and replace the other.
instance Semigroup GeneralTransactionMetadata where
  append (GeneralTransactionMetadata hm) (GeneralTransactionMetadata hm') =
    GeneralTransactionMetadata $ hm `appendRightMap` hm'

instance Monoid GeneralTransactionMetadata where
  mempty = GeneralTransactionMetadata Map.empty

newtype TransactionMetadatumLabel = TransactionMetadatumLabel BigInt

derive instance Newtype TransactionMetadatumLabel _
derive newtype instance Eq TransactionMetadatumLabel
derive newtype instance Ord TransactionMetadatumLabel
derive instance Generic TransactionMetadatumLabel _

instance Show TransactionMetadatumLabel where
  show (TransactionMetadatumLabel tml) =
    showWithParens "TransactionMetadatumLabel" tml

data TransactionMetadatum
  = MetadataMap (Map TransactionMetadatum TransactionMetadatum)
  | MetadataList (Array TransactionMetadatum)
  | Int Int.Int
  | Bytes ByteArray
  | Text String

derive instance Eq TransactionMetadatum
derive instance Ord TransactionMetadatum
derive instance Generic TransactionMetadatum _

instance Show TransactionMetadatum where
  show x = genericShow x
