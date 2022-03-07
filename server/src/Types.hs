{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}

module Types (
  AppM (AppM),
  Env (..),
  Cbor (..),
  Fee (..),
  ExUnitsRequest (..),
  ExUnitsResponse (..),
  ExUnitsResult (..),
  RedeemerTag (..),
  ScriptIndex (..),
  ExUnits (..),
  DecodeError (..),
  CardanoBrowserServerError (..),
  newEnvIO,
  exUnitsFromCardanoMap,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as Shelley
import Cardano.Binary qualified as Cbor
import Cardano.Slotting.Time (SystemStart (SystemStart))
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toJSON))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (withText)
import Data.Bifunctor (bimap, second)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Format.ISO8601 (iso8601ParseM)
import GHC.Generics (Generic)
import Paths_cardano_browser_tx_server (getDataFileName)
import Servant (FromHttpApiData, QueryParam', Required, ToHttpApiData)
import Servant.Docs qualified as Docs
import System.Exit (die)
import Text.Read (readMaybe)
import Utils (tshow)

newtype AppM (a :: Type) = AppM (ReaderT Env IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Env
    , MonadThrow
    )

data Env = Env
  { systemStart :: SystemStart
  , protocolParams :: Shelley.ProtocolParameters
  }
  deriving stock (Generic)

newEnvIO :: IO Env
newEnvIO = Env <$> decodeSystemStart <*> readParams
  where
    readParams :: IO Shelley.ProtocolParameters
    readParams =
      either die pure
        =<< Aeson.eitherDecodeFileStrict @Shelley.ProtocolParameters
        =<< getDataFileName "config/pparams.json"

decodeSystemStart :: IO SystemStart
decodeSystemStart = SystemStart <$> iso8601ParseM shelleySystemStart
  where
    shelleySystemStart :: String
    -- System start time from the Shelley genesis file
    shelleySystemStart = "2019-07-24T20:20:16Z"

newtype Cbor = Cbor Text
  deriving stock (Show)
  deriving newtype
    ( Eq
    , FromHttpApiData
    , ToHttpApiData
    , FromJSON
    , ToJSON
    )

-- Some integral values should be stringified before being sent to the frontend
-- to avoid precision loss with Purescript\'s JSON decoder. This type provides
-- @To@/@FromJSON@ instances where this might be desirable
newtype Stringified (a :: Type) = Stringified a

instance Show a => ToJSON (Stringified a) where
  toJSON (Stringified x) = Aeson.String $ tshow x

instance Read a => FromJSON (Stringified a) where
  parseJSON =
    withText "Stringified" $
      maybe (fail "Expected quoted integral type") (pure . Stringified)
        . readMaybe @a
        . Text.unpack

newtype Fee = Fee Integer
  deriving stock (Show, Generic)
  deriving newtype (Eq)
  deriving (ToJSON, FromJSON) via Stringified Integer

data ExUnitsRequest = ExUnitsRequest
  { -- | CBOR-encoded transaction. Technically we only need the tx body for
    -- @evaluateTransactionExecutionUnits@, but we may need the signatures if
    -- re-writing that function is necessary. See:
    -- https://github.com/input-output-hk/cardano-node/issues/3307
    tx :: Cbor
  , utxo :: C.UTxO C.AlonzoEra
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- @evaluateTransactionExecutionUnits@ returns a @Map@ which would not be
-- particularly JSON-friendly as it would have @ScriptWitnessIndex@ keys
newtype ExUnitsResponse = ExUnitsResponse [ExUnitsResult]
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON, ToJSON)

exUnitsFromCardanoMap ::
  Map
    C.ScriptWitnessIndex
    ( Either C.ScriptExecutionError C.ExecutionUnits
    ) ->
  ExUnitsResponse
exUnitsFromCardanoMap = ExUnitsResponse . fmap (uncurry f) . Map.toList
  where
    f ::
      C.ScriptWitnessIndex ->
      Either C.ScriptExecutionError C.ExecutionUnits ->
      ExUnitsResult
    f swi es =
      let (tag, index) = convertIndex swi
          exUnits = bimap tshow ExUnits es
       in ExUnitsResult {tag, index, exUnits}

    convertIndex :: C.ScriptWitnessIndex -> (RedeemerTag, ScriptIndex)
    convertIndex =
      second ScriptIndex . \case
        C.ScriptWitnessIndexTxIn idx -> (Spend, idx)
        C.ScriptWitnessIndexMint idx -> (Mint, idx)
        C.ScriptWitnessIndexCertificate idx -> (Cert, idx)
        C.ScriptWitnessIndexWithdrawal idx -> (Reward, idx)

data ExUnitsResult = ExUnitsResult
  { tag :: RedeemerTag
  , index :: ScriptIndex
  , -- @ScriptExecutionError@ has no @ToJSON@ instance, so can be converted to
    -- text instead
    exUnits :: Either Text ExUnits
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data RedeemerTag
  = Spend
  | Mint
  | Cert
  | Reward
  deriving stock (Show, Eq, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON)

newtype ScriptIndex = ScriptIndex Word
  deriving stock (Show, Eq)
  deriving (FromJSON, ToJSON) via Stringified Word

-- TODO replace this with data type holding `Stringified` fields
newtype ExUnits = ExUnits C.ExecutionUnits
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON)

instance ToJSON ExUnits where
  toJSON (ExUnits (C.ExecutionUnits mem steps)) =
    -- Again, encode naturals as text to avoid precision loss
    Aeson.object
      [ "mem" .= tshow mem
      , "steps" .= tshow steps
      ]

-- Errors

-- We'll probably extend this with more error types over time
newtype CardanoBrowserServerError = DecodeError DecodeError
  deriving stock (Show)

instance Exception CardanoBrowserServerError

data DecodeError
  = InvalidCbor Cbor.DecoderError
  | InvalidHex String
  deriving stock (Show)

instance Exception DecodeError

-- API doc stuff
instance Docs.ToParam (QueryParam' '[Required] "tx" Cbor) where
  toParam _ =
    Docs.DocQueryParam
      "tx"
      [sampleTx]
      "A CBOR-encoded `Tx AlonzoEra`; should be sent as a hexadecimal string"
      Docs.Normal
    where
      sampleTx =
        mconcat
          [ "84a300818258205d677265fa5bb21ce6d8c7502aca70b93"
          , "16d10e958611f3c6b758f65ad9599960001818258390030"
          , "fb3b8539951e26f034910a5a37f22cb99d94d1d409f69dd"
          , "baea9711c12f03c1ef2e935acc35ec2e6f96c650fd3bfba"
          , "3e96550504d5336100021a0002b569a0f5f6"
          ]

instance Docs.ToSample Fee where
  toSamples _ =
    [
      ( "The `Fee` will be returned encoded as a JSON string"
      , Fee 160265
      )
    ]

instance Docs.ToSample ExUnitsRequest where
  toSamples = error "TODO"

instance Docs.ToSample ExUnitsResponse where
  toSamples = error "TODO"
