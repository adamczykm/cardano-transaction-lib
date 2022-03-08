{-# LANGUAGE NamedFieldPuns #-}

module Api.ExUnits (
  calculateExUnits,
) where

import Api.Utils (decodeCborTx)
import Cardano.Api qualified as C
import Control.Monad.Catch (throwM)
import Control.Monad.Reader (MonadReader (ask))
import Types (
  AppM,
  CardanoBrowserServerError (DecodeError, ExUnitsError),
  Env (Env, protocolParams, systemStart),
  ExUnitsRequest (ExUnitsRequest, tx, utxo),
  ExUnitsResponse,
  exUnitsFromCardanoMap,
 )

calculateExUnits :: ExUnitsRequest -> AppM ExUnitsResponse
calculateExUnits ExUnitsRequest {tx, utxo} = do
  Env {systemStart, protocolParams} <- ask
  txBody <-
    C.getTxBody
      <$> either (throwM . DecodeError) pure (decodeCborTx tx)
  either (throwM . ExUnitsError) (pure . exUnitsFromCardanoMap) $
    C.evaluateTransactionExecutionUnits
      C.AlonzoEraInCardanoMode
      systemStart
      (error "TODO") -- Era history
      protocolParams
      utxo
      txBody
