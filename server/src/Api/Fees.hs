module Api.Fees (estimateTxFees) where

import Api.Utils (decodeCborTx)
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as Shelley
import Control.Monad.Catch (throwM)
import Control.Monad.Reader (asks)
import Types (
  AppM,
  CardanoBrowserServerError (..),
  Cbor (..),
  Env (..),
  Fee (..),
 )

estimateTxFees :: Cbor -> AppM Fee
estimateTxFees cbor = do
  decoded <- either (throwM . DecodeError) pure $ decodeCborTx cbor
  pparams <- asks protocolParams
  pure . Fee $ estimateFee pparams decoded

estimateFee :: Shelley.ProtocolParameters -> C.Tx C.AlonzoEra -> Integer
estimateFee pparams (C.Tx txBody keyWits) = estimate
  where
    estimate :: Integer
    C.Lovelace estimate =
      let -- No. of Shelley key witnesses
          numWits = fromIntegral $ length keyWits
       in C.evaluateTransactionFee
            pparams
            txBody
            numWits
            -- No. of Byron key witnesses; there shouldn't be any of these and
            -- 'evaluateTransactionFee' won't work with them anyway
            0
