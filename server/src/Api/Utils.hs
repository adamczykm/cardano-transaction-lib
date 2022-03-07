module Api.Utils (decodeCborTx) where

import Cardano.Api qualified as C
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import Types (
  Cbor (..),
  DecodeError (..),
 )

decodeCborTx :: Cbor -> Either DecodeError (C.Tx C.AlonzoEra)
decodeCborTx (Cbor txt) =
  first InvalidCbor
    . C.deserialiseFromCBOR (C.proxyToAsType Proxy)
    =<< decode txt
  where
    decode :: Text -> Either DecodeError ByteString
    decode = first InvalidHex . Base16.decode . Text.Encoding.encodeUtf8
