module Api (
  app,
  getTransactionFeeEstimate,
  getExUnits,
  apiDocs,
) where

import Api.ExUnits qualified as ExUnits
import Api.Fees qualified as Fees
import Control.Monad.Catch (try)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.Kind (Type)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant (
  Application,
  Get,
  Handler,
  HasServer (ServerT),
  JSON,
  Post,
  Proxy (..),
  QueryParam',
  ReqBody,
  Required,
  Server,
  ServerError (errBody),
  err400,
  hoistServer,
  serve,
  type (:<|>) ((:<|>)),
  type (:>),
 )
import Servant.Client (ClientM, client)
import Servant.Docs qualified as Docs
import Types (
  AppM (..),
  CardanoBrowserServerError (..),
  Cbor,
  DecodeError (InvalidCbor, InvalidHex),
  Env,
  ExUnitsRequest,
  ExUnitsResponse,
  Fee,
 )
import Utils (lbshow)

type Api =
  "fees" :> QueryParam' '[Required] "tx" Cbor :> Get '[JSON] Fee
    -- This doesn't need to be a post request of course, but it would probably be
    -- less straightforward to URL-encode the JSON utxo on the client and pass
    -- it as a query param
    :<|> "ex-units"
      :> ReqBody '[JSON] ExUnitsRequest
      :> Post '[JSON] ExUnitsResponse

app :: Env -> Application
app = simpleCors . serve api . appServer

appServer :: Env -> Server Api
appServer env = hoistServer api appHandler server
  where
    appHandler :: forall (a :: Type). AppM a -> Handler a
    appHandler (AppM x) = tryServer x >>= either handleError pure
      where
        tryServer ::
          ReaderT Env IO a ->
          Handler (Either CardanoBrowserServerError a)
        tryServer =
          liftIO
            . try @_ @CardanoBrowserServerError
            . flip runReaderT env

        handleError ::
          CardanoBrowserServerError ->
          Handler a
        handleError (Decode fe) = case fe of
          InvalidCbor ic -> throwError err400 {errBody = lbshow ic}
          InvalidHex ih -> throwError err400 {errBody = LC8.pack ih}

api :: Proxy Api
api = Proxy

server :: ServerT Api AppM
server = Fees.estimateTxFees :<|> ExUnits.calculateExUnits

apiDocs :: Docs.API
apiDocs = Docs.docs api

getTransactionFeeEstimate :: Cbor -> ClientM Fee
getExUnits :: ExUnitsRequest -> ClientM ExUnitsResponse
getTransactionFeeEstimate :<|> getExUnits = client api
