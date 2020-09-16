module Main.DB
(
  session,
  oneRow,
  unit,
  integerDatetimes,
  serverVersion,
)
where

import Main.Prelude hiding (unit)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Data.ByteString as ByteString; import Data.ByteString (ByteString)


type Session =
  ExceptT ByteString (ReaderT LibPQ.Connection IO)

session :: Session a -> IO (Either ByteString a)
session m =
  do
    c <- connect
    initConnection c
    r <- runReaderT (runExceptT m) c
    LibPQ.finish c
    return r

oneRow :: ByteString -> [Maybe (LibPQ.Oid, ByteString, LibPQ.Format)] -> LibPQ.Format -> Session ByteString
oneRow statement params outFormat =
  do
    Just result <- result statement params outFormat
    Just result <- liftIO $ LibPQ.getvalue result 0 0
    return result

unit :: ByteString -> [Maybe (LibPQ.Oid, ByteString, LibPQ.Format)] -> Session ()
unit statement params =
  void $ result statement params LibPQ.Binary

result :: ByteString -> [Maybe (LibPQ.Oid, ByteString, LibPQ.Format)] -> LibPQ.Format -> Session (Maybe LibPQ.Result)
result statement params outFormat =
  do
    result <- ExceptT $ ReaderT $ \connection -> fmap Right $ LibPQ.execParams connection statement params outFormat
    checkResult result
    return result

checkResult :: Maybe LibPQ.Result -> Session ()
checkResult result =
  ExceptT $ ReaderT $ \connection -> do
    case result of
      Just result -> do
        LibPQ.resultErrorField result LibPQ.DiagMessagePrimary >>= maybe (return (Right ())) (return . Left)
      Nothing -> do
        m <- LibPQ.errorMessage connection
        return $ Left $ maybe "Fatal PQ error" (\m -> "Fatal PQ error: " <> m) m

integerDatetimes :: Session Bool
integerDatetimes =
  lift (ReaderT getIntegerDatetimes)

serverVersion :: Session Int
serverVersion =
  lift (ReaderT LibPQ.serverVersion)

-- *
-------------------------

connect :: IO LibPQ.Connection
connect =
  LibPQ.connectdb bs
  where
    bs = 
      ByteString.intercalate " " components
      where
        components = 
          [
            "host=" <> host,
            "port=" <> (fromString . show) port,
            "user=" <> user,
            "password=" <> password,
            "dbname=" <> db
          ]
          where
            host = "localhost"
            port = 5432
            user = "postgres"
            password = ""
            db = "postgres"

initConnection :: LibPQ.Connection -> IO ()
initConnection c =
  void $ LibPQ.exec c $ mconcat $ map (<> ";") $ 
    [ 
      "SET client_min_messages TO WARNING",
      "SET client_encoding = 'UTF8'",
      "SET intervalstyle = 'postgres'"
    ]

getIntegerDatetimes :: LibPQ.Connection -> IO Bool
getIntegerDatetimes c =
  fmap parseResult $ LibPQ.parameterStatus c "integer_datetimes"
  where
    parseResult = 
      \case
        Just "on" -> True
        _ -> False
