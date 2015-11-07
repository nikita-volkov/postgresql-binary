module Main.DB
(
  module LibPQ,
  session,
  oneRow,
  unit,
  integerDatetimes,
)
where

import Main.Prelude
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Database.PostgreSQL.LibPQ as LibPQ
import qualified Data.ByteString as ByteString; import Data.ByteString (ByteString)


type Session =
  EitherT ByteString (ReaderT Connection IO)

session :: Session a -> IO (Either ByteString a)
session m =
  do
    c <- connect
    initConnection c
    r <- runReaderT (runEitherT m) c
    finish c
    return r

oneRow :: ByteString -> [Maybe (Oid, ByteString, Format)] -> Format -> Session ByteString
oneRow statement params outFormat =
  do
    Just result <- result statement params outFormat
    Just result <- liftIO $ getvalue result 0 0
    return result

unit :: ByteString -> [Maybe (Oid, ByteString, Format)] -> Session ()
unit statement params =
  void $ result statement params Binary

result :: ByteString -> [Maybe (Oid, ByteString, Format)] -> Format -> Session (Maybe Result)
result statement params outFormat =
  do
    result <- EitherT $ ReaderT $ \connection -> fmap Right $ execParams connection statement params outFormat
    checkResult result
    return result

checkResult :: Maybe Result -> Session ()
checkResult result =
  EitherT $ ReaderT $ \connection -> do
    case result of
      Just result -> do
        resultErrorField result DiagMessagePrimary >>= maybe (return (Right ())) (return . Left)
      Nothing -> do
        m <- errorMessage connection
        return $ Left $ maybe "Fatal PQ error" (\m -> "Fatal PQ error: " <> m) m

integerDatetimes :: Session Bool
integerDatetimes =
  lift (ReaderT getIntegerDatetimes)

-- *
-------------------------

connect :: IO Connection
connect =
  connectdb bs
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

initConnection :: Connection -> IO ()
initConnection c =
  void $ exec c $ mconcat $ map (<> ";") $ 
    [ 
      "SET client_min_messages TO WARNING",
      "SET client_encoding = 'UTF8'",
      "SET intervalstyle = 'postgres'"
    ]

getIntegerDatetimes :: Connection -> IO Bool
getIntegerDatetimes c =
  fmap parseResult $ parameterStatus c "integer_datetimes"
  where
    parseResult = 
      \case
        Just "on" -> True
        _ -> False
