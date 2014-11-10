module PostgreSQLBinary.Decoder where

import PostgreSQLBinary.Prelude hiding (bool)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Scientific as Scientific
import qualified Data.UUID as UUID
import qualified PostgreSQLBinary.Decoder.Atto as Atto
import qualified PostgreSQLBinary.Decoder.Zepto as Zepto
import qualified PostgreSQLBinary.Array as Array
import qualified PostgreSQLBinary.Time as Time
import qualified PostgreSQLBinary.Integral as Integral
import qualified PostgreSQLBinary.Numeric as Numeric


-- |
-- A function for decoding a byte string into a value.
type D a = ByteString -> Either Text a


-- * Numbers
-------------------------

-- |
-- Any of PostgreSQL integer types.
{-# INLINABLE int #-}
int :: (Integral a, Bits a) => D a
int =
  Right . Integral.pack

{-# INLINABLE float4 #-}
float4 :: D Float
float4 =
  unsafeCoerce . (int :: D Word32)

{-# INLINABLE float8 #-}
float8 :: D Double
float8 =
  unsafeCoerce . (int :: D Word64)

{-# INLINABLE numeric #-}
numeric :: D Scientific
numeric =
  evalStateT $ do
    componentsAmount <- intOfSize 2
    pointIndex :: Int16 <- intOfSize 2
    signCode <- intOfSize 2
    modify (B.drop 2)
    components <- replicateM componentsAmount (intOfSize 2)
    signer <-
      if | signCode == Numeric.negSignCode -> return negate
         | signCode == Numeric.posSignCode -> return id
         | signCode == Numeric.nanSignCode -> lift $ Left "NAN sign"
         | otherwise -> lift $ Left $ "Unexpected sign value: " <> (fromString . show) signCode
    let
      c = signer $ fromIntegral $ (Numeric.mergeComponents components :: Word64)
      e = (fromIntegral (pointIndex + 1) - length components) * 4
      in return $ Scientific.scientific c e
  where
    intOfSize n =
      lift . int =<< state (B.splitAt n)


-- * Text
-------------------------

-- |
-- A UTF-8-encoded char.
{-# INLINABLE char #-}
char :: D Char
char x =
  maybe (Left "Empty input") (return . fst) . T.uncons =<< text x

-- |
-- Any of the variable-length character types:
-- BPCHAR, VARCHAR, NAME and TEXT.
{-# INLINABLE text #-}
text :: D Text
text =
  either (Left . fromString . show) Right . TE.decodeUtf8'

{-# INLINE bytea #-}
bytea :: D ByteString
bytea =
  Right

-- * Date and Time
-------------------------

{-# INLINABLE date #-}
date :: D Day
date =
  fmap (Time.postgresJulianToDay . fromIntegral) . (int :: D Int32)

-- |
-- Decoding strategy depends on whether the server supports @integer_datetimes@.
{-# INLINABLE time #-}
time :: Bool -> D TimeOfDay
time =
  \case
    True ->
      fmap Time.microsToTimeOfDay . int
    False ->
      fmap Time.secondsToTimeOfDay . float8

-- |
-- Decoding strategy depends on whether the server supports @integer_datetimes@.
{-# INLINABLE timetz #-}
timetz :: Bool -> D (TimeOfDay, TimeZone)
timetz integer_datetimes =
  \x -> 
    let (timeX, zoneX) = B.splitAt 8 x
        in (,) <$> time integer_datetimes timeX <*> tz zoneX
  where
    tz =
      fmap (minutesToTimeZone . negate . (`div` 60) . fromIntegral) . (int :: D Int32)

{-# INLINABLE timestamp #-}
timestamp :: D UTCTime
timestamp =
  fmap fromMicros . int
  where
    fromMicros =
      evalState $ do
        days <- state $ (`divMod` (10^6 * 60 * 60 * 24))
        micros <- get
        return $
          UTCTime 
            (Time.postgresJulianToDay days)
            (picosecondsToDiffTime . (* (10^6)) . fromIntegral $ micros)

{-# INLINABLE timestamptz #-}
timestamptz :: D LocalTime
timestamptz =
  fmap fromMicros . int
  where
    fromMicros =
      evalState $ do
        days <- state $ (`divMod` (10^6 * 60 * 60 * 24))
        micros <- get
        return $
          LocalTime 
            (Time.postgresJulianToDay days)
            (Time.microsToTimeOfDay micros)

{-# INLINABLE interval #-}
interval :: D DiffTime
interval =
  evalStateT $ do
    ub <- state $ B.splitAt 8
    db <- state $ B.splitAt 4
    mb <- get
    lift $ do
      u <- int ub
      d <- int db
      m <- int mb
      return $ picosecondsToDiffTime $ fromIntegral $
        (10 ^ 6 * (u + 10 ^ 6 * 60 * 60 * 24 * (d + 31 * m)) :: Int)



-- * Misc
-------------------------

{-# INLINABLE bool #-}
bool :: D Bool
bool b =
  case B.uncons b of
    Just (0, _) -> return False
    Just (1, _) -> return True
    _ -> Left ("Invalid value: " <> (fromString . show) b)

{-# INLINABLE uuid #-}
uuid :: D UUID
uuid =
  evalStateT $ 
    UUID.fromWords <$> word <*> word <*> word <*> word
  where
    word = 
      lift . int =<< state (B.splitAt 4)


-- |
-- Arbitrary array.
-- 
-- Returns an intermediate representation,
-- which can then be used to decode into a specific data type.
{-# INLINABLE array #-}
array :: D Array.Data
array =
  flip Zepto.run Zepto.array
