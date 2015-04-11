module PostgreSQLBinary.Decoder where

import PostgreSQLBinary.Prelude hiding (bool)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Scientific as Scientific
import qualified Data.UUID as UUID
import qualified PostgreSQLBinary.Decoder.Zepto as Zepto
import qualified PostgreSQLBinary.Array as Array
import qualified PostgreSQLBinary.Time as Time
import qualified PostgreSQLBinary.Integral as Integral
import qualified PostgreSQLBinary.Numeric as Numeric
import qualified PostgreSQLBinary.Interval as Interval


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
  unsafeCoerce (int :: D Word32)

{-# INLINABLE float8 #-}
float8 :: D Double
float8 =
  unsafeCoerce (int :: D Word64)

{-# INLINABLE numeric #-}
numeric :: D Scientific
numeric =
  flip Zepto.run (inline Zepto.numeric)


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
-- The decoding strategy depends on whether the server supports @integer_datetimes@.
{-# INLINABLE time #-}
time :: Bool -> D TimeOfDay
time =
  \case
    True ->
      fmap Time.microsToTimeOfDay . int
    False ->
      fmap Time.secsToTimeOfDay . float8

-- |
-- The decoding strategy depends on whether the server supports @integer_datetimes@.
{-# INLINABLE timetz #-}
timetz :: Bool -> D (TimeOfDay, TimeZone)
timetz integer_datetimes =
  \x -> 
    let (timeX, zoneX) = B.splitAt 8 x
        in (,) <$> time integer_datetimes timeX <*> tz zoneX
  where
    tz =
      fmap (minutesToTimeZone . negate . (`div` 60) . fromIntegral) . (int :: D Int32)

-- |
-- The decoding strategy depends on whether the server supports @integer_datetimes@.
{-# INLINABLE timestamptz #-}
timestamp :: Bool -> D LocalTime
timestamp =
  \case
    True ->
      fmap Time.microsToLocalTime . int
    False ->
      fmap Time.secsToLocalTime . float8

-- |
-- The decoding strategy depends on whether the server supports @integer_datetimes@.
{-# INLINABLE timestamp #-}
timestamptz :: Bool -> D UTCTime
timestamptz =
  \case
    True ->
      fmap Time.microsToUTC . int
    False ->
      fmap Time.secsToUTC . float8

-- |
-- The decoding strategy depends on whether the server supports @integer_datetimes@.
{-# INLINABLE interval #-}
interval :: Bool -> D DiffTime
interval integerDatetimes =
  evalState $ do
    t <- state $ B.splitAt 8
    d <- state $ B.splitAt 4
    m <- get
    return $ do
      ux <- if integerDatetimes
              then int t
              else float8 t >>= return . round . (* (10^6)) . toRational
      dx <- int d
      mx <- int m
      return $ Interval.toDiffTime $ Interval.Interval ux dx mx


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

-- * Composite types
-------------------------

-- a composite type looks like:-
--   1. int32 number of fields;
--   2. fields
--
-- fields look like:-
--   1. OID;
--   2. size or NULL (\255\255\255\255 ie -1);
--   3. payload if it wasn't NULL

size :: D Int32
size s = case B.take 4 s of
  s' | B.length s' == 4 -> int s'
     | otherwise        -> Left "No length."

data Field a
  = Field
    { fieldOid   :: {-# UNPACK #-} !Int32
    , fieldSize  :: {-# UNPACK #-} !Int32
    , field      :: !a } 
  | NULL
 deriving (Show, Eq, Ord)

compositeFields :: D (V.Vector (Field ByteString))
compositeFields row = do
  s <- size row

  let
    fields :: ByteString
    fields = B.drop 4 row

    int32At :: Int -> Either Text Int32
    int32At n = int (B.take 4 (B.drop n fields))

    area :: Int -> Int -> B.ByteString
    area n len = B.take len (B.drop n fields)
  
  runST (do
    vector <- VM.new (fromIntegral s)
    
    let
      parse fi i | fi < fromIntegral s = do
        case int32At i of
          Left  err -> return (Just err)
          Right oid -> case int32At (i+4) of
            Left  err -> return (Just err)
            Right len -> do
              let leni    = fromIntegral len
                  payload = area (i+8) leni

              VM.write vector fi $!
                if len /= -1
                then Field oid len payload
                else NULL

              parse (fi+1) (i+8+max 0 leni)
      parse _ _ = return Nothing

    merr <- parse 0 0
    case merr of
      Just err -> return (Left err)
      Nothing  -> fmap Right (V.unsafeFreeze vector)
    )
