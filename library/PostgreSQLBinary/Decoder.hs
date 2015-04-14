module PostgreSQLBinary.Decoder where
import PostgreSQLBinary.Prelude hiding (bool)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
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
import qualified PostgreSQLBinary.Composite as Composite


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
--   1. word32 number of fields;
--   2. fields
--
-- fields look like:-
--   1. OID;
--   2. size or NULL (\255\255\255\255 ie -1);
--   3. payload if it wasn't NULL

{-# INLINE slice #-}
slice :: B.ByteString -> Int -> Int -> Either Text B.ByteString
slice bs n len = 
  if len+n <= B.length bs
  then Right $! BU.unsafeTake len $! BU.unsafeDrop n bs
  else Left "slice: string too short"

-- | Parse the fields of a composite type
composite :: D (V.Vector Composite.Field)
composite row = do
  let
    {-# INLINE num32At #-}
    num32At :: (Integral a, Bits a) => Int -> Either Text a
    num32At n = int =<< slice row n 4

  size <- num32At 0
  let sizei = fromIntegral (size :: Word32) :: Int
  
  runST (do
    vector <- VM.new sizei
    let
      -- fi : current field
      -- pos : position in the bytestring
      parse fi pos =
        if fi < sizei
        then
          let
            -- the next position and the field we parse here
            field :: Either Text (Int, Composite.Field)
            field = do
              oid <- num32At pos
              len <- num32At (pos+4)
              let leni = fromIntegral len
              (,) (pos+8+max 0 leni) <$>
                if leni /= -1
                then Composite.Field oid len <$> slice row (pos+8) leni
                else Right (Composite.NULL oid)

          in case field of
            Left err        -> return (Just err)
            Right (pos', f) -> do
              VM.write vector fi f
              parse (fi+1) pos'
        else return Nothing

    merr <- parse 0 4 -- start immediately following the size
    case merr of
      Just err -> return (Left err)
      Nothing  -> Right <$> V.unsafeFreeze vector)
