{-# LANGUAGE MultiWayIf #-}

module PostgreSQL.Binary.Decoder
(
  Decoder,
  run,
  -- * Primitive
  int,
  float4,
  float8,
  bool,
  bytea_strict,
  bytea_lazy,
  -- * Textual
  text_strict,
  text_lazy,
  char,
  -- * Misc
  fn,
  numeric,
  uuid,
  inet,
  json_ast,
  json_bytes,
  jsonb_ast,
  jsonb_bytes,
  -- * Time
  date,
  time_int,
  time_float,
  timetz_int,
  timetz_float,
  timestamp_int,
  timestamp_float,
  timestamptz_int,
  timestamptz_float,
  interval_int,
  interval_float,
  -- * Exotic
  -- ** Array
  ArrayDecoder,
  array,
  arrayDimension,
  arrayValue,
  arrayNonNullValue,
  -- ** Composite
  CompositeDecoder,
  composite,
  compositeValue,
  compositeNonNullValue,
  -- ** HStore
  hstore,
  -- **
  enum,
)
where

import PostgreSQL.Binary.Prelude hiding (take, bool, drop, state, fail, failure)
import BinaryParser
import qualified PostgreSQL.Binary.Data as Data
import qualified PostgreSQL.Binary.Integral as Integral
import qualified PostgreSQL.Binary.Interval as Interval
import qualified PostgreSQL.Binary.Numeric as Numeric
import qualified PostgreSQL.Binary.Time as Time
import qualified Data.Vector as Vector
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.UUID as UUID
import qualified Data.Aeson as Aeson
import qualified Network.IP.Addr as IPAddr


type Decoder =
  BinaryParser


-- * Helpers
-------------------------

-- |
-- Any int number of a limited byte-size.
{-# INLINE intOfSize #-}
intOfSize :: (Integral a, Bits a) => Int -> Decoder a
intOfSize x =
  fmap Integral.pack (bytesOfSize x)

{-# INLINABLE onContent #-}
onContent :: Decoder a -> Decoder ( Maybe a )
onContent decoder =
  size >>=
  \case
    (-1) -> pure Nothing
    n -> fmap Just (sized (fromIntegral n) decoder)
  where
    size =
      intOfSize 4 :: Decoder Int32

{-# INLINABLE content #-}
content :: Decoder (Maybe ByteString)
content =
  intOfSize 4 >>= \case
    (-1) -> pure Nothing
    n -> fmap Just (bytesOfSize n)

{-# INLINE nonNull #-}
nonNull :: Maybe a -> Decoder a
nonNull =
  maybe (failure "Unexpected NULL") return


-- * Primitive
-------------------------

-- |
-- Lifts a custom decoder implementation.
{-# INLINE fn #-}
fn :: (ByteString -> Either Text a) -> Decoder a
fn fn =
  BinaryParser.remainders >>= either BinaryParser.failure return . fn

{-# INLINE int #-}
int :: (Integral a, Bits a) => Decoder a
int =
  fmap Integral.pack remainders

float4 :: Decoder Float
float4 =
  unsafeCoerce (int :: Decoder Int32)

float8 :: Decoder Double
float8 =
  unsafeCoerce (int :: Decoder Int64)

{-# INLINE bool #-}
bool :: Decoder Bool
bool =
  fmap (== 1) byte

{-# NOINLINE numeric #-}
numeric :: Decoder Scientific
numeric =
  do
    componentsAmount <- intOfSize 2
    pointIndex <- intOfSize 2
    signCode <- intOfSize 2
    unitOfSize 2
    components <- Vector.replicateM componentsAmount (intOfSize 2)
    either failure return (Numeric.scientific pointIndex signCode components)

{-# INLINABLE uuid #-}
uuid :: Decoder UUID
uuid =
  UUID.fromWords <$> intOfSize 4 <*> intOfSize 4 <*> intOfSize 4 <*> intOfSize 4

{-# INLINABLE ipv4 #-}
ipv4 :: Decoder IPAddr.IP
ipv4 = fmap IPAddr.IPv4 (IPAddr.ip4FromOctets <$> intOfSize 1 <*> intOfSize 1 <*> intOfSize 1 <*> intOfSize 1)

{-# INLINABLE ipv6 #-}
ipv6 :: Decoder IPAddr.IP
ipv6 =
  fmap
    IPAddr.IPv6
    (IPAddr.ip6FromWords <$> intOfSize 2 <*> intOfSize 2 <*> intOfSize 2 <*> intOfSize 2 <*> intOfSize 2 <*> intOfSize 2 <*> intOfSize 2 <*> intOfSize 2)

{-# INLINABLE inet #-}
inet :: Decoder Data.Inet
inet = do
  af <- intOfSize 1
  netmask <- intOfSize 1
  isCidr <- intOfSize 1
  ipSize <- intOfSize 1
  if | af == Data.afInet ->
       do ip <- ipv4
          return $ inetFromBytes af netmask isCidr ipSize ip
     | af == Data.afInet6 ->
       do ip <- ipv6
          return $ inetFromBytes af netmask isCidr ipSize ip
     | otherwise -> BinaryParser.failure ("Unknown address family: " <> fromString (show af))
  where
    inetFromBytes :: Word8 -> Word8 -> Word8 -> Int8 -> IPAddr.IP -> IPAddr.NetAddr IPAddr.IP
    inetFromBytes _ netmask _ _ ip = IPAddr.netAddr ip netmask

{-# INLINABLE json_ast #-}
json_ast :: Decoder Aeson.Value
json_ast =
  bytea_strict >>= either (BinaryParser.failure . fromString) pure . Aeson.eitherDecodeStrict'

-- |
-- Given a function, which parses a plain UTF-8 JSON string encoded as a byte-array,
-- produces a decoder.
{-# INLINABLE json_bytes #-}
json_bytes :: (ByteString -> Either Text a) -> Decoder a
json_bytes cont =
  getAllBytes >>= parseJSON
  where
    getAllBytes =
      BinaryParser.remainders
    parseJSON =
      either BinaryParser.failure return . cont

{-# INLINABLE jsonb_ast #-}
jsonb_ast :: Decoder Aeson.Value
jsonb_ast =
  jsonb_bytes $ mapLeft fromString . Aeson.eitherDecodeStrict'

-- |
-- Given a function, which parses a plain UTF-8 JSON string encoded as a byte-array,
-- produces a decoder.
-- 
-- For those wondering, yes,
-- JSONB is encoded as plain JSON string in the binary format of Postgres.
-- Sad, but true.
{-# INLINABLE jsonb_bytes #-}
jsonb_bytes :: (ByteString -> Either Text a) -> Decoder a
jsonb_bytes cont =
  getAllBytes >>= trimBytes >>= parseJSON
  where
    getAllBytes =
      BinaryParser.remainders
    trimBytes =
      maybe (BinaryParser.failure "Empty input") return .
      fmap snd . ByteString.uncons
    parseJSON =
      either BinaryParser.failure return . cont


-- ** Textual
-------------------------

-- |
-- A UTF-8-decoded char.
{-# INLINABLE char #-}
char :: Decoder Char
char =
  fmap Text.uncons text_strict >>= \case
    Just (c, "") -> return c
    Nothing -> failure "Empty input"
    _ -> failure "Consumed too much"

-- |
-- Any of the variable-length character types:
-- BPCHAR, VARCHAR, NAME and TEXT.
{-# INLINABLE text_strict #-}
text_strict :: Decoder Text
text_strict =
  remainders >>= either (failure . exception) return . Text.decodeUtf8'
  where
    exception =
      \case
        Text.DecodeError message byte -> fromString message
        _ -> $bug "Unexpected unicode exception"

-- |
-- Any of the variable-length character types:
-- BPCHAR, VARCHAR, NAME and TEXT.
{-# INLINABLE text_lazy #-}
text_lazy :: Decoder LazyText
text_lazy =
  bytea_lazy >>= either (failure . exception) return . LazyText.decodeUtf8'
  where
    exception =
      \case
        Text.DecodeError message byte -> fromString message
        _ -> $bug "Unexpected unicode exception"

-- |
-- BYTEA or any other type in its undecoded form.
{-# INLINE bytea_strict #-}
bytea_strict :: Decoder ByteString
bytea_strict =
  remainders

-- |
-- BYTEA or any other type in its undecoded form.
{-# INLINE bytea_lazy #-}
bytea_lazy :: Decoder LazyByteString
bytea_lazy =
  fmap LazyByteString.fromStrict remainders


-- * Date and Time
-------------------------

-- |
-- @DATE@ values decoding.
date :: Decoder Day
date =
  fmap (Time.postgresJulianToDay . fromIntegral) (int :: Decoder Int32)

-- |
-- @TIME@ values decoding for servers, which have @integer_datetimes@ enabled.
time_int :: Decoder TimeOfDay
time_int =
  fmap Time.microsToTimeOfDay int

-- |
-- @TIME@ values decoding for servers, which don't have @integer_datetimes@ enabled.
time_float :: Decoder TimeOfDay
time_float =
  fmap Time.secsToTimeOfDay float8

-- |
-- @TIMETZ@ values decoding for servers, which have @integer_datetimes@ enabled.
timetz_int :: Decoder (TimeOfDay, TimeZone)
timetz_int =
  (,) <$> sized 8 time_int <*> tz

-- |
-- @TIMETZ@ values decoding for servers, which don't have @integer_datetimes@ enabled.
timetz_float :: Decoder (TimeOfDay, TimeZone)
timetz_float =
  (,) <$> sized 8 time_float <*> tz

{-# INLINE tz #-}
tz :: Decoder TimeZone
tz =
  fmap (minutesToTimeZone . negate . (flip div 60) . fromIntegral) (int :: Decoder Int32)

-- |
-- @TIMESTAMP@ values decoding for servers, which have @integer_datetimes@ enabled.
timestamp_int :: Decoder LocalTime
timestamp_int =
  fmap Time.microsToLocalTime int

-- |
-- @TIMESTAMP@ values decoding for servers, which don't have @integer_datetimes@ enabled.
timestamp_float :: Decoder LocalTime
timestamp_float =
  fmap Time.secsToLocalTime float8

-- |
-- @TIMESTAMP@ values decoding for servers, which have @integer_datetimes@ enabled.
timestamptz_int :: Decoder UTCTime
timestamptz_int =
  fmap Time.microsToUTC int

-- |
-- @TIMESTAMP@ values decoding for servers, which don't have @integer_datetimes@ enabled.
timestamptz_float :: Decoder UTCTime
timestamptz_float =
  fmap Time.secsToUTC float8

-- |
-- @INTERVAL@ values decoding for servers, which don't have @integer_datetimes@ enabled.
interval_int :: Decoder DiffTime
interval_int =
  do
    u <- sized 8 int
    d <- sized 4 int
    m <- int
    return $ Interval.toDiffTime $ Interval.Interval u d m

-- |
-- @INTERVAL@ values decoding for servers, which have @integer_datetimes@ enabled.
interval_float :: Decoder DiffTime
interval_float =
  do
    u <- sized 8 (fmap (round . (*(10^6)) . toRational) float8)
    d <- sized 4 int
    m <- int
    return $ Interval.toDiffTime $ Interval.Interval u d m


-- * Exotic
-------------------------

-- |
-- A lower-level array data parser,
-- which aggregates the intermediate data representation as per the Postgres format.
-- 
-- Only use this if 'array' doesn't fit your case.
{-# INLINABLE arrayRep #-}
arrayRep :: Decoder Data.Array
arrayRep =
  do
    dimensionsAmount <- intOfSize 4
    nullsValue <- nulls
    oid <- intOfSize 4
    dimensions <- Vector.replicateM dimensionsAmount dimension
    let valuesAmount = (Vector.product . Vector.map fst) dimensions
    values <- Vector.replicateM (fromIntegral valuesAmount) content
    return (dimensions, values, nullsValue, oid)
  where
    dimension =
      (,) <$> intOfSize 4 <*> intOfSize 4
    nulls =
      intOfSize 4 >>= \(x :: Word32) -> case x of
        0 -> return False
        1 -> return True
        w -> failure $ "Invalid value: " <> (fromString . show) w

{-# INLINABLE compositeRep #-}
compositeRep :: Decoder Data.Composite
compositeRep =
  do
    componentsAmount <- intOfSize 4
    Vector.replicateM componentsAmount component
  where
    component =
      (,) <$> intOfSize 4 <*> content

-- |
-- A function for generic in place parsing of an HStore value.
-- 
-- Accepts:
-- 
-- * An implementation of the @replicateM@ function
-- (@Control.Monad.'Control.Monad.replicateM'@, @Data.Vector.'Data.Vector.replicateM'@),
-- which determines how to produce the final datastructure from the rows.
-- 
-- * A decoder for keys.
-- 
-- * A decoder for values.
-- 
-- Here's how you can use it to produce a parser to list:
-- 
-- @
-- hstoreAsList :: Decoder [ ( Text , Maybe Text ) ]
-- hstoreAsList =
--   hstore replicateM text text
-- @
-- 
{-# INLINABLE hstore #-}
hstore :: ( forall m. Monad m => Int -> m ( k , Maybe v ) -> m r ) -> Decoder k -> Decoder v -> Decoder r
hstore replicateM keyContent valueContent =
  do
    componentsAmount <- intOfSize 4
    replicateM componentsAmount component
  where
    component =
      (,) <$> key <*> value
      where
        key =
          onContent keyContent >>= nonNull
        value =
          onContent valueContent

{-# INLINABLE hstoreRep #-}
hstoreRep :: Decoder Data.HStore
hstoreRep =
  do
    componentsAmount <- intOfSize 4
    Vector.replicateM componentsAmount component
  where
    component =
      (,) <$> key <*> content
      where
        key =
          intOfSize 4 >>= bytesOfSize


-- * Composite
-------------------------

newtype CompositeDecoder a =
  CompositeDecoder ( Decoder a )
  deriving ( Functor , Applicative , Monad )

-- |
-- Unlift a 'CompositeDecoder' to a value 'Decoder'.
{-# INLINE composite #-}
composite :: CompositeDecoder a -> Decoder a
composite (CompositeDecoder decoder) =
  numOfComponents *> decoder
  where
    numOfComponents =
      unitOfSize 4

-- |
-- Lift a value 'Decoder' into 'CompositeDecoder'.
{-# INLINE compositeValue #-}
compositeValue :: Decoder a -> CompositeDecoder ( Maybe a )
compositeValue valueDecoder =
  CompositeDecoder (skipOid *> onContent valueDecoder)
  where
    skipOid =
      unitOfSize 4

-- |
-- Lift a non-nullable value 'Decoder' into 'CompositeDecoder'.
{-# INLINE compositeNonNullValue #-}
compositeNonNullValue :: Decoder a -> CompositeDecoder a
compositeNonNullValue valueDecoder =
  CompositeDecoder (skipOid *> onContent valueDecoder >>= maybe (failure "Unexpected NULL") return)
  where
    skipOid =
      unitOfSize 4


-- * Array
-------------------------

-- |
-- An efficient generic array decoder,
-- which constructs the result value in place while parsing.
-- 
-- Here's how you can use it to produce a specific array value decoder:
-- 
-- @
-- x :: Decoder [ [ Text ] ]
-- x =
--   array (arrayDimension replicateM (fmap catMaybes (arrayDimension replicateM (arrayValue text))))
-- @
-- 
newtype ArrayDecoder a =
  ArrayDecoder ( [ Word32 ] -> Decoder a )
  deriving ( Functor )

-- |
-- Unlift an 'ArrayDecoder' to a value 'Decoder'.
{-# INLINE array #-}
array :: ArrayDecoder a -> Decoder a
array (ArrayDecoder decoder) =
  do
    dimensionsAmount <- intOfSize 4
    if dimensionsAmount /= 0
      then do
        unitOfSize (4 + 4)
        dimensionSizes <- replicateM dimensionsAmount dimensionSize
        decoder dimensionSizes
      else decoder [0]
  where
    dimensionSize =
      intOfSize 4 <* unitOfSize 4

-- |
-- A function for parsing a dimension of an array.
-- Provides support for multi-dimensional arrays.
-- 
-- Accepts:
-- 
-- * An implementation of the @replicateM@ function
-- (@Control.Monad.'Control.Monad.replicateM'@, @Data.Vector.'Data.Vector.replicateM'@),
-- which determines the output value.
-- 
-- * A decoder of its components, which can be either another 'arrayDimension' or 'arrayValue'.
-- 
{-# INLINE arrayDimension #-}
arrayDimension :: ( forall m. Monad m => Int -> m a -> m b ) -> ArrayDecoder a -> ArrayDecoder b
arrayDimension replicateM (ArrayDecoder component) =
  ArrayDecoder $ \case
    head : tail -> replicateM (fromIntegral head) (component tail)
    _ -> failure "A missing dimension length"

-- |
-- Lift a value 'Decoder' into 'ArrayDecoder' for parsing of nullable leaf values.
{-# INLINE arrayValue #-}
arrayValue :: Decoder a -> ArrayDecoder ( Maybe a )
arrayValue =
  ArrayDecoder . const . onContent

-- |
-- Lift a value 'Decoder' into 'ArrayDecoder' for parsing of non-nullable leaf values.
{-# INLINE arrayNonNullValue #-}
arrayNonNullValue :: Decoder a -> ArrayDecoder a
arrayNonNullValue =
  ArrayDecoder . const . join . fmap (maybe (failure "Unexpected NULL") return) . onContent


-- * Enum
-------------------------

-- |
-- Given a partial mapping from text to value,
-- produces a decoder of that value.
{-# INLINE enum #-}
enum :: (Text -> Maybe a) -> Decoder a
enum mapping =
  text_strict >>= onText
  where
    onText text =
      maybe onNothing onJust (mapping text)
      where
        onNothing =
          failure ("No mapping for text \"" <> text <> "\"")
        onJust =
          pure
