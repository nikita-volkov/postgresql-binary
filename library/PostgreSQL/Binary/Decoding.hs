module PostgreSQL.Binary.Decoding
  ( valueParser,
    --
    Value,

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
    Array,
    array,
    valueArray,
    nullableValueArray,
    dimensionArray,

    -- ** Composite
    Composite,
    composite,
    valueComposite,
    nullableValueComposite,

    -- ** HStore
    hstore,
    enum,
    refine,
  )
where

import BinaryParser
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.IP as IP
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
import qualified PostgreSQL.Binary.Inet as Inet
import qualified PostgreSQL.Binary.Integral as Integral
import qualified PostgreSQL.Binary.Interval as Interval
import qualified PostgreSQL.Binary.Numeric as Numeric
import PostgreSQL.Binary.Prelude hiding (bool, drop, fail, state, take)
import qualified PostgreSQL.Binary.Time as Time

type Value =
  BinaryParser

valueParser :: Value a -> ByteString -> Either Text a
valueParser =
  BinaryParser.run

-- * Helpers

-- |
-- Any int number of a limited byte-size.
{-# INLINE intOfSize #-}
intOfSize :: (Integral a, Bits a) => Int -> Value a
intOfSize x =
  fmap Integral.pack (bytesOfSize x)

{-# INLINEABLE onContent #-}
onContent :: Value a -> Value (Maybe a)
onContent decoder =
  size
    >>= \case
      (-1) -> pure Nothing
      n -> fmap Just (sized (fromIntegral n) decoder)
  where
    size =
      intOfSize 4 :: Value Int32

{-# INLINE nonNull #-}
nonNull :: Maybe a -> Value a
nonNull =
  maybe (failure "Unexpected NULL") return

-- * Primitive

-- |
-- Lifts a custom decoder implementation.
{-# INLINE fn #-}
fn :: (ByteString -> Either Text a) -> Value a
fn fn =
  BinaryParser.remainders >>= either BinaryParser.failure return . fn

{-# INLINE int #-}
int :: (Integral a, Bits a) => Value a
int =
  fmap Integral.pack remainders

float4 :: Value Float
float4 =
  unsafeCoerce (int :: Value Int32)

float8 :: Value Double
float8 =
  unsafeCoerce (int :: Value Int64)

{-# INLINE bool #-}
bool :: Value Bool
bool =
  fmap (== 1) byte

{-# NOINLINE numeric #-}
numeric :: Value Scientific
numeric =
  do
    componentsAmount <- intOfSize 2
    pointIndex <- intOfSize 2
    signCode <- intOfSize 2
    unitOfSize 2
    components <- Vector.replicateM componentsAmount (intOfSize 2)
    either failure return (Numeric.scientific pointIndex signCode components)

{-# INLINEABLE uuid #-}
uuid :: Value UUID
uuid =
  UUID.fromWords <$> intOfSize 4 <*> intOfSize 4 <*> intOfSize 4 <*> intOfSize 4

{-# INLINE ip4 #-}
ip4 :: Value IP.IPv4
ip4 =
  IP.toIPv4w <$> intOfSize 4

{-# INLINE ip6 #-}
ip6 :: Value IP.IPv6
ip6 =
  IP.toIPv6w <$> ((,,,) <$> intOfSize 4 <*> intOfSize 4 <*> intOfSize 4 <*> intOfSize 4)

{-# INLINEABLE inet #-}
inet :: Value IP.IPRange
inet = do
  af <- intOfSize 1
  netmask <- intOfSize 1
  isCidr <- intOfSize 1 :: Value Int -- unused
  ipSize <- intOfSize 1 :: Value Int -- unused
  if
    | af == Inet.inetAddressFamily ->
        do
          ip <- ip4
          return . IP.IPv4Range $ IP.makeAddrRange ip netmask
    | af == Inet.inet6AddressFamily ->
        do
          ip <- ip6
          return . IP.IPv6Range $ IP.makeAddrRange ip netmask
    | otherwise -> BinaryParser.failure ("Unknown address family: " <> fromString (show af))

{-# INLINEABLE json_ast #-}
json_ast :: Value Aeson.Value
json_ast =
  bytea_strict >>= either (BinaryParser.failure . fromString) pure . Aeson.eitherDecodeStrict'

-- |
-- Given a function, which parses a plain UTF-8 JSON string encoded as a byte-array,
-- produces a decoder.
{-# INLINEABLE json_bytes #-}
json_bytes :: (ByteString -> Either Text a) -> Value a
json_bytes cont =
  getAllBytes >>= parseJSON
  where
    getAllBytes =
      BinaryParser.remainders
    parseJSON =
      either BinaryParser.failure return . cont

{-# INLINEABLE jsonb_ast #-}
jsonb_ast :: Value Aeson.Value
jsonb_ast =
  jsonb_bytes $ mapLeft fromString . Aeson.eitherDecodeStrict'

-- |
-- Given a function, which parses a plain UTF-8 JSON string encoded as a byte-array,
-- produces a decoder.
--
-- For those wondering, yes,
-- JSONB is encoded as plain JSON string in the binary format of Postgres.
-- Sad, but true.
{-# INLINEABLE jsonb_bytes #-}
jsonb_bytes :: (ByteString -> Either Text a) -> Value a
jsonb_bytes cont =
  getAllBytes >>= trimBytes >>= parseJSON
  where
    getAllBytes =
      BinaryParser.remainders
    trimBytes =
      maybe (BinaryParser.failure "Empty input") return
        . fmap snd
        . ByteString.uncons
    parseJSON =
      either BinaryParser.failure return . cont

-- ** Textual

-- |
-- A UTF-8-decoded char.
{-# INLINEABLE char #-}
char :: Value Char
char =
  fmap Text.uncons text_strict >>= \case
    Just (c, "") -> return c
    Nothing -> failure "Empty input"
    _ -> failure "Consumed too much"

-- |
-- Any of the variable-length character types:
-- BPCHAR, VARCHAR, NAME and TEXT.
{-# INLINEABLE text_strict #-}
text_strict :: Value Text
text_strict =
  do
    input <- remainders
    either (failure . exception input) return (Text.decodeUtf8' input)
  where
    exception input =
      \case
        Text.DecodeError _ _ -> fromString ("Failed to decode the following bytes in UTF-8: " <> show input)
        _ -> error "Unexpected unicode exception"

-- |
-- Any of the variable-length character types:
-- BPCHAR, VARCHAR, NAME and TEXT.
{-# INLINEABLE text_lazy #-}
text_lazy :: Value LazyText
text_lazy =
  do
    input <- bytea_lazy
    either (failure . exception input) return (LazyText.decodeUtf8' input)
  where
    exception input =
      \case
        Text.DecodeError _ _ -> fromString ("Failed to decode the following bytes in UTF-8: " <> show input)
        _ -> error "Unexpected unicode exception"

-- |
-- BYTEA or any other type in its undecoded form.
{-# INLINE bytea_strict #-}
bytea_strict :: Value ByteString
bytea_strict =
  remainders

-- |
-- BYTEA or any other type in its undecoded form.
{-# INLINE bytea_lazy #-}
bytea_lazy :: Value LazyByteString
bytea_lazy =
  fmap LazyByteString.fromStrict remainders

-- * Date and Time

-- |
-- @DATE@ values decoding.
date :: Value Day
date =
  fmap (Time.postgresJulianToDay . fromIntegral) (int :: Value Int32)

-- |
-- @TIME@ values decoding for servers, which have @integer_datetimes@ enabled.
time_int :: Value TimeOfDay
time_int =
  fmap Time.microsToTimeOfDay int

-- |
-- @TIME@ values decoding for servers, which don't have @integer_datetimes@ enabled.
time_float :: Value TimeOfDay
time_float =
  fmap Time.secsToTimeOfDay float8

-- |
-- @TIMETZ@ values decoding for servers, which have @integer_datetimes@ enabled.
timetz_int :: Value (TimeOfDay, TimeZone)
timetz_int =
  (,) <$> sized 8 time_int <*> tz

-- |
-- @TIMETZ@ values decoding for servers, which don't have @integer_datetimes@ enabled.
timetz_float :: Value (TimeOfDay, TimeZone)
timetz_float =
  (,) <$> sized 8 time_float <*> tz

{-# INLINE tz #-}
tz :: Value TimeZone
tz =
  fmap (minutesToTimeZone . negate . (flip div 60) . fromIntegral) (int :: Value Int32)

-- |
-- @TIMESTAMP@ values decoding for servers, which have @integer_datetimes@ enabled.
timestamp_int :: Value LocalTime
timestamp_int =
  fmap Time.microsToLocalTime int

-- |
-- @TIMESTAMP@ values decoding for servers, which don't have @integer_datetimes@ enabled.
timestamp_float :: Value LocalTime
timestamp_float =
  fmap Time.secsToLocalTime float8

-- |
-- @TIMESTAMP@ values decoding for servers, which have @integer_datetimes@ enabled.
timestamptz_int :: Value UTCTime
timestamptz_int =
  fmap Time.microsToUTC int

-- |
-- @TIMESTAMP@ values decoding for servers, which don't have @integer_datetimes@ enabled.
timestamptz_float :: Value UTCTime
timestamptz_float =
  fmap Time.secsToUTC float8

-- |
-- @INTERVAL@ values decoding for servers, which don't have @integer_datetimes@ enabled.
interval_int :: Value DiffTime
interval_int =
  do
    u <- sized 8 int
    d <- sized 4 int
    m <- int
    return $ Interval.toDiffTime $ Interval.Interval u d m

-- |
-- @INTERVAL@ values decoding for servers, which have @integer_datetimes@ enabled.
interval_float :: Value DiffTime
interval_float =
  do
    u <- sized 8 (fmap (round . (* (10 ^ 6)) . toRational) float8)
    d <- sized 4 int
    m <- int
    return $ Interval.toDiffTime $ Interval.Interval u d m

-- * Exotic

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
-- hstoreAsList :: Value [ ( Text , Maybe Text ) ]
-- hstoreAsList =
--   hstore replicateM text text
-- @
{-# INLINEABLE hstore #-}
hstore :: (forall m. (Monad m) => Int -> m (k, Maybe v) -> m r) -> Value k -> Value v -> Value r
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

-- * Composite

newtype Composite a
  = Composite (Value a)
  deriving (Functor, Applicative, Monad, MonadFail)

-- |
-- Unlift a 'Composite' to a value 'Value'.
{-# INLINE composite #-}
composite :: Composite a -> Value a
composite (Composite decoder) =
  numOfComponents *> decoder
  where
    numOfComponents =
      unitOfSize 4

-- |
-- Lift a value 'Value' into 'Composite'.
{-# INLINE nullableValueComposite #-}
nullableValueComposite :: Value a -> Composite (Maybe a)
nullableValueComposite valueValue =
  Composite (skipOid *> onContent valueValue)
  where
    skipOid =
      unitOfSize 4

-- |
-- Lift a non-nullable value 'Value' into 'Composite'.
{-# INLINE valueComposite #-}
valueComposite :: Value a -> Composite a
valueComposite valueValue =
  Composite (skipOid *> onContent valueValue >>= maybe (failure "Unexpected NULL") return)
  where
    skipOid =
      unitOfSize 4

-- * Array

-- |
-- An efficient generic array decoder,
-- which constructs the result value in place while parsing.
--
-- Here's how you can use it to produce a specific array value decoder:
--
-- @
-- x :: Value [ [ Text ] ]
-- x =
--   array (dimensionArray replicateM (fmap catMaybes (dimensionArray replicateM (nullableValueArray text))))
-- @
newtype Array a
  = Array ([Word32] -> Value a)
  deriving (Functor)

-- |
-- Unlift an 'Array' to a value 'Value'.
{-# INLINE array #-}
array :: Array a -> Value a
array (Array decoder) =
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
-- * A decoder of its components, which can be either another 'dimensionArray' or 'nullableValueArray'.
{-# INLINE dimensionArray #-}
dimensionArray :: (forall m. (Monad m) => Int -> m a -> m b) -> Array a -> Array b
dimensionArray replicateM (Array component) =
  Array $ \case
    head : tail -> replicateM (fromIntegral head) (component tail)
    _ -> failure "A missing dimension length"

-- |
-- Lift a value 'Value' into 'Array' for parsing of nullable leaf values.
{-# INLINE nullableValueArray #-}
nullableValueArray :: Value a -> Array (Maybe a)
nullableValueArray =
  Array . const . onContent

-- |
-- Lift a value 'Value' into 'Array' for parsing of non-nullable leaf values.
{-# INLINE valueArray #-}
valueArray :: Value a -> Array a
valueArray =
  Array . const . join . fmap (maybe (failure "Unexpected NULL") return) . onContent

-- * Enum

-- |
-- Given a partial mapping from text to value,
-- produces a decoder of that value.
{-# INLINE enum #-}
enum :: (Text -> Maybe a) -> Value a
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

-- * Refining values

-- | Given additional constraints when
-- using an existing value decoder, produces
-- a decoder of that value.
{-# INLINE refine #-}
refine :: (a -> Either Text b) -> Value a -> Value b
refine fn m = m >>= (either failure pure . fn)
