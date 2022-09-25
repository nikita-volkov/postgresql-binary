module Main.Composite where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
import qualified Main.Gens as Gens
import Main.Prelude hiding (assert, choose, isLeft, isRight)
import qualified Network.IP.Addr as IPAddr
import qualified PostgreSQL.Binary.Decoding as Decoding
import qualified PostgreSQL.Binary.Encoding as Encoding
import Test.QuickCheck hiding (vector)
import Test.QuickCheck.Instances

newtype Composite = Composite [CompositeFieldValue]
  deriving (Show, Eq, Ord)

instance Arbitrary Composite where
  arbitrary = Composite <$> listOf arbitrary

data CompositeFieldValue
  = Int4CompositeFieldValue !(Maybe Int32)
  | TextCompositeFieldValue !(Maybe Text)
  | JsonbCompositeFieldValue !(Maybe Aeson.Value)
  deriving (Show, Eq, Ord)

instance Arbitrary CompositeFieldValue where
  arbitrary =
    oneof
      [ Int4CompositeFieldValue <$> arbitrary,
        TextCompositeFieldValue <$> Gens.maybeOf Gens.text,
        JsonbCompositeFieldValue <$> Gens.maybeOf Gens.aeson
      ]

encodeToByteString :: Composite -> ByteString
encodeToByteString = Encoding.encodingBytes . encodeComposite

encodeComposite :: Composite -> Encoding.Encoding
encodeComposite (Composite fields) =
  Encoding.composite $ foldMap compositeField fields
  where
    compositeField = \case
      Int4CompositeFieldValue z -> fieldEncoder 23 Encoding.int4_int32 z
      TextCompositeFieldValue z -> fieldEncoder 25 Encoding.text_strict z
      JsonbCompositeFieldValue z -> fieldEncoder 3802 Encoding.jsonb_ast z
      where
        fieldEncoder oid encoder = \case
          Nothing -> Encoding.nullField oid
          Just value -> Encoding.field oid $ encoder value

decodingProperty :: Composite -> ByteString -> Property
decodingProperty composite input =
  case Decoding.valueParser (decoder composite) input of
    Right p -> p
    Left err -> counterexample (Text.unpack err) False

decoder :: Composite -> Decoding.Value Property
decoder (Composite fields) =
  conjoin <$> Decoding.composite (traverse field fields)
  where
    field = \case
      Int4CompositeFieldValue z -> expect z Decoding.int
      TextCompositeFieldValue z -> expect z Decoding.text_strict
      JsonbCompositeFieldValue z -> expect z Decoding.jsonb_ast
      where
        expect expectedValue decoder =
          case expectedValue of
            Just expectedValue ->
              Decoding.valueComposite decoder <&> (===) expectedValue
            Nothing ->
              Decoding.nullableValueComposite decoder <&> (===) Nothing
