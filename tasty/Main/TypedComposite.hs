module Main.TypedComposite where

import qualified Main.Gens as Gens
import Main.Prelude
import qualified PostgreSQL.Binary.Decoding as Decoding
import qualified PostgreSQL.Binary.Encoding as Encoding
import Test.QuickCheck

data Person = Person
  { personName :: Text,
    personAge :: Int32,
    personMaybePhone :: Maybe Text
  }
  deriving (Show, Eq)

instance Arbitrary Person where
  arbitrary =
    Person
      <$> Gens.text
      <*> arbitrary
      <*> Gens.maybeOf Gens.text

nameOid, ageOid, phoneOid :: Word32
(nameOid, ageOid, phoneOid) = (25, 23, 25) -- text, int4, text

encodePerson :: Person -> Encoding.Encoding
encodePerson (Person name age maybePhone) =
  Encoding.composite
    $ Encoding.field nameOid (Encoding.text_strict name)
    <> Encoding.field ageOid (Encoding.int4_int32 age)
    <> case maybePhone of
      Nothing -> Encoding.nullField phoneOid
      Just phone -> Encoding.field phoneOid (Encoding.text_strict phone)

decodePerson :: Decoding.Value Person
decodePerson =
  Decoding.composite
    $ Person
    <$> Decoding.typedValueComposite nameOid (Decoding.text_strict)
    <*> Decoding.typedValueComposite ageOid (Decoding.int)
    <*> Decoding.typedNullableValueComposite phoneOid (Decoding.text_strict)

roundtrip :: Person -> Property
roundtrip person =
  let encoded = Encoding.encodingBytes (encodePerson person)
      decoded = Decoding.valueParser decodePerson encoded
   in Right person === decoded
