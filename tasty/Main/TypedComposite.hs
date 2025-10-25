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

personOids :: (Word32, Word32, Word32)
personOids = (25, 23, 25) -- text, int4, text

encodePerson :: Person -> Encoding.Encoding
encodePerson (Person name age maybePhone) =
  let (nameOid, ageOid, phoneOid) = personOids
   in Encoding.composite
        $ Encoding.field nameOid (Encoding.text_strict name)
        <> Encoding.field ageOid (Encoding.int4_int32 age)
        <> case maybePhone of
          Nothing -> Encoding.nullField phoneOid
          Just phone -> Encoding.field phoneOid (Encoding.text_strict phone)

decodePerson :: Decoding.Value Person
decodePerson =
  let (nameOid, ageOid, phoneOid) = personOids
   in Decoding.composite
        $ Person
        <$> Decoding.valueComposite (Decoding.text_strict)
        <*> Decoding.valueComposite (Decoding.int)
        <*> Decoding.nullableValueComposite (Decoding.text_strict)

roundtrip :: Person -> Property
roundtrip person =
  let encoded = Encoding.encodingBytes (encodePerson person)
      decoded = Decoding.valueParser decodePerson encoded
   in Right person === decoded
