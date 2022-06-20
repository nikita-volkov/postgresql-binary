module Main.Gens where

import Main.Prelude hiding (assert, isRight, isLeft, choose)
import Test.QuickCheck hiding (vector)
import Test.QuickCheck.Instances
import qualified Main.PTI as PTI
import qualified Data.Scientific as Scientific
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import qualified PostgreSQL.Binary.Encoding as Encoder
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.Aeson.Key as AesonKey
import qualified Network.IP.Addr as IPAddr


-- * Generators
-------------------------

auto :: Arbitrary a => Gen a
auto =
  arbitrary

vector :: Gen a -> Gen (Vector a)
vector element =
  join $ Vector.replicateM <$> arbitrary <*> pure element

hashMap :: (Eq a, Hashable a) => Gen a -> Gen b -> Gen (HashMap a b)
hashMap key value =
  fmap HashMap.fromList $ join $ replicateM <$> arbitrary <*> pure row
  where
    row =
      (,) <$> key <*> value

aeson :: Gen Aeson.Value
aeson =
  byDepth 0
  where
    byDepth depth =
      frequency (primitives <> composites)
      where
        primitives =
          map (freq,) [null, bool, number, string]
          where
            freq =
              maxFreq
        composites =
          map (freq,) [array, object]
          where
            freq =
              maxFreq - depth
        maxFreq =
          4
        null =
          pure Aeson.Null
        bool =
          fmap Aeson.Bool arbitrary
        number =
          fmap Aeson.Number arbitrary
        string =
          fmap Aeson.String text
        array =
          fmap Aeson.Array (vector (byDepth (succ depth)))
        object =
          Aeson.Object . AesonKeyMap.fromList <$> listOf pair
          where
            pair =
              (,) <$> key <*> value
              where
                key =
                  AesonKey.fromText <$> text
                value =
                  byDepth (succ depth)

postgresInt :: (Bounded a, Ord a, Integral a, Arbitrary a) => Gen a
postgresInt =
  arbitrary >>= \x -> if x > halfMaxBound then postgresInt else pure x
  where
    halfMaxBound =
      div maxBound 2

text :: Gen Text
text =
  arbitrary >>= \x -> if Text.find (== '\NUL') x == Nothing then return x else text

char :: Gen Char
char =
  arbitrary >>= \x -> if x /= '\NUL' then return x else char

scientific :: Gen Scientific
scientific =
  Scientific.scientific <$> arbitrary <*> arbitrary

microsTimeOfDay :: Gen TimeOfDay
microsTimeOfDay =
  fmap timeToTimeOfDay $ fmap picosecondsToDiffTime $ fmap (* (10^6)) $ 
    choose (0, (10^6)*24*60*60)

microsLocalTime :: Gen LocalTime
microsLocalTime = 
  LocalTime <$> arbitrary <*> microsTimeOfDay

microsUTCTime :: Gen UTCTime
microsUTCTime =
  localTimeToUTC <$> timeZone <*> microsLocalTime

intervalDiffTime :: Gen DiffTime
intervalDiffTime = do
  unsafeCoerce ((* (10^6)) <$> choose (uMin, uMax) :: Gen Integer)
  where
    uMin = unsafeCoerce minInterval `div` 10^6
    uMax = unsafeCoerce maxInterval `div` 10^6

timeZone :: Gen TimeZone
timeZone =
  minutesToTimeZone <$> choose (- 60 * 12 + 1, 60 * 12)

timetz :: Gen (TimeOfDay, TimeZone)
timetz =
  (,) <$> microsTimeOfDay <*> timeZone

uuid :: Gen UUID.UUID
uuid =
  UUID.fromWords <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

inet :: Gen (IPAddr.NetAddr IPAddr.IP)
inet = do
  ipv6 <- choose (True, False)
  if ipv6
    then IPAddr.netAddr <$> (IPAddr.IPv6 <$> (IPAddr.ip6FromWords <$>
         arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)) <*> choose (0, 128)
    else IPAddr.netAddr <$> (IPAddr.IPv4 <$> (IPAddr.ip4FromOctets <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)) <*> choose (0, 32)

array3 :: Gen a -> Gen [[[a]]]
array3 gen =
  do
    width1 <- choose (1, 10)
    width2 <- choose (1, 10)
    width3 <- choose (1, 10)
    replicateM width1 (replicateM width2 (replicateM width3 gen))


-- * Constants
-------------------------

maxInterval :: DiffTime = 
  unsafeCoerce $ 
    (truncate (1780000 * 365.2425 * 24 * 60 * 60 * 10 ^ 12 :: Rational) :: Integer)

minInterval :: DiffTime = 
  negate maxInterval
