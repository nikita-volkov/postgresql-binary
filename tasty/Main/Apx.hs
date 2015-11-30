module Main.Apx where

import Main.Prelude


-- |
-- A wrapper which provides a type-specific 'Eq' instance
-- with approximate comparison.
newtype Apx a =
  Apx { unApx :: a }
  deriving (Show)

instance (Eq (Apx a), Eq (Apx b)) => Eq (Apx (a, b)) where
  (==) (Apx (a1, a2)) (Apx (b1, b2)) =
    Apx a1 == Apx b1 &&
    Apx a2 == Apx b2

instance Eq (Apx LocalTime) where
  (==) (Apx a) (Apx b) =
    Apx (localTimeToSeconds a) ==
    Apx (localTimeToSeconds b)

instance Eq (Apx UTCTime) where
  (==) (Apx a) (Apx b) =
    Apx (utcTimeToSeconds a) ==
    Apx (utcTimeToSeconds b)

instance Eq (Apx TimeOfDay) where
  (==) (Apx a) (Apx b) =
    Apx (timeOfDayToSeconds a) ==
    Apx (timeOfDayToSeconds b)

instance Eq (Apx TimeZone) where
  (==) (Apx a) (Apx b) =
    a == b

instance Eq (Apx Double) where
  (==) (Apx a) (Apx b) =
    toRational a == toRational b

instance Eq (Apx Rational) where
  (==) (Apx a) (Apx b) =
    a + error >= b &&
    a - error <= b
    where
      error =
        10 ^^ negate 3


utcTimeToSeconds :: UTCTime -> Rational
utcTimeToSeconds (UTCTime days diffTime) =
  dayToSeconds days + diffTimeToSeconds diffTime

diffTimeToSeconds :: DiffTime -> Rational
diffTimeToSeconds diffTime =
  unsafeCoerce diffTime % (10 ^ 12)

dayToSeconds :: Day -> Rational
dayToSeconds (ModifiedJulianDay day) =
  (unsafeCoerce day * 24 * 60 * 60) % 1

localTimeToSeconds :: LocalTime -> Rational
localTimeToSeconds (LocalTime day tod) =
  dayToSeconds day +
  timeOfDayToSeconds tod

timeOfDayToSeconds :: TimeOfDay -> Rational
timeOfDayToSeconds (TimeOfDay h m s) =
  ((toInteger h * 60 + toInteger m) * 60) % 1 + unsafeCoerce s % (10 ^ 12)
