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
    localTimeRep a == localTimeRep b

instance Eq (Apx UTCTime) where
  (==) (Apx a) (Apx b) =
    utcTimeApxRep a == utcTimeApxRep b

instance Eq (Apx TimeOfDay) where
  (==) (Apx a) (Apx b) =
    timeOfDayApxRep a == timeOfDayApxRep b

instance Eq (Apx TimeZone) where
  (==) (Apx a) (Apx b) =
    a == b


utcTimeApxRep (UTCTime d d') =
  (d, picoApxRep (unsafeCoerce d'))

localTimeRep (LocalTime d t) =
  (d, timeOfDayApxRep t)

timetzApxRep (t, tz) = 
  (timeOfDayApxRep t, tz)

timeOfDayApxRep :: TimeOfDay -> (Int, Int, Integer)
timeOfDayApxRep (TimeOfDay h m s) =
  (h, m, picoApxRep s)

picoApxRep :: Pico -> Integer
picoApxRep s =
  let p = unsafeCoerce s :: Integer
      in floor (p % 10^6)

