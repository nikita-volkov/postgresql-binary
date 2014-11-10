module PostgreSQLBinary.Time where

import PostgreSQLBinary.Prelude
import Data.Time.Calendar.Julian


type YMD = (Integer, Int, Int)

{-# INLINE dayToJulianYMD #-}
dayToJulianYMD :: Day -> YMD
dayToJulianYMD = 
  toJulian

{-# INLINE dayToGregorianYMD #-}
dayToGregorianYMD :: Day -> YMD
dayToGregorianYMD = 
  toGregorian

{-# INLINABLE ymdToInt #-}
ymdToInt :: YMD -> Int
ymdToInt (y, m, d) =
  let (m', y') = if m > 2 then (m + 1, fromIntegral $ y + 4800)
                          else (m + 13, fromIntegral $ y + 4799)
      century = y' `div` 100
      in 
          y' * 365 - 32167 + 
          y' `div` 4 - century + century `div` 4 + 
          7834 * m' `div` 256 + d

{-# INLINABLE dayToPostgresJulian #-}
dayToPostgresJulian :: Day -> Integer
dayToPostgresJulian =
  (+ (2400001 - 2451545)) . toModifiedJulianDay

{-# INLINABLE postgresJulianToDay #-}
postgresJulianToDay :: Int -> Day
postgresJulianToDay =
  ModifiedJulianDay . fromIntegral . subtract (2400001 - 2451545)

{-# INLINABLE microsToTimeOfDay #-}
microsToTimeOfDay :: Int -> TimeOfDay
microsToTimeOfDay =
  evalState $ do
    h <- state $ flip divMod (10 ^ 6 * 60 * 60)
    m <- state $ flip divMod (10 ^ 6 * 60)
    u <- get
    let p = fromIntegral u * 10 ^ 6 :: Integer
    return $
      TimeOfDay h m (unsafeCoerce p)

{-# INLINABLE secondsToTimeOfDay #-}
secondsToTimeOfDay :: Double -> TimeOfDay
secondsToTimeOfDay =
  evalState $ do
    h <- state $ flip divMod' (60 * 60)
    m <- state $ flip divMod' (60)
    s <- get
    let p = truncate $ toRational s * 10 ^ 12 :: Integer
    return $
      TimeOfDay h m (unsafeCoerce p)
