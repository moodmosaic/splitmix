--
-- This is a port of "Fast Splittable Pseudorandom Number Generators"
-- by Steele et. al. [1].

-- The paper's algorithm provides decent randomness for most purposes
-- but sacrifices cryptographic-quality randomness in favor of speed.
-- The original implementation is tested with DieHarder and BigCrush;
-- see the paper for details.

-- This implementation is a port from the paper, and also taking into
-- account the SplittableRandom.java source code in OpenJDK v8u40-b25
-- as well as splittable_random.ml in Jane Street's standard library
-- overlay (kernel) v113.33.03, and Random.fs in FsCheck v3, which is
-- the initial motivation of doing this port [2] although the idea of
-- doing this port is for having it as the default, splittable random
-- generator in dotnet-jack [3] – QuickCheck with shrinking for free.

-- Other than the choice of initial seed for 'ofRandomSeed' this port
-- should be faithful. Currently, we have not rerun the DieHarder, or
-- BigCrush tests on this implementation.

-- 1. Guy L. Steele, Jr., Doug Lea, Christine H. Flood
--    Fast splittable pseudorandom number generators
--    Comm ACM, 49(10), Oct 2014, pp453-472.

-- 2. https://github.com/fscheck/FsCheck/issues/198
-- 3. https://github.com/jystic/dotnet-jack/issues/26
--

module SplitMix (
    Seed
  , ofInteger
  , ofRandomSeed
  , split
  , nextInt32
  , nextInt64
  , nextFloat
  ) where

import           Data.Bits      (popCount, shiftR, xor, (.|.))
import           Data.Int       (Int32, Int64)
import           System.CPUTime (getCPUTime)

data Seed = Seed
  { value :: Int64,
    gamma :: Int64 }

-- A predefined gamma value's needed for initializing the "root"
-- instances of SplittableRandom that is, instances not produced
-- by splitting an already existing instance. We choose: the odd
-- integer closest to 2^64/φ, where φ = (1 + √5)/2 is the golden
-- ratio, and call it GOLDEN_GAMMA.
goldenGamma :: Int64
goldenGamma =  -7046029254386353131

ofInteger :: Int64 -> Seed
ofInteger x = Seed
  { value = x,
    gamma = goldenGamma }

-- Mix the bits of a 64-bit arg to produce a result, computing a
-- bijective function on 64-bit values.
mix64 :: Int64 -> Int64
mix64 x =
  let y = (x `xor` (x `shiftR` 33)) * (-49064778989728563)
      z = (y `xor` (y `shiftR` 33)) * (-4265267296055464877)
   in z `xor` (z `shiftR` 33)

-- Mix the bits of a 64-bit arg to produce a result, computing a
-- bijective function on 64-bit values.
mix32 :: Int64 -> Int32
mix32 x =
  let y = (x `xor` (x `shiftR` 33)) * (-49064778989728563)
      z = (y `xor` (y `shiftR` 33)) * (-4265267296055464877)
   in (fromIntegral z `shiftR` 32) :: Int32

-- Mix the bits of a 64-bit arg to produce a result, computing a
-- bijective function on 64-bit values.
mix64variant13 :: Int64 -> Int64
mix64variant13 x =
  let y = (x `xor` (x `shiftR` 30)) * (-4658895280553007687)
      z = (y `xor` (y `shiftR` 27)) * (-7723592293110705685)
   in z `xor` (z `shiftR` 31)

-- Mix the bits of a 64-bit arg to produce a result, computing a
-- bijective function on 64-bit values.
mixGamma :: Int64 -> Int64
mixGamma x =
  let y = mix64variant13 x .|. 1
      n = popCount $ y `xor` (y `shiftR` 1)
   in if n < 24 then y `xor` (-6148914691236517206) else y

ofRandomSeed :: () -> IO Seed
ofRandomSeed () =
  getCPUTime >>= \x ->
    let x' = (fromIntegral x :: Int64) + (2 * goldenGamma) in
    return Seed
      { value = mix64    x',
        gamma = mixGamma x' + goldenGamma }

nextSeed :: Seed -> Seed
nextSeed s0 =
  s0 { value = value s0 + gamma s0 }

split :: Seed -> (Seed, Seed)
split s0 =
  let s1 = nextSeed s0
      s2 = nextSeed s1
  in (s0 { value = mix64 (value s1) }
    , s1 { value = mix64 (value s2) })

nextInt32 :: Seed -> Int32
nextInt32 s0 = let s1 = nextSeed s0 in mix32 (value s1)

nextInt64 :: Seed -> Int64
nextInt64 s0 = let s1 = nextSeed s0 in mix64 (value s1)

nextFloat :: Seed -> Float
nextFloat s0 =
  -- The value 'DOUBLE_ULP' is the positive difference between
  -- 1.0 and the smallest double value, larger than 1.0; it is
  -- used for deriving a double value via a 64-bit long value.
  fromIntegral (nextInt64 s0 `shiftR` 11) * doubleUlp :: Float
    where doubleUlp = 1.110223025e-16
