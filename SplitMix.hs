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
  , ofWord64
  , split
  , nextWord32
  , nextWord64
  , nextFloat
  ) where

import           Data.Bits (popCount, shiftR, xor, (.|.))
import           Data.Word (Word32, Word64)

data Seed = Seed
  { value :: Word64,
    gamma :: Word64 }

-- A predefined gamma value's needed for initializing the "root"
-- instances of SplittableRandom that is, instances not produced
-- by splitting an already existing instance. We choose: the odd
-- integer closest to 2^64/φ, where φ = (1 + √5)/2 is the golden
-- ratio, and call it GOLDEN_GAMMA.
goldenGamma :: Word64
goldenGamma =  0x9e3779b97f4a7c15

-- Mix the bits of a 64-bit arg to produce a result, computing a
-- bijective function on 64-bit values.
mix64 :: Word64 -> Word64
mix64 x =
  let y = (x `xor` (x `shiftR` 33)) * 0xff51afd7ed558ccd
      z = (y `xor` (y `shiftR` 33)) * 0xc4ceb9fe1a85ec53
   in z `xor` (z `shiftR` 33)

-- Mix the bits of a 64-bit arg to produce a result, computing a
-- bijective function on 64-bit values.
mix32 :: Word64 -> Word32
mix32 x =
  let y = (x `xor` (x `shiftR` 33)) * 0xff51afd7ed558ccd
      z = (y `xor` (y `shiftR` 33)) * 0xc4ceb9fe1a85ec53
   in fromIntegral (z `shiftR` 32)

-- Mix the bits of a 64-bit arg to produce a result, computing a
-- bijective function on 64-bit values.
mix64variant13 :: Word64 -> Word64
mix64variant13 x =
  let y = (x `xor` (x `shiftR` 30)) * 0xbf58476d1ce4e5b9
      z = (y `xor` (y `shiftR` 27)) * 0x94d049bb133111eb
   in z `xor` (z `shiftR` 31)

-- Mix the bits of a 64-bit arg to produce a result, computing a
-- bijective function on 64-bit values.
mixGamma :: Word64 -> Word64
mixGamma x =
  let y = mix64variant13 x .|. 1
      n = popCount $ y `xor` (y `shiftR` 1)
   in if n < 24 then y `xor` 0xaaaaaaaaaaaaaaaa else y

ofWord64 :: Word64 -> Seed
ofWord64 x =
  let rnd = x + (2 * goldenGamma) in
  Seed
    { value = mix64    rnd,
      gamma = mixGamma rnd + goldenGamma }

nextSeed :: Seed -> Seed
nextSeed s0 =
  s0 { value = value s0 + gamma s0 }

split :: Seed -> (Seed, Seed)
split s0 =
  let s1 = nextSeed s0
      s2 = nextSeed s1
  in (s0 { value = mix64 (value s1) }
    , s1 { value = mix64 (value s2) })

nextWord32 :: Seed -> Word32
nextWord32 s0 = let s1 = nextSeed s0 in mix32 (value s1)

nextWord64 :: Seed -> Word64
nextWord64 s0 = let s1 = nextSeed s0 in mix64 (value s1)

nextFloat :: Seed -> Float
nextFloat s0 =
  -- The value 'DOUBLE_ULP' is the positive difference between
  -- 1.0 and the smallest double value, larger than 1.0; it is
  -- used for deriving a double value via a 64-bit long value.
  fromIntegral (nextWord64 s0 `shiftR` 11) * doubleUlp :: Float
    where doubleUlp = 1.110223025e-16
