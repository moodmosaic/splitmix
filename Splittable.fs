/// "Fast Splittable Pseudorandom Number Generators,
/// Guy L. Steele Jr., Doug Lea, Christine H. Flood"
module Splittable =
    type SplitMix =
        { Seed : int64
          Gamma : int64 }
        member this.NextSeed() = { this with Seed = this.Seed + this.Gamma }

    /// Mix the bits of a 64-bit arg to produce a result, computing a
    /// bijective function on 64-bit values.
    let mix64 (x : SplitMix) : int64 =
        let z = x.Seed
        let z = (z ^^^ (z >>> 33)) * 0xff51afd7ed558ccdL
        let z = (z ^^^ (z >>> 33)) * 0xc4ceb9fe1a85ec53L
        z ^^^ (z >>> 33)

    /// Mix the bits of a 64-bit arg to produce a result, computing a
    /// bijective function on 64-bit values.
    let mix32 (x : SplitMix) : int =
        let z = x.Seed
        let z = (z ^^^ (z >>> 33)) * 0xff51afd7ed558ccdL
        let z = (z ^^^ (z >>> 33)) * 0xc4ceb9fe1a85ec53L
        (int) (z >>> 32)

    /// Mix the bits of a 64-bit arg to produce a result, computing a
    /// bijective function on 64-bit values.
    let mix64variant13 (x : SplitMix) : int64 =
        let z = x.Seed
        let z = (z ^^^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
        let z = (z ^^^ (z >>> 27)) * 0x94d049bb133111ebL
        z ^^^ (z >>> 31)

    let private bitCount (n : int64) : int =
        let n = n - ((n >>> 1) &&& 0x5555555555555555L)
        let n = (n &&& 0x3333333333333333L) + ((n >>> 2) &&& 0x3333333333333333L)
        let n = (n + (n >>> 4)) &&& 0x0f0f0f0f0f0f0f0fL
        let n = n + (n >>> 8)
        let n = n + (n >>> 16)
        let n = n + (n >>> 32)
        (int n) &&& 0x7f

    /// Mix the bits of a 64-bit arg to produce a result, computing a
    /// bijective function on 64-bit values.
    let mixGamma (x : SplitMix) : int64 =
        let z = mix64variant13 x ||| 1L
        let n = bitCount (z ^^^ (z >>> 1))
        if n < 24 then z ^^^ 0xaaaaaaaaaaaaaaaaL
        else z

    /// The value 'DOUBLE_ULP' is the positive difference between 1.0
    /// and the smallest double value larger than 1.0; it is used for
    /// deriving a double value from a 64-bit long value.
    let doubleUlp : float = 1.0 / double (1L <<< 53)

    /// A predefined gamma value's needed for initializing the "root"
    /// instances of SplittableRandom that is, instances not produced
    /// by splitting an already existing instance. We choose: the odd
    /// integer closest to 2^64/φ, where φ = (1 + √5)/2 is the golden
    /// ratio, and call it GOLDEN_GAMMA.
    [<Literal>]
    let goldenGamma : int64 = 0x9e3779b97f4a7c15L

    let create' (seed: int64) (gamma : int64) =
        { Seed = seed; Gamma = gamma }

    let create (seed : int64) =
        create' seed goldenGamma

    let split (sm : SplitMix) =
        { Seed =
            mix64
            <| sm.NextSeed ()
          Gamma =
            mixGamma
            <| sm.NextSeed () }

    let nextInt32 (x : SplitMix) =
        mix32 <| x.NextSeed ()

    let nextInt64 (x : SplitMix) =
        mix64 <| x.NextSeed ()

    let nextFloat (s : SplitMix) =
        double (nextInt64 s >>> 11) * doubleUlp
