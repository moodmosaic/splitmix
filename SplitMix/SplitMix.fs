namespace SplittableRandom

/// "Fast Splittable Pseudorandom Number Generators,
/// Guy L. Steele Jr., Doug Lea, Christine H. Flood"
/// http://2014.splashcon.org/event/oopsla2014-fast-splittable-pseudorandom-number-generators

type [<Struct>] SplitMix internal (seed : int64, gamma : int64) =
    member this.Seed = seed
    member this.Gamma = gamma

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SplitMix =
    /// A predefined gamma value's needed for initializing the "root"
    /// instances of SplittableRandom that is, instances not produced
    /// by splitting an already existing instance. We choose: the odd
    /// integer closest to 2^64/φ, where φ = (1 + √5)/2 is the golden
    /// ratio, and call it GOLDEN_GAMMA.
    let [<Literal>] private goldenGamma : int64 = 0x9e3779b97f4a7c15L

    let ofSeed (seed : int64) : SplitMix =
        SplitMix (seed, goldenGamma)

    /// Mix the bits of a 64-bit arg to produce a result, computing a
    /// bijective function on 64-bit values.
    let private mix64 (x : int64) : int64 =
        let z = x
        let z = (z ^^^ (z >>> 33)) * 0xff51afd7ed558ccdL
        let z = (z ^^^ (z >>> 33)) * 0xc4ceb9fe1a85ec53L
        z ^^^ (z >>> 33)

    /// Mix the bits of a 64-bit arg to produce a result, computing a
    /// bijective function on 64-bit values.
    let private mix32 (x : int64) : int =
        let z = x
        let z = (z ^^^ (z >>> 33)) * 0xff51afd7ed558ccdL
        let z = (z ^^^ (z >>> 33)) * 0xc4ceb9fe1a85ec53L
        (int) (z >>> 32)

    /// Mix the bits of a 64-bit arg to produce a result, computing a
    /// bijective function on 64-bit values.
    let private mix64variant13 (x : int64) : int64 =
        let z = x
        let z = (z ^^^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
        let z = (z ^^^ (z >>> 27)) * 0x94d049bb133111ebL
        z ^^^ (z >>> 31)

    let private bitCount (x : int64) : int =
        let x = x - ((x >>> 1) &&& 0x5555555555555555L)
        let x = (x &&& 0x3333333333333333L) + ((x >>> 2) &&& 0x3333333333333333L)
        let x = (x + (x >>> 4)) &&& 0x0f0f0f0f0f0f0f0fL
        let x = x + (x >>> 8)
        let x = x + (x >>> 16)
        let x = x + (x >>> 32)
        (int x) &&& 0x7f

    /// Mix the bits of a 64-bit arg to produce a result, computing a
    /// bijective function on 64-bit values.
    let private mixGamma (x : int64) : int64 =
        let z = mix64variant13 x ||| 1L
        let n = bitCount (z ^^^ (z >>> 1))
        if n < 24 then z ^^^ 0xaaaaaaaaaaaaaaaaL
        else z

    let ofRandomSeed () =
        let x = System.DateTimeOffset.UtcNow.Ticks + 2L * goldenGamma
        SplitMix (mix64 x, mixGamma x + goldenGamma)

    let private nextSeed (x : SplitMix) : SplitMix =
        SplitMix (x.Seed + x.Gamma, x.Gamma)

    let split (x : SplitMix) : SplitMix =
        let y : SplitMix = x |> nextSeed
        let z : SplitMix = y |> nextSeed
        SplitMix (mix64 y.Seed, mixGamma z.Gamma)

    let nextInt32 (x : SplitMix) : int32 = mix32 <| (nextSeed x).Seed
    let nextInt64 (x : SplitMix) : int64 = mix64 <| (nextSeed x).Seed

    let nextFloat (x : SplitMix) : float =
        /// The value 'DOUBLE_ULP' is the positive difference between
        /// 1.0 and the smallest double value, larger than 1.0; it is
        /// used for deriving a double value via a 64-bit long value,
        /// and in F# this is calculated as 1.0 / double (1L <<< 53).
        let doubleUlp = 1.110223025e-16
        float (nextInt64 x >>> 11) * doubleUlp
