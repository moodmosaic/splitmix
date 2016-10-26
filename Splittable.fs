module Splittable =
    type SplitMix =
        { Seed : int64
          Gamma : int64 }
        member this.NextSeed() = { this with Seed = this.Seed + this.Gamma }

    let mix64 (x : SplitMix) : int64 =
        let z = x.Seed
        let z = (z ^^^ (z >>> 33)) * 0xff51afd7ed558ccdL
        let z = (z ^^^ (z >>> 33)) * 0xc4ceb9fe1a85ec53L
        z ^^^ (z >>> 33)

    let mix32 (x : SplitMix) : int =
        let z = x.Seed
        let z = (z ^^^ (z >>> 33)) * 0xff51afd7ed558ccdL
        let z = (z ^^^ (z >>> 33)) * 0xc4ceb9fe1a85ec53L
        (int) (z >>> 32)

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

    let mixGamma (x : SplitMix) : int64 =
        let z = mix64variant13 x ||| 1L
        let n = bitCount (z ^^^ (z >>> 1))
        if n < 24 then z ^^^ 0xaaaaaaaaaaaaaaaaL
        else z

    let doubleUlp : float = 1.0 / double (1L <<< 53)

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
