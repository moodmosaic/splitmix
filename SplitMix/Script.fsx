// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "SplitMix.fs"
open SplittableRandom

let rnd1 = SplitMix.ofRandomSeed ()

let num1 = SplitMix.nextInt32 rnd1
let num2 = SplitMix.nextInt64 rnd1
let num3 = SplitMix.nextInt64 rnd1

let rnd2 = SplitMix.split rnd1

let num4 = SplitMix.nextInt32 rnd2
let num5 = SplitMix.nextInt64 rnd2
let num6 = SplitMix.nextInt64 rnd2

let rnd3 = SplitMix.split rnd2

let num7 = SplitMix.nextInt32 rnd3
let num8 = SplitMix.nextInt64 rnd3
let num9 = SplitMix.nextInt64 rnd3

// Define your library scripting code here

