# SplitMix

Pure Haskell and F# implementations of SplitMix pseudo-random number generator.

*Guy L. Steele, Jr., Doug Lea, Christine H. Flood*
*Fast splittable pseudorandom number generators*
*Comm ACM, 49(10), Oct 2014, pp453-472*

http://gee.cs.oswego.edu/dl/papers/oopsla14.pdf

## Source code

After https://github.com/hedgehogqa/haskell-hedgehog/issues/191 and https://github.com/hedgehogqa/haskell-hedgehog/pull/198 I decided to stop maintaining the changes in both places, so the source code can be found in:

 * Haskell Hedgehog [Seed.hs](https://github.com/hedgehogqa/haskell-hedgehog/blob/39b15b9b4d147f6001984c4b7edab00878269da7/hedgehog/src/Hedgehog/Internal/Seed.hs)
 * F# Hedgehog [Seed.fs](https://github.com/hedgehogqa/fsharp-hedgehog/blob/master/src/Hedgehog/Seed.fs)
