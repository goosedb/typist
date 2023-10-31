# Typist

Typist is a library for interpolation. It uses type-level strings (`Symbol`s) for template. It uses `UnconsSynbol` type family which is added in GHC-9.2, so you need GHC version 9.2 or higher to use it.   

Core functionality is in `Typist.Internal.Format`: 
* Type family `Format` which transforms `Symbol` to function which renders final _string_ 
* `Arg` newtype over `Text.Lazy.Builder` which carries name of parameter and it's position in string on type-level (for perfomance goods)
* Type class `Interpolate` which renders final _string_ on term level

An example of wrapping it (and also ready to use interface) is located in `Typist.Logging`:
* Type class `Logged` which is about `Show` for special purpose (logging, huh) 
* Functions `fmt` which simply renders _string_ and `fmtt` which also returns template
* `Unquoted` newtype for interpolation strings without `\"` 
Example (more examples can be found in `test/Test`):

```haskell

question = fmt @"Hello, #{name}, do you like #{dish}?"
  (#name $ Unquoted @String "Mike")
  (#dish $ Unquoted @String "pasta")
```

Performance is comparable with bare concatenation with `<>`. It slightly slower. Benches can be found in `test/Bench`

![Alt text](./results_cpu.svg)
