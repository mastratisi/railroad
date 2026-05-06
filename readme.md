# railroad

**A terse Haskell railroad error handling DSL abstracting over error functors by catamorphism via `Either`.**


The purpose of `railroad` is to error handle tersely, keeping the
happy-path clean of control-flow and functor unpacking.  It does this
with 8 combinators, that abstract over binary co-products (`Bool`,
`Maybe`, `Either`, `Validation`) and Traversables or Folds over them.

```haskell
import Railroad.MonadError
import Control.Monad.Except

example :: Either String Int
example = runExcept $ do
 -- False ? "error" would make the block short circuit to Left "error"
 
 x <- pure (Just 2) ? "Value missing"
 
 -- Right (Just 1) ~> Just 1 ~> 1
 y <- pure (Right $ Just 1) ? "Outer fail" ? "Inner fail"
 
 -- [Just 4] ~> [4] ~> 4
 z <- pure [Just 4] ? "List failed" ?! const "Not a single element"
 
 q <- pure Nothing ?~ 1 
 
 pure (x + y + z + q) -- ~> Right 8 
```

You just need to work in a monad which supports a `MonadError`-like
context. Either by being an instance of `MonadError` or in an Eff
stack with `Error`.

[![Haskell](https://img.shields.io/badge/language-Haskell-%23612a7e.svg)](https://www.haskell.org/)
[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

---

## Install

### Nix

Here is an expression you can add to `ghcWithPackages`:
```nix
railroad = pkgs.haskellPackages.callCabal2nix "railroad" (pkgs.fetchFromGitHub 
  { owner  = "mastratisi"
  ; repo   = "railroad"
  ; rev    = "master"
  ; sha256 = pkgs.lib.fakeSha256; # Nix will tell you the real hash on first build   
  }) {};
```

### Hackage
The package is under the name `railroad`. Link: https://hackage.haskell.org/package/railroad



## Understanding the Operators

| Operator | Purpose                                       | Example                       |
|----------|-----------------------------------------------|-------------------------------|
| `??`     | Derail on error with custom error mapping     | `action ?? toMyError`         |
| `?`      | Derail with constant error                    | `action ? MyError`            |
| `?>`     | Derail on predicate                           | `(action ?> isGood) toErr`    |
| `??~`    | Recover with a mapped default (error → value) | `action ??~ toDefaultVal`     |
| `?~`     | Recover with a const default value            | `action ?~ defaultVal`        |
| `?+`     | Derail on empty collection                    | `items ?+ NoResults`          |
| `?!`     | Derail if not exactly one element             | `items ?! fromCardinalityErr` |
| `?∅`     | Derail on non-empty collections (alias `?@`)  | `items ?∅ DuplicateFound`     |


For the semantics of the operators there are 3 relevant questions:
What counts as an error?  What happens in the error case?  What
happens in the success case?


### What counts as an error?
Here we can split the operators into two families. Those that
interpret errors on the structure of Functors and Traversable of
Functor. Those that interpret error on the cardinality of Foldables.

For the former, they all (`?`, `??`, `?~`, `??~`) interpret what
an error is the same way:

 | Functor          | Error Constructor    | Error Info (`CErr`) | Result Info (`CRes`) |
 |------------------|----------------------|---------------------|----------------------|
 | `Bool`           | `False`              | `()`                | `()`                 |
 | `Maybe a`        | `Nothing`            | `()`                | `a`                  |
 | `Either e a`     | `Left`               | `e`                 | `a`                  |
 | `Validation e a` | `Failure`            | `e`                 | `a`                  |
 | `t f`            | Any element is error | `CErr f`            | `t (CRes f)`         |


For the latter each operator (`?+`, `?!`, `?∅`) interpret what
cardinalities of a Foldable counts as errors differently. To wit:

| Operator    | Success Condition   | Error Info               | Result Info |
|:------------|:--------------------|:-------------------------|:------------|
| `?+`        | Non-empty           | `()`                     | `t a`       |
| `?!`        | Exactly one element | `CardinalityError (t a)` | `a`         |
| `?∅` / `?@` | Is empty            | `t a`                    | `()`        |

Where
```haskell
-- | Structurally a @Maybe ta@ for cardinality failures
data CardinalityError ta = IsEmpty | TooMany ta

-- | Catamorphism for CardinalityError
cardinalityErr :: e -> (ta -> e) -> CardinalityError ta -> e
```

### What happens in the error case?
When an error is detected, the railroad "derails":

1. The provided error-mapping function is applied to the **Error
   Info** (`CErr` or `CardinalityError`).
2. The result of the error mapping is thrown
3. This causes early termination of the monadic sequence.

**Recovery Operators (`?~`, `??~`):** Unlike the other operators,
these do **not** derail the computation. Instead, they provide a
"switch" to bring the logic back onto the happy path by providing a
default value through a recovery function.


**Traversal case:** For `Bool`, `Maybe`, `Either` the error
accumulation is short-circuiting and stops at the first error. For 
`t (Validation e a)` the semigroup constraint on `e` is used to
accumulate all errors together. For more information on accumulating
errors, see the
[validation](https://hackage.haskell.org/package/validation/docs/Data-Validation.html)
package on Hackage.
### What happens in the success case?
In the success case the result is unwrapped or validated (`?>`, `?+`)
and the monad continues its execution. Result Info in the above tables
shows the resulting type inside the monad.

## More
See [`railroad.md`](railroad.md) for a deeper tour, or just read the
code at [`Railroad.hs`](src/Railroad.hs). It's a short file.

## Contact
Feel free to DM me on https://x.com/mastratisi97 . I find it
interesting to know if other people also have found this module
useful.
