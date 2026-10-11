# railroad

**A terse Haskell railroad error handling DSL abstracting over error functors by catamorphism via `Either`.**

Instead of

```haskell
rows <- runQuery (userById uid) >>= either (const (throwError err503)) pure
user <- case rows of
  []  -> throwError err404
  [u] -> pure u
  _   -> throwError err500
```

write

```haskell
user <- runQuery (userById uid) ? err503 ?! cardinalityErr err404 (const err500)
```

Here `runQuery` returns `Eff es (Either DbError [row])`. Each operator
peels one layer (the database failure, then "exactly one row") and says
what to throw if it fails, so the happy path stays one line. A longer
[servant example](#example-a-servant-backend) is below.

The purpose of `railroad` is to error handle tersely, keeping the
happy-path clean of control-flow and functor unpacking.  It does this
with 10 combinators, that abstract over binary co-products (`Bool`,
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

## Modules

- `Railroad`: the operators for the `effectful` `Error` effect.
- `Railroad.MonadError`: the same operators for any `MonadError` (`mtl`, `ExceptT`, servant's `Handler`).
- `Railroad.Bifurcate`: the `Bifurcate` class and the operators that never throw
  (`?>`, `?~`, `??~`, `?|`, `?|<>`); `Railroad.Cardinality`: `CardinalityError`.
  Both of the above re-export these, so you rarely import them directly.

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



## Example: a servant backend

Handlers in a servant + `effectful` backend (adapted from a real one)
are chains of queries and checks where each failure is an HTTP status.
The queries return `Eff es (Either DbError [row])`: a database failure
wrapping the rows. With `railroad`:

```haskell
getUser :: Error ServerError :> es => UserId -> Eff es UserDTO
getUser uid = do
  user <- runQuery (userById uid) ? err503 ?! cardinalityErr err404 (const err500)
  pure (toUserDTO user)

register :: Error ServerError :> es => Email -> Eff es Token
register email = do
  runQuery (userByEmail email) ? err503 ?∅ const err409   -- must not exist yet
  makeJWT email ? err500

updateAsset :: Error ServerError :> es => UserId -> Asset -> Eff es ()
updateAsset uid asset = do
  _ <- runQuery (permitted uid asset) ? err500 ? err403   -- db error, then "not permitted"
  ...
```

Each operator peels one layer, left to right: `? err503` unwraps the
`Either`, then `?!` demands exactly one row (none is a 404, several a
500). The same handlers in plain Haskell:

```haskell
getUser' uid = do
  rows <- runQuery (userById uid) >>= either (const (throwError err503)) pure
  user <- case rows of
    []  -> throwError err404
    [u] -> pure u
    _   -> throwError err500
  pure (toUserDTO user)

register' email = do
  rows <- runQuery (userByEmail email) >>= either (const (throwError err503)) pure
  unless (null rows) (throwError err409)
  makeJWT email >>= either (const (throwError err500)) pure

updateAsset' uid asset = do
  allowed <- runQuery (permitted uid asset) >>= either (const (throwError err500)) pure
  unless (and allowed) (throwError err403)
  ...
```

The failure paths no longer take up lines, and the order of the layers is
the order you read them in. Validating a request with a `Validation`
reports every problem at once: `pure (map validateFile files) ?? badRequest`
turns all the invalid files into one 400. `Railroad.MonadError` has the
same operators for plain `MonadError`, including servant's `Handler`.

## Understanding the Operators

| Operator | Purpose                                       | Example                       |
|----------|-----------------------------------------------|-------------------------------|
| `??`     | Derail on error with custom error mapping     | `action ?? toMyError`         |
| `?`      | Derail with constant error                    | `action ? MyError`            |
| `?>`     | Tag by predicate (`Left` carries the value)   | `action ?> isGood ? NotGood`  |
| `??~`    | Recover with a mapped default (error → value) | `action ??~ toDefaultVal`     |
| `?~`     | Recover with a const default value            | `action ?~ defaultVal`        |
| `?+`     | Derail on empty collection                    | `items ?+ NoResults`          |
| `?!`     | Derail if not exactly one element             | `items ?! fromCardinalityErr` |
| `?∅`     | Derail on non-empty collections (alias `?@`)  | `items ?∅ DuplicateFound`     |
| `?\|`    | Fall back to another action on error         | `cache k ?\| db k ? NotFound` |
| `?\|<>`  | Fall back, combining the errors with `<>`     | `a ?\|<> b ?? toErr`         |


All operators are `infixl 1`, like `>>=`, `<&>` and `&`, so they chain
left to right with them without parentheses:
`runQuery q ? err503 <&> toDTO`.

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

An empty `t f` has no element to fail, so it succeeds: see [the gotcha](#gotcha-an-empty-collection-passes).


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
In the success case the result is unwrapped or validated (`?+`, `?!`)
and the monad continues its execution. Result Info in the above tables
shows the resulting type inside the monad.

### Fallback operators
`?|` and `?|<>` try one source after another and stop at the first
success. They are for sources that may be different structures (for
example a cache returning `Maybe` and a database returning `Either`)
with the same success type (`CRes`). The result is bifurcatable, so a
final `?` or `??` turns a total failure into a throw:

```haskell
cache k ?| db k ?| legacy k ? NotFound
```

`?|` keeps the last error. `?|<>` also needs the sources to share the
error type (`CErr`) and combines all the errors with `<>`, in source
order, when every source fails. Map differing error types to a common one
first, for example with `note`-like functions:

```haskell
(cache k <&> note [CacheMiss]) ?|<> (db k <&> first pure) ?? NotFound
```

Neither throws by itself, and the right-hand side only runs if the left
one failed. Unlike `Validation`, which runs all independent checks and
collects every failure, a fallback chain succeeds on the first good answer.

## Gotcha: an empty collection passes

On a traversable, `?` and `??` check that **every** element succeeds
(∀). Over an empty collection that holds vacuously, like `and []`:

```haskell
pure ([] :: [Bool]) ? err403    -- succeeds, with []
```

That surprises a permission check where "no rows" should mean "not
permitted". Require at least one element first, with `?+` (∃), and
chain the two checks:

```haskell
_ <- runQuery (permitted uid asset) ? err500 ?+ err403 ? err403
--                                    ^ db error ^ >= 1 row ^ all True
```

The cardinality operators are the other quantifiers, so the usual
conditions are each one operator, and they chain:

| Operator                            | Succeeds when                       | Logic                           |
|-------------------------------------|-------------------------------------|---------------------------------|
| `?` / `??` on `[Bool]`, `[Maybe a]` | every element succeeds              | ∀ x ∈ xs. succeeds x            |
| `?+`                                | there is at least one element       | ∃ x ∈ xs                        |
| `?!`                                | there is exactly one element        | ∃! x ∈ xs                       |
| `?∅`                                | there is no element                 | ¬∃ x ∈ xs                       |
| `?+` then `?`                       | there is one, and every one succeeds | (∃ x ∈ xs) ∧ (∀ x ∈ xs. succeeds x) |

## More
See [`railroad.md`](railroad.md) for a deeper tour, or just read the
code at [`Railroad.hs`](src/Railroad.hs). It's a short file.

## Contact
Feel free to DM me on https://x.com/mastratisi97 . I find it
interesting to know if other people also have found this module
useful.
