# railroad

**Railway-oriented error handling for Haskell. Unwrap a `Maybe`, `Either`, `Bool` or `Validation`, or throw a mapped error, with one short operator.**

[![Haskell](https://img.shields.io/badge/language-Haskell-%23612a7e.svg)](https://www.haskell.org/)
[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

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

`runQuery` returns `Eff es (Either DbError [row])`. Each operator unwraps one
layer, left to right: `? err503` unwraps the database `Either`, then `?!`
demands exactly one row (none is a 404, several a 500).

It works in any `MonadError` (`mtl`, `ExceptT`, servant's `Handler`) and in
`effectful` with the `Error` effect.

```haskell
import Railroad.MonadError
import Control.Monad.Except

example :: Either String Int
example = runExcept $ do
  x <- pure (Just 2) ? "Value missing"                          -- Just 2        ~> 2
  y <- pure (Right (Just 1)) ? "Outer fail" ? "Inner fail"      -- Right (Just 1) ~> Just 1 ~> 1
  z <- pure [Just 4] ? "List failed" ?! const "Not one element" -- [Just 4]      ~> [4] ~> 4
  q <- pure Nothing ?~ 1                                        -- Nothing       ~> 1
  pure (x + y + z + q)                                          -- Right 8
```

## Contents

- [Install](#install)
- [Example: a servant backend](#example-a-servant-backend)
- [The operators](#the-operators)
- [What counts as an error](#what-counts-as-an-error)
- [Gotcha: an empty collection passes](#gotcha-an-empty-collection-passes)
- [Behavior under an evolving code base](#behavior-under-an-evolving-code-base)
- [Exercises](#exercises)
- [Modules](#modules)

## Install

### Hackage

The package is called `railroad`: https://hackage.haskell.org/package/railroad

### Nix

An expression for `ghcWithPackages`:

```nix
railroad = pkgs.haskellPackages.callCabal2nix "railroad" (pkgs.fetchFromGitHub
  { owner  = "mastratisi"
  ; repo   = "railroad"
  ; rev    = "master"
  ; sha256 = pkgs.lib.fakeSha256; # Nix prints the real hash on the first build
  }) {};
```

## Example: a servant backend

Handlers in a servant + `effectful` backend (adapted from a real one) are
chains of queries and checks where each failure is an HTTP status. The queries
return `Eff es (Either DbError [row])`.

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
  -- `permitted` is an SQL exists: one row, so no empty-collection gotcha (see below)
  _ <- runQuery (permitted uid asset) ? err500 ? err403   -- db error, then "not permitted"
  ...
```

The same handlers in plain Haskell:

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

## The operators

Twelve operators.

| Role | Operator | What it does | Example |
|---|---|---|---|
| Throw | `?` | Unwrap, or throw a constant | `action ? MyError` |
| | `??` | Unwrap, or throw an error computed from the failure | `action ?? toMyError` |
| Collections | `?+` | Throw on an empty collection | `items ?+ NoResults` |
| | `?!` | Throw unless there is exactly one element | `items ?! fromCardinalityErr` |
| | `?∅` (alias `?@`) | Throw on a non-empty collection | `items ?∅ DuplicateFound` |
| | `?∃` | Throw unless at least one element succeeded; keep the successes | `sources ?∃ toErr` |
| Recover | `?~` | Recover with a constant | `action ?~ defaultVal` |
| | `??~` | Recover with a value computed from the failure | `action ??~ toDefault` |
| Classify | `?>` | Turn a predicate into a failure | `action ?> isGood ? NotGood` |
| Observe | `?<*` | Run an effect on a failure, then carry on | `action ?<* logIt ? MyError` |
| Fall back | `?\|` | Try the next source on failure; keep the last error | `cache k ?\| db k ? NotFound` |
| | `?\|<>` | The same, combining all the errors with `<>` | `a ?\|<> b ?? toErr` |

All operators are `infixl 1`, like `>>=`, `<&>` and `&`, so they chain left to
right with them without parentheses: `runQuery q ? err503 <&> toDTO`. `<$>` binds
tighter (infixl 4), so `f <$> action ? err` applies `f` to the wrapped value before
it is unwrapped, and `f` receives the `Either`. Map the result with `<&>` after the
operator.

### Classifiers

The operators that throw are built on pure functions that never do. They are in
`Railroad.Bifurcate`, usable without any monad:

```haskell
neg         :: Bifurcate a => a -> Either (CRes a) (CErr a)   -- swap success and failure
tagBy       :: (a -> Bool) -> a -> Either a a                 -- Right when the predicate holds
successes   :: (Foldable t, Bifurcate a) => t a -> [CRes a]
failures    :: (Foldable t, Bifurcate a) => t a -> [CErr a]
anySucceeds :: (Foldable t, Bifurcate a) => t a -> Either [CErr a] [CRes a]
```

`neg` expresses "must not exist" on a single value: `findByEmail e <&> neg ? err409`
throws `err409` when a user is found and passes on `Nothing`. `?∃` is "at least one
of these sources answered": it throws when all of them failed and returns the
successes otherwise.

### Fallbacks

`?|` and `?|<>` try one source after another and stop at the first success. The
sources may be different structures (a cache returning `Maybe`, a database
returning `Either`) with the same success type. The right-hand side only runs if
the left one failed. The result is bifurcatable, so a final `?` or `??` turns a
total failure into a throw:

```haskell
cache k ?| db k ?| legacy k ? NotFound
```

`?|<>` needs the sources to share an error type and combines all the errors with
`<>`, in source order. Map differing error types to a common one first:

```haskell
(cache k <&> note [CacheMiss]) ?|<> (db k <&> first pure) ?? NotFound
```

`Validation` runs all independent checks and collects every failure. A fallback
chain succeeds on the first good answer.

### Observing failures

`? err503` throws the underlying failure away. `?<*` runs an effect on the failure
first and passes the value through unchanged, so the next operator still sees it:

```haskell
runQuery q ?<* logDbError ? err503
```

The handler takes the failure (`CErr`), returns `m ()`, and runs only on a
failure. It never throws.

## What counts as an error

The operators `?`, `??`, `?~`, `??~`, `?>`, `?|` and `?<*` agree on what a failure
is, through the `Bifurcate` class:

| Type | Failure | Failure info (`CErr`) | Success info (`CRes`) |
|---|---|---|---|
| `Bool` | `False` | `()` | `()` |
| `Maybe a` | `Nothing` | `()` | `a` |
| `Either e a` | `Left` | `e` | `a` |
| `Validation e a` | `Failure` | `e` | `a` |
| `t f` | any element fails | `CErr f` | `t (CRes f)` |

On a traversable of `Bool`, `Maybe` or `Either` the check stops at the first
failure. On `t (Validation e a)` the `Semigroup` on `e` collects all of them, so
`pure (map validateFile files) ?? badRequest` turns every invalid file into one 400.
An empty `t f` has no element to fail, so it succeeds (see the gotcha below).

The collection operators `?+`, `?!` and `?∅` look at the number of elements:

| Operator | Succeeds when | Failure info | Success info |
|:---|:---|:---|:---|
| `?+` | non-empty | `()` | `t a` |
| `?!` | exactly one element | `CardinalityError (t a)` | `a` |
| `?∅` | empty | `t a` | `()` |

```haskell
data CardinalityError ta = IsEmpty | TooMany ta

cardinalityErr :: e -> (ta -> e) -> CardinalityError ta -> e   -- a fold for it
```

When an operator throws, the error mapping is applied to the failure info, the
result is thrown, and the monadic sequence stops. `?~` and `??~` never throw:
they put a default value on the happy path instead.

## Gotcha: an empty collection passes

On a traversable, `?` and `??` check that **every** element succeeds (∀). Over
an empty collection that holds vacuously, like `and []`:

```haskell
pure ([] :: [Bool]) ? err403    -- succeeds, with []
```

That surprises a permission check where "no rows" should mean "not permitted".
Require at least one element first, with `?+`:

```haskell
_ <- runQuery (permitted uid asset) ? err500 ?+ err403 ? err403
--                                  ^ db error ^ >= 1 row ^ all True
```

The collection operators are the other quantifiers, and they chain:

| Operator | Succeeds when | Logic |
|---|---|---|
| `?` / `??` on `[Bool]`, `[Maybe a]` | every element succeeds | ∀ x ∈ xs. succeeds x |
| `?+` | there is at least one element | ∃ x ∈ xs |
| `?!` | there is exactly one element | ∃! x ∈ xs |
| `?∅` | there is no element | ¬∃ x ∈ xs |
| `?∃` | at least one element succeeds | ∃ x ∈ xs. succeeds x |
| `?+` then `?` | there is one, and every one succeeds | (∃ x ∈ xs) ∧ (∀ x ∈ xs. succeeds x) |

## Behavior under an evolving code base

The same operator works on `Maybe a`, `Either e a` and a list of either, so a
dependency can change from `Maybe` to `Either` without touching its callers.
[`examples/error-handling-styles`](examples/error-handling-styles) writes eleven
handlers in three styles (railroad, plain `case`, and `orThrow`/`expectOne`
helpers) and applies nine changes to each. Cells are edit sites / lines touched.

| Change | railroad | helpers | `case` |
|---|---|---|---|
| `Maybe a` to `Either CacheError a` | 0 / 0 | 2 / 6 | 2 / 8 |
| `Either DbError [User]` to `Either DbError (Maybe User)` | 1 / 2 | 4 / 18 | 3 / 14 |
| add a third fallback source | 1 / 1 | 1 / 6 | 2 / 11 |
| database timeout becomes 504 | 4 / 10 | 9 / 19 | 8 / 16 |
| all nine changes | 17 / 34 | 28 / 72 | 29 / 85 |

Adding a source to a fallback chain is one line:

```haskell
-- before
cache k ?| db k ? NotFound
-- after
cache k ?| db k ?| legacy k ? NotFound
```

In the `case` version it is another level of nesting around the existing branches.
The timeout change took four edits, one per `? Unavailable`, because the error is a
literal at each call site; with `?? dbErr` from the start it is one.

One change went wrong silently. `validateName` changed from `Validation` to
`Either`, and `pure (map validateName names) ?? BadRequest` kept compiling but
reported the first problem instead of all of them. `?` over a collection chooses
between stopping at the first failure and collecting every one by the element
type. The other two styles stopped compiling. After changing a return type, check
the operators at its call sites: a list where there was one value changes what `?`
means, and an empty list passes (see the gotcha above).

One LLM wrote the handlers. Eleven handlers and nine changes is a small sample,
and only GHC 9.10.3 was used.

## Exercises

[`examples/exercises`](examples/exercises) teaches the operators by translation.
Each exercise is a function written with `case` and an empty stub. You fill in the
stub with the operators until a test that runs both on random inputs agrees. There
are 38 exercises in 13 lessons, with hints and an answer key.

```
cd examples/exercises && cabal test exercises
```

## Modules

- `Railroad`: the operators for the `effectful` `Error` effect.
- `Railroad.MonadError`: the same operators for any `MonadError`.
- `Railroad.Bifurcate`: everything that does not throw. The `Bifurcate` class, the classifiers
  (`neg`, `tagBy`, `successes`, `failures`, `anySucceeds`, `atLeastOne`, `exactlyOne`,
  `noneAtAll`), `CardinalityError`, and the operators `?>`, `?~`, `??~`, `?|`, `?|<>`, `?<*`.

The first two re-export it, so you rarely import it directly. For the
reasoning behind the design, read [`railroad.md`](railroad.md), or the code: the
core is a short file.

## Contact

DM me on https://x.com/mastratisi97 . I like to hear if other people find this
useful.
