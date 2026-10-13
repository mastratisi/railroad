# The idea behind railroad

A longer tour than the [readme](readme.md): why the operators exist, and
why they are small. The code is short enough to read next to this.

## The problem

Here is a servant handler written with the operators:

```haskell
registerStart :: EmailAddress -> Eff UserStack Text
registerStart email = do
  time <- getTime
  runQuery (userByEmail time email) ? err503 ?∅ const err409
  makeJWT email (Just time) ? err500
```

And the same handler in the most common style:

```haskell
registerStart :: EmailAddress -> Eff UserStack Text
registerStart email = do
  time <- getTime

  userMaybe <- runQuery (userByEmail time email)
  case userMaybe of
    Left _         -> throwError err503
    Right Nothing  -> pure ()
    Right (Just _) -> throwError err409

  jwtResult <- makeJWT email (Just time)
  case jwtResult of
    Left _      -> throwError err500
    Right token -> pure token
```

Or with the eliminators `either` and `maybe`:

```haskell
registerStart email = do
  time <- getTime
  runQuery (userByEmail time email) >>= either
    (const $ throwError err503)
    (maybe (pure ()) (const $ throwError err409))
  makeJWT email (Just time) >>= either
    (const $ throwError err500)
    pure
```

The first version buries the happy path in control flow. The second is
shorter but you must work out, at every call site, which branch is the
failure and which eliminator to reach for. The operators remove both
chores, and the layers read left to right in the order they are unwrapped.

## Why one operator is enough

Most error handling in the wild happens in a `MonadError`-like context
(`mtl`, or an `effectful` stack with `Error`). It is repetitive: a call
returns what we care about wrapped in something, `Maybe a`, `Either e a`,
`Bool`, and we case-split, and in the failure branch we translate the
failure into the monad's error type and throw it.

All of those wrappers are binary coproducts. Up to isomorphism:

| type             | is isomorphic to  | failure carries | success carries |
|------------------|-------------------|-----------------|-----------------|
| `Bool`           | `Either () ()`    | `()`            | `()`            |
| `Maybe a`        | `Either () a`     | `()`            | `a`             |
| `Either e a`     | itself            | `e`             | `a`             |
| `Validation e a` | `Either e a`      | `e`             | `a`             |

(`Validation` and `Either` are isomorphic as *types*; as applicatives
they differ, and that matters below.)

So we fix, once, which summand is the failure, and every one of these
becomes "an `Either` in disguise".

## `Bifurcate`

```haskell
class Bifurcate f where
  bifurcate :: f -> Either (CErr f) (CRes f)

type family CErr f   -- what the failure carries
type family CRes f   -- what the success carries

instance Bifurcate Bool where
  bifurcate = bool (Left ()) (Right ())

instance Bifurcate (Maybe a) where
  bifurcate = maybe (Left ()) Right

instance Bifurcate (Either e a) where
  bifurcate = id

instance Bifurcate (Validation e a) where
  bifurcate = validation Left Right
```

For these four, `bifurcate` is an isomorphism onto `Either (CErr f) (CRes f)`.
`CErr` and `CRes` are type families, so `CErr Bool = ()` and
`CErr (Either e a) = e`.

## Embedding into the monad

A `MonadError e m` gives us a way to embed `Either e` into `m`: the
fold `either throwError pure`. Precompose it with a translation of the
failure and with `bifurcate` and you get `collapse`:

```haskell
collapse :: (MonadError e m, Bifurcate f) => (CErr f -> e) -> f -> m (CRes f)
collapse toErr = either (throwError . toErr) pure . bifurcate
```

`toErr` is where each call site says what the failure means. The one
real operator is then just `collapse` applied with `>>=`:

```haskell
(??) :: (MonadError e m, Bifurcate a) => m a -> (CErr a -> e) -> m (CRes a)
action ?? toErr = action >>= collapse toErr
```

which allows code like

```haskell
-- fetchFromS3 permission path :: m (Either S3Error UserData)
userData <- fetchFromS3 permission path ?? \case
  BadCredentials -> http403
  WrongPath      -> http404 { body = path ++ " doesn't exist" }
```

No tarpit of repetitive control flow, yet each call site still decides
how its failure is reported. The failure information is simply passed
to `toErr`.

`Railroad` provides these for the `effectful` `Error` effect and
`Railroad.MonadError` for any `MonadError`.

## The other operators

```haskell
-- constant error: (?) is (??) with const
(?)   :: (MonadError e m, Bifurcate a) => m a -> e -> m (CRes a)
action ? err = action ?? const err

-- tag by a predicate; the Left carries the rejected value
(?>)  :: Functor m => m a -> (a -> Bool) -> m (Either a a)
action ?> predicate = (\x -> if predicate x then Right x else Left x) <$> action

-- recover instead of throwing; these never throw, and need only a Functor
(??~) :: (Functor m, Bifurcate a) => m a -> (CErr a -> CRes a) -> m (CRes a)
(?~)  :: (Functor m, Bifurcate a) => m a -> CRes a -> m (CRes a)

-- run an effect on the failure and pass the value through; never throws
(?<*) :: (Monad m, Bifurcate a) => m a -> (CErr a -> m ()) -> m a
```

`?>` does not throw by itself: it produces an `Either a a`, which is
`Bifurcate`, so a trailing `?` or `??` finishes it:
`action ?> isGood ?? toErr`.

`?<*` is for logging or counting a failure. A constant error such as
`? err503` throws the underlying failure away, so tap it first:
`runQuery q ?<* logDbError ? err503`. It returns the original value, so
every other operator still works after it.

All operators are `infixl 1`, like `>>=`, `<&>` and `&`, so they chain
left to right without parentheses.

### Layers

Each operator unwraps one layer. For `Either e (Maybe a)` the outer
`Either` is unwrapped first:

```haskell
pure (Right (Just 1)) ? "outer" ? "inner"   -- Either e (Maybe a) ~> Maybe a ~> a
```

This relies on the instances for traversables overlapping the base ones
(`Either e (Maybe a)` is also a `t (Maybe a)` with `t = Either e`). The
traversable instances are marked `INCOHERENT` so the base instance wins
and the outermost layer is the one unwrapped.

## Traversables

For a traversable `t` of one of the base types, `bifurcate` is

```haskell
bifurcate = bifurcate . sequenceA          -- for Maybe, Validation
bifurcate = sequenceA                      -- for Either (already an Either)
bifurcate = bifurcate . sequenceA . fmap bifurcate   -- for Bool, which is not an Applicative
```

`sequenceA` swaps the two layers, `t (f a) -> f (t a)`, using the
`Applicative` of `f`. That is why the semantics of each type carry over
unchanged:

- `Either e` is applicative by short-circuiting: the first `Left` wins.
- `Validation e` is applicative by accumulating failures with the
  `Semigroup` on `e`.

So `Validation` is *not* just `Either` under another name. The two are
isomorphic as types, but their `Applicative`s differ, and the `Semigroup`
constraint is a consequence of that, not the only difference. The two
instances therefore cannot be merged into one.

This is also not an isomorphism any more: `[Either e a] -> Either e [a]`
forgets which element failed and everything after the first failure. It
is a lossy projection, which is exactly what "all or nothing" means.

An empty traversable succeeds, since `sequenceA [] = pure []`. That is
vacuous truth, like `and []`, and the readme's
[gotcha](readme.md#gotcha-an-empty-collection-passes) shows how to
require at least one element with `?+`.

## Cardinality operators

Traversal gives all-or-nothing semantics. Sometimes the question is how
many elements there are, for example the rows of a database query. Three
operators bifurcate on the cardinality of a `Foldable`:

```haskell
-- non-empty is success; returns the collection
(?+) :: (MonadError e m, Foldable t) => m (t a) -> e -> m (t a)

-- exactly one is success; returns the element
(?!) :: (MonadError e m, Foldable t) => m (t a) -> (CardinalityError (t a) -> e) -> m a

-- empty is success; returns ()   (ASCII alias: ?@)
(?∅) :: (MonadError e m, Foldable t) => m (t a) -> (t a -> e) -> m ()
```

`?!` inspects `toList xs` and splits it into the cases none, one, many.
Its failure is a type that records which of the two failures happened
(it is isomorphic to `Maybe ta`):

```haskell
data CardinalityError ta = IsEmpty | TooMany ta

cardinalityErr :: e -> (ta -> e) -> CardinalityError ta -> e
cardinalityErr onEmpty onTooMany = \case
  IsEmpty    -> onEmpty
  TooMany xs -> onTooMany xs
```

`cardinalityErr` is the eliminator of `CardinalityError`, so in normal use
it is how you build the monad's error from either case.

## Fallbacks

`?|` and `?|<>` run the right action only when the left one fails, so the
sources may be different structures with the same success type:

```haskell
cache k ?| db k ?| legacy k ? NotFound
```

`?|` keeps the last error. `?|<>` combines all of them with `<>`, which
needs the sources to share an error type. Both are associative, and
neither throws by themselves. See the readme for details.

## The algebra

Read an `Either e a` as a decision with evidence on both sides, and the operators fall
into logical connectives:

| Symbol | Realisation |
|---|---|
| ⊤ / ⊥ with evidence | `bifurcate` to `Either (CErr f) (CRes f)` |
| ∧, first counterexample | `Either`'s monad, chained `?` |
| ∧, all counterexamples | `Validation` |
| ∨, last counterexample | `?\|` (binary only) |
| ∨, combined counterexamples | `?\|<>` (binary only) |
| ¬ | `neg` |
| ∀ | `?` on a collection |
| ∃ over elements | `?+`, `atLeastOne` |
| ∃ over successes | `?∃`, `anySucceeds` |
| ∃! over elements | `?!`, `exactlyOne` |
| ∃! over successes | `successes`, then `exactlyOne` |
| ¬∃ over elements | `?∅`, `noneAtAll` |
| ¬∃ over successes | `neg . anySucceeds` |

The connectives do not commute: `a ? e1 ? e2` and `a ? e2 ? e1` fail with different
errors, and a fallback chain tries its sources in order.

## Recap

- `Bifurcate` maps a wrapper to `Either (CErr f) (CRes f)`: an
  isomorphism for the base types, a lossy projection for traversables.
- `collapse` embeds that `Either` into a `MonadError`, translating the
  failure on the way; `??` and `?` are `collapse` applied with `>>=`.
- `?>`, `?~`, `??~`, `?<*` and the fallbacks never throw: they produce or
  consume `Bifurcate` values.
- `?+`, `?!` and `?∅` bifurcate on cardinality (none, one, many)
  instead of structure.
