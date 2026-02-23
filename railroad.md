# Terse railroad error handling DSL abstracting over error types by catamorphism to either

Check out how terse my Servant http handler is:

```haskell
serveUserAPI :: ServerT UserAPI (Eff UserStack)
serveUserAPI = registerStart :<|> registerComplete :<|> getUserById
 where
   registerStart :: EmailAddress -> Eff UserStack Text
   registerStart email = do
     time <- getTime
     runQuery (userByEmail time email) ? err503 ∅? const err409
     makeJWT email (Just time) ? err500
   registerComplete :: RegisterDTO -> Eff UserStack ()
   registerComplete RegisterDTO {..} = do
     email <- verifyJWT jwtToken ? err400
     ph <- hashPassword password
     time <- getTime
     uid <- Id @User <$> genUUID
     runLockedWrite (registerUser email name ph time uid) ? err500
   getUserById :: Id User -> Eff UserStack UserDTO
   getUserById uid = do
     time <- getTime
     user <- runQuery (userById time uid) ? err503 ?! cardinalityErr err404 (const err500)
     pure $ toUserDTO user
```
The underlying idea is that most error handling happens in a
`MonadError` like context (here an effect stack with the Error effect),
and is very repetitive: We have some function that returns what we
care about wrapped in some functor like `Maybe a`. These functor's state
space is bifurcated into a space representing errors (e.g. `Nothing`)
and another representing results (e.g. `Just a`). We therefore need to
unwrap the function's return by case analysis (explicitly or
implicitly) where in the error case we need to map/interpret the error
into our `MonadError`'s error and throw it.

This can be done very tersely with a single operator `(??)` for nearly every
error functor if we just fix what part of the functor's space counts
as error and notice that `Bool`, `Maybe`, `Validation`, and traversables of
those functors, morally are just specializations of `Either`. Their
notable differences, that we need to abstract over, is the information
they carry with them, i.e. the structure of their bifurcated spaces.


| functor        | Error Constructor | Error Info | Result Info |
|----------------|-------------------|------------|-------------|
| Bool           | False             | ()         | ()          |
| Maybe a        | Nothing           | ()         | a           |
| Either e a     | Left              | e          | a           |
| Validation e a | Failure           | e          | a           |
| [Either e a]   | --                | e          | [a]         |

To restate, we need to bifurcate by error/result and inject into our
monad that supports `MonadError`.

That all these functors are Either-like means that the bifurcation can
happen by catamorphism into `Either`. Note the class, skim the instances:
```haskell
class Bifurcate f where
  bifurcate :: f -> Either (CErr f) (CRes f)
instance Bifurcate Bool where
  -- bool   :: b -> b -> Bool -> b
  bifurcate = bool (Left ()) (Right ())
instance Bifurcate (Maybe a) where
  -- maybe  :: b -> (a -> b) -> Maybe a -> b
  bifurcate = maybe (Left ()) Right
instance Bifurcate (Either e a) where
  -- either :: (e -> b) -> (a -> b) -> Either e a -> b
  bifurcate = id
```

`CErr`, `CRes` are type families defined by the table above. So 
`CErr Bool = ()` and `CErr (Either e a) = e`.

We can then inject into our monad by what I call our collapse combinator:
```haskell
collapse :: (MonadError e m, Bifurcate f) => (CErr f -> e) -> f -> m (CRes f)
collapse toErr = either (throwError . toErr) pure . bifurcate
```

It interprets what the error space means with the toErr function, 
which we will use in our operator (??).

```haskell
(??) :: (MonadError e m, Bifurcate a) => m a -> (CErr a -> e) -> m (CRes a)
action ?? toErr = action >>= collapse toErr
```haskell

this allows code like:

```haskell
-- fetchFromS3 permission path :: Either S3Error UserData
userData <- fetchFromS3 permission path ?? \case ->  
    BadCredentials -> http403
    Wrong path -> http404 {body = path ++ " doesn't exist"}
```

i.e. no tarpit of repetitive controlflow that can go wrong while still
allowing per callsite interpretation with informative messages; the
error type of the functor is just threaded into the toErr function
given. So for Maybe and Bool it would just be unit.

I've also added operators like:
```haskell
-- | Collapses a structure using a constant error.
(?) :: (MonadError e m, Bifurcate a) => m a -> e -> m (CRes a)
action ? err = action ?? const err

-- | Collapses any value based on a predicate
(?>) :: (MonadError e m) => m a -> (a -> Bool) -> (a -> e) -> m a
(?>) action predicate toErr = do
  val <- action
  if predicate val then pure val else throwError_ $ toErr val

-- | Collapses a structure, and recovers to a default value in the error case
(?~) :: (MonadError e m, Bifurcate a) => m a -> CRes a -> m (CRes a)
action ?~ defaultVal = action >>= either (const (pure defaultVal)) pure . bifurcate
```

What's left to explain is how I handle the abstraction over
traversables of `Either` and `Validation`. What is nice about their
instances is their semantics. Traversables of `Either` short circuts on
the first error, while traversables of `Validation` accumulates the
errors.

It's also a nice showcase of how easy it is to extend what my Railroad
module abstracts over. I only need to add two Bifurcate instances: 
```haskell
instance Traversable t => Bifurcate (t (Either e a)) where 
  bifurcate = sequenceA 
  -- pedagogically: bifurcate = bifurcate . sequenceA instance
(Traversable t, Semigroup e) => Bifurcate (t (Validation e a)) where
  bifurcate = toEither . sequenceA 
  -- pedagogically: bifurcate = bifurcate . sequenceA
```

If not for the `Semigroup` constraint (necessary for `Validation`'s
`sequenceA` to accumulate errors), both could have been implemented
with the same instance.


## Extra: Cardinality operators that play nicely with libraries such as Hasql/Rel8

`(??)` on traversables work nicely when we want all or nothing error
semantics. Sadly life can sometimes be more complicated. I have
therefore added a family of 3 operators that morally bifurcates on the
cardinality of traversables (actually foldable), each with their own
interpretation of what cardinalities are error state:

```Haskell
-- | Non-empty is success state. Returns the collection as-is.
(?+) :: (MonadError e m, Foldable t)
     => m (t a) -> e -> m (t a)
(?+) action err = do
  xs <- action
  if null xs then throwError err else pure xs


-- | Single element is success state. Returns the single element
(?!) :: (MonadError e m, Foldable t)
     => m (t a) -> (CardinalityError (t a) -> e) -> m a
(?!) action toErr = do
  xs <- action
  case toList xs of
    []  -> throwError $ toErr IsEmpty
    [x] -> pure x
    _   -> throwError $ toErr $ TooMany xs


-- | Empty is success state. Returns Unit.
(?∅) :: (MonadError e m, Foldable t)
     => m (t a) -> ((t a) -> e) -> m ()
(?∅) action toErr = do
  xs <- action
  if null xs then pure () else throwError (toErr xs)
```

To support the `(?!)` operator I have created a Maybe-like type, with
a convenience catamorphism that in normal usage is a catamorphism to
the error type of the monad `(?!)` is working in

```haskell
data CardinalityError ta = IsEmpty | TooMany !ta

-- | Catamorphism for CardinalityError
cardinalityErr :: e -> (ta -> e) -> CardinalityError ta -> e
cardinalityErr onEmpty onTooMany = \case
  IsEmpty -> onEmpty
  TooMany xs -> onTooMany xs
```


