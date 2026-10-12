# Revision history for railroad

## 0.2.1.1 -- 2026-10-12
* Widen dependency upper bounds (tests pass):
  - `QuickCheck < 2.20`

## 0.2.1.0 -- 2026-10-11
* Add the tap operator `(?<*)`: runs an effect on the failure and passes the original value through
  unchanged, so a failure can be logged or counted before a constant error throws it away:
  `runQuery q ?<* logDbError ? err503`. The handler is a function of the failure (`CErr a -> m ()`)
  and runs only when there is one. It needs only `Monad`, never throws, and is defined once, in
  `Railroad.Bifurcate`, so `Railroad` and `Railroad.MonadError` re-export it.
* Documentation: say "unwrap" instead of "peel" for taking apart one layer of a result.

## 0.2.0.1 -- 2026-10-11
* Documentation: a README section on the empty-collection gotcha (`?` on a traversable checks that
  every element succeeds, so an empty one passes; chain `?+` before it), with the quantifiers each
  operator corresponds to, and a note in the haddocks of `Bifurcate` and `?+`. A test pins it.

## 0.2.0.0 -- 2026-10-11
* Breaking: `(?>)` no longer throws. `m ?> p` tags the value (`Right a` if `p a`, else `Left a`,
  carrying the rejected value) and is finished with `?` or `??`: `(m ?> p) toErr` becomes
  `m ?> p ?? toErr`. It needs only `Functor`, so `Railroad.MonadError` re-exports the one definition.
* Breaking: all operators are now `infixl 1` (they were `infixl 0`, and `?>` was `infixl 1`), like
  `>>=`, `<&>` and `&`. `runQuery q ? err <&> f`, `f $ m ? e` and `m ? e ?> p` now parse as they read.
  Code that compiled before keeps its meaning, except where an operator was mixed with another of
  a different level.
* New modules `Railroad.Bifurcate` (the class, `CErr`, `CRes`, and the operators that never throw:
  `?>`, `??~`, `?~`, `?|`, `?|<>`) and `Railroad.Cardinality` (`CardinalityError`, `cardinalityErr`).
  `Railroad` and `Railroad.MonadError` re-export both, so imports keep working, and all three
  modules now have explicit export lists. `Railroad.MonadError` no longer imports `Railroad`.
* Breaking: `(??~)` and `(?~)` are defined once, in `Railroad.Bifurcate`, with the more general type
  `Functor m => m a -> ...` (they were `Eff es a` in `Railroad`).
* `(?|)` and `(?|<>)` simplified internally; their behaviour is unchanged.

## 0.1.2.1 -- 2026-10-11
* Fix overlapping instances: `Either e (Maybe a)`, `Maybe (Maybe a)`, `Either e Bool` and the like
  did not compile when used with `?`, so nested layers could not be peeled (the README's first
  example did not compile). The outermost layer is now peeled first, as documented; lists and other
  traversables inside a layer behave as before. Tests added, including the README example.

## 0.1.2.0 -- 2026-10-11
* Add the fallback operator `(?|)`: runs the left action, and only if it fails runs the right one.
  The two sides can be different structures (`Maybe`, `Either`, ...) with the same success type, the
  last error is kept, and the result chains and is finished with `?` or `??`. It never throws, so
  `Railroad.MonadError` re-exports the one definition.
* Add `(?|<>)`, the same fallback but when every source fails the errors are combined with `<>`
  in source order. The sources must agree on the error type (`CErr`) and the success type (`CRes`).
* Documentation: a servant + effectful example and a section on the fallback operators in the README,
  clearer haddocks (module headers, `CErr`, `CRes`, each operator), and `readme.md` in `extra-doc-files`.
* Add `tested-with` (GHC 9.4.8 to 9.14.1, the versions the test suite is run on).
* Write the `validation` bound without a trailing zero or a strict lower bound:
  `(>= 1.1 && < 1.3) || (>= 1.3.1 && < 1.4)`, the same set of releases as in 0.1.1.5.

## 0.1.1.5 -- 2026-10-11
* Exclude `validation-1.3.0` from the bounds: it does not build (it uses `foldable1-classes-compat`
  without declaring it). `validation >= 1.3.1` needs `base >= 4.18`, so on GHC 9.4 the solver picked
  1.3.0 under 0.1.1.4's `validation < 1.4` and the build failed.

## 0.1.1.4 -- 2026-10-10
* Widen dependency upper bounds (tests pass):
  - `effectful < 2.8`
  - `validation < 1.4`
* Stop using `Data.Validation.toEither`, which `validation-1.3` no longer exports.
* Declare `hspec-discover` as a `build-tool-depends` of the test suite, so the tests build outside the nix shell.

## 0.1.1.3 -- 2026-10-10
* Documentation fixes: correct "success" typo and the doc comment of `(?)` in `Railroad.MonadError`.

## 0.1.1.2 -- 2026-04-17
* Remove strictness annotation from `TooMany` constructor in `CardinalityError`.

## 0.1.1.1 -- 2026-02-28
* Widen dependency bounds for better GHC compatibility:
  - `base` → `>= 4.17 && < 4.23` (supports GHC 9.4 through 9.14)
  - `effectful` → `>= 2.5 && < 2.7` (allows current 2.6.x series)
  - `mtl` lower bound relaxed to `>= 2.2`

## 0.1.1.0 -- 2026-02-26
* Add non unicode alias for `(?∅)`
* Fix the fixity of `(??~)` to be inline with `(?~)`

## 0.1.0.1 -- 2026-02-26

* Fix that `Railroad.MonadError` is not exposed in the cabal file
* Fix ambiguous occurrence of `(??~)` in `Railroad.MonadError` by hiding it in import.
* Add tests for the MonadError version of the operators such that issues like the above will be caught 

## 0.1.0.0 -- 2026-02-25

* First version. Released on an unsuspecting world.
