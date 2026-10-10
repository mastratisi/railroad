# Revision history for railroad

## 0.1.1.4 -- 2026-10-10
* Widen dependency upper bounds (tests pass):
  - `effectful < 2.8`
  - `validation < 1.4`, excluding `1.3.0`, which does not build (it uses `foldable1-classes-compat` without declaring it)
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
