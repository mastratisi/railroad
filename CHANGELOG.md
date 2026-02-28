# Revision history for railroad

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
