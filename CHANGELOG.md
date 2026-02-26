# Revision history for railroad

## 0.1.0.1 -- 2026-02-26

* Fix that `Railroad.MonadError` is not exposed in the cabal file
* Fix ambiguous occurrence of `(??~)` in `Railroad.MonadError` by hiding it in import.
* Add tests for the MonadError version of the operators such that issues like the above will be caught 

## 0.1.0.0 -- 2026-02-25

* First version. Released on an unsuspecting world.
