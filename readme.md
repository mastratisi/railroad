# railroad

**A terse Haskell railroad error handling DSL abstracting over error functors by catamorphism via `Either`.**

`railroad` is a tiny, opinionated library that brings **Railway Oriented Programming** (ROP) to Haskell.
It gives you a clean, linear DSL for collapsing error-carrying values
(`Maybe`, `Either`, `Validation`, `Bool`, and collections thereof)
directly into your `MonadError` (or `Effectful.Error`) context — no
noisy `case` expressions, no repeated `either`/`maybe` boilerplate.

[![Haskell](https://img.shields.io/badge/language-Haskell-%23612a7e.svg)](https://www.haskell.org/)
[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

---

## Why railroad?

Typical error handling in Haskell quickly turns into visual noise:

```haskell
do
  user <- case fetchUser uid of
    Nothing -> throwError UserNotFound
    Just u  -> pure u

  cfg <- case loadConfig path of
    Left e  -> throwError (toConfigError e)
    Right c -> pure c
```

`railroad` replaces all of that with a single, consistent operator
family that reads left-to-right like a railway track:
```haskell
do
  user <- fetchUser uid ? UserNotFound          -- Maybe / Either / Validation / Bool
  cfg  <- loadConfig path ?? toConfigError      -- custom error mapping
  items <- queryItems filters ?+ NoResultsFound -- cardinality guard
```


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




## Operators 

| Operator | Purpose                                       | Example                        |
|----------|-----------------------------------------------|--------------------------------|
| `??`     | Main collapse with custom error mapping       | `action ?? toMyError`          |
| `?`      | Collapse to constant error                    | `action ? MyError`             |
| `?>`     | Predicate guard                               | `val ?> isBad toErr`           |
| `??~`    | Recover with a mapped default (error → value) | `action ??~ toDefaultVal`      |
| `?~`     | Recover with a fixed default value            | `action ?~ defaultVal`         |
| `?+`     | Require non-empty collection                  | `items ?+ NoResults`           |
| `?!`     | Require exactly one element                   | `items ?! fromCardinalityErr`      |
| `?∅`     | Require empty collection                      | `duplicates ?∅ DuplicateFound` |

All operators work uniformly on:

- `Bool`
- `Maybe a`
- `Either e a`
- `Validation e a`
- Traversable containers of the above (`[Maybe a]`, `Vector (Either e a)`, …)


## Design Highlights

- **Bifurcate** typeclass turns any “railroad-shaped” functor into `Either (CErr f) (CRes f)` via catamorphism  
- Native support for **Effectful** (`Error` effect) and **MTL** (`MonadError`) via `Railroad.MonadError`  
- Short-circuiting for `Either`, error accumulation for `Validation`  

See [`railroad.md`](railroad.md) for a deeper tour.
