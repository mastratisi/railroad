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

Typical error handling in Haskell quickly turns into a tarpit of
accidental controlflow:

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
family that reads linearly:
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

See [`railroad.md`](railroad.md) for a deeper tour, or just read the
code at [`Railroad.hs`](src/Railroad.hs). It's a short file.


## Comparison with `hoist-error`

Both [`hoist-error`](https://hackage.haskell.org/package/hoist-error)
and **Railroad** are small, focused Haskell libraries that make error
handling in MonadError (or Error effects) far more pleasant than raw
case expressions, either, or maybe. They let you collapse partiality
structures (Maybe, Either, etc.) into a linear monadic flow unpacking
values while still controlling how errors are turned into your
application’s error type.


They solve the same core pain point—**“railroad-style” error handling** 
where the happy path stays on the main track and errors
branch off cleanly—but they do it with different levels of abstraction
and power.


### Quick Feature Comparison

| Feature                 | hoist-error                             | Railroad                                                                         |
|-------------------------|-----------------------------------------|----------------------------------------------------------------------------------|
| **Core mechanism**      | `PluckError` class + `hoistError`       | `Bifurcate` type family + catamorphism to `Either`                               |
| **Supported functors**  | `Maybe`, `Either e`, `ExceptT e m`      | `Bool`, `Maybe`, `Either`, `Validation`, traversables of any of the above        |
| **Collection handling** | None (manual)                           | Built-in cardinality: `?+` (non-empty), `?!` (exactly one), `?∅` (must be empty) |
| **Error mapping**       | Function or constant (`<%?>`, `<?>`)    | Function (`??`) or constant (`?`) with typed `CErr` constructors                 |
| **Recovery / defaults** | No built-in                             | `?~` / `??~` (constant or computed fallback)                                     |
| **Guards**              | No                                      | `?>` (predicate → error)                                                         |
| **Monadic hoisting**    | `<%!?>`, `hoistErrorM`                  | Handled naturally by `>>=` + `??`                                                |
| **Primary ecosystem**   | Any `MonadError` / `ExceptT` / `mtl`    | `Effectful` `Error` (with full `MonadError` re-export)                           |
| **Dependencies**        | Minimal (`base` + `mtl`/`transformers`) | `effectful`, `validation`, `mtl`                                                 |
| **Learning curve**      | Very low (just a few operators)         | Slightly higher (8 operators, but `??` and `?` cover ~90%)                       |
| **Size**                | ~150 LOC                                | ~150 LOC                                                                         |

### Side-by-side example
```haskell
-- hoist-error (4 operators total)
user <- fetchUser uid <%?> UserNotFound
user <- fetchUser uid <?> UserNotFound
```

```haskell
-- Railroad (8 operators, but 2 cover 90 % of code)
user <- fetchUser uid ? UserNotFound
user <- fetchUser uid ?? toAppError
age  <- getAge ?> (> 0) $ NegativeAge
users <- queryUsers ?+ NoUsersFound
single <- queryOne ?! WrongCount
```

### When to choose which?

**Choose `hoist-error`** if you want:  
- The absolute smallest API and dependency footprint  
- Only need simple `Maybe`/`Either`/`ExceptT` hoisting  
- Classic `mtl` or `transformers` codebases  

**Choose Railroad** if you want:  
- A full **DSL** that feels built into the language  
- Excellent `Validation` support (error accumulation)  
- Frequent collection checks or predicate guards  
- Maximum readability in `Effectful` applications  
