# Railroad exercises

Learn the `railroad` combinators by translating plain `case` code into them.

Every exercise gives you a function written with `case` that is correct, and an empty stub. You
write the stub with the railroad operators so that it behaves exactly like the `case` version. A
test feeds both functions the same random inputs and tells you the first input where they differ.

```haskell
-- given:
ref00 q = do
  r <- q
  case r of
    Left _  -> throwError Unavailable
    Right u -> pure u

-- you write:
ex00 q = q ? Unavailable
```

There are 38 exercises in 13 lessons, plus one worked example.

## Running

From this directory:

```
cabal test exercises                                     # all of them
cabal test exercises --test-options="--match 05"         # only exercise 05
```

With Nix, from this directory: `nix-shell ../../shell.nix --run 'cabal test exercises --offline'`.

The first run builds the library from `../..`, so it takes a little while. You need GHC with `mtl`,
`validation`, `QuickCheck` and `hspec` (tested with GHC 9.10.3).

Edit only the `exNN` definitions in `src/Exercises.hs`. A stub says `todo` (it fails with "TODO")
or has a `_` hole (it fails until you fill it). Everything else is already in place.

## How to work through it

1. **Read the lesson's note first**, at the top of its section in `src/Exercises.hs`. It is a few
   lines.
2. **Study the worked example** (exercise 00), and read each `refNN` before you write `exNN`.
3. **Try before you look anything up.** Run the test and read the counterexample; it is usually the
   hint you need. If you are stuck, `HINTS.md` has three hints per exercise, vaguest first. Read
   one, try again, and only then the next.
4. **Move on when a lesson is green without hints.** If a lesson needed hint 3 twice, redo its
   first exercise from scratch tomorrow instead of pushing on.
5. **Do the reviews.** Lessons 5 and 11 mix what came before and do not say which operator to use.
   Choosing is part of the skill.
6. **Do not use `case`, `either`, `maybe` or `if`** to take a result apart. The tests cannot stop
   you; the point is to let the operators do it. A function that maps a failure, like the
   `\case` in exercise 20, is fine.

## The lessons

| Lesson | Skill | Operators | Exercises | Kind |
|---|---|---|---|---|
| 1 | Unwrap, or throw a constant | `?` | 00 to 04 | 00 worked, 01 faded, 02 to 04 open |
| 2 | Layers, unwrapped left to right | `?` chained | 05 to 07 | open |
| 3 | The error depends on what failed | `??` | 08 to 10 | open |
| 4 | How many rows? | `?!` `?+` `?∅` `cardinalityErr` | 11 to 14 | open |
| 5 | Review A | lessons 1 to 4, mixed | 15 to 17 | review |
| 6 | Recover instead of throwing | `?~` `??~` | 18 to 20 | open |
| 7 | Look at the failure first | `?<*` | 21 to 22 | open |
| 8 | A whole list of results, and a trap | `?` on lists, `?+` | 23 to 25 | open |
| 9 | `Validation` collects every problem | `??` on `Validation` | 26 to 27 | open |
| 10 | Try one source, then the next | `?\|` `?\|<>` | 28 to 30 | open |
| 11 | Review B | lessons 1 to 10, mixed | 31 to 33 | review |
| 12 | Classify first, then act | `neg` `?∃` `exactlyOne` `<&>` | 34 to 36 | open |
| 13 | Capstones: whole handlers | everything | 37 to 38 | open |

The order is deliberate:

- One new operator per exercise until the reviews.
- A worked example, then one with a hole, then open ones, so the support shrinks as you go.
- `?` and `?+` are introduced in different lessons because they are easy to mix up. Exercise 25
  is where they meet; it is built so the obvious answer fails.
- Exercises 26 and 27 have the same solution and different behaviour. Predict before you run.

## What the tests check

Each test runs your function and the reference on random inputs, in a monad that can throw an
error and also keeps a log. They are compared on **both** the result and the log. That is how the
tests know a step ran when it should not have: exercise 04 (the second step must not run after the
first fails), 28 to 30 (a later source must not run after an earlier one answered), 21 and 22 (a
log line only on failure).

The generators are weighted so that the interesting cases (empty lists, one row, many rows) come up
often. A failing test shows the smallest input it found.

## The answer key

`solutions/Solutions.hs` has one solution for every exercise. Do not open it before you have tried.

```
cabal test check-solutions
```

runs the same tests against the key. It must pass: it shows that every exercise is solvable and
that every `case` reference behaves like its railroad solution. If you change a reference or add
an exercise, this is the check that you did not break the set.

Mutation check: breaking a solution on purpose (swapping two sources, an off-by-one in a limit,
dropping a log line, forgetting that no rows means no access) makes exactly the corresponding test
fail. Twelve such mutations were tried when this set was written.

## Adding an exercise

1. In `src/Exercises.hs`, add `refNN` (correct, plain `case`) and a stub `exNN = todo`, with a
   short task comment above them.
2. In `solutions/Solutions.hs`, add `exNN`.
3. In `test/Spec.hs`, add an `it` that compares `X.refNN` with `A.exNN` on random inputs. Use
   `act "name" value` for any action whose running order matters; it records that it ran.
4. Add its hints to `HINTS.md`.
5. Run `cabal test check-solutions` (must pass) and `cabal test exercises` (must fail for the new
   stub), and break the solution once to confirm the test notices.

Keep the rule that one exercise teaches one new thing, and that a review exercise does not name
the operator it needs.

## Not covered

The operators are all here except the ASCII alias `?@` (a spelling of `?∅`). The classifiers
`tagBy`, `atLeastOne`, `noneAtAll`, `successes`, `failures`, `anySucceeds` and
`partitionBifurcate`, and `collapse`, have no exercise of their own; some are used inside the
operators the exercises teach.

The exercises run in a pure monad and use `Railroad.MonadError`. The same operators exist for an
`effectful` `Error` effect in `Railroad`; the operators and their behaviour are the same.
