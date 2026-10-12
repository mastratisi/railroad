# Error handling in three styles: a change-impact experiment

This directory measures what happens to error-handling code when the code
around it changes. The same eleven handlers are written three ways, nine
realistic changes are applied to each way, and the cost of each change is
recorded.

The question: when a dependency changes its return type (`Maybe` becomes
`Either`, `[a]` becomes `Maybe a`, and so on) or a policy changes (a new error
code, a new fallback source), how many places must be edited, which breaks at
compile time, and does anything keep compiling while meaning something
different?

Nothing here is part of the `railroad` package. It uses the library's `src/`
directly, from the working tree it sits in.

## The three styles

All three styles implement the same behaviour. The tests enforce that.

| Style | File | Rules |
|---|---|---|
| railroad | `base/Rail.hs` | Operators and classifiers from `Railroad`. One local helper, `one`, for "database failure, then exactly one row". |
| case | `base/Case.hs` | Plain `case` and `either`-style pattern matching. No helpers at all, not even for the repeated "unwrap the database result" step. |
| helper | `base/Helper.hs` | `orThrow`, `expectOne` and `single`, plus ordinary `>>=`, `unless`, `when`. A helper is added only when a shape needs one. |

Fairness rules, which matter if you redo this:

- The `case` style is deliberately the explicit extreme. It is not what a
  careful programmer writes after the third copy.
- The helper style is the realistic middle: the helpers a team grows before
  they would reach for a library.
- Do not add a helper to one style to win a comparison. If a change needs a new
  helper in the helper style, count its lines as part of the edit.
- The domain types and the data layer (`Types.hs`, `Core.hs`) are shared and
  are not counted as edit cost. Only the three handler files are measured.

## Requirements

- GHC 9.10 (tested with 9.10.3) with these packages: `effectful`
  (`effectful-core`), `validation`, `QuickCheck`, `mtl`, `base`.
- Python 3.
- The `railroad` working tree must contain the classifiers and `?∃`:
  `neg`, `anySucceeds`, `successes`, `failures`, `exactlyOne`, `noneAtAll`,
  `atLeastOne`, `partitionBifurcate`, `?∃`. That is version 0.2.2.0 or later,
  not 0.2.1.0.

The driver runs its commands with `nix-shell --run` from the repository root,
which uses `shell.nix` there. Without Nix, run with `NO_NIX=1`; then `ghc` must
already see the packages above.

## Quick start

From the repository root:

```
python3 examples/error-handling-styles/changes.py S0      # harness check, about 15 seconds
python3 examples/error-handling-styles/changes.py         # all changes, about 2 minutes
python3 examples/error-handling-styles/summarise.py report_all.json
```

`changes.py` with ids runs only those changes (`changes.py S1 P3`) and writes
`report_<ids>.json`. With no ids it writes `report_all.json`. `results.json` is
a committed copy of a full run; `summarise.py` reads it by default.

Run `S0` first. It applies no change and compares every style against the
reference implementation. Every line must say `ok` (33 of 33). If it does not,
the harness is broken, and no other number means anything.

## The example

A small in-memory service, with no HTTP or SQL, so the whole thing runs inside
`effectful` in a pure test.

- `base/Core.hs`: plain domain types (`Err`, `User`, `Cap`, ...) and their
  `Arbitrary` instances.
- `base/Types.hs`: the `Db es` record, which holds the queries the handlers call
  and returns raw results as a Rel8 runner would; the `World` record, which is
  one random state of the world (what each query returns); `mkDb`, which turns
  a `World` into a `Db`; and `validateName`.
- `base/H.hs`: the `Handlers` record (one field per handler), the `Oracle`
  record, and `run`, which runs an `Eff` computation to
  `(Either Err a, [auditLog])`.
- `base/Style{Rail,Case,Helper}.hs`: build a `Handlers` from each style.
- `base/Main.hs`: the property runner.

The eleven handlers, and the library features the railroad version uses:

| Handler | What it does | Railroad features |
|---|---|---|
| `getUser` | by id, 404 / 500 on bad row counts | `?`, `?!`, `cardinalityErr` |
| `register` | email must not exist, then mint a token | `?∅`, `?` |
| `checkWrite` | permission query returns one row that is `True` | `?`, `?!` on `Bool` |
| `findUser` | cache, then by id, then by email | `?\|` across `Maybe` and `Either`, `??`, `exactlyOne` |
| `grantRead` | permission, target lookup, "must not already read it", insert | `?<*`, `neg`, `?!`, `??` |
| `uploadNames` | validate every name, report all problems | `??` over `[Validation]` |
| `prefsFor` | default when missing | `?~` |
| `displayName` | recover to a placeholder string | `??~` |
| `useQuota` | positive quota or 429 | `?>` |
| `canonical` | two normalisers, combine their errors | `?\|<>` |
| `shareWithMany` | non-empty list, per-email lookup, at least one resolves, report skipped | `?+`, `?∃`, `failures` |

Not exercised by the base handlers: `collapse`, the `?@` alias, `atLeastOne`,
`partitionBifurcate`, `anySucceeds` called directly, `tagBy` called directly.
`successes` appears only in the S2 fix. Add a handler if you want one covered.

## Procedure for one change

A change is defined in `changes.py` by a call to `change(...)`. Every change
is run in up to two stages.

### Stage 1, `naive`: only the data layer changes

Shape changes only. Edit `Types.hs` (and `Core.hs` if needed) so the new shape
is in place, and leave the three handler files exactly as they were. Then:

1. **Compile errors.** Each style module is compiled alone with
   `ghc -fno-code`. Errors are counted, and each error line is mapped to the
   top-level function it falls in. This gives "N compile errors in M
   functions". Zero means the style compiles unchanged.
2. **Behaviour.** The test program is built with
   `-fdefer-type-errors -Wno-deferred-type-errors`, so functions with compile
   errors still build and throw when called. It then runs every handler of
   every style against the oracle. Each (style, handler) gets one status:
   - `ok`: matches the oracle.
   - `ERR`: threw an exception, which here means a deferred compile error.
   - `DRIFT`: no exception, but the answer differs from the oracle. This is
     the important one: the code compiles and is wrong.

### Stage 2, `fixed`: apply the handler edits

Apply the edits to `Rail.hs`, `Case.hs` and `Helper.hs`, build normally, and
run the same comparison. Every property must be `ok`. Policy changes also run
`Unit.hs` checks (concrete worlds with known expected results). If anything is
not `ok` the edit is wrong or incomplete; fix the edit, not the oracle.

### Cost of the edit

For each style, `diff -U0` between the original handler file and the fixed one:

- **edit sites** = number of hunks (`@@` lines);
- **lines touched** = added plus removed lines.

An edit that changes one line counts as 1 site and 2 lines.

### The oracle

- **Shape changes.** The reference is `Old.Case`, the unchanged `case`
  implementation, run on the old-shaped version of the same world. Each change
  supplies `old={...}`, a mapping from the new `World` fields to the old ones
  (for example `wCache = either (const Nothing) Just (wCache w)`). Fields not
  mentioned map to themselves. `base/Old/` is generated from `Types.hs` and
  `Case.hs` by `ensure_old()`.
- **Policy changes.** The behaviour is new, so there is no old reference. The
  oracle is the variant's own `Case` module (the most explicit style), and the
  `Unit.hs` checks pin down the new behaviour with concrete expected values.

Results are compared as `show` strings of `(Either Err a, [auditLog])`, which
includes the audit log, so a missing or extra log line fails the comparison.

### Generators

The random `World` is tuned so the interesting paths are common. After
changing the generators, check the spread of outcomes: add `tabulate` to a
property, and confirm each handler shows its success case and each of its
error codes in a reasonable share. An early version produced a `grantRead`
success in 0.4% of runs, which would have hidden bugs; the current version
gives about 21%. If a path is rare, a handler can be wrong on it and every
test still passes.

## Results

From `results.json` (`python3 summarise.py`). Cells are edit sites / lines
touched.

| | Rail | Case | Helper | Change |
|---|---|---|---|---|
| S1 | 0 / 0 | 2 / 8 | 2 / 6 | `Maybe a` to `Either CacheError a` |
| S2 | 3 / 5 | 3 / 5 | 3 / 6 | `Maybe a` to `[Maybe a]` (shards, first hit wins) |
| S3 | 1 / 2 | 3 / 14 | 4 / 18 | `usersById`: `[User]` to `Maybe User` |
| S4 | 3 / 6 | 6 / 15 | 3 / 6 | `permitted`: `[Bool]` to `Bool` |
| S5 | 1 / 2 | 1 / 2 | 1 / 2 | `validateName`: `Validation` to `Either` |
| P1 | 4 / 10 | 8 / 16 | 9 / 19 | a database timeout becomes 504 |
| P2 | 3 / 6 | 3 / 12 | 4 / 7 | several grants, any granting row suffices |
| P3 | 1 / 2 | 1 / 2 | 1 / 2 | log every duplicate registration |
| P4 | 1 / 1 | 2 / 11 | 1 / 6 | add a third, legacy fallback source |
| **sum** | **17 / 34** | **29 / 85** | **28 / 72** | |

Naive stage, shape changes (compile errors / functions with errors):

| | Rail | Case | Helper |
|---|---|---|---|
| S1 | 0 / 0 | 4 / 2 | 3 / 2 |
| S2 | 4 / 2 | 4 / 2 | 3 / 2 |
| S3 | 2 / 2 | 5 / 3 | 4 / 3 |
| S4 | 3 / 2 | 3 / 2 | 3 / 2 |
| S5 | 0 / 0, **silent drift in `uploadNames`** | 2 / 1 | 2 / 1 |

Baseline sizes (code lines, excluding imports, comments and blanks):
Rail 43, Case 155, Helper 79.

What the numbers say:

- Compatible shape changes cost nothing in railroad. `?|` and `?~` depend only
  on the success type, so S1 needs no edit, and `?!` accepts any `Foldable`, so
  S3 needs one signature change in `one`.
- Incompatible shape changes fail to compile in all three styles, at the same
  functions. The compile-time safety is equal; the fix is smaller in railroad.
- Adding a source to a fallback chain (P4) is one line in railroad and a new
  nesting level in the `case` style.
- S5 is the counterexample. Changing `validateName` to return `Either` leaves
  `pure (map validateName names) ?? BadRequest` compiling, with the meaning
  changed from "report every problem" to "report the first". The `case` and
  helper styles stop compiling because they match on `Failure` and `Success`.
  Only the tests caught the drift in railroad.
- P1 needed one more edit in railroad than expected: `one` had a fully generic
  failure type, and mapping with `dbErr` fixes it to `DbError`.

## Adding a change

1. In `changes.py`, add a `change('X1', 'shape' | 'policy', 'title', ...)`.
2. Data layer: `types=lambda t: ...` edits `Types.hs` text; `core=lambda t: ...`
   edits `Core.hs`. Use the `sub(text, old, new, count)` helper, which asserts
   the old text exists and, with `count`, that it occurs exactly that many
   times.
3. Shape change: add `old={field: 'expression in terms of w'}`, the mapping from
   the new world to the old one. Policy change: add `unit=` with Haskell source
   for `Unit.hs` (it must define `units :: [(String, Handlers -> Bool)]`; see
   P1 to P4 for the pattern, including an `okW` world).
4. `edits={'Rail': f, 'Case': f, 'Helper': f}`: one text-to-text function per
   style, applying the fix.
5. Run `python3 changes.py X1`, then read the naive stage (for shape changes)
   and confirm the fixed stage is all `ok`.

Write the fix as a person would. Do not golf it, and do not leave a
now-redundant clause that changes nothing but passes the tests.

## Pitfalls already hit

- `open(p, 'w').write(f(open(p).read()))` truncates the file before it is read.
  Use the `rw()` helper.
- `Data.Validation` and `Test.QuickCheck` both export `Success` and `Failure`.
  `Types.hs` imports QuickCheck with `hiding (Success, Failure)`.
- A generator that is correct but rare in a path hides bugs; check the outcome
  spread (see Generators).
- If a data-layer edit leaves an unused generator (S4's `bools`), its type
  becomes ambiguous and `Types.hs` itself fails to compile. The symptom is
  "0 compile errors in the style modules", because the errors are not in them.
- Two oracle modules cannot each define their own `User` or `DbError`; that
  breaks the world mapping. The shared types live in `Core.hs` for that reason.
- A policy change that touches `Core.hs` (P1 changes `DbError`) also affects
  `Old.*`. That is fine because policy changes use the variant's own `Case` as
  oracle and never import `Old.*`.

## Limitations

- One author wrote all three styles and the library. Another author, or a
  different set of helpers, would move the numbers.
- Eleven handlers and nine changes is a small sample. Edit sites and lines are
  crude proxies for effort.
- "Zero edits" for S1 depends on the success type staying the same.
- The generator spread is tuned by hand; the outcome check is manual.
- Only GHC 9.10.3 was used.
- Detecting drift needs an oracle. Where a change is not a pure refactor, as in
  the policy changes, drift against intent is checked only by the `Unit.hs`
  cases.

## Files

```
changes.py        the nine changes and the measuring driver
summarise.py      prints the tables from a report
results.json      committed copy of a full run
base/             the example: types, three styles, harness, templates
var/              generated: one directory per change, naive/ and fixed/
report_*.json     generated by changes.py
```

`var/`, `base/Old/`, `base/Oracle.hs`, build output and `report_*.json` are
generated and ignored by git.
