# Do the combinators hold up?

An assessment of the `railroad` combinators after the change-impact experiment
in this folder. The method and raw numbers are in `README.md` and
`results.json`; this document is about what they mean for the library.

## Verdict

The design holds up. Nothing in the experiment called for removing or
redefining an operator. The evidence supports three small changes and one
warning, listed under "What to change".

The strongest result is that the pipeline form makes each change local to one
stage of a chain. The strongest counter-result is that the operators' meaning
depends on the element type, and in one case a refactor kept compiling with a
different meaning.

## What the experiment shows

Eleven handlers were written in three styles (railroad, plain `case`, and
`orThrow`/`expectOne`/`single` helpers) and checked equal against each other and
against an oracle on thousands of random worlds. Nine changes were then applied
to each. Totals, as edit sites / lines touched:

| | railroad | `case` | helpers |
|---|---|---|---|
| nine changes | 17 / 34 | 29 / 85 | 28 / 72 |

Two things in this table are worth separating.

**The operators denote what they claim.** All eleven railroad handlers match
the explicit `case` versions on every sampled world, including the audit log.
That covers `?`, `??`, `?!`, `?∅`, `?∃`, `?~`, `??~`, `?>`, `?|`, `?|<>`, `?<*`
and `neg`. So the equations the documentation gives for them are what the code
does. Before this, the tests checked each operator against its own definition,
which is a weaker statement.

**The cost advantage is real but not uniform.** It is large where the change
is local to one stage of a chain, and small where the change cuts across every
call site.

| Kind of change | Examples | railroad vs the others |
|---|---|---|
| Compatible shape change | S1 (`Maybe` to `Either`), S3 (`[User]` to `Maybe User`) | 0 and 1 edit sites, against 2 to 4 |
| Incompatible shape change | S2, S4 | same functions fail to compile in all three; the fix is shorter in railroad |
| Local policy change | P3 (log), P4 (add a source) | P3 is one line in all three styles; P4 is one line, against 6 lines (helpers) and 11 (`case`) |
| Cross-cutting policy change | P1 (504 for timeouts) | 4 sites, against 8 and 9 |

## Why the pipeline form helps

Each of the local changes edits one stage of a chain and leaves the others
alone:

- P3 inserts one verb, `?<*`, between the classification and the throw.
- P4 appends one source, `?| one (legacyByEmail db e)`.
- S4 deletes one stage, the `?! cardinalityErr ...` clause, because the query
  now returns a single `Bool`.

In the `case` style the same changes restructure nested branches: P4 adds a
nesting level, S4 removes a pair of `case` blocks per call site. The
classifier-then-verb model describes these edits accurately. A change was
always "add, remove or replace one classifier, or one verb".

Compatible shape changes cost nothing because the operators depend on the
*success type* (`CRes`), not on the wrapper. `?|` and `?~` accept `Maybe` or
`Either` interchangeably, and `?!` accepts any `Foldable`. This is the
universality over `Bifurcate` and `Foldable` paying off, and it should be
kept.

## Where the design is weaker

### 1. One operator, two meanings (S5)

When `validateName` changed from `Validation` to `Either`, this kept compiling:

```haskell
uploadNames names = pure (map validateName names) ?? BadRequest
```

and its meaning changed from "report every problem" to "report the first
failing name". The `case` and helper versions stopped compiling, because they
match on `Failure` and `Success`. Only the tests caught the railroad version.

The cause is in the algebra. There are two conjunctions: stop at the first
counterexample (`Either`) and collect all of them (`Validation`). `?` over a
collection picks one by the element type. That is convenient, and it is also
the one place where changing a type silently changes behaviour. Every other
shape change in the experiment either compiled with the same meaning or failed
to compile.

This is not a defect in the operators, since the semantics are documented and
tested (`BifurcateSpec` has one law for each). It is a hazard at the use site.

### 2. Cross-cutting policy has no home (P1)

P1 maps database timeouts to 504 and other database failures to 503. In
railroad it meant replacing every `? Unavailable` with `?? dbErr`, four sites,
plus one signature, because `one` had a fully generic failure type. The `case`
and helper styles each had eight sites. The library offers nothing for "the
database layer maps its failures this way everywhere". The constant operator
`?` makes the policy a literal at each call site, so changing it means visiting
every one.

### 3. Sugar does not accept an inserted stage

`?∅ f` is shorthand for `<&> noneAtAll ?? f`. To add the P3 log, the shorthand
had to be written out as `<&> noneAtAll ?<* log ?? f`. The edit was still one
line, but the shorthand is a dead end when a stage has to go in the middle. The
same applies to `?+`, `?!` and `?∃`.

### 4. A recurring local helper

Both the fallback chain in `findUser` and the per-email lookup in
`shareWithMany` needed `one q = q ? Unavailable <&> exactlyOne`. A fallback
over classified sources needs each source classified, and the classifier is
applied with `<&>`, which has the same precedence as `?|`, so a helper (or
parentheses) is needed. Every codebase that uses `?|` with `?!`-style sources
will write this helper.

## What to change

Ordered by how directly the evidence supports it.

### Add `firstSuccess` (evidence: S2, twice)

The railroad fix for `Maybe a` to `[Maybe a]` was `<&> listToMaybe . successes`
at two sites, once for the cache and once for the preferences. The `case` and
helper versions used `listToMaybe (catMaybes ...)`. It is the natural reading
of "first source that answered", and it is a classifier:

```haskell
firstSuccess :: (Foldable t, Bifurcate a) => t a -> Either [CErr a] (CRes a)
firstSuccess xs = case partitionBifurcate xs of
  (_, r : _) -> Right r
  (es, [])   -> Left es
```

It returns the failures when nothing succeeded, so it composes with `?` and
`?|` directly, and the S2 edit becomes `<&> firstSuccess`. It is lazy in the
same way `anySucceeds` is. It belongs in `Railroad.Classify` next to
`anySucceeds`.

### Document the element-type rule, and add a regression test (evidence: S5)

State in the Haddock for `?` and `Bifurcate` that conjunction over a collection
is chosen by the element type: `Either` stops at the first failure,
`Validation` collects them all, and that changing the element type changes
which. Add the S5 case to the test suite as a named example: two failing names
through `map validateName` with `Either` and with `Validation`, asserting that
the error lists differ. The library already tests each semantics, but nothing
tells a reader that they are two settings of the same operator.

No type-level guard is proposed. A guard would need either a separate operator
for accumulating or a wrapper type, and the experiment has one instance of the
hazard.

### Document the named-policy idiom (evidence: P1)

`?? dbErr` is the same length as `? Unavailable`, and `dbErr` is one definition.
Written that way from the start, the P1 change is an edit to `dbErr` and
nothing at the call sites; retrofitting it cost four sites. The readme can say:
where the error depends, or may come to depend, on what failed, pass a named
function to `??` instead of a literal to `?`. This needs no library change, only
a paragraph and an example.

### Consider an n-ary fallback (evidence: none yet)

`anyOf :: (Foldable t, Monad m, Bifurcate a) => t (m a) -> m (Either [CErr a] (CRes a))`
would run actions in order and stop at the first success. Nothing in the
experiment needed it: the sources were fixed, so chained `?|` was enough. Add
it when a caller has a dynamic list of sources.

## What to leave alone

- **`?`/`??` over collections reading as ∀, with the empty collection passing.**
  The experiment used `?!` for every permission check, so the empty-list
  hazard did not occur, but the homomorphism law that justifies it still holds.
- **The fused sugar (`?+`, `?!`, `?∅`, `?∃`).** They shorten the common case,
  and the classifier form is there for the rest.
- **`INCOHERENT` instances.** No handler was generic in the container, so the
  experiment neither exercised nor contradicted that decision.
- **The set of operators.** `collapse`, `?@` and `atLeastOne` were not used by
  the example, but a sample of eleven handlers is not grounds to remove them.

## What the experiment did not test

- **Adding a new `Bifurcate` type** (for example a `These`). The closed set of
  shapes was never stressed.
- **Generic code over the container**, where the incoherent-instance behaviour
  would matter.
- **Real servant, Rel8 or `effectful` stacks.** The `Db` record stands in for
  them.
- **Other authors.** One person wrote the library and all three styles.
- **Long-term effects:** whether the pipeline form stays readable as a handler
  grows past a few stages.

## How to read the numbers

Edit sites and lines touched are proxies for the work of a change. They do not
measure how long the change takes to understand, which is the cost the blog
post argues for. The `case` style in this experiment is also the extreme, and a
team would have shared its repeated blocks into helpers well before the ninth
change. The helper column is the closer comparison, and railroad is ahead of
it by a factor of about 1.6 on edit sites and 2.1 on lines.

# Qualitative assessment: how an expert would read it

The voice here is a senior Haskell engineer who reviews libraries for a living
and cares about abstraction barriers, composition and what a team can maintain.
The judgements are mine; the evidence is the code in `base/` and the numbers
above.

## One snapshot, read statically

The same handler, `findUser`: try the cache, then the id, then the email, and
map the final failure to 404 or 500.

**railroad**

```haskell
findUser db i e =
  cacheLookup db i
    ?| one (usersById db i)
    ?| one (usersByEmail db e)
    ?? cardinalityErr NotFound (const Internal)
```

**helpers**

```haskell
findUser db i e = do
  cached <- cacheLookup db i
  case cached of
    Just u  -> pure u
    Nothing -> do
      byId <- single <$> orThrow Unavailable (usersById db i)
      case byId of
        Just u  -> pure u
        Nothing -> orThrow Unavailable (usersByEmail db e) >>= expectOne NotFound Internal
```

**case**: about 20 lines, three nested `case` blocks and an `either`-shaped
unwrap inside each.

### railroad

The reviewer would say the code has the shape of the problem. The requirement
is "prefer the cache, then the id, then the email, else 404", and the code is
five lines in that order. The reader sees the policy and nothing else.

This is the strong point, and it has a name: the handler sits behind an
abstraction barrier. It speaks in decisions (sources in priority order, which
error to raise), and the representation of each answer (`Maybe`, `Either`,
a list) is below the barrier. `?|` joins a `Maybe` source and two `Either`
sources without the handler saying so.

The reviewer would then ask what the line costs to read.

- **Meaning is selected by the type to the left.** `?` unwraps a `Bool`, a
  `Maybe`, an `Either`, or a whole collection depending on what comes before it.
  To know what `... ? Internal ?! f ? Forbidden` does, you simulate the types
  through the chain. The `grantRead` lines have five operators in one
  expression. A newcomer must know the table before the code reads.
- **The vocabulary is larger than the library needs to be.** The example
  handlers use twelve distinct operators. Four of them (`?`, `??`, `?!`, `?∅`)
  account for 72% of the uses. A reader can learn four and skim the rest, but
  they must still recognise the other eight on sight.
- **Repetition the vocabulary should absorb.** `?! cardinalityErr Internal
  (const Internal)` appears three times word for word. The library already has
  the shorter form, `<&> exactlyOne ? Internal`, and the example did not use it.
  That is a documentation problem as much as a style one.
- **Types are opaque in signatures.** `CErr` and `CRes` are type families; hover
  text and error messages mention them, and "couldn't match `CRes a0`" is
  harder to act on than "couldn't match `Maybe` with `Either`".

### case

The reviewer's verdict: it is the clearest code to *debug* and the worst to
*read*. Nothing is hidden. Every branch is on the page, a stack trace points at
a line you can understand, and a new hire needs no vocabulary.

The price is vigilance. Every one of the fourteen `Left _ -> throwError ...` blocks
has to be checked for the right code, every `_ ->` wildcard silently absorbs
whatever case comes next, and the decision (which error for which condition) is
spread through the unwrapping. The `Unavailable` block alone is written eight
times, with eight chances to diverge. This is accidental control in the
Tarpit sense, in its purest form.

### helpers

The reviewer would call it the honest middle. `orThrow`, `expectOne` and
`single` are first-order functions with obvious types, and nothing is
overloaded. The cost is shape: prefix application with parentheses reads
inside-out (`orThrow Unavailable (usersById db i)` puts the error before the
query), and the split between `expectOne` (throws) and `single` (does not) is
exactly the pure/effectful boundary the library names explicitly. The
helper style rediscovers part of the algebra without its composition.

## Under change

The reviewer's frame: a design is good if the cost of a change is proportional
to the size of the change.

**Locality.** In railroad, a change is almost always the edit of one stage of a
chain: add a source, drop an assertion, insert a log. In the other two styles
the same change reshapes nested control flow. P4 is the cleanest illustration:
one added line in railroad, a deeper nesting level in `case`. The pipeline form
earns its keep here.

**The barrier holds for compatible changes.** `Maybe` to `Either` (S1) and
`[User]` to `Maybe User` (S3) need little or nothing, because the handlers never
named the wrapper. In the `case` style, the representation leaks into every
handler as a constructor pattern, so every representation change touches every
handler. A reviewer would call this the central architectural win: it is the
difference between data abstraction and pattern-matching on the representation.

**Loudness.** For incompatible changes all three styles fail to compile in the
same places, so the type checker protects them equally. A reviewer would not
mark railroad down here.

**The barrier leaks the other way (S5).** The same property that makes
`Maybe` to `Either` free makes `Validation` to `Either` silent. An operator
whose behaviour depends on the instance chosen by the type is overloading, and
overloading has a known failure mode: the code keeps compiling and means
something else. The Haskell standard library has the classic example
(`length (1, 2) == 1`, and `sum (Just 3)`); `?` over a collection is a smaller
cousin. The reviewer would say this is the price of the convenience, that it is
documented and tested, and that it still belongs in the "cautions" section of
the readme, not the footnotes.

**Cross-cutting policy is not served (P1).** The library composes well along
the chain and poorly across handlers. A project-wide rule ("timeouts are 504")
has no place to live except a named function passed to `??`. That is fine, but
it is a convention the library does not state.

## Where railroad is not elegant

An honest list, from the same reviewer:

1. **The type machinery is heavier than the problem.** Closed type families for
   `CErr`/`CRes`, a class with `INCOHERENT` instances to make outer-first
   layering work, and a closed set of supported types. It works, it is tested
   at concrete types, and it behaves differently in container-generic code.
   For a library whose job is to make code short, that is a lot of cleverness
   behind the barrier. The reviewer would want the clever part small, isolated
   and explained, and mostly it is.
2. **Uniform `infixl 1` hides structure.** All operators bind equally, which
   keeps chains left to right and unsurprising, but there is no precedence
   hierarchy to tell a classifier from a verb from a source. Reading depends
   on the symbols and nothing else. Fallbacks need parentheses or a local
   helper (`one`) as soon as sources are classified.
3. **Symbols versus names.** `?!`, `?∅`, `?∃`, `?<*`, `?|<>` are compact and
   unsearchable. The Haskell community's experience with operator-heavy
   libraries is mixed: the terseness is the appeal for the author and the
   barrier for the second reader. The lines in `grantRead` are the cost.
4. **Two ways to say the same thing.** Fused sugar (`?!`) and classifier plus
   verb (`<&> exactlyOne ??`) both exist. The sugar is right for the common
   case; the grid is right for everything else, but a reader has to know both.
5. **Prior art is not acknowledged in the code.** Helpers such as `note` and
   `??`/`!?`-style operators for lifting `Maybe`/`Either` into `ExceptT`
   already exist in the ecosystem. What is new here is the layering,
   the cardinality quantifiers and the algebra; the readme should say so.

## Scorecard

| Criterion | railroad | helpers | case |
|---|---|---|---|
| Decisions visible at a glance | very good | fair | poor |
| Reading without prior vocabulary | poor | good | very good |
| Locality of change | very good | fair | poor |
| Representation hidden from handlers | very good | fair | none |
| Silent change of meaning | one known case | none | none |
| Debuggability | fair | good | very good |
| Cost of the underlying machinery | high | none | none |
| Vocabulary to maintain | high | low | none |

## What the reviewer would require before adopting it

1. A short core. Teach `?`, `??`, `?!` and `?∅` first; they cover most of the
   lines. Mark the rest as a second tier.
2. The S5 caution in the readme, next to the empty-list one.
3. A named-policy idiom for error mapping (`?? dbErr`), shown in the readme.
4. The constant form of the cardinality check (`<&> exactlyOne ? e`) alongside
   `?! cardinalityErr e (const e)`, since the second appears whenever both cases
   map to the same error.
5. No further growth of the operator set without a handler that needs it.

## Overall

On a snapshot, railroad is the most elegant of the three for a reader who knows
it, and the least readable for a reader who does not. Under change it is the
best of the three on locality and on hiding representation, and it has one
failure mode the others do not. A reviewer would adopt it for a team that will
read this code many times, accept the vocabulary cost, and keep the core of
four operators visible. They would not adopt it for a team that rotates often
or has no one willing to own the type-level machinery.
