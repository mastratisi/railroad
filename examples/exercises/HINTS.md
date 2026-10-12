# Hints

Try the exercise first. Run `cabal test exercises` and read what the failing test shows you; the
counterexample is often the hint you need. If you are stuck, read hint 1 only, try again, and
only then hint 2. Hint 3 is close to the answer.

Exercise 00 is solved for you.

## Lesson 1: unwrap, or throw a constant

**01** 1. For a `Maybe`, `Nothing` is the failure. Which operator takes the error to throw on its
right? 2. It is the one in the worked example. 3. `q ? <the error from the task>`.

**02** 1. For a `Bool`, `False` is the failure. 2. The same operator as the worked example.
3. The result is `m ()`: unwrapping a `Bool` gives `()`.

**03** 1. `Left` is the failure, and what is wrapped is a token. 2. Same operator again. 3. The
error is `Internal`.

**04** 1. Two steps in a `do` block; the second must not run if the first fails. 2. Bind the user
with `<-`, then apply the operator to the permission check. 3. `u <- q ? ...`, then
`perm ? ...`, then `pure u`.

## Lesson 2: layers

**05** 1. Which layer is outermost, `Either` or `Maybe`? 2. One operator per layer. 3. `q ? A ? B`:
`A` is for the outer layer.

**06** 1. Read the type: which layer is outermost now? 2. The same two operators as 05, in the
order the layers come. 3. `q ? <for Maybe> ? <for Either>`.

**07** 1. An `Either` wrapped around a `Bool`. 2. Chain the operator twice. 3. `q ? A ? B`.

## Lesson 3: the error depends on what failed

**08** 1. A constant cannot give two different answers. You need a function from the failure to the
error. 2. The operator with two question marks. `fromDb` is that function. 3. `q ?? fromDb`.

**09** 1. A constructor is a function. 2. The same operator as 08. 3. `q ?? BadRequest`.

**10** 1. Two layers; the outer one needs the failure, the inner one does not. 2. A mapped
operator, then a constant one. 3. `q ?? f ? e`.

## Lesson 4: how many rows?

**11** 1. After the database layer there are three outcomes for the rows: none, one, many.
2. The operator with an exclamation mark, and `cardinalityErr`. 3.
`q ? Unavailable ?! cardinalityErr <none> (const <many>)`.

**12** 1. There is no database layer here, just a list. 2. The operator with a plus sign.
3. `q ?+ <the error>`.

**13** 1. The success case is an *empty* list. 2. The operator with an empty-set sign (`?∅`, or the
ASCII spelling `?@`). It takes a function of the rows. 3. `q ? A ?∅ const B`.

**14** 1. `cardinalityErr`'s second argument receives the rows. 2. Build `Ambiguous` from
`map name`. 3. `cardinalityErr NotFound (Ambiguous . map name)`.

## Lesson 5: review A

**15** 1. Compare with 11 and 08. 2. The database layer is mapped, not constant. 3. `q ?? f ?! ...`.

**16** 1. Two statements, like 04. 2. The first is a "must not exist" check, like 13, with a mapped
database failure. 3. `usersQ ?? fromDb ?∅ ...`, then `mint ? ...`.

**17** 1. Count the layers in the type. 2. Two of the three errors are the same. 3. `q ? A ? B ? B`.

## Lesson 6: recover instead of throwing

**18** 1. Nothing is thrown. The failure is replaced. 2. The operator with a tilde. 3.
`q ?~ <the default>`.

**19** 1. The same operator works on an `Either`. 2. The default is a number. 3. `q ?~ 0`.

**20** 1. The replacement depends on which failure it was. 2. The tilde operator with two question
marks takes a function. 3. `q ??~ \case { DbDown -> ...; DbTimeout -> ... }`.

## Lesson 7: look at the failure first

**21** 1. You want to observe the failure and then still fail. 2. The operator with an asterisk
runs a handler on the failure and passes the value on. Order matters: the constant error throws the
failure away. 3. `q ?<* (\_ -> logIt "...") ? <the error>`.

**22** 1. Now the handler needs the failure itself. 2. A function from the failure to the log line.
3. `q ?<* (logIt . show) ?? fromDb`.

## Lesson 8: a whole list of results

**23** 1. A list of `Maybe`s: every element must succeed. 2. The same operator as 01. 3.
`q ? <the error>`.

**24** 1. The first failing element is the one reported. 2. A mapped operator. 3. `q ?? fromDb`.

**25** 1. Write the obvious thing, run the test, read the counterexample. 2. Which operator demands
at least one element, and where does it go relative to the one that checks every element? 3.
`q ? A ?+ B ? B`; the result is a list, so discard it (`void`).

## Lesson 9: Validation

**26** 1. The type does the work; the operator is one you know. 2. A mapped operator. 3.
`q ?? BadRequest`.

**27** 1. Predict first: same expression as 26? Same behaviour? 2. Compare `ref26` and `ref27`.
3. `q ?? BadRequest`. The difference is in the type, not the code.

## Lesson 10: try one source, then the next

**28** 1. The second source should run only if the first failed. 2. The operator with a bar. Finish
the chain with a constant. 3. `cache ?| db ? <the error>`.

**29** 1. A longer chain. 2. The bar operator again. 3. `a ?| b ?| c ? e`.

**30** 1. When both fail, their problems are combined. 2. The bar operator with a diamond (`?|<>`),
finished with a mapped error. 3. `a ?|<> b ?? BadRequest`.

## Lesson 11: review B

**31** 1. After unwrapping, a check on the value. 2. The operator with a greater-than sign tags a
value by a predicate; finish with a constant. 3. `q ? A ?> p ? B`.

**32** 1. The error carries the rejected value. 2. Finish the tagged value with a mapped operator.
3. `... ?> p ?? QuotaExceeded`.

**33** 1. Two things happen when the primary misses: a log line, then the next source. 2. Observe
before you fall back. 3. `primary ?<* h ?| backup ? e`.

## Lesson 12: classify first, then act

**34** 1. Here a success is the problem. 2. `neg` swaps the two sides; apply it with `<&>`. 3.
`q <&> neg ? <the error>`.

**35** 1. "At least one worked", and the mapper receives all failures. 2. The operator with an
exists sign (`?∃`). 3. `q ?∃ const <the error>`.

**36** 1. Classify "exactly one" into an `Either`, then recover. 2. `exactlyOne`, `<&>`, `?~`. 3.
`q ? e <&> exactlyOne ?~ guest`.

## Lesson 13: capstones

**37** 1. One source after another is the bar operator. 2. Each database source is "unwrap with a
mapped error, then exactly one row", as an `Either`: write that as a small local helper with
`<&> exactlyOne`. 3. `cache ?| one byId ?| one byEmail ?? cardinalityErr ...`.

**38** 1. Four statements, in order; compare 16 and 11. 2. The first needs an observer in front of
the constant error. The read-permission step wants the row to be `False`: that is `neg`. 3. Four
lines: `writeQ ?<* ... ? ... ?! ... ? ...`, `_ <- targetQ ?? ... ?! ...`,
`readQ ? ... ?! ... <&> neg ? ...`, `insertQ ? ...`.
