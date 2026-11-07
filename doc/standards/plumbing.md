# Coercion Helpers

Soundness converts by abstraction, not by hand. A value changes type or shape through a
typeclass (`Showable`, `Encodable`, `Decodable`, `Convertible`), a bridge that names the boundary
it crosses (`.tt`, `.s`, `List.stdlib`, `Array.unsafeFrozen`), or a method on the type itself. A
*coercion helper* is a small `def` — private, or local to a method — whose whole job is to get one
value into the type the next line wants. It is not a solution; it is evidence that an abstraction
is missing, insufficient, or present but unused, and it is where a simplistic or wrong conversion
(a `toString` where a `show` was meant, a round trip through `String` that drops an encoding)
creeps in unreviewed. The same is true of a `given Conversion` written to paper over a mismatch.

`flare assess plumbing` finds them. The rule `plumbing` in `.pyrocosm/flare/config.tel` names
the model, the criteria below with their weights, and this file as the rubric; flare gathers the
candidate definitions from the parse tree, has the model answer the criteria in batches, and
records each verdict as BinTEL in git notes under `refs/notes/flare-assess/plumbing`. A
definition is identified by its *locus* — path, enclosing definitions and signature, never a
line — and its verdict is keyed by a digest of its normalised text, so a definition is judged
once for as long as its text, this rubric, the criteria and the model stand. `make plumbing`
runs it; `flare assess plumbing --show HEAD` prints the ranking recorded for a commit.

## What replaces what

```tabular
column  Hand-rolled
column  Use instead
row  `x.toString.tt`, a `render(x): Text` or `name(x): Text` helper  `x.show` (`Showable`), or `x.in[Text]`
row  `private inline def kebab(s: String): Text = Text(s)`  `.tt` at the call site
row  `text.s.replace(…).nn.tt`, any `.s … .tt` round trip  a `Text` operation from gossamer, or add one
row  `Integer.parseInt(hex.s, 16)`  `hex.deserialize[Hex]`, or `text.as[Int]` (`Decodable`)
row  `Array.unsafeFrozen(text.s.getBytes(UTF_8).nn)`  `text.in[Data]` with a `CharEncoder` in scope
row  a hand-written `toList`/`toMap` on a container  a `Convertible` instance, then `.to[List]`/`.to[Map]`
row  a `match` mapping each constructor of one enum to one of another  `Enumerable`/`Extractable`, or a method on the source type
row  `if x == sentinel then Unset else x`  `x.puncture(sentinel)`
row  `Option` to `Optional` by hand  `.optional`
row  `try … catch` to `Optional`  `safely`
row  `asInstanceOf` to reach a stdlib collection  `stdlib` on the collection, as `List.stdlib`
row  shuffling between 0- and 1-based indices  `.z`, `.u`, `.n0`, `.n1`
row  the same `tag(out, char) = out.append(char.toByte)` copied into six atomizers  one extension on `Scribe[Byte]`
row  `given Conversion[Int, Expression] = int => Number(int.toDouble)`  an `Expression.apply(Int)` or a `Decodable`
```

The fix is never to keep the helper and rename it. Either the abstraction exists and the call site
uses it, or it does not and the library that owns the target type gains it — in the companion, so
it resolves without an import (see the given-placement rules in `.claude/CLAUDE.md`).

## What may stay

A small function is not a coercion helper merely because its result has a different type from
its argument. Four shapes are legitimate.

1. **Parsing and decoding that can fail.** A body that raises, mitigates, returns `Optional`, or
   is a `Decodable`/`Parsable` instance is doing the work the abstraction exists for.
2. **Computation.** `toJulianDay`, `convertBitDepthsToSymbols`, a CRC, a bit-field extraction:
   arithmetic on the value, not re-wrapping of it.
3. **A boundary crossed once.** A single `.tt`, `.s`, `.nn`, `List.stdlib` or `Array.unsafeJvm`
   where Soundness meets the JDK, WASI or Scala.js, inline at the call that needs it. A helper
   that exists only to *name* that crossing is the smell; the crossing itself is not.
4. **A hoist with a proof.** A local `def` pulled out of a lambda to dodge a compiler crash
   (`wildApprox`, a `FunProto` cache) carries a comment on the line above naming the crash, as
   `stratiform.internal.scala`'s `liftText` does. The comment is what exempts it.

<!-- rubric:begin -->
## Rubric

You are scoring Scala 3 method definitions from the Soundness codebase. Each candidate is a
small method (or a `given Conversion`) that may be a *coercion helper*: a function whose whole
job is to change one value's type or shape, standing in for an abstraction that should have been
used instead. You will see the definition with two lines of context above it, and a header line
giving facts the driver computed (kind, call-site count, duplicates). Judge only from the text
shown. Do not follow references, guess at code you cannot see, or invent APIs.

For each candidate, answer every criterion below with `true` or `false`. Answer from the code
alone; the point values are for your information and the driver sums them, so do not compute a
total. The **adapter list** referred to by S2 and S3 is exactly these, and any bare constructor
call `Name(…)` / `new Name(…)`:

```
.tt .s .nn .toString .toInt .toLong .toDouble .toByte .toChar .toList
.toMap .toSeq .toArray .getBytes .stdlib Text(…) Array.unsafeFrozen(…)
Array.unsafeJvm(…) asInstanceOf[…]
```

```tabular
column  id
column  criterion
column  answer `true` when
column  points
row  S1  single expression  the body is one expression: no `val`, `if`, `match`, `{…}` block, loop or second statement  +2
row  S2  adapters only  every call in the body is on the adapter list or is a field access, with no arithmetic and no literals other than a charset or algorithm name (`"UTF-8"`, `"SHA-256"`)  +3
row  S3  round trip  one chain both leaves and re-enters a representation: `.s … .tt`, `.toString … .tt`, `.stdlib … .to[…]`, `Array.unsafeJvm … Array.unsafeFrozen`, `.s … .nn.tt` around a Java call  +3
row  S4  coercion verb  the name starts with `to`, `from`, `as`, `make`, `mk`, `convert`, `lift`, `wrap` or `unwrap` followed by a capital letter, or is exactly `text`, `string`, `bytes`, `number`, `decimal`, `hex`, `key` or `node`  +1
row  S5  synonym of an existing operation  the name is `toList`, `toMap`, `toSeq`, `toArray`, `text`, `show`, `render`, `str`, `string`, `bytes`, `optional`, `orNull`, or the return type's name in lower case  +2
row  S6  pure forwarder  the body is a single call to one method or constructor passing the parameters through unchanged, in order, possibly with one adapter applied to one of them  +3
row  S7  one-to-one ladder  the body is a `match` in which every case is `pattern => Constant` or `pattern => Wrap(binding)`, with no guards and no other logic  +2
row  S8  shape-only return type  the declared return type is `Text`, `String`, `Data`, `Array[Byte]`, `List[…]`, `Map[…]`, `Seq[…]` or `Unit`  +1
row  S9  one parameter  the method takes exactly one value parameter  +1
row  S10  conversion given  the declaration is a `given … Conversion[A, B]` (answer S4, S5 and S6 `false` when this is `true`)  +3
row  X1  handles failure  the body contains `raise`, `abort`, `mitigate`, `safely`, `lest`, `Optional`, `Unset`, `.or(`, `try`, `panic`, or a guarded case  −3
row  X2  computes  the body does arithmetic on values, uses a numeric literal other than `0`, `1` or a radix, concatenates three or more parts, loops, or recurses  −3
row  X3  justified by a comment  a comment on the definition or the line above it mentions `compiler`, `crash`, `macro`, `boundary`, `workaround`, `JVM` or `Java`  −2
row  X4  codec-family name  the name starts with `read`, `write`, `parse`, `decode`, `encode`, `serialize` or `deserialize`  −2
```

Then choose one `replacement` from this vocabulary only — the abstraction the call site should
use instead — or `none` if you cannot tell. Entries are separated by spaces, except that
`inline at call site` is a single entry:

```
.tt .s .show .as[T] .in[Text] .in[Data] .to[List] .to[Map] .stdlib
.puncture .optional safely Enumerable Extractable Conversion→method extension
inline at call site
none
```

Finally give a one-sentence `reason`, quoting the fragment of the body that decided it.

### Output

Return a JSON array, one object per candidate, in the order given, and nothing else:

```json
[{"id": "<id from the header>",
  "signs": {"S1": true, "S2": true, "S3": false, "S4": false, "S5": false, "S6": false,
            "S7": false, "S8": true, "S9": true, "S10": false,
            "X1": false, "X2": false, "X3": false, "X4": false},
  "replacement": ".show",
  "reason": "`month.toString.tt` renders through toString where Showable exists"}]
```
<!-- rubric:end -->

flare recomputes the score from the booleans (a small model's arithmetic is not trusted), adds
its own signals, and validates `replacement` against the vocabulary, so a hallucinated API cannot
enter the census. It adds the rule's `signal` lines:

```tabular
column  Signal
column  When
column  Points
row  `local`  a local `def`  +2
row  `private`  a `private` definition  +1
row  `calls 1`  one call site in scope  +2
row  `calls 2`  two call sites in scope  +1
row  `calls 4+`  four or more call sites in scope  −1
row  `duplicate`  a body duplicated verbatim (parameters renamed) in another definition  +3
row  `one-liner`  the whole definition on one line  +1
```

The census is worked from the top of the total, in bands:

```tabular
column  Band
column  Total
row  **definite**  10 or more
row  **likely**  5 to 9
row  **weak**  0 to 4
row  **legitimate**  below 0
```

## Worked examples

```tabular
column  Candidate
column  Signs
column  Model
column  Driver
column  Total
row  local `def bytes(text: Text): Data = Array.unsafeFrozen(text.s.getBytes(UTF_8).nn)`  S1 S2 S3 S5 S8 S9  12  local, one call, one line  17
row  `private def tag(out: Scribe[Byte], char: Char): Unit = out.append(char.toByte)`, in six files  S1 S2 S6 S8  9  private, duplicated, one line  14
row  `private inline def kebab(s: String): Text = Text(s)`  S1 S2 S6 S8 S9  10  private, two calls, one line  13
row  `private def normalise(text: Text): Text = text.s.replace("proscenium.", "").nn.tt`  S1 S3 S8 S9  7  private, one call, one line  11
row  `def monthName(month: Mensual): Text = month.toString.tt`  S1 S2 S3 S8 S9  10  public, one call  12
row  `given Conversion[Int, Expression] = int => Expression.Number(int.toDouble)`  S1 S2 S9 S10  9  —  9
row  `private def typeKindToFrame(kind: jlc.TypeKind): Frame = kind match …`  S4 S7 S9  4  private, one call  7
row  `def toJulianDay(date: Date): Int = … arithmetic …`  S4 S9 X2  −1  three calls  −1
row  `def parseHeader(text: Text): Header raises ParseError = …`  excluded by name before scoring  —  —  —
```

The first five are the census's definite band: pure re-wrapping, each with a replacement that
already exists. The ladder is likely, and a human decides whether the enum gains a method or an
`Extractable`. The last two are what the exclusions and the X criteria exist to keep out.

## Traps

A hand-written `toList` on a type with no `Convertible` instance scores high, and should: the fix
is to add the instance, not to keep the helper because nothing else serves.

A private forwarder that exists to give an internal name to a public function (`private def
number = Css.number`) scores as a pure forwarder. It usually is one; the exceptions are worth a
comment, which then trips X3.

The extractor admits a public definition only as a `given Conversion`, a round trip, or a
one-line adapter chain with no logic in it; a private or local definition also by a coercion
verb or an adapter call in a short body. Widen or narrow with the rule's `gate` lines.

The model is nondeterministic. The census is a ranking to work from, not a count to defend; the
booleans are recorded so a surprising score can be audited against the code, and a judgement
is kept by the definition's digest, so a re-run asks the model only about definitions that have
changed — or every one, once, after the rubric has.
