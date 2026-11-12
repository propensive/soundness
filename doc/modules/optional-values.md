## Optional Values

### About

A value that might be absent has the type `Optional`. An `Optional[Text]` is either a `Text`, or
the single value `Unset`, which stands for absence.

Unlike an `Option`, an `Optional` adds no wrapper. A present value is stored as itself, and is
written as itself, so the common case of a value that is there costs nothing and needs no
ceremony.

Optionality is also flat. `Optional[Optional[Int]]` is the same type as `Optional[Int]`, so the
ambiguous state which `Option` allows, a "present absence" such as `Some(None)`, cannot be
represented at all.

### On absence

Three ways of saying "no value" are in common use:

```tabular
column  Absence as
column  Visible in the type
column  A present value is
column  Nests
row  `null`  no, so it fails only when dereferenced  itself  no
row  `Option`  yes  boxed in a `Some`  yes: `Option[Option[T]]` has two distinct empties
row  `Optional`  yes  itself  no
```

`null` is invisible to the compiler. `Option` is visible, but makes every present value pay for
a wrapper, and code which meets a nested `Option` must thread through both layers.

An `Optional` keeps the safety of `Option`: absence is in the type, and the compiler makes sure
it is handled. It behaves like the value itself when the value is present. It rests on Scala's
[union types](https://docs.scala-lang.org/scala3/reference/new-types/union-types.html):
`Optional[value]` is exactly the union `Unset | value`. A union needs no wrapper, and a union of
a type with itself is that same type, which is why optionality never nests.
[Optionality](../philosophy/optionality.md) describes the principle. Everything comes from the
`soundness` package, and the second import lets the samples print their results:

```scala
import soundness.*
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Representing absence

An optional value has the type `Optional[…]`. A present value is assigned exactly as a plain
value would be, and an absent one is `Unset`:

```scala
val name: Optional[Text] = t"Hamlet"
val nickname: Optional[Text] = Unset
```

An optional value cannot be used where a plain value is needed until the absent case has been
dealt with. The compiler insists on it:

```scala fuel
compile
errors show
##
val greeting: Text = name
```

The operations which deal with it, each described below, are:

```tabular
column  Operation
column  Gives
row  `or`  the value, or a fallback evaluated only when needed
row  `let`  a function's result for a present value, with `Unset` passed through
row  `lay`  a fallback for absence, or a function's result for presence, as a plain value
row  `present`, `absent`  which case a value is in
row  `mask`  `Unset` in place of a present value that satisfies a predicate
row  `assume`  the value, or an `Optional.Error` thrown
row  `presume`  the value, or the type's `Default`
row  `compact`  the present values of a collection
row  `unless`  `Unset` in place of any value that satisfies a predicate
row  `puncture`  `Unset` in place of one particular value
row  `only`  a partial function's result, or `Unset` where it is not defined
row  `option`  an `Option`
row  `optional`  an `Optional`, from an `Option`
row  `default[T]`  the `Default` value of the type `T`
```

### Supplying a fallback

`or` gives the value if it is present, and a fallback otherwise. The result is a plain value:

```scala fuel
run
##
Out.println(name.or(t"anonymous"))
Out.println(nickname.or(t"anonymous"))
```

The fallback is evaluated only when it is needed, so it may be expensive, or may itself fail.

### Transforming

`let` applies a function to a present value, and passes `Unset` through untouched:

```scala fuel
run
##
Out.println(name.let(_.upper).inspect)
Out.println(nickname.let(_.upper).inspect)
```

`inspect` displays a present optional value between corner brackets, and an absent one as `○`.

Because optionality is flat, `let` does the work of both `map` and `flatMap` on an `Option`. A
function which itself returns an `Optional` does not add a layer, so calls to `let` chain
without nesting:

```scala
def initial(text: Text): Optional[Char] = text.chars.head
```

```scala fuel
run
##
Out.println(name.let(initial).inspect)
Out.println(nickname.let(initial).inspect)
```

`lay` handles both cases at once. It takes a fallback for absence and a function for presence,
so its result is a plain value, not an `Optional`:

```scala fuel
run
##
Out.println(name.lay(0)(_.length).show)
Out.println(nickname.lay(0)(_.length).show)
```

### Testing presence

`present` and `absent` say which case a value is in:

```scala fuel
run
##
Out.println(name.present.inspect)
Out.println(nickname.absent.inspect)
```

`mask` discards a present value which satisfies a predicate, and leaves any other value as it
was:

```scala fuel
run
##
Out.println(name.mask(_.length > 64).inspect)
Out.println(name.mask(_.length < 64).inspect)
```

### Requiring a value

Sometimes a value is known to be present, though its type cannot say so. `assume` asserts it,
and gives the plain value. The assertion is unchecked, so `assume` needs the capability to
throw, and throws an `Optional.Error` if it is wrong:

```scala
import scala.unsafeExceptions.canThrowAny
```

```scala fuel
run
##
Out.println(name.assume)
```

```scala fuel
run
errors show
##
nickname.assume
```

`presume` falls back to the type's default instead: an empty text, a zero, an empty list. It is
available for the types which [declare a default](#defaults):

```scala fuel
run
##
Out.println(nickname.presume.inspect)
```

### Absence from collections

`compact` reduces a collection of optional values to the values which are present, in a
collection of the same kind:

```scala fuel
run
##
Out.println(List[Optional[Text]](t"a", Unset, t"c").compact.inspect)
```

Optional values also come *out* of [collections](collections.md#positions-and-access). Looking
up a key in a map, or the first element of a list, gives an `Optional`, since there may be
nothing there.

### Building an optional

A condition turns any value into an optional one. `unless` discards the value when a predicate
holds, `puncture` discards one particular value, and `only` keeps a value where a partial
function is defined:

```scala
val count = 0
val input = t""
val value = 7
```

```scala fuel
run
##
Out.println(count.unless(_ == 0).inspect)
Out.println(input.puncture(t"").inspect)
Out.println(value.only { case n if n > 0 => n*n }.inspect)
```

`safely` builds one from an operation that may fail: it gives the result, or `Unset` if the
operation raised an error. [Errors](errors.md#choosing-a-strategy) describes it.

### Working with `Option`

An `Option` is sometimes unavoidable, at the boundary with another library. The two convert in
both directions:

```scala fuel
run
##
Out.println(name.option.inspect)
Out.println(Some(t"Hamlet").optional.inspect)
```

### Optional fields in data

A case class field typed `Optional` is understood by the data formats. An absent field is left
out of the output, and a field missing from a document is read as `Unset`.
[JSON](json.md#optional-fields-and-defaults) shows this, along with the imports which make a
missing field an error instead; the other formats behave alike.

### What it costs

An `Optional[Text]` *is* a `Text`. There is no wrapper, so a present value costs nothing to
hold, nothing to pass and nothing to read. `Option`, by contrast, allocates a `Some` for every
present value. In a data structure of a million records, that is a million objects which exist
only to say "yes, there is one".

The operations are inlined too, and `or` is optimized at [compiletime]. A chain of fallbacks
collapses without building intermediate optionals, and a fallback which is a constant becomes a
constant. [Zero cost](../philosophy/zero-cost.md) explains why this matters to the design.

The price of the representation is that `Optional[Optional[T]]` cannot exist. Absence has one
representation, so it cannot nest. This is checked at compiletime: a type which could not be
made optional unambiguously is rejected where it is written.

### Requiring an unambiguous type

Three [typeclasses][typeclass] support that check, and are occasionally useful directly:

```tabular
column  Typeclass
column  Witnesses
row  `Concrete`  that a type is a definite type, not an abstract one
row  `Distinct`  that a type is not a member of a given union
row  `Mandatable`  the type with its optionality stripped: the type a present value has
```

`Concrete` matters because an abstract type might later be instantiated to something which is
already optional. `Distinct` is the check that keeps `Unset` out of the value type.

A generic method which takes an `Optional[value]`, for an abstract `value`, therefore demands
evidence that `value` is concrete. The demand appears in the method's signature, an
[honest signature], and not as a failure at some distant call.

### Defaults

`presume` and `default` draw on a `Default` instance, which supplies the value a type falls back
to. Instances exist for the types with an obvious zero: empty text, zero, an empty collection. A
type declares its own by defining one:

```scala
case class Volume(level: Int)
given Default[Volume] = () => Volume(0)

val setting: Optional[Volume] = Unset
```

```scala fuel
run
##
Out.println(setting.presume.inspect)
```

A `Default` is computed each time it is asked for, not stored, so a default which is expensive
to compute is computed only where it is used.
