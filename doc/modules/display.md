## Display

### About

A value becomes text in two ways, for two audiences. `show` writes it for a person, in the
form which belongs in a message, a log line, or on a screen. `inspect` writes it for a
programmer, in an unambiguous form for debugging. Keeping the two apart lets a type be
presentable to users, presentable only to developers, or presentable differently to each.

`show` requires a `Showable` instance, and is a compile error without one, so a type which
should never reach a user simply has none. `inspect` always produces something. It falls back
through a value's encodings and, as a last resort, its structure, so a developer can always
see what a value is.

### On display

A single rendering cannot serve both a user and a programmer. `toString` tries, and manages
neither. It leaks internal shapes into output which users see, and yet it is too haphazard
to trust when debugging. Nothing stops it from being called on a value which has no sensible
textual form.

The familiar `Show` typeclass fixes the trustworthiness, but still collapses the two
audiences into one.

Here two [typeclasses][typeclass] keep them apart. `Showable` is deliberately demanding: its
absence is the signal that a value is not meant for display. `Inspectable` is deliberately
total, so that debugging output is never blocked. Separating what a value is from how it is
displayed, through typeclasses chosen by import, is
[decoupling](../philosophy/decoupling.md) at the smallest scale.

Everything comes from the `soundness` package. The second import lets the samples print
their results:

```scala
import soundness.*
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Showing a value

`show` writes a value as `Text`, through its `Showable` instance. The primitive types, `Text`
itself, and collections all have one:

```scala fuel
run
##
Out.println(t"Hello world".show)
Out.println(43.show)
Out.println(List(1, 2, 3).show)
```

The same instances drive [interpolation](text.md#writing-text), so a value which is
substituted into a `t"…"` string is shown by its `Showable`.

A type with no instance cannot be shown. Asking to show one is a compile error, and there is
no silent fallback to some default rendering:

```scala
case class Money(pence: Int)
```

```scala fuel
compile
errors show
##
Money(1099).show
```

### Writing a Showable

`Showable` is a typeclass with a single method, so an instance is a function from the value
to its text:

```scala
given moneyShowable: (Money is Showable) = money => t"£${money.pence/100}.${money.pence%100}"
```

```scala fuel
run
##
Out.println(Money(1099).show)
Out.println(t"The total is ${Money(1099)}")
```

### Inspecting

`inspect` writes a value for a developer, and always succeeds. Case classes, enumerations and
collections are written structurally, with no instance to write, each in a form chosen to be
unambiguous:

```scala
case class Person(name: Text, age: Int)
```

```scala fuel
run
##
Out.println(Person(t"Lear", 80).inspect)
Out.println(List(t"one", t"two").inspect)
Out.println((5: Optional[Int]).inspect)
Out.println((Unset: Optional[Int]).inspect)
```

`inspect` writes a `Text` in the form in which it appears in source code, `t"Lear"`, where
`show` writes it verbatim. That is the distinction which the two audiences call for.

The source form is a real source form. Control characters and quotes are escaped as Scala
would escape them, so an inspected value could be pasted back into code. A text which
contains a newline can be told apart from one which contains the two characters `\` and `n`:

```scala fuel
run
##
Out.println(t"Hello\nworld".inspect)
Out.println(t"Hello \"world\"".inspect)
```

Collections are written so that the *kind* of collection is visible without being named, and
so that nesting is unambiguous. A map has arrows, a list is in square brackets, and an
[optional](optional-values.md) value is in corner brackets, or is a circle when it is unset:

```scala fuel
run
##
Out.println(Map(1 -> 2, 3 -> 4).inspect)
Out.println(Map[Int, Int]().inspect)
Out.println(Set(1).inspect)
```

Enumerations and sealed hierarchies are derived too. A case with no fields is written as its
bare name, without empty parentheses:

```scala
enum Animal:
  case Dog(name: Text)
  case Cat

val dog: Animal = Animal.Dog(t"Crab")
val cat: Animal = Animal.Cat
```

```scala fuel
run
##
Out.println(dog.inspect)
Out.println(cat.inspect)
```

A custom `Inspectable` is written just as a `Showable` is, where the structural default is
not wanted.

### The fallback chain

`Inspectable` is total, but it is not indiscriminate. Asked to inspect a value, it tries in
turn:

```tabular
column  Tried
column  Rendering
row  1  the type's own `Inspectable` instance
row  2  its `Showable` instance
row  3  a structural rendering derived from its shape
row  4  a last-resort rendering
```

A type which has said how it should look is therefore shown in that way in debugging output
too. `Money` has a `Showable` and no `Inspectable`, so `inspect` uses the former:

```scala fuel
run
##
Out.println(Money(1099).inspect)
```

A type which has said nothing still produces something useful, and not a hash code.

### Booleans

A boolean has no single obvious rendering. "Yes" and "no", "on" and "off", and "true" and
"false" each suit a different context. Showing one therefore draws on an `Affirmation` in
scope, and there is no default. Without an `Affirmation`, a boolean does not show at all,
because presuming one would be presuming the context:

```scala fuel
compile
errors show
##
true.show
```

Four affirmations are provided:

```tabular
column  Import
column  `true`
column  `false`
row  `affirmations.yesNoAffirmation`  `yes`  `no`
row  `affirmations.onOffAffirmation`  `on`  `off`
row  `affirmations.trueFalseAffirmation`  `true`  `false`
row  `affirmations.oneZeroAffirmation`  `1`  `0`
```

```scala fuel
run
##
locally:
  import affirmations.yesNoAffirmation
  Out.println(t"the answer is ${true}")

locally:
  import affirmations.onOffAffirmation
  Out.println(t"the switch is ${false}")

locally:
  given affirmation: Affirmation = Affirmation(t"+", t"-")
  Out.println(t"the sign is ${true}")
```

The last is any other pair, defined in one line. `inspect` needs no affirmation, and always
writes `true` or `false`.

### Related

- [Text](text.md) describes the `Text` which `show` produces.
- [Terminal](terminal.md#styled-text) displays values with color and style.
- [Tables](tables.md) lays out many values in columns.
- [Internationalization](internationalization.md) displays values in a user's own language.
- [Derivation](derivation.md) explains how `inspect` is derived for a case class.
