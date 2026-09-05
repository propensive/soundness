## Display

### About

A value becomes text in two ways, for two audiences. `show` renders it for a person — the
form that belongs in a message, a log line, or on screen — and `inspect` renders it for a
programmer, an unambiguous form for debugging. Keeping the two apart lets a type be
presentable to users, presentable only to developers, or presentable differently to each.

`show` requires a `Showable` instance and is a compile error without one, so a type that
should never reach a user simply has none. `inspect` always produces something, falling
back through a value's encodings and, in the last resort, its structure, so a developer can
always see what a value is.

### On display

A single rendering cannot serve both a user and a programmer. `toString` tries, and manages
neither: it leaks internal shapes into user-facing output, yet is too haphazard to trust
when debugging, and nothing stops it being called on a value with no sensible textual form.
The familiar `Show` typeclass fixes the trustworthiness but still collapses the two
audiences into one.

Separating what a value is from how it is displayed, through typeclasses chosen by import, is [decoupling](../philosophy/decoupling.md) at the smallest scale.

Two typeclasses keep them apart. `Showable` is deliberately demanding — its absence is the
signal that a value is not meant for display — while `Inspectable` is deliberately total, so
debugging output is never blocked. Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Showing a value

`show` renders a value to `Text` through its `Showable` instance. The primitive types, `Text`
itself, and collections all have one:

```scala
t"Hello world".show   // t"Hello world"
43.show               // t"43"
```

The same instances drive interpolation, so a value substituted into a `t"…"` string is shown
by its `Showable`. A type with no instance cannot be shown, and asking to is a compile error
rather than a silent fallback to some default rendering.

### Writing a Showable

`Showable` is a single-method typeclass, so an instance is a function from the value to its
text:

```scala
case class Money(pence: Int)
given Money is Showable = money => t"£${money.pence/100}.${money.pence%100}"

Money(1099).show   // t"£10.99"
```

### Inspecting

`inspect` renders a value for a developer, and always succeeds. Case classes, enumerations
and collections render structurally with no instance to write, each in a form chosen to be
unambiguous:

```scala
case class Person(name: Text, age: Int)
```

```scala
Person(t"Simon", 72).inspect      // t"Person(name:t\"Simon\" ╱ age:72)"
List(t"one", t"two").inspect      // t"""[t"one", t"two"]"""
(5: Optional[Int]).inspect        // t"｢5｣"
(Unset: Optional[Int]).inspect    // t"○"
```

`inspect` renders a `Text` as its source form, `t"Simon"`, where `show` renders it verbatim —
the distinction the two audiences call for. A custom `Inspectable` is written just as a
`Showable` is, where the structural default is not wanted.

The source form is a real source form: control characters and quotes are escaped as Scala would
escape them, so an inspected value could be pasted back into code, and a text containing a newline
is distinguishable from one containing the two characters `\` and `n`:

```scala
t"Hello\nworld".inspect     // t"t\"Hello\\nworld\""
t"Hello \"world\"".inspect  // the quotes escaped
```

Collections render in forms chosen so that the *kind* of collection is visible without naming it,
and so that nesting is unambiguous — a map with arrows, a list in brackets, an optional value in
its own brackets or as a circle when unset:

```scala
Map(1 -> 2, 3 -> 4).inspect     // t"{1 → 2, 3 → 4}"
Map[Int, Int]().inspect         // t"{}"
Set(1).inspect                  // a set, not an optional
```

Enumerations and sealed hierarchies derive too, and a case object renders as its bare name rather
than with empty parentheses, since it has no fields to show:

```scala
enum Animal:
  case Dog(name: Text)
  case Cat
```

```scala
(Animal.Dog(t"Rex"): Animal).inspect   // t"""Dog(name:t"Rex")"""
(Animal.Cat: Animal).inspect           // t"Cat"
```

### The fallback chain

`Inspectable` is total, but it is not indiscriminate. Asked to inspect a value, it tries in turn:
the type's own `Inspectable` instance, then its `Showable`, then a structural rendering derived
from its shape, and only then a last-resort rendering. A type that has said how it should look is
therefore shown that way in debugging output too, and a type that has said nothing still produces
something useful rather than a hash code.

### Booleans

A boolean has no single obvious rendering — "yes" and "no", "on" and "off", "true" and
"false" each suit a different context — so showing one draws on an `Affirmation` in scope,
chosen by import:

```scala
import affirmations.yesNoAffirmation

t"the answer is ${true}"   // t"the answer is yes"
```

`onOffAffirmation`, `trueFalseAffirmation` and `oneZeroAffirmation` are the other choices, and any
other pair is one line:

```scala
given Affirmation = Affirmation(t"+", t"-")
```

Note that there is no default. Without an `Affirmation` in scope a boolean does not show at all,
because presuming one would be presuming the context.
