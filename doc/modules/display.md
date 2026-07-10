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

Person(t"Simon", 72).inspect      // t"Person(name:t\"Simon\" ╱ age:72)"
List(t"one", t"two").inspect      // t"""[t"one", t"two"]"""
(5: Optional[Int]).inspect        // t"｢5｣"
(Unset: Optional[Int]).inspect    // t"○"
```

`inspect` renders a `Text` as its source form, `t"Simon"`, where `show` renders it verbatim —
the distinction the two audiences call for. A custom `Inspectable` is written just as a
`Showable` is, where the structural default is not wanted.

### Booleans

A boolean has no single obvious rendering — "yes" and "no", "on" and "off", "true" and
"false" each suit a different context — so showing one draws on an `Affirmation` in scope,
chosen by import:

```scala
import affirmations.yesNoAffirmation

t"the answer is ${true}"   // t"the answer is yes"
```

`onOffAffirmation`, `trueFalseAffirmation` and `oneZeroAffirmation` are the other choices.
