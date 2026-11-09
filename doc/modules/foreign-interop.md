## Foreign Interoperability

### About

Other languages have type systems too. Their definitions, such as a TypeScript declaration file,
a C header, a [WebIDL](https://en.wikipedia.org/wiki/Web_IDL) interface or a WebAssembly
[WIT](https://component-model.bytecodealliance.org/design/wit.html) description, say precisely
which members exist and what they accept.

Those definitions are read as the code compiles, and *foreign* expressions are checked against
them. Navigating a foreign type, selecting a member and applying a function are each verified
against the foreign language's own declarations.

An `Ecosystem` names a foreign world and the grammar its definitions are written in. An
`Interface` loads a definition file for it. A `Foreign` value navigates the declared types, and
produces an expression to hand to whatever executes in that world. This is the mechanism beneath
the [typed DOM scripting](web-automation.md) of the browser, and it is open to any ecosystem with
a readable interface language.

### On foreign types

Interoperation usually discards the foreign type system at the border. A JavaScript call from
Scala is a string. A C function is a binding declared by hand. The authoritative declarations,
the `*.d.ts`, the header or the IDL, sit unread while the programmer transcribes them, and the
mistakes are exactly the ones which the foreign compiler would have caught.

Here the declarations are read instead. The definition file is parsed at [compiletime], each
step of a navigation is resolved against it, and a member which does not exist, or an argument of
the wrong foreign type, is a Scala compile error. Checking a foreign call against the foreign
world's own definitions is
[safety by construction](../philosophy/safety-by-construction.md) across a language boundary.

Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Loading an interface

An `Interface` binds an ecosystem to a definition file on the [classpath](classpath.md). The
file is read as the code compiles, so it must be on the compiler's classpath. Each supported
grammar is a `Dialect` which the ecosystem names:

```tabular
column  Definitions
column  Ecosystem
column  Dialect
row  TypeScript declarations  `Typescript`  `TypescriptDialect`
row  C headers  `Native`  `CHeader.Dialect`
row  WebIDL  `WebIdl`  `WebIdlDialect`
row  WIT  `Wit`  `WitDialect`
row  Kotlin `@Metadata` in classfiles  `Kotlin`  `KotlinDialect`
```

A TypeScript interface, for example:

```scala fuel
typecheck
##
given (Interface in Typescript at "/xenophile/definitions.ts") =
  Interface[Typescript](cp"/xenophile/definitions.ts")
```

### Navigating foreign types

A `Foreign` value stands for a value of a foreign type. Its members navigate as though the
foreign declarations were Scala. The type of each step comes from the declarations, so the
refinement tracks exactly what the foreign type system says. Here `Foo` declares a member `bar`
of type `Bar`, and a method `greet` which takes a string:

```scala fuel
typecheck
##
val foo: Foreign of "Foo" from Typescript = Foreign["Foo", Typescript]

val bar: Foreign of "Bar" from Typescript = foo.bar
val greeting = foo.greet(t"hello")
```

`Foo` declares no member called `missing`, and `greet` does not take a number, so neither of
these compiles:

```scala fuel
compile
errors show
##
foo.missing
```

```scala fuel
compile
errors show
##
foo.greet(42)
```

Scala values pass as arguments where an `Interoperable` instance maps them onto the foreign
type: text to a string, and the sized numbers to their foreign widths. The boundary is typed in
both directions.

### Kotlin and Java facades

A JVM library written in Kotlin is on the same classpath, so its declarations need no separate
file. They are in the classfiles, in the `@Metadata` which a Kotlin compiler writes. Those
declarations are read at compiletime, and a *facade* wraps a live value so that its members are
reached as though they were Scala's. Each access becomes a direct, statically-typed JVM call,
with no reflection and no wrapper object. With the Kotlin standard library on the classpath:

```scala fuel
typecheck
##
val pair = make[kotlin.Pair[Text, Text]](t"a", t"b")
val first: Text = pair.first

val regex = make[kotlin.text.Regex](t"[0-9]+")
val matched: Boolean = regex.matches(t"123")
```

Three methods enter a Kotlin type:

```tabular
column  Method
column  Reaches
row  `make[T](…)`  a new instance of `T`, through its constructor
row  `companion[T]`  the members of `T`'s companion object
row  `singleton[T]`  the members of the object `T`
```

Types substitute through, so `pair.first` is a `Text` and not an `Any`, and a `Text` passes
wherever a `CharSequence` is expected. Kotlin's nullability is honored. A result which may be
null is an [`Optional`](optional-values.md), absent where Kotlin would return null.

Properties read as members, and `var` properties accept assignment, through the getters and
setters which Kotlin generated. Assigning to a `val` does not compile. A Kotlin parameter of
function type takes an ordinary Scala lambda. The parameter of the lambda is itself a facade
over the Kotlin type, so the value inside it navigates too:

```scala fuel
typecheck
##
val replaced = regex.replace(t"a1b2", (m: Facade over kotlin.text.MatchResult) => t"<${m.value}>")
```

Here `replaced` is `t"a<1>b<2>"`. A member which the class does not declare, or an argument of
the wrong type, is a compile error. Where the name is close to a real one, the error suggests
it, spelled as Kotlin spells it.

### Expressions, not effects

Navigation builds an *expression*, a typed tree of references, selections and applications, and
performs nothing itself. Running the expression is the business of the ecosystem. The browser
ecosystem renders it to JavaScript for an event handler, and a WebAssembly ecosystem lowers WIT
calls to imports. The checking is the point. By the time an expression leaves Scala, every step
of it has been verified against the foreign world's own definitions.

### Java's own types

The standard library of the JVM has types for things which these libraries have their own types
for: paths, URLs, instants, durations. Code which meets both worlds needs to cross between them
without copying values by hand. Rather than converting, each Java type is made a
*representation* of the concept. Any operation which asks for its result "as a path" or "as an
instant" can then produce the Java type directly. Which representation is meant is a choice,
made by import:

```tabular
column  Java type
column  Stands as
column  Import
row  `java.nio.file.Path`  a path  `pathInterfaces.javaNioPath`
row  `java.io.File`  a path  `pathInterfaces.javaIoFile`
row  `java.net.URL`  a URL  `urlInterfaces.javaNetUrl`
row  `java.time.Instant`  an instant  `instantInterfaces.javaTimeInstant`
row  `java.util.Date`  an instant  `instantInterfaces.javaUtilDate`
row  `Long`, milliseconds since the epoch  an instant  `instantInterfaces.javaLongInstant`
row  `Long`, nanoseconds  a duration  `durationInterfaces.javaLongDuration`
```

For example:

```scala
import instantInterfaces.javaTimeInstant
import durationInterfaces.javaLongDuration
```

With those in scope, a [time](time.md) read from a file or a clock arrives as a
`java.time.Instant`, and a duration as a `Long` of nanoseconds, ready to hand to a Java API
which expects them. The rest of the program keeps its own types.

### Related

- [Web automation](web-automation.md) scripts the browser's DOM through foreign expressions.
- [Classpath](classpath.md) describes the classpath which an interface file is read from.
- [Compiler](compiler.md) compiles Kotlin sources into the classfiles which a facade reads.
- [Time](time.md) and [paths](paths.md) describe the types which the Java representations stand
  for.
- [Browser DOM](dom.md) and [syntax highlighting](syntax-highlighting.md) build on foreign interface definitions.
