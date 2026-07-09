## Foreign Interoperability

### About

Other languages have type systems too, and their definitions — a TypeScript declaration file, a C
header, a [WebIDL](https://en.wikipedia.org/wiki/Web_IDL) interface, a WebAssembly
[WIT](https://component-model.bytecodealliance.org/design/wit.html) description — say precisely
which members exist and what they accept. Soundness reads those definitions as the code compiles
and checks *foreign* expressions against them: navigating a foreign type, selecting a member,
applying a function, each step verified against the foreign language's own declarations.

An `Ecosystem` names a foreign world and the grammar its definitions are written in; an
`Interface` loads a definition file for it; and a `Foreign` value navigates the declared types,
producing an expression to hand to whatever executes in that world — the mechanism beneath the
[typed DOM scripting](web-automation.md) of the browser, and open to any ecosystem with a readable
interface language.

### On foreign types

Interoperation usually discards the foreign type system at the border: a JavaScript call from
Scala is a string, a C function a hand-declared binding, and the authoritative declarations — the
`.d.ts`, the header, the IDL — sit unread while the programmer transcribes them, imperfectly, by
hand. The mistakes are exactly the ones the foreign compiler would have caught.

Soundness reads the declarations instead. The definition file is parsed at compiletime, each
navigation step is resolved against it, and a member that does not exist, or an argument of the
wrong foreign type, is a Scala compile error. Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Loading an interface

An `Interface` binds an ecosystem to a definition file on the classpath. Each supported grammar —
TypeScript declarations, C headers, WebIDL, WIT — is a `Dialect` the ecosystem names:

```scala
given (Interface in Typescript at "/definitions.ts") =
  Interface[Typescript](cp"/definitions.ts")
```

### Navigating foreign types

A `Foreign` value stands for a value of a foreign type, and its members navigate as though the
foreign declarations were Scala. Each step's type comes from the declarations, so the refinement
tracks exactly what the foreign type system says:

```scala
val foo: Foreign of "Foo" from Typescript = Foreign["Foo", Typescript]

val bar: Foreign of "Bar" from Typescript = foo.bar   // Foo declares bar: Bar
foo.greet(t"hello")                                   // typed against greet's signature

foo.missing        // does not compile: Foo declares no such member
foo.greet(42)      // does not compile: greet takes a string
```

Scala values pass as arguments where an `Interoperable` instance maps them onto the foreign
type — text to a string, the sized numbers to their foreign widths — so the boundary is typed in
both directions.

### Expressions, not effects

Navigation builds an *expression* — a typed AST of references, selections and applications — and
performs nothing itself. What runs the expression is the ecosystem's business: the browser
ecosystem renders it to JavaScript for an event handler, and a WebAssembly ecosystem lowers WIT
calls to imports. The checking is the point: by the time an expression leaves Scala, every step of
it has been verified against the foreign world's own definitions.
