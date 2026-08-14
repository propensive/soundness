## Metaprogramming

### About

Soundness's compiletime machinery rests on a set of tools for writing and debugging macros, and
they are available to any project doing the same. `Every` collects *all* the given instances of a
type visible at a call site — the mechanism behind pluggable configuration throughout Soundness.
Type-level collections reify into runtime values for macros parameterized by static data. Types
render back to source text exactly as they would be written under the caller's imports. Macro
expansions display as inspectable trees. And compiled bytecode can be read back from a quoted
expression, for the rare work that must see what the compiler produced.

### On metaprogramming

Scala 3's macros are principled and under-tooled. A macro author stares at expansions with
`println`, renders types with a `show` that ignores the caller's imports, and cannot ask an
innocent question — "what givens of this type are in scope?" — because implicit search answers with
one, or ambiguity. These gaps are generic; every macro project refills them.

Soundness fills them once. Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Every given in scope

Implicit search finds *the* given; `Every` finds *all of them*, as a compiletime search that
returns their list — including instances that would be ambiguous against each other:

```scala
trait Plugin

given alpha: Plugin = new Plugin {}
given beta: Plugin = new Plugin {}

every[Plugin].values.length   // 2
```

This inverts the usual configuration flow: instead of one value naming all the options, each option
is declared as a given where it applies, and the consumer collects whatever is in scope. It is how
[socket options](sockets.md) accumulate, how every [log sink](logging.md) receives events, and how
[terminal features](terminal.md) switch on.

### Type-level values

A type parameter is to typechecking what an ordinary parameter is to execution: a way of passing
information to a place that will use it. Types are the poorer medium for it, though — type-level
operations are far less expressive than value-level ones, and code written in them is harder to
write and harder to read. Macros close the gap by letting the logic be written as ordinary
value-level code that happens to *run* at compiletime, which leaves one thing missing: a way to
carry a collection between the two worlds.

That is what the type-level collections are for. A macro parameterized by static data — a list of
names, a mapping — carries that data in a type and *reifies* it to a runtime value:

```scala
reify[TypeList[("one", "two", "three")]]   // List("one", "two", "three")
reify[TypeSet["yes" | "no" | "maybe"]]     // the members of the union
```

Singleton types of `Int`, `String`, `Double` and `Boolean` map to and from their values directly;
a `List` becomes a `Tuple` under `TypeList`, a `Set` a union under `TypeSet`, and a `Map` a tuple
of pairs under `TypeMap`. `reify` also has a form taking a `Type` rather than a static type
parameter, for use from inside a macro implementation where the type is only known abstractly, and
`reifyAs` fixes the result type where inference needs the help.

The data stays in the type system, where macros can inspect it, until the moment a value is
needed.

### Types as source text

Rendering a type in an error message or generated code should produce what the *user* would write —
respecting their imports, using infix syntax where the type is infix. `Syntax.name` does exactly
that, covering the language's full type grammar:

```scala
Syntax.name[Int | String & Double]        // t"Int | String & Double"
Syntax.name[(e: Enumeration) => e.Value]  // t"(e: Enumeration) => e.Value"
Syntax.name[Addable by Int to Double]     // the infix typeclass form
```

Macro error messages built with it read as Scala, not as compiler internals.

### Inspecting expansions

`Introspect.syntax` captures the tree of an expression — as the compiler sees it, tags and nodes
and types — as a value that renders as a navigable diagram, and `Introspect.semantics` does the
same for a symbol's flags and members:

```scala
Introspect.syntax(true):
  println("hello world")
// the full TASTy tree of the expression, inspectable
```

For a macro author, this replaces `println`-archaeology with a structured view of what an
expansion actually produced. Both work from *outside* a macro too, on any expression in ordinary
code, which makes them useful for answering questions about the language itself. Introspecting
`1 + x` for a `val x = 5` shows the `Apply` of a `Select` over a `Literal` and an `Ident`; doing
the same for a `val y: 5 = 5` shows a single `Literal`, because the singleton type let the
compiler fold the addition away. Inside a macro, where a `Quotes` is available, the `syntax` and
`semantics` extension methods do the same for an `Expr` or a `Symbol`, and the result is coloured
through a `TastyPalette` so it reads at a terminal.

A macro that must fail does so with `halt`, which takes a `Message` — so a macro's compile errors
are built from the same structured messages as [runtime errors](errors.md), with their
substitutions highlighted where the compiler's output supports it, and can be attached to a
specific position in the source.

### Down to bytecode

Occasionally the question is not what the compiler *typed* but what it *emitted* — whether a
combinator inlines away, what a hot path compiles to. A classfile parses into typed methods and
instructions, each instruction with its operand-stack state reconstructed, and a quoted expression
can be compiled and disassembled in one step:

```scala
Classfile[SomeType].let(_.methods.find(_.name == t"run")).let(_.bytecode)
// the JVM instructions, with stack states, ready to render
```

This is the foundation for performance work that must verify, rather than assume, what the
optimizer did.
