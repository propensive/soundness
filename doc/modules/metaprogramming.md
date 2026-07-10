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

A macro parameterized by static data — a list of names, a mapping — can carry that data in a type
and *reify* it to a runtime value:

```scala
reify[TypeList[("one", "two", "three")]]   // List("one", "two", "three")
reify[TypeSet["yes" | "no" | "maybe"]]     // the members of the union
```

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
expansion actually produced.

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
