## Metaprogramming

### About

Much of the machinery which runs at [compiletime] here rests on a small set of tools for writing
and debugging macros. They are available to any project which does the same:

```tabular
column  Tool
column  Does
row  `every`  collects *all* the given instances of a type visible at a call site
row  `reify`  turns a type-level collection into a runtime value
row  `Syntax.name`  renders a type as it would be written under the caller's imports
row  `Introspect`  displays a macro expansion as an inspectable tree
row  `Classfile`  reads compiled bytecode back, for the rare work that must see what the compiler produced
```

Collecting every given is the mechanism behind pluggable configuration throughout these
libraries.

### On metaprogramming

Scala 3's macros are principled, and short of tools. A macro author stares at expansions with
`println`, renders types with a `show` which ignores the caller's imports, and cannot ask an
innocent question, "which givens of this type are in scope?", because implicit search answers
with one given, or with an ambiguity. These gaps are generic, and every macro project fills them
again.

Here they are filled once. Macros which check code as it compiles are how much of
[safety by construction](../philosophy/safety-by-construction.md) is achieved elsewhere.

Everything comes from the `soundness` package, and the second import lets the samples print
their results:

```scala
import soundness.*
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Every given in scope

Implicit search finds *the* [given]. `every` finds *all of them*. It is a compiletime search
which returns their list, and it includes instances which would be ambiguous against each
other:

```scala
case class Plugin(name: Text)

given alpha: Plugin = Plugin(t"alpha")
given beta: Plugin = Plugin(t"beta")
```

```scala fuel
run
##
every[Plugin].values.each(plugin => Out.println(plugin.name))
```

This inverts the usual flow of configuration. Instead of one value which names all the options,
each option is declared as a given where it applies, and the consumer collects whatever is in
scope. It is how [socket options](sockets.md) accumulate, how every [log sink](logging.md)
receives events, and how [terminal features](terminal.md) are switched on.

### Type-level values

A type parameter is to typechecking what an ordinary parameter is to execution: a way of passing
information to the place which will use it. Types are the poorer medium. Type-level operations
are far less expressive than those on values, and code written in them is harder to write and to
read. Macros close the gap. The logic is written as ordinary code over values, which happens to
*run* at compiletime. One thing is then missing: a way to carry a collection between the two
worlds.

The type-level collections do that. A macro which is parameterized by static data, such as a
list of names or a mapping, carries the data in a type, and *reifies* it to a runtime value:

```scala fuel
run
##
Out.println(reify[TypeList[("one", "two", "three")]].inspect)
Out.println(reify[TypeSet["yes" | "no" | "maybe"]].inspect)
```

Each runtime value has a type-level form:

```tabular
column  Value
column  Type-level form
row  an `Int`, `String`, `Double` or `Boolean`  its singleton type
row  a `List`  a `Tuple`, under `TypeList`
row  a `Set`  a union, under `TypeSet`
row  a `Map`  a tuple of pairs, under `TypeMap`
```

`reify` has a second form, which takes a `Type` rather than a static type parameter, for use
inside a macro implementation where the type is known only abstractly. `reifyAs` fixes the type
of the result where inference needs the help.

The data stays in the type system, where macros can inspect it, until the moment a value is
needed.

### Abstracting over containers

Many types are *containers* in the same sense. An [`Optional`](optional-values.md), an
`Attempt`, a [`Task`](concurrency.md) and a [collection](collections.md) all allow a value to be
put in, mapped over, and flat-mapped through. Writing an operation once over all of them needs
those capabilities as values, and not as methods which happen to share names.

`Functor` supplies `point`, which lifts a value into the container, and mapping. `Monad` adds
`bind`, which is `flatMap` under a name which does not collide with the method the container
already has. Instances are constructed for any type which provides the needed methods, so a
container written without any of this in mind takes part anyway.

Two operations follow at once, and are the reason this is worth having. `sequence` turns a
collection of containers inside out:

```scala fuel
run
##
Out.println(List[Option[Int]](Some(1), Some(2), Some(3)).sequence.inspect)
Out.println(List[Option[Int]](Some(1), None, Some(3)).sequence.inspect)
```

`traverse` maps a function which may fail over a collection, and gathers the results in one
container, not in a collection of containers. This is the shape of "parse all of these, and fail
if any of them fails":

```scala
def parseNumber(text: Text): Option[Int] = safely(text.as[Int]).let(Some(_)).or(None)
```

```scala fuel
run
##
Out.println(List(t"1", t"2", t"3").traverse(parseNumber).inspect)
Out.println(List(t"1", t"2", t"x").traverse(parseNumber).inspect)
```

Both work on any collection, and the result keeps the shape of the collection. A `List` of
`Option`s sequences to an `Option` of a `List`, and a `Set` to an `Option` of a `Set`.

### Types as source text

A type rendered in an error message or in generated code should read as the *user* would write
it, with their imports respected and infix syntax used where the type is infix. `Syntax.name`
does that, and covers the full grammar of types:

```scala fuel
run
##
Out.println(Syntax.name[Int | String & Double])
Out.println(Syntax.name[(text: Text) => text.type])
Out.println(Syntax.name[Addable by Int to Double])
```

Error messages from a macro built with it read as Scala, not as compiler internals.

### Inspecting expansions

`Introspect.syntax` captures the tree of an expression, as the compiler sees it, with its tags,
nodes and types. The result is a value which renders as a navigable diagram.
`Introspect.semantics` does the same for the flags and members of a symbol:

```scala
val x = 5

val tree = Introspect.syntax(true):
  1 + x
```

For a macro author, this replaces archaeology with `println` by a structured view of what an
expansion actually produced. Both work from *outside* a macro too, on any expression in ordinary
code, which makes them useful for answering questions about the language itself. Introspecting
`1 + x`, as above, shows an `Apply` of a `Select` over a `Literal` and an `Ident`. Doing the
same for a `val y: 5 = 5` shows a single `Literal`, because the [singleton type] let the
compiler fold the addition away.

Inside a macro, where a `Quotes` is available, the `syntax` and `semantics` extension methods do
the same for an `Expr` or a `Symbol`. The result is colored through a `TastyPalette`, so it
reads well at a terminal.

A macro which must fail does so with `halt`, which takes a `Message`. The compile errors of a
macro are therefore built from the same structured messages as [runtime errors](errors.md),
with their substitutions highlighted where the compiler's output supports it, and they can be
attached to a position in the source.

### Down to bytecode

Occasionally the question is not what the compiler *typed*, but what it *emitted*: whether a
combinator inlines away, or what a hot path compiles to. A classfile parses into typed methods
and instructions, and each instruction carries the state of the operand stack, reconstructed:

```scala
import classloaders.threadContextClassloader

val methods = Classfile[Exit].let(_.methods.map(_.name))
```

The `bytecode` of each method is its JVM instructions, with their stack states, ready to render.
A quoted expression can also be compiled and disassembled in one step. This is the foundation
for performance work which must verify what the optimizer did, and not assume it.

### Related

- [Staging](staging.md) runs quoted code in another environment.
- [Derivation](derivation.md) generates typeclass instances from the structure of a type.
- [Annotations](annotations.md) reads annotations without a macro.
- [Compiler](compiler.md) runs the Scala compiler as a library.
- [Classpath](classpath.md) describes the classloaders which a classfile is read through.
- [Interpolation](interpolation.md) and [records](records.md) are built with these tools.
