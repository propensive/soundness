## Syntax Highlighting

### About

Scala source code is highlighted by the Scala compiler itself. The language is not
approximated with regular expressions. The real tokenizer, and, when asked, the whole front
end of the compiler, is run over the source, so the highlighting is exactly as the compiler
reads it.

The result is a `SourceCode` of accented tokens, which is displayed as styled
[terminal](terminal.md#styled-text) output, or as [HTML](html.md). At the deeper levels it
has the *types* of its expressions, the compiler's diagnostics, and even completions.

### On highlighting

Scala resists casual highlighting. Its soft keywords are keywords only in certain positions.
Its syntax for givens, and its significant whitespace, defeat grammars based on patterns.
The highlighting files of editors chase the language, release by release. The one program
which always tokenizes Scala correctly is the compiler, so the reliable highlighter is the
compiler, asked politely.

It is driven at three depths, each of which is chosen by a given in the package
`highlighting`:

```tabular
column  Depth
column  Given
column  Needs
column  Gives
row  tokenized  `tokenizedScala`, the default  the text alone  classified tokens
row  typechecked  `typecheckedScala`  a compiler and a classpath  types and diagnostics, from the frontend
row  typechecked  `sessionScala`  a warm compiler session  the same, within the session
row  compiled  `compiledScala`  a compiler and a classpath  the diagnostics only later phases produce
```

A highlighted token which knows its type is
[composability](../philosophy/composability.md) between the compiler and whatever displays
the code.

Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Highlighting

`Scala.highlight` tokenizes source into lines of accented tokens, such as keywords,
identifiers, numbers, strings and modifiers, ready to display:

```scala
val code = Scala.highlight(t"val xs = List(1, 2, 3)")
```

The accents are the categories to which a palette assigns colors:

```tabular
column  Accent
column  Marks
row  `Keyword`  keywords, other than modifiers
row  `Modifier`  modifier keywords
row  `Term`, `Typal`  definitions
row  `Symbol`  operators
row  `Parens`  brackets
row  `Number`, `String`  literals, including interpolated ones
row  `Error`, `Unparsed`  tokens that did not come out cleanly
```

The default depth is tokenized, which needs nothing but the text. `Java.highlight` does the
same for Java source.

A palette, as a given, maps the accents to [colors](colors.md), and the ANSI renderers,
numbered or plain, turn a `SourceCode` into terminal output. Fenced `scala` and `java` code
blocks in a [Markdown](markdown.md) document are displayed through the same machinery.

### Types and diagnostics

With a [compiler](compiler.md) and a [classpath](classpath.md) in scope, `typecheckedScala`
runs the front end, and each identifier has the type which the compiler gave it. That is the
difference between coloring `xs` as an identifier, and knowing that it is a `List[Int]`:

```scala fuel
typecheck
##
given scalac: Scalac[3.8, Universe.Classfile] = Scalac[3.8](Nil)
given classpath: LocalClasspath = LocalClasspath()
import highlighting.typecheckedScala

val typed = Scala.highlight(t"val xs = List(1, 2, 3)")
```

`typecheckedScala` comes from the module `harlequin.typed`, which is the only part of the
highlighter that depends on the compiler.

Source which is ill-typed does not fail to highlight. It is highlighted *with its errors*.
Each `Diagnostic` has its span and its message, which is what documentation tools and
editors need:

```scala
val diagnostics = Scala.highlight(t"""val n: Int = "oops"""").diagnostics
```

### Completions

A caret position turns highlighting into completion. The compiler's own interactive engine
proposes what could follow, with kinds and signatures:

```scala
val source = t"val xs = List(1, 2, 3)\nval y = xs.m"
val completions = Scala.highlight(source, caret = source.length.z).completions
```

Under a typechecked depth, the proposals here are `map`, `max`, `mkString` and the other
members of `List[Int]` which begin with `m`. They come from the same compiler which will
eventually compile the code, so they are never guesses.

Members after a `.` are the easy case. A bare identifier which is being typed is harder,
because the statement which contains it does not typecheck. That is precisely its state
while it is being written, and a batch compilation discards the code. Completions therefore
come from the compiler's interactive driver, which keeps code that has errors. Terms and
types which are in scope are completed wherever the caret is, and in the position of a type,
the offers are narrowed to types, modules and packages.

### Completing keywords and dynamic members

Two kinds of proposal which the compiler cannot make are supplied beside its own.

*Keywords* are grammatical, not semantic. What may follow depends on the tokens before the
caret, not on any table of symbols. A trie over reversed contexts of tokens, derived from a
corpus of real Scala, and looking back only as far as remains relevant, answers which
keywords could come next, and what the grammar expects there. `case ` therefore offers what
a case may begin with, and a position which must be a type offers no keywords at all.

*Dynamic* members have no symbols to offer at all. Where the caret sits on a selection whose
qualifier derives from `scala.Dynamic`, the companion of the type is asked which members its
refinement admits, if it implements the interface for saying so. That is how a
[foreign](foreign-interop.md) value is completed with the members which its foreign
declarations name, and how any `Dynamic` type can offer honest completions, instead of none.

### Related

- [Compiler](compiler.md) describes running the Scala compiler from a program.
- [LSP](lsp.md) serves completions and diagnostics to an editor.
- [Markdown](markdown.md) displays highlighted code blocks.
- [Terminal](terminal.md) and [HTML](html.md) are the two forms of output.
