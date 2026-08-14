## Syntax Highlighting

### About

Scala source code is highlighted by the Scala compiler itself. Rather than approximating the
language with regular expressions, Soundness runs the real tokenizer — and, when asked, the whole
compiler frontend — over the source, so the highlighting is exactly as the compiler reads it. The
result is a `SourceCode` of accented tokens that renders as styled [terminal](terminal.md) output
or as [HTML](html.md), and at the deeper levels carries the *types* of its expressions, the
compiler's diagnostics, and even code completions.

### On highlighting

Scala resists casual highlighting. Its soft keywords are keywords only in position, its given
syntax and significant whitespace defeat pattern-based grammars, and editor highlighting files
chase the language release by release. The one program that always tokenizes Scala correctly is
the compiler — so the reliable highlighter is the compiler, asked politely.

Soundness drives it at three depths: *tokenized*, which classifies tokens with no classpath at
all; *typechecked*, which runs the frontend to attach types and collect diagnostics; and
*compiled*, which runs later phases for the diagnostics only they can produce. Everything comes
from the `soundness` package:

```scala
import soundness.*
```

### Highlighting

`Scala.highlight` tokenizes source into lines of accented tokens — keywords, identifiers, numbers,
strings, modifiers — ready to render:

```scala
val code = Scala.highlight(t"val xs = List(1, 2, 3)")
```

The accents are the categories a palette assigns colors to: `Keyword` and `Modifier` distinguish
modifier keywords from the rest, `Term` and `Typal` mark definitions, `Symbol` and `Parens` cover
operators and brackets, `Number` and `String` the literals (including interpolated ones), and
`Error` and `Unparsed` the tokens that did not come out cleanly.

The default depth is tokenized, which needs nothing but the text; `Java.highlight` does the same
for Java source. A palette given maps the accents to [colors](colors.md), and the ANSI renderers —
numbered or plain — turn a `SourceCode` into terminal output; the Markdown integration renders
fenced `scala` and `java` code blocks in [Markdown](markdown.md) documents through the same
machinery.

### Types and diagnostics

With a compiler and classpath in scope, `typecheckedScala` runs the frontend, and each identifier
carries the type the compiler gave it — the difference between coloring `xs` as an identifier and
knowing it is a `List[Int]`:

```scala
given Scalac[3.8] = Scalac[3.8](Nil)
given LocalClasspath = classpath
import highlighting.typecheckedScala

val typed = Scala.highlight(t"val xs = List(1, 2, 3)")
// the token for xs knows its type is List[Int]
```

Ill-typed source does not fail to highlight — it highlights *with its errors*, each `Diagnostic`
carrying its span and message, which is what documentation tooling and editors need:

```scala
Scala.highlight(t"""val n: Int = "oops"""").diagnostics   // one type error, located
```

### Completions

A caret position turns highlighting into completion: the compiler's own interactive engine
proposes what could follow, with kinds and signatures:

```scala
val source = t"val xs = List(1, 2, 3)\nval y = xs.m"
Scala.highlight(source, caret = source.length.z).completions
// map, max, mkString, … — the members of List[Int]
```

Because the proposals come from the same compiler that will eventually compile the code, they are
never guesses.

Members after a `.` are the easy case. A bare identifier being typed is harder, because the
statement containing it does not typecheck — that is precisely its state while it is being
written — and a batch compilation discards the tree. Completions therefore come from the
compiler's interactive driver, which keeps error trees, so in-scope terms and types complete
wherever the caret is, and in a type position the offers are narrowed to types, modules and
packages.

### Completing keywords and dynamic members

Two kinds of proposal the compiler cannot make are supplied alongside its own.

*Keywords* are grammatical, not semantic: what may follow depends on the tokens before the caret,
not on any symbol table. A trie over reversed token contexts, derived from a corpus of real Scala
and looking back only as far as remains relevant, answers what keywords could come next and what
the grammar expects there — so `case ` offers what a case may begin with, and a position that
must be a type offers no keywords at all.

*Dynamic* members have no symbols to offer at all. Where the caret sits on a selection whose
qualifier derives from `scala.Dynamic`, the type's companion is asked which members its
refinement admits, if it implements the interface for saying so. That is how a
[foreign](foreign-interop.md) value completes with the members its foreign declarations name, and
how any `Dynamic` type can offer honest completions rather than none.
