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
