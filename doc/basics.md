Markdown may be read from a source such as a string or file with the `Markdown.parse` method. For example,
```scala
import punctuation.*
val md = Markdown.parse(t"## This is a subheading")
```
will return an instance of `Markdown[Block]`, which is a wrapper for a sequence of block-level Markdown
AST elements. In the example above, the result would be equal to,
`Markdown(Heading(2, Textual(t"This is a subheading")))`.

Block-level AST elements are:
- `ThematicBreak()`
- `Paragraph(inline*)`
- `Heading(level, inline*)`
- `FencedCode(lang, meta, text)`
- `BulletList(numbered, loose, items*)`
- `BlockQuote(block*)`
- `Reference(id, location)`
- `Table(parts*)`

Using [Honeycomb](https://github.com/propensive/honeycomb/) a `Markdown[Block]` instance may be converted
to HTML just by calling the `html` extension method on it. This extension method returns an instance of
`Seq[Html[Flow]].

### Inline Markdown

Often block-level Markdown elements are not desired, and the subset of Markdown that is valid in "inline"
contexts is required. The `Markdown.parseInline` method will parse an inline fragment of Markdown, returning
an instance of `Markdown[Inline]`.

Inline AST elements are:
- `Break()`
- `Emphasis(inline*)`
- `HtmlNode(text)`
- `Image(alt, src)`
- `Code(text)`
- `Strong(inline*)`
- `Textual(text)`
- `Link(location, inline*)`

Likewise, a `Markdown[Inline]` instance can be converted to HTML with the `html` extension method. Since
inline Markdown uses a more limited set of HTML tags, this method returns a `Seq[Html[Phrasing]]` which
allows it to be embedded within nodes such as `P`, which would not be the case for block-level Markdown
converted to HTML.

### `md""` Interpolator

In addition to parsing Markdown at runtime, it's possible to construct Markdown literals with the `md""`
interpolator. This is as simple as writing markdown inside quotes preceded by `md`, and it will be
parsed at compiletime. For example, `md"This is _emphasised_."`

The type of a `md""` literal will depend upon its content. If it includes block-level elements, its
return type will be `Markdown[Block]`, whereas if it uses only inline elements, it will be typed as
`Markdown[Inline]`.

Any errors in the Markdown content will be detected at compiletime, causing compile errors.

Substitutions into Markdown literals are also supported, provided the type of the substitution can be
converted to Markdown, and can appear at the position it's substituted. Conversion of a particular type
to Markdown is permitted by the existence of a contextual typeclass instance.


