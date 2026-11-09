## HTML

### About

[HTML](https://en.wikipedia.org/wiki/HTML) is constructed, parsed, transformed and written
out through a representation which the compiler understands. An element is built by applying
a tag to its attributes and children. The construction is checked against the HTML
specification at [compiletime], so a `<br>` which is given children, or an `<li>` outside a
list, is a compile error, not malformed output.

The same representation reads HTML back from text, from files, or from a URL, matches it
with patterns, and writes it out, directly or as a stream. Above the level of single tags,
whole pages are assembled from reusable parts: a masthead, a hero banner and a set of panels,
each of which contributes its own markup, styles and metadata.

### On HTML

HTML is a tree of nodes, which are elements, text and comments. It is governed by a
specification, which says which elements may contain which others, which attributes each
element accepts, and how the text inside each is treated. Most libraries hold that tree
loosely. Any element may be nested in any other, and attributes are arbitrary strings. A
document which violates the specification is discovered only when a browser displays it
wrongly, if then.

Here the specification is encoded in the types, so an invalid document becomes an
[impossible state](../philosophy/impossible-states.md), not something to validate
afterwards. Each tag is a value whose type knows its name, what it may contain, and what
attributes it admits, and the rules of the specification are enforced where the markup is
written.

The vocabulary, the rules of containment, and the behavior of parsing and writing all come
from a contextual description of the specification. That is currently the
[WHATWG HTML Living Standard](https://html.spec.whatwg.org/), which is what most people mean
by "HTML" today, and which other specifications could replace.

Everything comes from the `soundness` package, together with a choice of specification, and
the vocabulary of tags which it defines. The last import lets the samples print their
results:

```scala
import soundness.*
import htmlDoms.whatwg
import htmlDoms.whatwg.*
import strategies.throwUnsafely
import attributives.textAttributive
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

An HTML value has the general type `Html`, and its kinds of node have types in the `Html`
companion:

```tabular
column  Type
column  Node
row  `Html.Element`  an element, with its attributes and children
row  `Html.Text`  text
row  `Html.Comment`  a comment
row  `Html.Doctype`  a doctype
row  `Html.Fragment`  a sequence of several nodes
```

Every node is [immutable], so transforming one gives a new value, and leaves the old one as
it was.

### Constructing elements

A builder object exists for each of the roughly 110 tag names in the specification. Applying
one to named arguments sets those attributes. Applying it to `Html` values, or to values
which convert to HTML, such as `Text`, adds children:

```scala fuel
run
##
Out.println(Br.show)
Out.println(Img(src = t"image.jpg").show)
Out.println(Div(Hr, P("Hello world")).show)
Out.println(Textarea(name = t"details", rows = 5)("Details go here").show)
```

Attributes and children form separate groups of arguments. Either may be omitted, neither
may appear more than once, and the attributes must come before the children.

### Compiletime checking

The construction is typechecked against the specification, and a construction which does
not conform is usually a compile error. A `<br>` takes no children:

```scala fuel
compile
errors show
##
Br(Hr)
```

An `<li>` belongs only in an `<ol>` or a `<ul>`:

```scala fuel
compile
errors show
##
P(Li)
```

And a `<ul>` may not contain text directly:

```scala fuel
compile
errors show
##
Ul("hello")
```

What an element may hold, and which attributes it accepts, follow from the static type of
its tag. Not every rule of the specification can be captured in that way, since some are
stated in terms of more than a simple relationship of nesting. Where a rule cannot be
encoded, the checking is *more permissive*, not more restrictive. A construction which
compiles may therefore, in rare cases, still not conform. But a construction which the
specification allows is never rejected.

### Writing HTML literally

Markup which is known when the code is written is better written as itself. The `h"…"`
[interpolator] parses HTML at compiletime, and reports any error where it is written. It
builds the nodes directly at [runtime], so there is no cost of parsing, and no loss of
checking:

```scala
val list = h"<ol><li>Alpha</li><li>Beta</li><li>Gamma</li></ol>"
```

The standard rules of parsing apply inside the literal, so the closing tags which the
specification makes optional may be left out, and the result is the same value:

```scala fuel
run
##
Out.println((h"<ol><li>Alpha<li>Beta<li>Gamma</ol>" == list).inspect)
```

Markup which the specification does not allow is rejected in a literal too:

```scala fuel
compile
errors show
##
h"<ul><p>Alpha</p></ul>"
```

Substitutions are allowed wherever they fit the surrounding markup, and each is checked in
its position:

```scala
val content = t"important"
val location = url"https://example.com/dog.jpg"
```

```scala fuel
run
##
Out.println(h"<p>$content</p>".show)
Out.println(h"<img src=$location>".show)
Out.println(h"<!--$content-->".show)
```

Each position asks something different of the value which is substituted into it:

```tabular
column  Substitution
column  Must be
row  body content  HTML acceptable as a child of the enclosing element
row  attribute value  a value the attribute accepts
row  comment  showable
```

`src` takes a [URL](network-addresses.md#urls), for instance. Without a `Text` attributive
in scope, a `Text` in its place would be rejected.

### Attributes

The values of attributes are stored as text, but are supplied from typed values, because the
specification gives different attributes different types of value. Some take an integer, and
some a member of a fixed set. Each attribute name has a type which describes what it
accepts. `width` takes a positive integer, so it admits an `Int`:

```scala
val pixels = 64
```

```scala fuel
run
##
Out.println(Img(width = pixels).show)
```

It rejects a `Double`:

```scala fuel
compile
errors show
##
Img(width = 64.5)
```

The set of types which are accepted without an import is still incomplete. Until it is
filled, the simplest course is to make `Text` acceptable for every attribute, with one
import. That is `attributives.textAttributive`, which is already in scope above, and is what
lets `src = t"image.jpg"` stand where a URL is expected.

The `class` attribute is awkward, because `class` is a keyword in Scala. Instead of writing
it in backticks, name the class as a method on the tag:

```scala
import stylesheets.uncheckedClasses
```

```scala fuel
run
##
Out.println(P.info("body text").show)
```

`uncheckedClasses` permits any class name. Binding a [stylesheet](css.md) instead makes the
names which it defines the only legitimate ones. A mistyped class is then caught as the code
compiles, and a name which the stylesheet declares as an id is written as one.
[Names](names.md#built-in-planes) describes the rules for a class name and an id.

### Fragments

An `Html` value is either an `Html.Node`, which is exactly one element, comment or text, or
an `Html.Fragment`, which is an arbitrary sequence of nodes. HTML is a text format, so
insisting on exactly one node is usually an artificial constraint, and `Html` admits
sequences everywhere. The type `Html.Node` marks the few places where a single node really
is required. Fragments behave intuitively. One is unwrapped when it is supplied as children,
and a fragment of a single node equals that node.

### Rendering other types as HTML

The `Renderable` [typeclass] lets a value of any type stand wherever HTML is expected. An
instance declares both how the value converts, and what HTML it produces. A list of text
which is rendered as list items declares that it produces `"li"` content:

```scala
given listRenderable: (List[Text] is Renderable in "li") =
  list => Html.Fragment(list.map(Li(_))*)

val items = List(t"one", t"two", t"three")
```

With that in scope, a `List[Text]` can be placed inside a `<ul>`:

```scala fuel
run
##
Out.println(Ul(items).show)
```

It cannot be placed in a `<div>`, which cannot hold list items.

### Transforming

Values are combined and amended with operators. The caret in `^+` and `+^` marks the end at
which the child joins:

```tabular
column  Operation
column  Applies to
column  Gives
row  `a + b`  any two `Html` values  a fragment of both
row  `element ^+ child`  an `Html.Element`, statically known  the element with a new first child
row  `element +^ child`  an `Html.Element`, statically known  the element with a new last child
row  `element.name = value`  an `Html.Element`  the element with the attribute added or replaced
```

Here an attribute is added by assignment:

```scala
val image: Html.Element of "img" in Whatwg = Img(src = t"puppy.jpg")
val described = image.alt = t"An image of a young dog"
```

```scala fuel
run
##
Out.println(image.show)
Out.println(described.show)
```

Values are immutable, so `image` is unchanged, and `described` is the new value.

### Parsing

HTML is read from any textual source, such as a `Text`, a
[file](filesystem.md#reading-and-writing), a URL or a [classpath](classpath.md) resource,
with `read` or `load`. `load` reads a whole document. It gives a `Document[Html]`, with an
`Html.Doctype` header and a root `<html>` node:

```scala
val document = t"<html><head><title>Hello</title></head><body><p>Hello</p></body></html>".load[Html]
```

`read` is for fragments. Called as `read[Html]`, it accepts anything from a single node to
several, and gives whichever of `Html.Element`, `Html.Comment`, `Html.Text` or
`Html.Fragment` the source yields:

```scala
val text = t"This is some <em>important</em> text"
```

```scala fuel
run
##
Out.println(text.read[Html].show)
```

Naming the tags which are expected at the top level makes the result more precise, and
guides the parser. Reading as `Html of "ul"` accepts an unordered list, and fails on
anything else. Reading as `Html of "ul" | "ol"` accepts either. The specification's rules
for inferring omitted tags apply, so reading `<p>Hello world</p>` as a whole document
supplies the missing `<html>` and `<body>`.

### Source positions

Ordinary parsing keeps no record of where each element sat in the text. A program which
needs that, to underline the element at fault in a linter, say, brings
`parsing.trackPositions` into scope. `load` then records positions on the `Document[Html]`
which it returns, and `locate` resolves a path to the line and column of the element which
it names:

```scala
import parsing.trackPositions

val tracked = t"<html>\n  <body>\n    <p class=\"x\">Hi</p>\n  </body>\n</html>".load[Html]
val paragraph = Html.Path().element(t"html").element(t"body").element(t"p")
```

```scala fuel
run
##
Out.println(tracked.locate(paragraph).let(_.line).inspect)
Out.println(tracked.locate(paragraph.attribute(t"class")).let(_.column).inspect)
```

A path begins at the root. Each step names a child element by its label and, for the second
and later children with that label, its position, so `element(t"p", 2)` is the second
paragraph. A final `attribute` step names one of the last element's attributes.

A position spans the whole element, from its start tag to the end of its end tag. An
element which the parser supplied itself, such as the `<html>` around a bare paragraph,
starts at the tag which prompted it. A path which does not resolve gives `Unset`. So does
any lookup on a document which was loaded without `parsing.trackPositions` in scope, which
records nothing, and costs nothing. [JSON](json.md#source-positions) tracks positions in the
same way.

### Serializing

`show` converts an `Html` value to text, which suits most needs:

```scala fuel
run
##
Out.println(Ol(Li("Alpha"), Li("Beta")).show)
```

For a large document, or for streaming output, a `Document[Html]` gives a
[`Stream[Text]`](streams.md) in chunks instead. That runs concurrently, so it needs the
[concurrency](concurrency.md) context in scope:

```scala fuel
parse
##
import probates.cancelProbate
import supervisors.globalSupervisor
import threads.virtualThreads

document.stream[Text]
```

### Pattern matching

The `h"…"` interpolator is also a pattern. A literal pattern matches an equal node, whether
the value is that node, or a fragment which contains only it. It is checked as real HTML,
against the structure of the nodes, not against the source text, so a pattern which is
written with closing tags matches input which was written without them:

```scala
val html = h"<li>two</li>"

val matched = html match
  case h"<li>one</li>" => 1
  case h"<li>two</li>" => 2
  case _               => 0
```

Substitutions bind the pieces which a pattern captures. An `<li>` may hold text or
elements, so a captured body is itself `Html`, which is matched in turn:

```scala
val nested = html match
  case h"<li>$value</li>" => value match
    case Html.Text("one") => 1
    case Html.Text("two") => 2
    case _                => 0
  case _                  => 0
```

```scala fuel
run
##
Out.println(matched.show)
Out.println(nested.show)
```

Attributes, comments and elements can be captured too:

```tabular
column  Pattern
column  Matches
row  `h"<li>$value</li>"`  an `<li>`, binding its body as `Html`
row  `h"<img src=$source>"`  an `<img>`, binding the value of `src`
row  `h"<img $attributes>"`  an `<img>`, binding its attribute map
row  `h"<$element>"`  any element, but no other kind of node
```

[Interpolation](interpolation.md#patterns) explains how one literal serves as both an
expression and a pattern.

### Precise types

An HTML value at runtime conforms to `Html`, and to one of its subtypes. Its static type
can say far more, through three refinements:

```tabular
column  Refinement
column  Records
column  Example
row  `of`  the tag name, or a union of names  `Html of "br"`, `Html of "ul" | "ol"`
row  `over`  what the element may contain  `Html.Element of "ul" over "li"`
row  `in`  the specification in force  `Html in Whatwg`
```

The content models of the specification are unions of tag names, ready to use:

```tabular
column  Content model
column  Includes, for example
row  `Flow`  `div`, `p`, `ul`, `table`, and all phrasing, heading and sectioning content
row  `Phrasing`  `em`, `span`, `a`, `img`, `br`, and text
row  `Interactive`  `a`, `button`, `input`, `select`, `details`
row  `Embedded`  `img`, `video`, `iframe`, `svg`, `math`
row  `Heading`  `h1` to `h6`, `hgroup`
row  `Sectioning`  `article`, `aside`, `nav`, `section`
row  `Metadata`  `title`, `meta`, `link`, `style`, `script`
row  `ScriptSupporting`  `script`, `template`
```

`Html of Flow` is any flow content, which is the level of precision that most code wants:

```scala
val nodes: List[Html of Flow] = List(Hr, P("Hello"), Br)
```

The refinements combine into types as exact as
`Container of "em" | "i" | "b" over Phrasing in Whatwg`. That precision is used internally.
Applications rarely need more than `Html of Flow`.

### Whole pages

A complete page is more than a tree of tags. It has a masthead, perhaps a hero banner,
navigation, side panels, and the metadata which belongs in the `<head>`. A page is assembled
from reusable parts. It is a type which extends `Archetype`, and supplies its central
`content`. Each feature is a trait which is mixed in, and each trait contributes its own
markup, its own styles and its own metadata for the head, which compose in the order in
which the traits are mixed:

```scala
case class HomePage(content: Html of Flow)
extends Archetype, Masthead, Hero(t"Welcome"), StandardMetadata(t"A home page"):
  override def pageTitle = t"Welcome"

val page = HomePage(P("Hello world")).document
```

The assembled value is a `Document[Html]`, with a leading doctype, and an inline stylesheet
gathered from every feature. It serves directly as a `text/html` response from an
[HTTP server](http-server.md#responses), so a handler can return a page, with no glue
between the HTML and the server. [Pages](pages.md) describes the parts in full.

### Related

- [CSS](css.md) writes the stylesheets which pages use, and checks class names.
- [DOM](dom.md) works with a live document in a browser.
- [XML](xml.md) and [SVG](svg.md) use the same kinds of node for other vocabularies.
- [Markdown](markdown.md) converts prose to HTML.
- [Forms](forms.md) generates an HTML form from a case class.
- [Web automation](web-automation.md) drives a browser over real pages.
