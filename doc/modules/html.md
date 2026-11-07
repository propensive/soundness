## HTML

### About

Soundness constructs, parses, transforms, and emits [HTML](https://en.wikipedia.org/wiki/HTML)
through a representation that the compiler understands. An element is built by
applying a tag to its attributes and children; the construction is checked against
the HTML specification as the code compiles, so a `<br>` given children, or an `<li>`
outside a list, is a compile error rather than malformed output.

The same representation reads HTML back from text, files, or a URL, matches it with
patterns, and serializes it — directly or as a stream. Above the level of individual
tags, whole pages can be assembled from reusable parts: a masthead, a hero, a set of
panels, each contributing its own markup, styles, and document metadata.

### On HTML

HTML is a tree of nodes — elements, text, comments — governed by a specification that
says which elements may contain which others, which attributes each element accepts,
and how text inside each is treated. Most libraries hold that tree loosely: any
element may be nested in any other, attributes are arbitrary strings, and a document
that violates the specification is discovered, if at all, only when a browser renders
it wrongly.

Soundness encodes the specification in the types, so an invalid document becomes an
[impossible state](../philosophy/impossible-states.md) rather than something to validate
afterwards. Each tag is a value whose type knows its name, what it may contain, and what
attributes it admits, and the rules of the specification are enforced where the markup is
written. The vocabulary, the
containment rules, and the parsing and emitting behavior all come from a contextual
description of the specification — currently the
[WHATWG HTML Living Standard](https://html.spec.whatwg.org/), what most people mean by
"HTML" today — which other specifications could replace.

Everything comes from the `soundness` package, together with a choice of
specification and the tag vocabulary it defines:

```scala
import soundness.*
import htmlDoms.whatwg
import htmlDoms.whatwg.*
import strategies.throwUnsafely
import attributives.textAttributive
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

Every node is immutable, so transforming one yields a new value rather than altering the old.

### Constructing elements

A builder object exists for each of the roughly 110 tag names in the specification.
Applying attributes to one as named arguments sets those attributes, and applying
`Html` values — or values convertible to HTML, such as `Text` — as further arguments
adds children:

```scala
Br                                  // <br>
Img(src = t"image.jpg")             // <img src="image.jpg">
Div(Hr, P("Hello world"))           // <div><hr><p>Hello world</p></div>
Textarea(name = t"details", rows = 5)("Details go here")
// <textarea name="details" rows="5">Details go here</textarea>
```

Attributes and children form separate argument groups. Either may be omitted, neither
may appear more than once, and the attributes must come before the children.

### Compiletime checking

The construction is typechecked against the specification, and a construction that
does not conform is usually a compile error:

```scala
Br(Hr)                // does not compile: <br> takes no children
P(Li)                 // does not compile: <li> belongs only in <ol> or <ul>
Ul("hello")           // does not compile: <ul> may not contain text directly
```

What an element may hold and which attributes it accepts follow from the static type
of its tag. Not every rule of the specification can be captured this way — some are stated in
terms of more than a simple nesting relationship — and where a rule cannot be encoded, the
checking is *more permissive* rather than more restrictive. So a construction that compiles may in
rare cases still be non-conforming, but a construction the specification allows is never
rejected.

### Writing HTML literally

Markup known as the code is written is better written as itself. The `h"…"`
interpolator parses HTML at compiletime, reports any error where it is written, and
instantiates the nodes directly at runtime, so there is no parsing cost and no loss of
checking:

```scala
h"<ol><li>Alpha</li><li>Beta</li><li>Gamma</li></ol>"
```

Standard parsing rules apply inside the literal, so the optional closing tags the
specification allows may be left out — `h"<ol><li>Alpha<li>Beta<li>Gamma</ol>"` is the
same value. Substitutions are allowed wherever they fit the surrounding markup, and
each is checked in its position:

```scala
val content = t"important"
val location = url"https://example.com/dog.jpg"

h"<p>$content</p>"          // a body-content substitution
h"<img src=$location>"      // an attribute-value substitution
h"<!--$content-->"          // a comment substitution
```

Each position asks something different of the value substituted into it:

```tabular
column  Substitution
column  Must be
row  body content  HTML acceptable as a child of the enclosing element
row  attribute value  a value the attribute accepts
row  comment  showable
```

`src` takes a URL, for instance, so without a `Text` attributive in scope a `Text` in its place
would be rejected.

### Attributes

Attribute values are stored as text but supplied from typed values, because the
specification gives different attributes different value types — some an integer, some
a member of an enumerated set. Each attribute name carries a type describing what it
accepts, so `width`, which takes a positive integer, admits an `Int` and rejects a
`Double`:

```scala
val pixels = 64
Img(width = pixels)   // <img width="64">
```

The set of types accepted out of the box is still incomplete. Until it is filled, the
simplest course is to make `Text` acceptable for every attribute with one import —
`attributives.textAttributive`, already in scope above, which is what lets `src = "image.jpg"`
stand where a URL is expected.

The `class` attribute is awkward, because `class` is a Scala keyword. Rather than
write it in backticks, name the class as a method on the tag:

```scala
import stylesheets.uncheckedClasses

P.info("body text")   // <p class="info">body text</p>
```

`uncheckedClasses` permits any class name. Binding a [stylesheet](css.md) instead makes the
names it defines the only legitimate ones, so a mistyped class is caught as the code compiles,
and a name the stylesheet declares as an id renders as one.

### Fragments

An `Html` value is either an `Html.Node` — exactly one element, comment, or text — or an
`Html.Fragment`, an arbitrary sequence of nodes. Because HTML is a text format, insisting
on exactly one node is usually an artificial constraint, so `Html` admits sequences
everywhere; the `Html.Node` type marks the few places where a single node is genuinely
required. Fragments behave intuitively: one is unwrapped when supplied as children,
and a fragment of a single node equals that node.

### Rendering other types as HTML

The `Renderable` typeclass lets a value of any type stand wherever HTML is expected,
declaring both how it converts and what HTML it produces. A list of text rendered as
list items declares itself to produce `"li"` content:

```scala
given (List[Text] is Renderable in "li") =
  list => Html.Fragment(list.map(Li(_))*)
```

With that in scope a `List[Text]` can be rendered directly, placed inside a `<ul>`, or
substituted into a list — but not into a `<div>`, which cannot hold list items:

```scala
val items = List(t"one", t"two", t"three")
Ul(items)          // a <ul> of three <li> children
h"<ol>$items</ol>"
```

### Transforming

Values are combined and amended with operators; the caret in `^+` and `+^` marks which end the
child joins:

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
// <img src="puppy.jpg" alt="An image of a young dog">
```

Since values are immutable, `image` is unchanged and `described` is the new value.

### Parsing

HTML is read from any textual source — a `Text`, a file path, a URL, a classpath
resource — with `read` or `load`. `load` reads a whole document, returning a
`Document[Html]` with an `Html.Doctype` header and a root `<html>` node:

```scala
val document = t"<html><head><title>Hello</title></head><body><p>Hello</p></body></html>".load[Html]
```

`read` is for fragments. Called as `read[Html]`, it accepts anything from a single
node to several, returning whichever of `Html.Element`, `Html.Comment`, `Html.Text`, or
`Html.Fragment` the source yields:

```scala
val text = t"This is some <em>important</em> text"
text.read[Html]
```

Naming the tags expected at the top level makes the result more precise and guides the
parser. Reading as `Html of "ul"` accepts an unordered list and fails on anything
else; reading as `Html of "ul" | "ol"` accepts either. The specification's rules for
inferring omitted tags apply, so reading `<p>Hello world</p>` as a whole document
supplies the missing `<html>` and `<body>`.

### Source positions

Ordinary parsing keeps no record of where each element sat in the text. When a program needs
that — to underline the element at fault in a linter, say — bringing `parsing.trackPositions`
into scope makes `load` record positions on the `Document[Html]` it returns, and `locate`
resolves a path to the line and column of the element it names:

```scala
import parsing.trackPositions

val tracked = t"<html>\n  <body>\n    <p class=\"x\">Hi</p>\n  </body>\n</html>".load[Html]
val paragraph = Html.Path().element(t"html").element(t"body").element(t"p")
tracked.locate(paragraph).let(_.line)                   // 3
tracked.locate(paragraph.attribute(t"class")).let(_.column)  // 8
```

A path is root-first: each step names a child element by its label and, for the second and
later children with that label, its ordinal — `element(t"p", 2)` is the second paragraph —
and a final `attribute` step names one of the last element's attributes. A position spans
the whole element, from its start tag to the end of its end tag; an element the parser
supplied itself, such as the `<html>` around a bare paragraph, starts at the tag that
prompted it. A path that does not resolve gives `Unset`, as does any lookup on a document
loaded without `parsing.trackPositions` in scope, which records nothing and costs nothing.

### Serializing

An `Html` value converts to text with `show`, which suits most needs:

```scala
Ol(Li("Alpha"), Li("Beta")).show
// t"<ol><li>Alpha</li><li>Beta</li></ol>"
```

For large documents or streaming output, a `Document[Html]` produces a chunked
`Stream[Text]` instead, which runs concurrently and so needs the
ambient concurrency context:

```scala
import probates.cancelProbate
import supervisors.globalSupervisor
import threads.virtualThreads

document.stream[Text]
```

### Pattern matching

The `h"…"` interpolator is also a pattern. A literal pattern matches an equal node,
whether the value is that node or a fragment containing only it, and is checked as
real HTML against the DOM structure rather than the source text — so a pattern written
with closing tags matches input written without them:

```scala
val html = h"<li>two</li>"

html match
  case h"<li>one</li>" => 1
  case h"<li>two</li>" => 2
  case _               => Unset
// 2
```

Holes bind the pieces a pattern captures. Because an `<li>` may hold text or elements,
a captured body is itself `Html`, matched in turn:

```scala
html match
  case h"<li>$value</li>" => value match
    case Html.Text("one") => 1
    case Html.Text("two") => 2
    case _                => Unset
  case _                  => Unset
```

Attributes, comments, and elements can be captured too:

```tabular
column  Pattern
column  Matches
row  `h"<li>$value</li>"`  an `<li>`, binding its body as `Html`
row  `h"<img src=$source>"`  an `<img>`, binding the value of `src`
row  `h"<img $attributes>"`  an `<img>`, binding its attribute map
row  `h"<$element>"`  any element, but no other kind of node
```

### Precise types

A runtime HTML value conforms to `Html` and to one of its subtypes, and its static
type can say far more, through three refinements:

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

`Html of Flow` is any flow content, the level of precision most code wants:

```scala
val nodes: List[Html of Flow] = List(Hr, P("Hello"), Br)
```

The refinements combine into types as exact as
`Container of "em" | "i" | "b" over Phrasing in Whatwg`. The library uses this
precision internally; applications rarely need more than `Html of Flow`.

### Whole pages

A complete page is more than a tree of tags: it has a masthead, perhaps a hero banner,
navigation, side panels, and the document metadata that belongs in the `<head>`.
Soundness assembles a page from reusable parts. A page is a type extending
`Archetype`, supplying its central `content`; each feature is a trait mixed in, and
each trait contributes its own markup, its own styles, and its own head metadata,
which compose in the order the traits are mixed:

```scala
case class HomePage(content: Html of Flow)
extends Archetype, Masthead, Hero(t"Welcome"), StandardMetadata(t"A home page"):
  override def pageTitle = t"Welcome"

HomePage(P("Hello world")).document
```

The assembled value renders to a `Document[Html]` with a leading doctype and an inline
stylesheet gathered from every feature, and it serves directly as an HTTP
`text/html` response, so a handler can return a page with no glue between the HTML and
the server.
