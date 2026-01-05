## Honeycomb

### About

Honeycomb is a library for constructing, parsing, transforming, and emitting
HTML through a dependently-typed representation.

### Overview

All terms and types are defined in the `soundness` package, and can be imported with:
```scala
import soundness.*
```

To work with HTML, we will represent values with the general `Html` type.
Different HTML node types are represented by subtypes of `Html`, and may be
e`Element`s, `Comment`s, `TextNode`s, a `Doctype` (document type declaration) or
`Fragment`s (sequences of several HTML nodes).

`Comment`s, `TextNode`s and `Doctype` are leaf nodes and have no children.
`Element`s are defined by a tag name, a map of attributes and an ordered
sequence of child nodes, creating a tree structure.

With the exception of `Fragment`, these are all subtypes of `Node`, which
indicates a _single_ HTML node.

The tree structure is called the _document object model_ or _DOM_, and is
expected to follow a structure prescribed by an HTML specification.

This specification is provided through a contextual `Dom` instance which
represents as much of the specification as possible, such as the vocabulary of
tag and attribute names; rules for which elements may be children of which other
elements; how text in different element types should be handled; and various
other details about how different tags behave.

The `Dom` instance also affects rules for parsing and emitting HTML.

Currently only the WHATWG HTML Living Standard specification is supported. It's
what most people mean when they talk about _HTML_ and it should be suitable for
most contemporary purposes. But other specifications, such as the W3C HTML 4.0
specification, could be easily implemented. It should be imported with:
```scala
import doms.html5.whatwg
```

All `Html` values are immutable datatypes. New nodes may be constructed from old
nodes.

### Constructing HTML values

For most purposes, `Html` values may be constructed using builder objects
corresponding to each of the (approximately) 110 tag names defined in the WHATWG
standard, and are applied like function application to support child nodes.

Start by importing them with:
```scala
import whatwg.*
```

These builder objects are all `Tag` instances, and subtypes of `Element`,
initially representing elements with no children or attributes. Applying
attributes as named parameters to `Tag`s constructs elements with attributes,
and applying `Html` values (or a type such as `Text` which can be automatically
converted to `Html`) as repeated arguments adds children to an existing element.


For example,
- `Br` represents an empty `<br>`
- `Img(src = "image.jpg")` represents `<img src="image.jpg">`
- `Div(Hr, P("Hello world"))` represents `<div><hr><p>Hello world</p></div>`
- `Textarea(name = "details", rows = 5)("Details go here")` represents
  `<textarea name="details" rows="5">Details go here</textarea>`

Attributes may be included as a parameter block of repeated named parameters.
Child nodes may be added as a parameter block of repeated `Html` values. Either
parameter block may be independently included (but no more than once each) or
omitted. If both appear, the attributes block must appear first.

This construction is carefully typechecked according to the HTML specification.
Constructions which do not conform to the specification are usually presented as
compile errors.

For example,
- the `Br` tag must not take children, so `Br(Hr)` is a compile error
- the `Li` tag may only appear as a direct child of an `Ol` or `Ul` element, so
  `P(Li)` is a compile error
- `Ul` may not contain text directly, so `Ul("hello")` is a compile error
- `src` is not an attribute of `P`, so `P(src = "image.gif")` is a compile error

These varying capabilities are determined by the static type of the `Tag`.
However, it's not convenient to represent every constraint of the HTML specification, so
certain constructions are permitted even when this is a contravention of the specification.

### Dependent typing

Any runtime HTML value will conform to `Html`. It will also conform to one of
its subtypes such as `Element` or `Comment`. If it has no attributes or
children, it may also be a `Tag`. If it is not a `Fragment` then it will also
conform to `Node`.

Tags which accept children, tags which don't, and _transparent_ tags (such as
`<a>`) have additional trait interfaces mixed in—`Container`, `Void` and
`Transparent` respectively. So an empty element like `<br>` is a `Tag & Void`.

Furthermore, if known, the tag name is encoded in an element's type, so `<br>`
is a `Tag of "br"`, as well as an `Element of "br"` and an `Html of "br"`.

Note that `of` is just an infix type alias, defined as:
```scala
infix type of [target, topic] = target { type Topic = topic }
```

It has the effect of "injecting" a type member called `Topic` into another type. So
`Html of "br"` is the same as `Html { type Topic = "br" }`.

We can also represent an HTML value which we know statically has one of a set of
possible tag names. `Html of "ul" | "ol"` can represent either `<ul>` or `<ol>`
nodes.

Note that `|` has higher precedence than `of` and other non-symbolic infix types.

The type aliases `Flow`, `Phrasing`, `Interactive`, `Embedded`, `Sectioning`,
`ScriptSupporting`, `Metadata` and `Heading` are defined as unions of string
singleton types corresponding to the named content models in the HTML
specification. `Flow` and `Phrasing` are the most useful since many element
types are defined as accepting _flow_ or _phrasing_ content.

For example, `List[Html of Flow](Hr, P("Hello"), Br)` is a list of HTML elements, all of
which are part of the _flow_ content model.

The types of elements that an `Element` may hold as children may also be encoded in its type.
An instance of `Html over "li"` represents an element, such as `Ol` or `Ul`, which can hold
`Li` children. As with `of`, `over` is a type alias which injects another type member called
`Transport`. The two infix types can be combined, so a `Ul` element is an
`Element of "ul" over "li"`.

This same type can be written, `Element over "li" of "ul"` without changing its meaning,
but this isn't conventional.

(The name `Transport` is a generalization of the subject of the `over` preposition, defined
in a more general context. It doesn't make so much sense when applied to HTML.)

Finally, the `in` infix type associates a `Dom` with `Html`, `Element`s and
`Tag`s. For most purposes, the WHATWG `Dom` will be used. Therefore all `Html` elements we
encounter will be `Html in Whatwg` elements.

Including the `Dom` in the type serves two purposes. Primarily, it provides the
context for interpreting the node's `Topic` (i.e. its tag name).

But its presence in the type automatically brings a number of contextual values
into scope. Most notably, this includes context which determines the attributes
that are valid for this particular element.

Together, this provides the expressivity to be very precise about what is
statically known about HTML values. We can write a type like,
`Node & Container of "em" | "i" | "b" over Phrasing in Whatwg` and know that it
referes to a single container node which accepts children that are `Phrasing`
content, is an `<em>`, `<i>` or `<b>` node and is defined in the `Whatwg` `Dom`.

While this expressivity is available, it's usually unnecessary to be so precise.
A type such as `Html of Flow` is often adequate precision.

### Parsing

Textual content may be parsed into `Html` values from any textual source. This includes `Text`
values, `Path`s representing files on disk, `HttpUrl`s, classpath resources, or any other source
for which a `Streamable by Text` typclass exists.

Two generic methods, `load` and `read`, can be used to parse the source as HTML. Of these,
`load` always loads an entire HTML document, with a document type declaration
(if it exists) and a single root `<html>` node. The result type is a `Document[Html]` whose
`root` member is an instance of `Html`.

Both require a `Dom` instance in scope, which will be used to control how parsing works.

```scala
val doc: Document[Html] = url"https://example.com/index.html".load[Html]
```

In contrast, `read` offers more flexibility for parsing fragments of HTML. At
its most generic, we can call `.read[Html]` on any streamable value, such as,
```scala
val content = t"This is some <em>important</em> text"
content.read[Html]
```
and this will produce a value of type `Html in dom`, where `dom` is the type of
the current contextual `Dom`. Indeed, if parsing succeeds, we would not have any
more precise static information about the runtime value it has produced.

The result of `read` will have the runtime value of `Element`, `Comment` or
`TextNode` if the source contains only a single top-level node. If it contains
more, then the return value will be an instance of `Fragment`. The `Html` type
encapsulates all these possibilities.

However, we can be more precise about the nature of the HTML we wish to read by
specifying the tag names we wish to accept (at the top level) as the `Topic`
type member of the `Html` we pass to the `read` method.

If we expect to read an unordered list, we can call, `input.read[Html of "ul"]`.
If we expect the possibility of ordered lists too, we can call, `input.read[Html
of "ul" | "ol"]`. If we want to accept _any flow content_ we can call
`input.read[Html of Flow]`.

This additional static information can influence the parser. It will fail at
runtime if the content does not match the desired type. But it may also parse
input differently depending on which type is expected. (This sort of behavior
is familiar in Scala where an expected return type may influence typechecking.)

For example, reading `"<p>Hello world</p>".read[Html of "p"]` is equivalent to
the construction, `P("Hello world")`. Whereas,
`"<p>Hello world</p>".read[Html of "html"]` is equivalent to the construction,
`Html(Body(P("Hello world")))`. And `"<p>Hello world</p>".read[Html of "ul"]` is
a runtime error.

The HTML specification defines rules for when additional tags may be inferred,
and when they cannot. These rules are encoded in the `Dom` instance.

### Static HTML values

While it's possible to construct `Html` values using factory methods, such as
`Ol(Li("Alpha"), Li("Beta"), Li("Gamma"))`, this may be written as raw HTML
using the `h""` string interpolator, for example
`h"<ol><li>Alpha</li><li>Beta</li><li>Gamma</li></ol>"`.

The contents of the interpolated string are parsed and checked at compiletime,
and any errors will be reported at compiletime.

Standard HTML parsing rules apply inside an interpolated string. Since closing
`</li>` tags are optional according to the specification, the example above may
also be written `h"<ol><li>Alpha<li>Beta<li>Gamma</ol>"`. Both are equivalent to
writing the function application version given first.

At runtime the `Html` value will be instantiated efficiently by constructing the
nodes directly, and no runtime parsing is necessary. So there is no performance
disadvantage to writing HTML code verbatim.

Any static fragments of HTML (i.e. content that is known at compiletime, not
from an external source) are better written this way, since they can be
typechecked with helpful error messages at compiletime.

What's more, `h""` interpolators will return precise types, based on the
content. The type of `h"<p>Hello world</p>"` is `Html of "p"`; the type of
`h"<header/><main>Main</main><footer/>"` is
`Html of "header" | "main" | "footer"`.

The distinction between an `item` value of `Html` and a value of, say, `Html of
"li"` is important to capture: if we wish to apply a constructor to it, say,
`Ul(item)`, that would not typecheck unless we know that the value `item` is
precisely `Html of "li"`.
