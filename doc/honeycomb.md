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
`Element`s, `Comment`s, `TextNode`s, a `Doctype` (document type declaration) or
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
`<a>`) have additional trait interfaces mixed in—`Tag.Container`, `Tag.Void` and
`Tag.Transparent` respectively. So an empty element like `<br>` is a `Tag & Void`.

Furthermore, if known, the tag name is encoded in an element's type, so `<br>`
is a `Tag of "br"`, as well as an `Element of "br"` and an `Html of "br"`.

Note that `of` is just an infix type alias, defined as:
```scala
infix type of [target, topic] = target { type Topic = topic }
```

It has the effect of "injecting" a type member called `Topic` into another type.
So `Html of "br"` is an alias of `Html { type Topic = "br" }`.

We can also represent an HTML value which we know statically has one of a set of
possible tag names. `Html of "ul" | "ol"` can represent either `<ul>` or `<ol>`
nodes.

Note that `|` has higher precedence than `of` and other non-symbolic infix
types, so we can write this type without parentheses.

The type aliases `Flow`, `Phrasing`, `Interactive`, `Embedded`, `Sectioning`,
`ScriptSupporting`, `Metadata` and `Heading` are defined as unions of string
singleton types corresponding to the named content models in the HTML
specification. `Flow` and `Phrasing` are the most useful since many element
types are defined as accepting _flow_ or _phrasing_ content.

For example, `List[Html of Flow](Hr, P("Hello"), Br)` is a list of HTML elements, all of
which are part of the _flow_ content model.

The types of elements that an `Element` may hold as children may also be encoded
in its type. An instance of `Html over "li"` represents an element, such as `Ol`
or `Ul`, which can hold `Li` children. As with `of`, `over` is a type alias
which injects another type member called `Transport`. The two infix types can be
combined, so a `Ul` element is an `Element of "ul" over "li"`.

This same type can be written as `Element over "li" of "ul"` without changing
its meaning—when expanded to type members, the types are identical—but this
isn't conventional.

(The name `Transport` is a generalization of the subject for the `over`
prepositional type alias, which is defined in a more general context. It doesn't
make so much sense when applied to HTML.)

Finally, the `in` infix type associates a `Dom` with `Html`, `Element`s and
`Tag`s. For most purposes, the WHATWG `Dom` will be used. Therefore all `Html` elements we
encounter will be `Html in Whatwg` elements.

Including the `Dom` as part of the type serves two purposes. Primarily, it
provides the context for interpreting the node's `Topic` (i.e. its tag name).

But its presence in the type automatically brings a number of contextual values
into scope. Most notably, this includes context which determines the attributes
that are valid for this particular element.

Together, this provides the expressivity to be very precise about what is
statically known about HTML values. We can write a type like,
`Node & Container of "em" | "i" | "b" over Phrasing in Whatwg` and know that it
refers to a single container node that accepts children that are `Phrasing`
content, is an `<em>`, `<i>` or `<b>` node and is defined in the `Whatwg` `Dom`.

While this expressivity is available, and the Honeycomb API uses it for
precision, it's usually unnecessary to be so specific in user applications. A
type such as `Html of Flow` is often adequate precision.

### Parsing

Textual content may be parsed into `Html` values from any textual source. This
includes `Text` values, `Path`s representing files on disk, `HttpUrl`s,
classpath resources, or any other source for which a `Streamable by Text`
typclass exists.

Two generic methods, `load` and `read`, can be used to parse the source as HTML. Of these,
`load` always loads an entire HTML document, with a document type declaration
(if it exists) and a single root `<html>` node. The result type is a `Document[Html]` whose
`root` member is an instance of `Html` and whose `header` is a `Doctype`.

Both require a `Dom` instance in scope, which controls how parsing works.

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
If we expect the possibility of ordered lists too, we can call,
`input.read[Html of "ul" | "ol"]`. If we want to accept _any flow content_ we
can call `input.read[Html of Flow]` and get an `Html of Flow in dom`.

This additional static information can influence the parser. It can fail at
runtime if the content does not match the desired type. But it may also parse
input differently depending on which type is expected.

For example, calling `"<p>Hello world</p>".read[Html of "p"]` is equivalent to
the construction, `P("Hello world")`. Whereas,
`"<p>Hello world</p>".read[Html of "html"]` is equivalent to the construction,
`Html(Body(P("Hello world")))`. And `"<p>Hello world</p>".read[Html of "ul"]` is
a runtime error.

The HTML specification defines rules for when additional tags may be
inferred—inserted into the DOM when they don't explicitly appear in the
source—and when they cannot. These rules are encoded in the `Dom` instance.

### Static HTML values

While it's possible to construct `Html` values using factory methods, such as
`Ol(Li("Alpha"), Li("Beta"), Li("Gamma"))`, this may also be written as raw HTML
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

Interpolators can additionally include substitutions, provided these appear in a
suitable position in the HTML content.

These positions include:
 - body content, e.g. `h"<p>$content</p>"`
 - attribute values, e.g. `h"<img src=$location>"`
 - attribute value text, e.g. `h"<img src="/images/$filename">"`
 - attribute maps, e.g. `h"<img src="dog.jpg" $attributes>"`
 - comments, e.g. `h"<!--$comment-->"`

Note the subtle difference between _attribute values_ and _attribute value
text_: the latter appears inside quotes, while the former doesn't.

These substitutions are typechecked according to their position and surrounding
context. A substitution of body content must be an `Html` type that is an
acceptable child of the enclosing element, or a type that is convertible to it;
attribute values must have a type that corresponds to the attribute; attribute
value text must be `Showable`, but is not constrained by the attribute type;
attribute maps must be an instance of `Map[Text, Optional[Text]]` containing
attribute keys and values; comments must be `Showable`.

### `Renderable`

The `Renderable` typeclass makes it possible to substitute other types anywhere
an `Html` value is expected. A `Renderable` instance defines both the method of
converting a value of some type to HTML through its implementation, and the type
of HTML it produces through its type, e.g. `Html of "li"` or `Html of Flow`.

For example, a `given` instance of `List[Text] is Renderable in "li"` could
define how to render a list of `Text` values as `Html of "li"`.

Here's how that might look:
```scala
given List[Text] is Renderable in "li" =
  list => Fragment(list.map(Li(_))*)
```

With this definition in scope, and given a `list`, an instance of `List[Text]`,
it becomes possible to call `list.html`, `Ul(list)` or `h"<ol>$list</ol>"`. But
`Div(list)` is not permitted because a `<div>` cannot contain `<li>` children.

### `Fragment`s

All `Html` values will be either a `Node`, representing exactly one node
(whether it be element, comment or text) or a `Fragment`, representing an
arbitrary sequence of nodes.

More often than not, it's acceptable and convenient to accept a sequence of
nodes anywhere a single node would be acceptable. The nature of HTML is that, as
a text-based data format, any constraint of a parameter or return type of
_exactly one node_ is arbitrary.

Therefore, the meaning given to the conventional `Html` type is that it should
always permit sequences of nodes. There are, however, places where exactly one
node is necessary, and these are represented by the `Node` type, which excludes
`Fragment`s.

`Fragment`s are designed to be as intuitive as possible. They are seamlessly
unwrapped whenever they are provided as children to an `Element`, and equality
is defined for all `Html` subtypes such that a `Fragment` of exactly one node is
equal to that node, and has the same hashcode.

### Attributes

Attributes of `Element`s are store internally as `Text`, but may be attributed
to elements from typed values. For example, the HTML specification requires that
some attributes be integers, while others take values from an enumerated list of
options.

The typing of attributes needs some explanation.

Each valid attribute name, such as `title` or `width`, has a phantom type
associated with it. Sometimes the same attribute has different phantom types
when applied to different elements because those attributes have different
meanings on different elements.

These phantom types _represent_ the types of values that attribute can take, but
are not the actual type of the value that attribute takes. But each phantom type
may be associated with a concrete type, such as `Int` or `Text` or an
enumeration of values, which _is_ the type of the value.

For example, `width` has the phantom type `Attributive.PositiveInt`, which is
associated with `Int`. So for an `Int` called `int`, we can write
`Img(width = int)` or `h"<img width=$int>"`, while a `Double` value could not be
used.

When deciding what concrete type an attribute takes, Honeycomb checks for an
`Attribute` typeclass corresponding to the attribute name and (except for global
attributes) the element name. That typeclass will be associated with ag phantom
type. Subsequently it checks for an `Attributive` typeclass corresponding to the
phantom type, and finds its associted concrete type.

For example, if we write `Img(title = info)`, the compiler is constrained in two ways:
1. the value `info` has a type
2. there is an expected type for a `title` attribute on an `<img>` element

In order for the code to compile, Honeycomb will need to find a phantom
type—let's call it `topic`—such that there is an instance of `info.type is
Attributive to topic` and an instance of `"title" is Attribute on "img" of
topic`. In this case, it could choose the phantom type `Attributive.Textual`.

Why this indirection? The reason is twofold.

Firstly, many HTML attributes, like `width`, `height`, `maxlength` and seven
other attributes all share the same type; in this case,
`Attributive.PositiveInt`. If we wish to support `Int`s for these attributes (or
`Long`s, `Short`s or `Byte`s) we should not have to define the same typeclass
instance for every attribute. Phantom types associated with each _HTML attribute
type_ make it possible to support all attributes of that type with a single
typeclass.

Secondly, attribute types should be _pluggable_. If an application uses a
particular library for representing URLs as `Weblink`s, for example, a single
`given Weblink is Attributive to Attributive.Url` declaration should be
sufficient to enable `Weblink`s to be used consistently anywhere an attribute
expects a URL.

Currently, the set of definitions of `Attributive` instances is incomplete.
Definitions exist to make primitive types attributive to attributes which take
those values, but most other types are not defined. For now, the best solution
is to make `Text` values attribtive for _all_ attributes. This can be achieved
with one import:
```scala
import attributives.textAttributes
```

#### CSS `class` attributes

A common global attribute is `class`. This is inconvenient to write in Scala
because it's a keyword. Instead, we would normally need to write it as,
`` P(`class` = "info")("body text") ``.

To make things easier, there's a shortcut, especially for `class` attributes.
Instead, we can write, `P.info("body text")`, provided `info` has been declared
as a valid CSS class name, with a contextual value such as,
```scala
given (Stylesheet of "info") = Stylesheet()
```
or if we wanted to define several classes at once,
```scala
given (Stylesheet of "info" | "warning" | "error") = Stylesheet()
```

Note that the right-hand side constructor is always `Stylesheet()`; the most
important part is the type.

The requirement of a contextual `Stylesheet` value adds some typesafety to
referring to class names. If this is not required, then freeform class names can
be used in the `Tag.class()` style by importing:
```scala
import stylesheets.uncheckedClasses
```

### Transforming HTML

A few methods are provided for producing new `Html` values from existing values.

Two or more `Html` values can be joined with, `html + html2`, producing a
`Fragment` whose precise type corresponds to the types of `html` and `html2`.

An `Html` value may also be inserted _inside_ another, provided it is statically
known to be an `Element`—since it is not possible to add a child to any other
subtype of `Html`. Two methods are available `^+` and `+^` both of which add the
right operand as a child of the left operand.

While `^+` inserts it at the start (as indicated by the `^` appearing to the
_left_ of the `+` in the symbolic operator), `+^` inserts the child at the end.

Given an `Element`, attributes may be added (or overwritten if they exist already) with,
```scala
val node = h"""<img src="puppy.jpg">"""
val node2 = node.alt = "An image of a young dog"
```

Since `Html` values are immutable, `node` remains unchanged and `node2` is now the value,
`h"""<img src="puppy.jpg" alt="An image of a young dog">"""`.

### Serialization

HTML can be converted to text in one of two ways.

Most simply, a `Showable` instance exists that allows `.show` to be called on
any `Html` value to convert it to `Text`. This works fine for most purposes, but
it is not optimized for large amounts of HTML or for streaming.

This works fast enough for most purposes, but is not optimized.

For performance-critical code, a streaming solution is available which produces
a `Stream[Text]` (an alias of `LazyList[Text]`), broken into chunks.

Any `Document[Html]` may be converted to a stream with, `doc.stream[Text]` (or
`doc.stream[Bytes]`), in a concurrent environment. This requires a few imports
to set up:
```scala
import codicils.cancel
import supervisors.global
import threading.virtual

doc.stream[Text]
```

### Pattern Matching

As with the construction of new `Html` objects using interpolators, it's possible
to do the reverse: to pattern-match against an `Html` value. For example,

```scala
html match
  case h"<li>one</li>" => 1
  case h"<li>two</li>" => 2
  case _               => Unset
```

Depending on the value of `html`, we could get a result of `1`, `2` or `Unset`. Matches against
literal patterns such as these must be exact, but the `Html` value may be either a matching
`Node` instance or a `Fragment` containing just that `Node`.

Patterns may be arbitrarily complex, if desired, like
`h"<ul><li>one</li><li>two</li></ul>"`—but such patterns become more fragile to
subtle differences. However, patterns are parsed, checked as correct HTML, and
matched on the DOM structure; not a serialized string. So
`h"<ul><li>one<li>two</ul>"` would match the pattern above, despite being
written without explicit closing `</li>` tags.

It's also possible to extrapolate values from a pattern by binding variables to
"holes" in a pattern. Here's the example from above, rewritten to extract just
the contents of the `<li>` element.

```scala
html match
  case h"<li>$value</li>" => value match
    case h"one"             => 1
    case h"two"             => 2
    case _                  => Unset
  case _                  => Unset
```

Here, `value` is extracted from the `<li>` element. An `<li>` may contain text or HTML elements,
so the type of `value` reflects that. We are interested in matching on a `TextNode`,
so we match against `h"one"` and `h"two"` patterns rather than strings.

It is also possible to write it like this,
```scala
html match
  case h"<li>${TextNode(value)}</li>" => value match
    case "one"                          => 1
    case "two"                          => 2
    case _                              => Unset
  case _                              => Unset
```
or this:
```scala
html match
  case h"<li>$value</li>" => value match
    case TextNode("one")    => 1
    case TextNode("two")    => 2
    case _                  => Unset
  case _                  => Unset
```

It is possible to extract from other positions, too. These include,
 - attribute values
 - collections of attributes
 - `Comment`s
 - `Node`s
 - `Element`s (i.e. nodes which are not text or comments)

This is almost the same as the list of insertion positions, but there are some limitations and
implementation restrictions.

- `case h"<img src=$source>"` will match an entire attribute
- `case h"<img $attributes>"` will match the attributes of the `<img>` tag
- `case h"<img src=$source $attributes>"` will extract the `src` attribute as `source`
  and any additional addributes (excluding `src`) as a `Map`
- `case h"<!--$comment-->"` will match an entire comment
- `case h"<$element>"` will match an HTML element, but not any other kind of node

Currently, it is not possible to extract a `Fragment` in a pattern match, though we would like
to be able to do this.
```scala
val html = t"<ul><li>one</li><li>two</li></ul>".read[Html]
html match
  case h"<ul>$items</ul>" => items
```

Unfortunately, an extractor will only match a single node. In a later release of Honeycomb it
will be possible to have `items` instantiated from this pattern as a `Fragment`.
