## The Browser DOM

### About

A web page is a tree of live objects — the document, its elements, the window around them — with
an interface the browser publishes in [WebIDL](https://en.wikipedia.org/wiki/Web_IDL): which
members `Document` has, what `getElementById` takes, what it returns. Soundness reads those
definitions as the code compiles, so an expression over the page — find an element, give it
focus, scroll the window — is written in Scala, checked member by member against the browser's
own declarations, and rendered to JavaScript for the page to run.

The page's globals, `document` and `window`, are foreign values of the `Browser` ecosystem. They
navigate as though the DOM's interfaces were Scala types, every step producing another foreign
value of the declared type, and the resulting expression can be serialized to JavaScript text or
placed directly in an [HTML](html.md) event-handler attribute such as `onclick`.

### On scripting the browser from Scala

A Scala program that generates a page runs on the JVM, but the behavior it attaches to the page
runs in a browser, and the usual way to bridge the gap is a string: `onclick="…"` holding
JavaScript the compiler never sees. A misspelled method, a wrong argument type, a call on something
that is not an element — each survives compilation and fails, silently or loudly, in someone else's
browser.

Nothing here runs the DOM on the JVM. A navigation over `document` builds an *expression* — a typed
tree of references, selections and applications — and performs nothing itself; the expression is
serialized to JavaScript and handed to the page. What the compiler checks is the construction: a
member must exist on the foreign type, and an argument must be a Scala value with a known foreign
counterpart. The mechanism is the general one for
[foreign interfaces](foreign-interop.md), with the browser as its ecosystem and the DOM's WebIDL as
its definition file. Everything comes from the `soundness` package:

```scala
import soundness.*
```

### The page's globals

`document` and `window` are values of type `Foreign of "Document" from Browser` and
`Foreign of "Window" from Browser`: foreign values whose *topic* is a WebIDL interface name and
whose *origin* is the `Browser` ecosystem. Each stands for the JavaScript global of the same name,
and nothing more: a reference, to be resolved when the page runs.

```scala
val page: Foreign of "Document" from Browser = document
val frame: Foreign of "Window" from Browser = window
```

The `Browser` ecosystem carries its own interface definitions, so no further import is needed to
navigate from either value.

### Navigating the DOM

A foreign value's members are the members its WebIDL interface declares, and selecting one yields
a foreign value of the declared result type. `Document` declares `getElementById` returning an
`HTMLElement`, so the result navigates onward to the members of `HTMLElement`, such as `focus`,
`click` and `blur`. Inheritance is flattened, so an `HTMLElement` also offers everything `Element`
and `Node` declare:

```scala
val heading: Foreign of "HTMLElement" from Browser = document.getElementById(t"heading")
heading.focus()
heading.setAttribute(t"hidden", t"true")
```

`querySelectorAll` returns a sequence, and a sequence indexes with an integer to give an element
of its declared type. `Window` declares a `document` attribute, so the same page is reachable by
either route:

```scala
val first: Foreign of "HTMLElement" from Browser = document.querySelectorAll(t".btn")(0)
window.document.getElementById(t"x").focus()
```

A member the interface does not declare, or an argument with no counterpart in the foreign type
system, is a compile error — the mistake a browser would otherwise report as `undefined is not a
function`, caught where the expression is written:

```scala
document.getElementByld(t"heading")   // does not compile: Document declares no such member
document.getElementById(42)           // does not compile: elementId is a string
```

### Values that cross the boundary

A Scala value passes as an argument only where an `Interoperable` instance says which foreign type
it corresponds to. `Text` crosses as a WebIDL string, `Boolean` as a boolean, and the sized numbers
of Soundness — `S8` to `S64`, `U8` to `U64`, `F32` and `F64` — as the WebIDL integer and
floating-point types of the same width, so a parameter declared `long` takes an `S32` and not an
unsized `Int`. A `List` of interoperable values corresponds to a WebIDL `sequence`, and an
`Optional` to a nullable `T?`, with `Unset` crossing as `null`:

```scala
val left: S32 = 0
val top: S32 = 480

window.scrollTo(left, top)
```

Each argument is checked against the declared parameter type, in the foreign type system's own
terms: `scrollTo` wants two values of foreign type `s32`, and an `S32` is the Scala value that
provides one.

### Serializing to JavaScript

A foreign value's `expr` is its expression — a `Foreign.Expression`, a small tree of references,
member selections, applications, index accesses and literals — and `Javascript.serialize` renders
that tree as JavaScript source text:

```scala
Javascript.serialize(heading.focus().expr)
// t"document.getElementById('heading').focus()"

Javascript.serialize(first.click().expr)
// t"document.querySelectorAll('.btn')[0].click()"
```

String literals render single-quoted, so the output nests cleanly inside an HTML attribute's double
quotes, and a backslash, a quote or a line break in the text is escaped:

```scala
Javascript.serialize(window.alert(t"it's done").expr)
// t"window.alert('it\\'s done')"
```

The expression itself is plain data, and can be inspected or matched on. The first sample above
is an `Apply` of a `Select` of the `Reference` named `document`, with a single `Literal`
argument, and tests of generated scripts can assert on exactly that structure.

### Scripts in HTML attributes

An event-handler attribute of an [HTML](html.md) element — `onclick`, `onchange` and the rest —
expects a *script*, a distinct attribute type that ordinary text does not satisfy. A foreign DOM
expression does satisfy it, and is serialized to JavaScript when the page is shown:

```scala
import htmlDoms.html4Transitional
import html4Transitional.*

Button(onclick = heading.focus()).show
// <button onclick="document.getElementById('heading').focus()"></button>
```

A string in the same position is rejected, which is the point of the distinction: the JavaScript
that reaches the page was constructed by navigating the DOM's own definitions, and no unchecked
text can take its place:

```scala
Button(onclick = t"document.getElementById('heading').focus()")   // does not compile: a script, not text
```

Only a foreign value from the `Browser` ecosystem is accepted. A foreign expression from another
ecosystem — a TypeScript declaration, a WebAssembly interface — has the same shape but no way to
run in a page, so it cannot be an event handler, however well typed it is.
