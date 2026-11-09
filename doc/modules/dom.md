## The Browser DOM

### About

A web page is a tree of live objects: the document, its elements, and the window around
them. The browser publishes their interface in
[WebIDL](https://en.wikipedia.org/wiki/Web_IDL), which says which members `Document` has,
what `getElementById` takes, and what it returns.

Those definitions are read at [compiletime]. An expression over the page, which finds an
element, gives it focus, or scrolls the window, is written in Scala. It is checked, member
by member, against the browser's own declarations, and is written out as JavaScript for the
page to run.

The page's globals, `document` and `window`, are foreign values of the `Browser`
*ecosystem*. They are navigated as though the DOM's interfaces were Scala types, and every
step gives another foreign value, of the declared type. The resulting expression can be
serialized to JavaScript text, or placed directly in an event-handler attribute of an
[HTML](html.md) element, such as `onclick`.

### On scripting the browser from Scala

A Scala program which generates a page runs on the JVM, but the behavior which it attaches
to the page runs in a browser. The usual way to bridge the gap is a string: `onclick="…"`,
holding JavaScript which the compiler never sees. A misspelled method, an argument of the
wrong type, or a call on something which is not an element, each survives compilation, and
fails, silently or loudly, in someone else's browser.

Nothing here runs the DOM on the JVM. A navigation over `document` builds an *expression*: a
typed tree of references, selections and applications. It performs nothing itself. The
expression is serialized to JavaScript, and handed to the page.

What the compiler checks is the construction. A member must exist on the foreign type, and
an argument must be a Scala value with a known foreign counterpart. The mechanism is the
general one for [foreign interfaces](foreign-interop.md), with the browser as its ecosystem,
and the DOM's WebIDL as its definition file.

Everything comes from the `soundness` package. The second import lets the samples print
their results:

```scala
import soundness.*
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### The page's globals

`document` and `window` are values of the types `Foreign of "Document" from Browser` and
`Foreign of "Window" from Browser`. Each is a foreign value, whose *topic* is the name of a
WebIDL interface, and whose *origin* is the `Browser` ecosystem. Each stands for the
JavaScript global of the same name, and nothing more: a reference, to be resolved when the
page runs.

```scala
val page: Foreign of "Document" from Browser = document
val frame: Foreign of "Window" from Browser = window
```

The `Browser` ecosystem has its own interface definitions, so no further import is needed
to navigate from either value.

### Navigating the DOM

The members of a foreign value are the members which its WebIDL interface declares, and
selecting one gives a foreign value of the declared result type. `Document` declares
`getElementById`, which returns an `HTMLElement`. The result is therefore navigated onward,
to the members of `HTMLElement`, such as `focus`, `click` and `blur`. Inheritance is
flattened, so an `HTMLElement` also offers everything which `Element` and `Node` declare:

```scala
val heading: Foreign of "HTMLElement" from Browser = document.getElementById(t"heading")
val focused = heading.focus()
val hidden = heading.setAttribute(t"hidden", t"true")
```

`querySelectorAll` returns a sequence, and a sequence is indexed with an integer, to give an
element of its declared type. `Window` declares a `document` attribute, so the same page can
be reached by either route:

```scala
val first: Foreign of "HTMLElement" from Browser = document.querySelectorAll(t".btn")(0)
val viaWindow = window.document.getElementById(t"x").focus()
```

A member which the interface does not declare is a compile error. It is the mistake which a
browser would otherwise report as `undefined is not a function`, caught where the expression
is written:

```scala fuel
compile
errors show
##
document.getElementByld(t"heading")
```

So is an argument of the wrong type:

```scala fuel
compile
errors show
##
document.getElementById(42)
```

### Values that cross the boundary

A Scala value is passed as an argument only where an `Interoperable` instance says which
foreign type it corresponds to:

```tabular
column  Scala type
column  WebIDL type
row  `Text`  `DOMString`, `USVString`, `ByteString`, `CSSOMString`
row  `Boolean`  `boolean`
row  `S8`  `byte`
row  `U8`  `octet`
row  `S16`  `short`
row  `U16`  `unsigned short`
row  `S32`  `long`
row  `U32`  `unsigned long`
row  `S64`  `long long`
row  `U64`  `unsigned long long`
row  `F32`  `float`, `unrestricted float`
row  `F64`  `double`, `unrestricted double`
row  `List[T]`  `sequence<T>`, `FrozenArray<T>`
row  `Map[K, V]`  `record<K, V>`
row  `Optional[T]`  `T?`, with `Unset` as `null`
```

The numbers are [sized](numbers.md#numeric-types), so a parameter which is declared `long`
takes an `S32`, and not an unsized `Int`:

```scala
val left: S32 = 0
val top: S32 = 480

val scrolled = window.scrollTo(left, top)
```

Each argument is checked against the declared type of its parameter, in the foreign type
system's own terms. `scrollTo` wants two values of the foreign type `s32`, and an `S32` is
the Scala value which provides one.

### Serializing to JavaScript

A foreign value's `expr` is its expression, a `Foreign.Expression`, which is a small tree of
these nodes:

```tabular
column  Node
column  Stands for
column  Renders as
row  `Reference(name)`  a global  `document`
row  `Select(target, member, owner)`  a member of a value  `target.member`
row  `Apply(target, arguments)`  a call  `target(arguments)`
row  `Index(target, index)`  an index access  `target[index]`
row  `Literal(value)`  a constant  `'text'`, `42`
```

`Javascript.serialize` writes the tree as JavaScript source text:

```scala fuel
run
##
Out.println(Javascript.serialize(focused.expr))
Out.println(Javascript.serialize(first.click().expr))
Out.println(Javascript.serialize(scrolled.expr))
```

String literals are written with single quotes, so the output nests cleanly inside the
double quotes of an HTML attribute. A backslash, a quote or a line break in the text is
escaped:

```scala fuel
run
##
Out.println(Javascript.serialize(window.alert(t"it's done").expr))
```

The expression itself is plain data, and can be inspected, or matched on. The first sample
above is an `Apply` of a `Select` of the `Reference` named `document`, with a single
`Literal` argument, and [tests](testing.md) of generated scripts can assert on exactly that
structure.

### Scripts in HTML attributes

An event-handler attribute of an HTML element, such as `onclick` or `onchange`, expects a
*script*. That is a distinct type of attribute, which ordinary text does not satisfy. A
foreign DOM expression does satisfy it, and is serialized to JavaScript when the page is
shown:

```scala
import htmlDoms.html4Transitional
import html4Transitional.*
```

```scala fuel
run
##
Out.println(Button(onclick = heading.focus()).show)
```

A string in the same position is rejected, which is the point of the distinction. The
JavaScript which reaches the page was constructed by navigating the DOM's own definitions,
and no unchecked text can take its place:

```scala fuel
compile
errors show
##
Button(onclick = t"document.getElementById('heading').focus()")
```

Only a foreign value from the `Browser` ecosystem is accepted. A foreign expression from
another ecosystem, such as a TypeScript declaration or a WebAssembly interface, has the same
shape, but no way to run in a page. It cannot be an event handler, however well typed it is.

### Related

- [HTML](html.md#attributes) builds the elements which scripts are attached to.
- [Foreign interfaces](foreign-interop.md) describes the general mechanism, for other
  ecosystems.
- [Web automation](web-automation.md) drives a real browser from outside the page.
