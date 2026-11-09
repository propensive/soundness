## Web Pages

### About

A web page is assembled from reusable parts: a masthead, a hero banner, a breadcrumb trail, a
side panel, a footer. Each contributes its own markup, its own CSS rules and its own `<head>`
metadata. A page is a class which mixes in the parts it wants and supplies its central content.
Everything else, from the `<title>` to the stylesheet, is composed from what the parts
contribute. The result is a complete document, typed as [HTML](html.md), which renders to text,
streams, or serves directly as an HTTP response.

### On composing pages

Templating systems treat a page as a text file with holes in it, and reuse comes from including
one file in another. The structure of the page, which is what wraps what, lives in the files,
where it is invisible to the compiler and fragile under change. Adding a sidebar means editing
every template which should have one, and the CSS it needs is kept somewhere else again.

A page here is a composition of *archetypes*: traits which each add one feature to a page. An
archetype may frame the content in a layout, put a region before or after it, add rules to the
stylesheet, or add elements to the head of the document. It does so by wrapping what the
archetypes before it produced. Mixing archetypes together nests them, so the order in which they
are listed is the order in which they frame each other, and independent features, such as a
left panel and a right panel, never collide.

Because the page is a class, the compiler sees its structure. A page which omits its content
does not compile, and two archetypes which conflict, by each defining the same slot, are
reported where they are combined.

Everything comes from the `soundness` package, with the choices which the samples need in
scope, and the last import lets the samples print their results:

```scala
import soundness.*
import htmlDoms.whatwg
import htmlDoms.whatwg.*
import attributives.textAttributive
import strategies.throwUnsafely
import formatting.indentedCssFormatting
import codepages.utf8Codepage
import errorDiagnostics.stackTracesDiagnostics
import logging.silentLogging
import probates.awaitProbate
import threads.virtualThreads
import webserverErrorPages.minimalErrorPage
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Defining a page

Every page extends `Archetype`, and its one obligation is `content`, the central matter of the
page. Each feature mixed in beside it introduces a *slot*, such as `masthead`, `colophon`,
`hero` or `verso`, which is empty by default and filled by overriding it:

```scala
class Welcome extends Archetype, Mainstay, Masthead, Colophon, Headline(t"Welcome"):
  def content: Html of (? <: Flow) = P("Hello, world!")
  override def masthead: Html of (? <: Flow) = Strong("Example Site")
  override def colophon: Html of (? <: Flow) = P("Copyright 2026")
```

```scala fuel
run
##
Out.println(Welcome().html.show)
```

A page which omits its content does not compile:

```scala fuel
compile
errors show
##
class Empty extends Archetype, Masthead
```

These are the features, and the slots they introduce:

```tabular
column  Feature
column  Adds
column  Slot
row
  cell  `Headline(title)`
  cell  the document title, and an `<h1>` above the content
  cell
row  `Masthead`  a `<header>` before the content  `masthead`
row  `Colophon`  a `<footer>` after the content  `colophon`
row
  cell  `Mainstay`
  cell  a `<main>` landmark around the content
  cell
row
  cell  `Breadcrumbs(path*)`
  cell  a breadcrumb trail, as a `<nav>`
  cell
row  `TopMenu`  a `<nav>` bar of links, also available as `menu`  `menuItems`
row  `Hero(headline)`  a banner headed by its parameter  `hero`
row  `VersoPanel`  a side panel on the inline-start edge  `verso`
row  `RectoPanel`  a side panel on the inline-end edge  `recto`
row  `FoldableRectoPanel`  a recto panel that folds away  `recto`
row
  cell  `Logo`
  cell  a `logo` mark, worded by `logoText`, for other features to place
  cell
```

`html` is the whole document: a `<head>` which carries the title, the stylesheet and any
metadata, and a `<body>` which holds the framed content. The types of the slots are the same
types which govern every other HTML value, so a slot given something which does not belong
there is a compile error.

The writing direction is `direction`, which is `HDir.Ltr` unless overridden. Flipping it to
`HDir.Rtl` sets `dir` on the body, and nothing else. The layouts use only logical properties,
so the browser mirrors them, and the stylesheet is identical either way.

### Framing order

Each archetype wraps the frame built by the archetypes listed before it. Listing `Mainstay`
before `Masthead` puts the content inside `<main>`, and then the masthead outside it. Listing
them the other way round would put the masthead inside the landmark. The order of the mixins is
the order of nesting, read from the inside out:

```scala
class Guide extends Archetype, Breadcrumbs(t"home", t"docs"), Mainstay, Masthead:
  def content: Html of (? <: Flow) = P("The guide")
  override def masthead: Html of (? <: Flow) = Strong("Example Site")
  override def stylesheetUrl: Optional[Text] = t"/site.css"
```

The page links its stylesheet instead of embedding it, as [described below](#stylesheets), so
the output is short enough to read:

```scala fuel
run
##
Out.println(Guide().html.show)
```

Features which do not nest, but sit beside each other, such as the side panels, are independent
of their order. `VersoPanel` places a panel on the inline-start edge, and `RectoPanel` on the
inline-end edge, and a page may mix in both in either order. Each exposes its own hooks for
configuration, such as a width or a gap, so a page adjusts a feature without touching its markup
or its CSS:

```scala
class Manual extends Archetype, VersoPanel, TopMenu, Headline(t"Manual"):
  def content: Html of (? <: Flow) = P("Chapter one")
  override def menuItems: List[Html of (? <: Phrasing)] = List(Strong("Home"), Strong("About"))
  override def verso: Html of (? <: Flow) = menu
  override def versoWidth = 20.0*Rem
```

`TopMenu` renders its items as a `<nav>`, and makes the result available as `menu`, which the
page here places in the side panel instead. A helper contributed by one feature can fill the
slot of another.

### Metadata

A feature which contributes to the `<head>` takes its value as a trait parameter, and adds
nothing to the body. `StandardMetadata` supplies what nearly every page wants, and the others
each add one more element:

```tabular
column  Feature
column  Adds to the head
row  `StandardMetadata(description)`  a description and a responsive viewport
row  `Description(text)`  `<meta name="description">`
row  `Viewport`  `<meta name="viewport">`, from `viewport`
row  `Author(name)`  `<meta name="author">`
row  `Keywords(keywords*)`  `<meta name="keywords">`, the keywords separated by commas
row  `Favicon(href)`  `<link rel="icon">`
row  `Canonical(href)`  `<link rel="canonical">`
row  `ThemeColor(color)`  `<meta name="theme-color">`
```

`Hero` prepends a banner headed by its parameter, with any further material in the `hero` slot,
and the title of the document is `pageTitle`:

```scala
class Landing extends Archetype, Hero(t"Build pages from parts"),
    StandardMetadata(t"Pages composed from archetypes"), Author(t"Ada"),
    Keywords(t"scala", t"html"), Favicon(t"/icon.png"),
    Canonical(t"https://example.com/"), ThemeColor(t"#336699"):
  override def pageTitle: Text = t"Example"
  override def stylesheetUrl: Optional[Text] = t"/site.css"
  def content: Html of (? <: Flow) = P("Welcome aboard")
  override def hero: Html of (? <: Flow) = P("Compose, don't template")

Landing().html
```

```scala fuel
run
##
Out.println(Landing().html.show)
```

A favicon or a canonical address is a path, so these take `Text` directly, and not a parsed URL.

### Stylesheets

Every feature carries its own [CSS](css.md) rules, and the page accumulates them into one
stylesheet, available as `css`. By default it is embedded in a `<style>` element in the head, so
a page is self-contained:

```scala fuel
run
##
Out.println(Welcome().css.show)
```

The rules of a feature are a hook like any other, such as `mastheadStyles`, `menuStyles` or
`heroStyles`, so a page restyles one feature by overriding its rules, and leaves the others
untouched. The rules are checked as the code compiles, like any `css"…"` literal:

```scala
class Branded extends Welcome:
  override def mastheadStyles: Css =
    super.mastheadStyles + css".graffiti-masthead { padding-block: 1rem }"
```

A site with many pages serves one stylesheet, instead of embedding a copy in each. Setting
`stylesheetUrl` makes the head refer to it with `<link rel="stylesheet">` in place of the inline
`<style>`, and the application serves `css` at that address:

```scala
class Linked extends Welcome:
  override def stylesheetUrl: Optional[Text] = t"/site.css"
```

```scala fuel
run
##
Out.println(Linked().html.show)
```

### Dashboards

Archetypes compose into larger archetypes. `Dashboard` is built from `Mainstay` and `Masthead`.
It titles the document after a `brand`, shows the brand in the masthead, and lays out `cards` in
a responsive grid as the content. A concrete dashboard supplies only those two details:

```scala
class Admin extends Dashboard:
  def brand: Text = t"Admin"

  def cards: List[Dashboard.Card] =
    List(Dashboard.Card(t"Users", P("128 active")), Dashboard.Card(t"Revenue", P("4200")))

  override def stylesheetUrl: Optional[Text] = t"/site.css"
```

```scala fuel
run
##
Out.println(Admin().html.show)
```

Each `Dashboard.Card` is a heading and its own flow content, rendered through `card`, which a
dashboard overrides to restructure a tile, while `dashboardStyles` restyles the grid.

### Serving a page

A page declares its media type, `text/html`, and streams its rendered document, so it is a
response body in its own right. A [server](http-server.md) handler returns it directly, and the
`Content-Type` header follows from the value. A `Css` value serves the same way, as `text/css`,
which is all a linked stylesheet needs:

```scala fuel
typecheck
##
def site(): Unit = supervise:
  SocketServer(8080).handle:
    request.target match
      case t"/"         => Http.Response(Http.Ok)(Linked())
      case t"/site.css" => Http.Response(Http.Ok)(Linked().css)
      case _            => Http.Response(Http.NotFound)(t"No such page")
```

The page is rendered when the response is streamed, within the `supervise` scope the server
runs under, and nothing is buffered. The document flows to the client as it is produced.

### Related

- [HTML](html.md) describes the typed elements which fill each slot.
- [CSS](css.md) describes the `css"…"` literals which restyle a feature.
- [HTTP server](http-server.md) serves a page as a response.
- [Markdown](markdown.md) renders written content into the HTML a page holds.
- [Internationalization](internationalization.md) describes the writing directions which
  `direction` chooses between.
- [Hyphenation](hyphenation.md) describes the soft hyphens which a page's text may carry.
