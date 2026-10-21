## Web Pages

### About

A web page is assembled from reusable parts — a masthead, a hero banner, a breadcrumb trail, a side
panel, a footer — each contributing its own markup, its own CSS rules and its own `<head>`
metadata. A page is a class that mixes in the parts it wants and supplies its central content;
everything else, from the `<title>` to the stylesheet, is composed from what the parts contribute.
The result is a complete document, typed as [HTML](html.md), which renders to text, streams, or
serves directly as an HTTP response.

### On composing pages

Templating systems treat a page as a text file with holes in it, and reuse comes from including one
file in another. The structure of the page — what wraps what — lives in the files, where it is
invisible to the compiler and fragile under change: adding a sidebar means editing every template
that should have one, and the CSS it needs is kept somewhere else again.

A page here is a composition of *archetypes*: traits that each add one feature to a page. An
archetype may frame the content in a layout, put a region before or after it, add rules to the
stylesheet, or add elements to the document head, and it does so by wrapping what the archetypes
before it produced. Mixing archetypes together nests them, so the order in which they are listed is
the order in which they frame each other, and independent features — a left panel and a right
panel, say — never collide. Because the page is a class, the compiler sees its structure: a page that
omits its content does not compile, and two archetypes that genuinely conflict, by each defining the
same slot, are reported where they are combined. Everything comes from the `soundness` package,
with the choices the samples need in scope:

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
```

### Defining a page

Every page extends `Archetype`, and its one obligation is `content`, the central matter of the page.
Each feature mixed in alongside it introduces a *slot* — `masthead`, `colophon`, `hero`, `verso` —
which is empty by default and filled by overriding it:

```scala
class Welcome extends Archetype, Mainstay, Masthead, Colophon, Headline(t"Welcome"):
  def content: Html of (? <: Flow) = P("Hello, world!")
  override def masthead: Html of (? <: Flow) = Strong("Example Site")
  override def colophon: Html of (? <: Flow) = P("Copyright 2026")

Welcome().html
```

`Headline` sets the document title and puts an `<h1>` above the content; `Masthead` prepends a
`<header>`, `Colophon` appends a `<footer>`, and `Mainstay` wraps the content in a `<main>` landmark.
`html` is the whole document — a `<head>` carrying the title, the stylesheet and any metadata, and a
`<body>` holding the framed content — and the types of the slots are the same types that govern
every other HTML value, so a slot given something that does not belong there is a compile error.

The writing direction is `direction`, `HDir.Ltr` unless overridden. Flipping it to `HDir.Rtl` sets
`dir` on the body and nothing else: the layouts use only logical properties, so the browser mirrors
them, and the stylesheet is identical either way.

### Framing order

Each archetype wraps the frame built by the archetypes listed before it. Listing `Mainstay` before
`Masthead` puts the content inside `<main>` and then the masthead outside it; listing them the other
way round would put the masthead inside the landmark. The order of the mixins is the order of
nesting, read from the inside out:

```scala
class Guide extends Archetype, Breadcrumbs(t"home", t"docs"), Mainstay, Masthead:
  def content: Html of (? <: Flow) = P("The guide")
  override def masthead: Html of (? <: Flow) = Strong("Example Site")

Guide().html   // <header>…</header><main><nav>…</nav><p>The guide</p></main>
```

Features that do not nest but sit beside each other, such as the side panels, are independent of
their order: `VersoPanel` places a panel on the inline-start edge and `RectoPanel` on the inline-end
edge, and a page may mix in both in either order. Each exposes its own hooks for configuration — a
width, a gap — so a page adjusts a feature without touching its markup or its CSS:

```scala
class Manual extends Archetype, VersoPanel, TopMenu, Headline(t"Manual"):
  def content: Html of (? <: Flow) = P("Chapter one")
  override def menuItems: List[Html of (? <: Phrasing)] = List(Strong("Home"), Strong("About"))
  override def verso: Html of (? <: Flow) = menu
  override def versoWidth = 20.0*Rem
```

`TopMenu` renders its items as a `<nav>` and makes the result available as `menu`, which the page
here places in the side panel instead; a helper contributed by one feature can fill the slot of
another.

### Metadata

Features that contribute to the `<head>` take their value as a trait parameter and add nothing to
the body. `StandardMetadata` supplies the description and a responsive viewport that nearly every
page wants, and `Author`, `Keywords`, `Favicon`, `Canonical` and `ThemeColor` each add one more
element. `Hero` prepends a banner headed by its parameter, with any further material in the `hero`
slot, and the document title is `pageTitle`:

```scala
class Landing extends Archetype, Hero(t"Build pages from parts"),
    StandardMetadata(t"Pages composed from archetypes"), Author(t"Ada"),
    Keywords(t"scala", t"html"), Favicon(t"/icon.png"),
    Canonical(t"https://example.com/"), ThemeColor(t"#336699"):
  override def pageTitle: Text = t"Example"
  def content: Html of (? <: Flow) = P("Welcome aboard")
  override def hero: Html of (? <: Flow) = P("Compose, don't template")

Landing().html
```

A favicon or canonical address is a path, so these take `Text` directly rather than a parsed URL.

### Stylesheets

Every feature carries its own [CSS](css.md) rules, and the page accumulates them into one
stylesheet, available as `css`. By default it is embedded in a `<style>` element in the head, so a
page is self-contained:

```scala
Welcome().css.show
```

A feature's rules are a hook like any other — `mastheadStyles`, `menuStyles`, `heroStyles` — so a
page restyles one feature by overriding its rules, leaving the others untouched. The rules are
checked as the code compiles, like any `css"…"` literal:

```scala
class Branded extends Welcome:
  override def mastheadStyles: Css =
    super.mastheadStyles + css".graffiti-masthead { padding-block: 1rem }"
```

A site with many pages serves one stylesheet instead of embedding a copy in each. Setting
`stylesheetUrl` makes the head refer to it with `<link rel="stylesheet">` in place of the inline
`<style>`, and the application serves `css` at that address:

```scala
class Linked extends Welcome:
  override def stylesheetUrl: Optional[Text] = t"/site.css"

Linked().html   // <link rel="stylesheet" href="/site.css"> in the head
```

### Dashboards

Archetypes compose into larger archetypes. `Dashboard` is built from `Mainstay` and `Masthead`: it
titles the document after a `brand`, shows the brand in the masthead, and lays out `cards` in a
responsive grid as the content, leaving a concrete dashboard to supply only those two details:

```scala
class Admin extends Dashboard:
  def brand: Text = t"Admin"

  def cards: List[Dashboard.Card] =
    List(Dashboard.Card(t"Users", P("128 active")), Dashboard.Card(t"Revenue", P("4200")))

Admin().html
```

Each `Dashboard.Card` is a heading and its own flow content, rendered through `card`, which a
dashboard overrides to restructure a tile, while `dashboardStyles` restyles the grid.

### Serving a page

A page declares its media type, `text/html`, and streams its rendered document, so it is a response
body in its own right: a [server](http-server.md) handler returns it directly, and the
`Content-Type` header follows from the value. A `Css` value serves the same way, as `text/css`, which
is all a linked stylesheet needs:

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

The page is rendered when the response is streamed, within the `supervise` scope the server runs
under, and nothing is buffered: the document flows to the client as it is produced.
