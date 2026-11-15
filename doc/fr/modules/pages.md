- ## Web Pages
+ ## Pages web

- ### About
+ ### Présentation

- A web page is assembled from reusable parts: a masthead, a hero banner, a breadcrumb trail, a
- side panel, a footer. Each contributes its own markup, its own CSS rules and its own `<head>`
- metadata. A page is a class which mixes in the parts it wants and supplies its central content.
- Everything else, from the `<title>` to the stylesheet, is composed from what the parts
- contribute. The result is a complete document, typed as [HTML](html.md), which renders to text,
- streams, or serves directly as an HTTP response.
+ Une page web s'assemble à partir de parties réutilisables : un en-tête, une bannière héroïque,
+ un fil d'Ariane, un panneau latéral, un pied de page. Chacune apporte son propre balisage, ses
+ propres règles CSS et ses propres métadonnées `<head>`. Une page est une classe qui intègre les
+ parties qu'elle souhaite et fournit son contenu central. Tout le reste, du `<title>` à la feuille
+ de style, est composé à partir de ce qu'apportent les parties. Le résultat est un document
+ complet, typé comme du [HTML](html.md), qui se rend en texte, se diffuse en flux, ou se sert
+ directement comme réponse HTTP.

- ### On composing pages
+ ### Composer des pages

- Templating systems treat a page as a text file with holes in it, and reuse comes from including
- one file in another. The structure of the page, which is what wraps what, lives in the files,
- where it is invisible to the compiler and fragile under change. Adding a sidebar means editing
- every template which should have one, and the CSS it needs is kept somewhere else again.
+ Les systèmes de templates traitent une page comme un fichier texte percé de trous, et la
+ réutilisation passe par l'inclusion d'un fichier dans un autre. La structure de la page,
+ c'est-à-dire ce qui enveloppe quoi, vit dans les fichiers, où elle est invisible pour le
+ compilateur et fragile face aux changements. Ajouter une barre latérale oblige à modifier chaque
+ template qui doit en avoir une, et le CSS dont elle a besoin est encore conservé ailleurs.

- A page here is a composition of *archetypes*: traits which each add one feature to a page. An
- archetype may frame the content in a layout, put a region before or after it, add rules to the
- stylesheet, or add elements to the head of the document. It does so by wrapping what the
- archetypes before it produced. Mixing archetypes together nests them, so the order in which they
- are listed is the order in which they frame each other, and independent features, such as a
- left panel and a right panel, never collide.
+ Ici, une page est une composition d'*archétypes* : des traits qui ajoutent chacun une
+ fonctionnalité à une page. Un archétype peut encadrer le contenu dans une mise en page, placer
+ une région avant ou après lui, ajouter des règles à la feuille de style, ou ajouter des éléments
+ à l'en-tête du document. Il le fait en enveloppant ce qu'ont produit les archétypes qui le
+ précèdent. Combiner des archétypes les imbrique : l'ordre dans lequel ils sont énumérés est donc
+ l'ordre dans lequel ils s'encadrent mutuellement, et des fonctionnalités indépendantes, comme un
+ panneau gauche et un panneau droit, n'entrent jamais en collision.

- Because the page is a class, the compiler sees its structure. A page which omits its content
- does not compile, and two archetypes which conflict, by each defining the same slot, are
- reported where they are combined.
+ Comme la page est une classe, le compilateur voit sa structure. Une page qui omet son contenu ne
+ compile pas, et deux archétypes en conflit, parce qu'ils définissent chacun le même
+ emplacement, sont signalés là où ils sont combinés.

- Everything comes from the `soundness` package, with the choices which the samples need in
- scope, and the last import lets the samples print their results:
+ Tout vient du package `soundness`, avec les choix dont les exemples ont besoin dans la portée,
+ et le dernier import permet aux exemples d'afficher leurs résultats :

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

- ### Defining a page
+ ### Définir une page

- Every page extends `Archetype`, and its one obligation is `content`, the central matter of the
- page. Each feature mixed in beside it introduces a *slot*, such as `masthead`, `colophon`,
- `hero` or `verso`, which is empty by default and filled by overriding it:
+ Chaque page étend `Archetype`, et sa seule obligation est `content`, la matière centrale de la
+ page. Chaque fonctionnalité intégrée à côté introduit un *emplacement*, tel que `masthead`,
+ `colophon`, `hero` ou `verso`, vide par défaut et rempli en le redéfinissant :

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

- A page which omits its content does not compile:
+ Une page qui omet son contenu ne compile pas :

```scala fuel
compile
errors show
##
class Empty extends Archetype, Masthead
```

- These are the features, and the slots they introduce:
+ Voici les fonctionnalités, et les emplacements qu'elles introduisent :

```tabular
- column  Feature
- column  Adds
- column  Slot
- row
-   cell  `Headline(title)`
-   cell  the document title, and an `<h1>` above the content
-   cell
- row  `Masthead`  a `<header>` before the content  `masthead`
- row  `Colophon`  a `<footer>` after the content  `colophon`
- row
-   cell  `Mainstay`
-   cell  a `<main>` landmark around the content
-   cell
- row
-   cell  `Breadcrumbs(path*)`
-   cell  a breadcrumb trail, as a `<nav>`
-   cell
- row  `TopMenu`  a `<nav>` bar of links, also available as `menu`  `menuItems`
- row  `Hero(headline)`  a banner headed by its parameter  `hero`
- row  `VersoPanel`  a side panel on the inline-start edge  `verso`
- row  `RectoPanel`  a side panel on the inline-end edge  `recto`
- row  `FoldableRectoPanel`  a recto panel that folds away  `recto`
- row
-   cell  `Logo`
-   cell  a `logo` mark, worded by `logoText`, for other features to place
-   cell
+ column  Fonctionnalité
+ column  Ajoute
+ column  Emplacement
+ row
+   cell  `Headline(title)`
+   cell  le titre du document, et un `<h1>` au-dessus du contenu
+   cell
+ row  `Masthead`  un `<header>` avant le contenu  `masthead`
+ row  `Colophon`  un `<footer>` après le contenu  `colophon`
+ row
+   cell  `Mainstay`
+   cell  un repère `<main>` autour du contenu
+   cell
+ row
+   cell  `Breadcrumbs(path*)`
+   cell  un fil d'Ariane, sous forme de `<nav>`
+   cell
+ row  `TopMenu`  une barre `<nav>` de liens, également disponible sous `menu`  `menuItems`
+ row  `Hero(headline)`  une bannière intitulée par son paramètre  `hero`
+ row  `VersoPanel`  un panneau latéral sur le bord de début de ligne  `verso`
+ row  `RectoPanel`  un panneau latéral sur le bord de fin de ligne  `recto`
+ row  `FoldableRectoPanel`  un panneau recto qui se replie  `recto`
+ row
+   cell  `Logo`
+   cell  une marque `logo`, libellée par `logoText`, que d'autres fonctionnalités placent
+   cell
```

- `html` is the whole document: a `<head>` which carries the title, the stylesheet and any
- metadata, and a `<body>` which holds the framed content. The types of the slots are the same
- types which govern every other HTML value, so a slot given something which does not belong
- there is a compile error.
+ `html` est le document entier : un `<head>` qui porte le titre, la feuille de style et toutes
+ les métadonnées, et un `<body>` qui contient le contenu encadré. Les types des emplacements sont
+ les mêmes types qui régissent toute autre valeur HTML : un emplacement qui reçoit quelque chose
+ qui n'y a pas sa place est donc une erreur de compilation.

- The writing direction is `direction`, which is `HDir.Ltr` unless overridden. Flipping it to
- `HDir.Rtl` sets `dir` on the body, and nothing else. The layouts use only logical properties,
- so the browser mirrors them, and the stylesheet is identical either way.
+ Le sens d'écriture est `direction`, qui vaut `HDir.Ltr` sauf redéfinition. Le basculer en
+ `HDir.Rtl` définit `dir` sur le corps, et rien d'autre. Les mises en page n'utilisent que des
+ propriétés logiques : le navigateur les inverse donc, et la feuille de style est identique dans
+ les deux cas.

- ### Framing order
+ ### Ordre d'encadrement

- Each archetype wraps the frame built by the archetypes listed before it. Listing `Mainstay`
- before `Masthead` puts the content inside `<main>`, and then the masthead outside it. Listing
- them the other way round would put the masthead inside the landmark. The order of the mixins is
- the order of nesting, read from the inside out:
+ Chaque archétype enveloppe le cadre construit par les archétypes énumérés avant lui. Énumérer
+ `Mainstay` avant `Masthead` place le contenu dans `<main>`, puis l'en-tête à l'extérieur.
+ Les énumérer dans l'autre sens placerait l'en-tête à l'intérieur du repère. L'ordre des mixins
+ est l'ordre d'imbrication, lu de l'intérieur vers l'extérieur :

```scala
class Guide extends Archetype, Breadcrumbs(t"home", t"docs"), Mainstay, Masthead:
  def content: Html of (? <: Flow) = P("The guide")
  override def masthead: Html of (? <: Flow) = Strong("Example Site")
  override def stylesheetUrl: Optional[Text] = t"/site.css"
```

- The page links its stylesheet instead of embedding it, as [described below](#stylesheets), so
- the output is short enough to read:
+ La page lie sa feuille de style au lieu de l'incorporer, comme [décrit plus bas](#stylesheets) :
+ la sortie est donc assez courte pour être lue :

```scala fuel
run
##
Out.println(Guide().html.show)
```

- Features which do not nest, but sit beside each other, such as the side panels, are independent
- of their order. `VersoPanel` places a panel on the inline-start edge, and `RectoPanel` on the
- inline-end edge, and a page may mix in both in either order. Each exposes its own hooks for
- configuration, such as a width or a gap, so a page adjusts a feature without touching its markup
- or its CSS:
+ Les fonctionnalités qui ne s'imbriquent pas, mais se placent côte à côte, comme les panneaux
+ latéraux, sont indépendantes de leur ordre. `VersoPanel` place un panneau sur le bord de début
+ de ligne, et `RectoPanel` sur le bord de fin de ligne, et une page peut intégrer les deux dans
+ n'importe quel ordre. Chacun expose ses propres points d'accroche de configuration, comme une
+ largeur ou un espacement : une page ajuste donc une fonctionnalité sans toucher à son balisage
+ ni à son CSS :

```scala
class Manual extends Archetype, VersoPanel, TopMenu, Headline(t"Manual"):
  def content: Html of (? <: Flow) = P("Chapter one")
  override def menuItems: List[Html of (? <: Phrasing)] = List(Strong("Home"), Strong("About"))
  override def verso: Html of (? <: Flow) = menu
  override def versoWidth = 20.0*Rem
```

- `TopMenu` renders its items as a `<nav>`, and makes the result available as `menu`, which the
- page here places in the side panel instead. A helper contributed by one feature can fill the
- slot of another.
+ `TopMenu` rend ses éléments sous forme de `<nav>`, et rend le résultat disponible sous `menu`,
+ que la page place ici plutôt dans le panneau latéral. Un utilitaire apporté par une
+ fonctionnalité peut remplir l'emplacement d'une autre.

- ### Metadata
+ ### Métadonnées

- A feature which contributes to the `<head>` takes its value as a trait parameter, and adds
- nothing to the body. `StandardMetadata` supplies what nearly every page wants, and the others
- each add one more element:
+ Une fonctionnalité qui contribue au `<head>` prend sa valeur sous forme de paramètre de trait,
+ et n'ajoute rien au corps. `StandardMetadata` fournit ce que veut presque toute page, et les
+ autres ajoutent chacune un élément de plus :

```tabular
- column  Feature
- column  Adds to the head
- row  `StandardMetadata(description)`  a description and a responsive viewport
- row  `Description(text)`  `<meta name="description">`
- row  `Viewport`  `<meta name="viewport">`, from `viewport`
- row  `Author(name)`  `<meta name="author">`
- row  `Keywords(keywords*)`  `<meta name="keywords">`, the keywords separated by commas
- row  `Favicon(href)`  `<link rel="icon">`
- row  `Canonical(href)`  `<link rel="canonical">`
- row  `ThemeColor(color)`  `<meta name="theme-color">`
+ column  Fonctionnalité
+ column  Ajoute à l'en-tête
+ row  `StandardMetadata(description)`  une description et un viewport adaptatif
+ row  `Description(text)`  `<meta name="description">`
+ row  `Viewport`  `<meta name="viewport">`, à partir de `viewport`
+ row  `Author(name)`  `<meta name="author">`
+ row  `Keywords(keywords*)`  `<meta name="keywords">`, les mots-clés séparés par des virgules
+ row  `Favicon(href)`  `<link rel="icon">`
+ row  `Canonical(href)`  `<link rel="canonical">`
+ row  `ThemeColor(color)`  `<meta name="theme-color">`
```

- `Hero` prepends a banner headed by its parameter, with any further material in the `hero` slot,
- and the title of the document is `pageTitle`:
+ `Hero` ajoute en tête une bannière intitulée par son paramètre, avec tout contenu
+ supplémentaire dans l'emplacement `hero`, et le titre du document est `pageTitle` :

```scala
class Landing extends Archetype, Hero(t"Build pages from parts"),
    StandardMetadata(t"Pages composed from archetypes"), Author(t"Shakespeare"),
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

- A favicon or a canonical address is a path, so these take `Text` directly, and not a parsed URL.
+ Un favicon ou une adresse canonique est un chemin : ces fonctionnalités prennent donc
+ directement un `Text`, et non une URL analysée.

- ### Stylesheets
+ ### Feuilles de style

- Every feature carries its own [CSS](css.md) rules, and the page accumulates them into one
- stylesheet, available as `css`. By default it is embedded in a `<style>` element in the head, so
- a page is self-contained:
+ Chaque fonctionnalité porte ses propres règles [CSS](css.md), et la page les accumule en une
+ seule feuille de style, disponible sous `css`. Par défaut, elle est incorporée dans un élément
+ `<style>` de l'en-tête : une page est donc autonome :

```scala fuel
run
##
Out.println(Welcome().css.show)
```

- The rules of a feature are a hook like any other, such as `mastheadStyles`, `menuStyles` or
- `heroStyles`, so a page restyles one feature by overriding its rules, and leaves the others
- untouched. The rules are checked as the code compiles, like any `css"…"` literal:
+ Les règles d'une fonctionnalité sont un point d'accroche comme un autre, tel que
+ `mastheadStyles`, `menuStyles` ou `heroStyles` : une page restyle donc une fonctionnalité en
+ redéfinissant ses règles, et laisse les autres intactes. Les règles sont vérifiées pendant la
+ compilation du code, comme tout littéral `css"…"` :

```scala
class Branded extends Welcome:
  override def mastheadStyles: Css =
    super.mastheadStyles + css".graffiti-masthead { padding-block: 1rem }"
```

- A site with many pages serves one stylesheet, instead of embedding a copy in each. Setting
- `stylesheetUrl` makes the head refer to it with `<link rel="stylesheet">` in place of the inline
- `<style>`, and the application serves `css` at that address:
+ Un site comptant de nombreuses pages sert une seule feuille de style, au lieu d'en incorporer
+ une copie dans chacune. Définir `stylesheetUrl` fait que l'en-tête y renvoie avec
+ `<link rel="stylesheet">` à la place du `<style>` en ligne, et l'application sert `css` à cette
+ adresse :

```scala
class Linked extends Welcome:
  override def stylesheetUrl: Optional[Text] = t"/site.css"
```

```scala fuel
run
##
Out.println(Linked().html.show)
```

- ### Dashboards
+ ### Tableaux de bord

- Archetypes compose into larger archetypes. `Dashboard` is built from `Mainstay` and `Masthead`.
- It titles the document after a `brand`, shows the brand in the masthead, and lays out `cards` in
- a responsive grid as the content. A concrete dashboard supplies only those two details:
+ Les archétypes se composent en archétypes plus grands. `Dashboard` est construit à partir de
+ `Mainstay` et de `Masthead`. Il donne au document le titre d'une `brand`, affiche la marque dans
+ l'en-tête, et dispose des `cards` dans une grille adaptative en guise de contenu. Un tableau de
+ bord concret ne fournit que ces deux détails :

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

- Each `Dashboard.Card` is a heading and its own flow content, rendered through `card`, which a
- dashboard overrides to restructure a tile, while `dashboardStyles` restyles the grid.
+ Chaque `Dashboard.Card` est un titre et son propre contenu de flux, rendu au moyen de `card`,
+ qu'un tableau de bord redéfinit pour restructurer une tuile, tandis que `dashboardStyles`
+ restyle la grille.

- ### Serving a page
+ ### Servir une page

- A page declares its media type, `text/html`, and streams its rendered document, so it is a
- response body in its own right. A [server](http-server.md) handler returns it directly, and the
- `Content-Type` header follows from the value. A `Css` value serves the same way, as `text/css`,
- which is all a linked stylesheet needs:
+ Une page déclare son type de média, `text/html`, et diffuse son document rendu : c'est donc un
+ corps de réponse à part entière. Un gestionnaire de [serveur](http-server.md) la renvoie
+ directement, et l'en-tête `Content-Type` découle de la valeur. Une valeur `Css` se sert de la
+ même façon, en `text/css`, ce qui suffit à une feuille de style liée :

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

- The page is rendered when the response is streamed, within the `supervise` scope the server
- runs under, and nothing is buffered. The document flows to the client as it is produced.
+ La page est rendue quand la réponse est diffusée, dans la portée `supervise` sous laquelle
+ s'exécute le serveur, et rien n'est mis en tampon. Le document est transmis au client au fur et
+ à mesure de sa production.

- ### Related
+ ### Voir aussi

- - [HTML](html.md) describes the typed elements which fill each slot.
- - [CSS](css.md) describes the `css"…"` literals which restyle a feature.
- - [HTTP server](http-server.md) serves a page as a response.
- - [Markdown](markdown.md) renders written content into the HTML a page holds.
- - [Internationalization](internationalization.md) describes the writing directions which
-   `direction` chooses between.
- - [Hyphenation](hyphenation.md) describes the soft hyphens which a page's text may carry.
+ - [HTML](html.md) décrit les éléments typés qui remplissent chaque emplacement.
+ - [CSS](css.md) décrit les littéraux `css"…"` qui restylent une fonctionnalité.
+ - [Serveur HTTP](http-server.md) sert une page comme réponse.
+ - [Markdown](markdown.md) rend le contenu rédigé dans le HTML que contient une page.
+ - [Internationalisation](internationalization.md) décrit les sens d'écriture entre lesquels
+   `direction` choisit.
+ - [Césure](hyphenation.md) décrit les traits d'union conditionnels que peut porter le texte d'une
+   page.
