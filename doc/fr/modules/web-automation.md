- ## Web Automation
+ ## Automatisation web

- ### About
+ ### Présentation

- Driving a browser and scripting a page are the two sides of web automation, and both are covered
- here. A browser is driven through the [WebDriver](https://www.w3.org/TR/webdriver2/) protocol.
- Chrome or Firefox is launched, a [session] is opened, pages are navigated, and elements are
- found, clicked and typed into, which is the machinery of end-to-end testing.
+ Piloter un navigateur et scripter une page sont les deux faces de l'automatisation web, et
+ les deux sont couvertes ici. Un navigateur se pilote au moyen du protocole
+ [WebDriver](https://www.w3.org/TR/webdriver2/). Chrome ou Firefox est lancé, une [session]
+ est ouverte, on navigue entre les pages, et on trouve des éléments, on clique dessus et on y
+ saisit du texte : c'est la mécanique des tests de bout en bout.

- Behavior on the browser's side is written as *typed* JavaScript. DOM expressions are checked
- against the browser's own [WebIDL](https://en.wikipedia.org/wiki/Web_IDL) interface definitions
- as the code compiles, and rendered to JavaScript for the event handlers of a page.
+ Le comportement côté navigateur s'écrit en JavaScript *typé*. Les expressions DOM sont
+ vérifiées par rapport aux propres définitions d'interfaces
+ [WebIDL](https://en.wikipedia.org/wiki/Web_IDL) du navigateur pendant la compilation du
+ code, et rendues en JavaScript pour les gestionnaires d'événements d'une page.

- ### On automating the web
+ ### À propos de l'automatisation du web

- Browser automation scripts are stringly typed twice over. Elements are located by selector
- strings, and injected behavior is JavaScript in string literals, unchecked until the browser
- runs it. A misspelled DOM method, such as `getElementByld`, is exactly the kind of mistake which
- a compiler exists to catch, and exactly the kind which these strings hide.
+ Les scripts d'automatisation de navigateur sont doublement typés par des chaînes. Les
+ éléments sont localisés par des chaînes de sélecteurs, et le comportement injecté est du
+ JavaScript dans des littéraux de chaîne, non vérifié jusqu'à ce que le navigateur
+ l'exécute. Une méthode DOM mal orthographiée, comme `getElementByld`, est exactement le
+ genre d'erreur qu'un compilateur existe pour détecter, et exactement le genre que ces
+ chaînes masquent.

- Here both layers are typed. Elements are located through the same typed selectors and validated
- identifiers which [CSS](css.md) and [HTML](html.md) use. DOM scripting is checked against the
- actual interface definitions of the DOM, so only methods which exist, with arguments of the
- right types, can be written. A session confined to its block, and page scripts checked against
- the DOM's own definitions, follow [delimited scopes](../../philosophy/delimited-scopes.md) and
- [safety by construction](../../philosophy/safety-by-construction.md).
+ Ici, les deux couches sont typées. Les éléments sont localisés au moyen des mêmes sélecteurs
+ typés et identifiants validés qu'utilisent [CSS](css.md) et [HTML](html.md). Le scripting
+ DOM est vérifié par rapport aux définitions d'interfaces réelles du DOM ; seules les
+ méthodes qui existent, avec des arguments des bons types, peuvent donc être écrites. Une
+ session confinée à son bloc, et des scripts de page vérifiés par rapport aux propres
+ définitions du DOM, suivent les [portées délimitées](../../philosophy/delimited-scopes.md)
+ et la [sûreté par construction](../../philosophy/safety-by-construction.md).

- Everything comes from the `soundness` package, with internet access, an HTTP client to speak to
- the driver, and concurrency for the driver's process in scope:
+ Tout provient du package `soundness`, avec dans la portée l'accès à internet, un client HTTP
+ pour dialoguer avec le pilote, et la concurrence pour le processus du pilote :

```scala
import soundness.*

import environments.javaBaseEnvironment
import errorDiagnostics.stackTracesDiagnostics
import httpBackends.javaNetHttp
import internetAccess.online
import logging.silentLogging
import probates.awaitProbate
import strategies.throwUnsafely
import threads.virtualThreads
import workingDirectories.javaBaseWorkingDirectory
```

- The samples which drive a browser are compiled but not run.
+ Les exemples qui pilotent un navigateur sont compilés mais pas exécutés.

- ### Driving a browser
+ ### Piloter un navigateur

- A browser session launches the driver, opens a session, and closes both when the block ends,
- whether it returns or throws. Within it, the session navigates and queries:
+ Une session de navigateur lance le pilote, ouvre une session, et ferme les deux lorsque le
+ bloc se termine, qu'il renvoie une valeur ou lève une exception. À l'intérieur, la session
+ navigue et interroge :

```scala fuel
typecheck
##
supervise:
  WebDriver.Chrome.headless.session:
    browser.navigateTo(url"https://example.com/")
    val title: Text = browser.title()
```

- `WebDriver.Firefox`, `WebDriver.Safari` and `WebDriver.Edge` work identically, and each drives
- its standard driver executable. `WebDriver(url"http://localhost:4444", capabilities)` drives a
- driver which is already running, including a remote grid, through the same interface.
+ `WebDriver.Firefox`, `WebDriver.Safari` et `WebDriver.Edge` fonctionnent de façon
+ identique, et chacun pilote l'exécutable de pilote standard correspondant.
+ `WebDriver(url"http://localhost:4444", capabilities)` pilote un pilote déjà en cours
+ d'exécution, y compris une grille distante, au travers de la même interface.

- The session handle is a [capability], confined by capture checking to the block which owns it.
- Returning it, or anything which captures it, is a compile error, since a session which has been
- closed is not something a program should be able to hold. Element handles, by contrast, are
- pure values. But every operation on one needs the session in scope, so they are inert outside
- it.
+ La poignée de session est une [capacité], confinée par le contrôle des captures au bloc qui
+ la possède. La renvoyer, ou renvoyer quoi que ce soit qui la capture, est une erreur de
+ compilation, puisqu'une session fermée n'est pas une chose qu'un programme devrait pouvoir
+ détenir. Les poignées d'éléments, en revanche, sont des valeurs pures. Mais chaque opération
+ sur l'une d'elles nécessite la session dans la portée ; elles sont donc inertes en dehors.

- ### Finding and using elements
+ ### Trouver et utiliser des éléments

- Elements are located by typed criteria. `\` returns every match, and `element` exactly one:
+ Les éléments sont localisés par des critères typés. `\` renvoie toutes les correspondances,
+ et `element` exactement une :

```tabular
- column  Locator
- column  Written as
- column  Matches by
- row  an HTML tag  `H1`, `Button`  tag name
- row  a CSS selector list  `SelectorList.parse(t"nav li")`  CSS selector
- row  a validated DOM id  `Name[DomId](t"query")`  CSS selector, the id prefixed with `#`
- row  a class list  `ClassList["checkbox"]()`  CSS selector, each class prefixed with `.`
- row  an XPath  `xp"//li"`  XPath
- row  a `Text`  `t"Sign in"`  link text
+ column  Localisateur
+ column  S'écrit
+ column  Correspond selon
+ row  une balise HTML  `H1`, `Button`  le nom de la balise
+ row  une liste de sélecteurs CSS  `SelectorList.parse(t"nav li")`  un sélecteur CSS
+ row  un identifiant DOM validé  `Name[DomId](t"query")`  un sélecteur CSS, l'identifiant préfixé par `#`
+ row  une liste de classes  `ClassList["checkbox"]()`  un sélecteur CSS, chaque classe préfixée par `.`
+ row  un XPath  `xp"//li"`  XPath
+ row  un `Text`  `t"Sign in"`  le texte du lien
```

- Elements found within elements descend the tree, and each offers the operations which a test
- needs:
+ Les éléments trouvés à l'intérieur d'éléments descendent dans l'arbre, et chacun offre les
+ opérations dont un test a besoin :

```scala fuel
typecheck
##
import htmlDoms.whatwg
import whatwg.*

supervise:
  WebDriver.Chrome.headless.session:
    val heading = browser.element(H1)
    val items = browser \ SelectorList.parse(t"nav li")
    val field = browser.element(Name[DomId](t"query"))
    val button = browser.element(Button)

    heading.click()
    val text: Text = heading.innerText()
    field.value(t"search terms")
    val live: Optional[Text] = field.property(t"value")
    val picture: Raster in Png = button.screenshot()
```

- `property` reads the live DOM property, and not the markup. These are the operations on an
- element, and all of them need the session in scope:
+ `property` lit la propriété DOM vivante, et non le balisage. Voici les opérations sur un
+ élément, et toutes nécessitent la session dans la portée :

```tabular
- column  Operation
- column  Does
- row  `click()`, `clear()`  clicks the element, or clears an input
- row  `value(text)`  types into an input
- row  `innerText()`  reads the rendered text
- row  `attribute(name)`, `property(name)`  reads an attribute of the markup, or a live DOM property, as an `Optional`
- row  `cssValue(name)`  reads a computed CSS value
- row  `tagName()`, `role()`, `label()`  read the element's tag, accessible role and accessible label
- row  `enabled()`, `selected()`, `displayed()`  report the element's state
- row  `rect()`  reads the element's position and size
- row  `screenshot()`, `screenshotData()`  capture the element as a `Raster in Png`, or as PNG bytes
- row  `element(locator)`, `\`  find elements within it
- row  `shadowRoot()`  reaches its open shadow root
+ column  Opération
+ column  Rôle
+ row  `click()`, `clear()`  clique sur l'élément, ou vide un champ de saisie
+ row  `value(text)`  saisit du texte dans un champ
+ row  `innerText()`  lit le texte rendu
+ row  `attribute(name)`, `property(name)`  lit un attribut du balisage, ou une propriété DOM vivante, sous forme d'`Optional`
+ row  `cssValue(name)`  lit une valeur CSS calculée
+ row  `tagName()`, `role()`, `label()`  lisent la balise, le rôle accessible et le libellé accessible de l'élément
+ row  `enabled()`, `selected()`, `displayed()`  indiquent l'état de l'élément
+ row  `rect()`  lit la position et la taille de l'élément
+ row  `screenshot()`, `screenshotData()`  capturent l'élément sous forme de `Raster in Png`, ou d'octets PNG
+ row  `element(locator)`, `\`  trouvent des éléments à l'intérieur
+ row  `shadowRoot()`  atteint sa racine fantôme ouverte
```

- Anything which a locator cannot express is reached with `execute`, which runs JavaScript in the
- page and returns its result as `Json`. Modifiers, chords, hovers and drags go through `perform`
- and the typed actions API. A failure of any kind arrives as a `WebDriver.Error`, which carries
- the W3C error code the driver reported, its message, and the stack trace from the browser's
- side.
+ Tout ce qu'un localisateur ne peut pas exprimer s'atteint avec `execute`, qui exécute du
+ JavaScript dans la page et renvoie son résultat sous forme de `Json`. Les modificateurs,
+ les accords de touches, les survols et les glisser-déposer passent par `perform` et l'API
+ typée des actions. Un échec de quelque nature que ce soit arrive sous forme de
+ `WebDriver.Error`, qui porte le code d'erreur W3C qu'a signalé le pilote, son message, et
+ la trace de pile côté navigateur.

- ### Waiting
+ ### Attente

- Most of what makes browser automation flaky is timing, and the protocol helps with only part of
- it. WebDriver's *implicit* timeout covers find commands, and is set once for the session:
+ L'essentiel de ce qui rend l'automatisation de navigateur instable tient au timing, et le
+ protocole n'aide que pour une partie. Le délai *implicite* de WebDriver couvre les
+ commandes de recherche, et se définit une fois pour la session :

```scala fuel
typecheck
##
supervise:
  WebDriver.Chrome.headless.session:
    browser.timeouts(WebDriver.Session.Timeouts(`implicit` = 5000L))
```

- Nothing else has a server-side notion of waiting. For a button which becomes enabled, or a
- heading whose text changes, three methods poll under the retry policy in scope:
+ Rien d'autre n'a de notion d'attente côté serveur. Pour un bouton qui devient actif, ou un
+ titre dont le texte change, trois méthodes interrogent périodiquement selon la politique de
+ nouvelle tentative dans la portée :

```tabular
- column  Method
- column  Waits until
- column  Returns
- row  `awaitElement(locator)`  an element matches  the first match
- row  `awaitElements(locator)`  any element matches  every match
- row  `awaitUntil(condition)`  a condition holds  nothing
+ column  Méthode
+ column  Attend que
+ column  Renvoie
+ row  `awaitElement(locator)`  un élément corresponde  la première correspondance
+ row  `awaitElements(locator)`  un élément quelconque corresponde  toutes les correspondances
+ row  `awaitUntil(condition)`  une condition soit vraie  rien
```

```scala fuel
typecheck
##
import retryTenacities.exponentialTenTimesTenacity

supervise:
  WebDriver.Chrome.headless.session:
    browser.awaitElement(Name[DomId](t"result"))
    browser.awaitUntil(browser.title() == t"Done")
```

- The policy is a `Tenacity`, which is the same [retry schedule](concurrency.md#retrying) used
- elsewhere, so the interval and the number of attempts belong to the caller. Exhausting them
- raises a `Tenacity.Error`. These methods poll the plural endpoint, which reports absence as an
- empty list and not as an error. That is deliberate. The error [strategy] of a session is fixed
- when it opens, so a failure raised inside the polled block could not be caught here to drive
- the next attempt.
+ La politique est un `Tenacity`, qui est le même
+ [calendrier de nouvelles tentatives](concurrency.md#retrying) qu'ailleurs ; l'intervalle et
+ le nombre de tentatives appartiennent donc à l'appelant. Les épuiser lève une
+ `Tenacity.Error`. Ces méthodes interrogent l'endpoint au pluriel, qui signale l'absence par
+ une liste vide et non par une erreur. C'est délibéré. La [stratégie] d'erreur d'une session
+ est fixée à son ouverture ; un échec levé à l'intérieur du bloc interrogé ne pourrait donc
+ pas être intercepté ici pour déclencher la tentative suivante.

- ### Typed page scripting
+ ### Scripting de page typé

- Behavior attached to a page, such as an `onclick` or an `onchange`, is written as a typed DOM
- expression, and not as a JavaScript string. The `document` and `window` values navigate the real
- interfaces of the DOM, checked against WebIDL, and render to JavaScript where an
- [HTML](html.md) attribute expects a script:
+ Le comportement attaché à une page, comme un `onclick` ou un `onchange`, s'écrit sous forme
+ d'expression DOM typée, et non de chaîne JavaScript. Les valeurs `document` et `window`
+ parcourent les interfaces réelles du DOM, vérifiées par rapport à WebIDL, et se rendent en
+ JavaScript là où un attribut [HTML](html.md) attend un script :

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import htmlDoms.html4Transitional

Out.println(html4Transitional.Button(onclick = document.getElementById(t"stage").focus()).show)
```

- A method which the DOM does not define, or an argument of the wrong type, is a compile error:
+ Une méthode que le DOM ne définit pas, ou un argument du mauvais type, est une erreur de
+ compilation :

```scala fuel
compile
errors show
##
document.getElementByld(t"stage")
```

- The JavaScript which reaches the page is known to be well-formed against the interfaces it
- calls.
+ Le JavaScript qui atteint la page est connu pour être bien formé par rapport aux interfaces
+ qu'il appelle.

- ### Where each fits
+ ### La place de chacun

- The two halves meet in testing. A page whose handlers were generated from checked expressions is
- exercised by a driver whose locators are checked selectors. The whole round trip, which is to
- generate, serve, drive and assert, stays inside the type system, with the browser as the only
- unchecked component.
+ Les deux moitiés se rejoignent dans les tests. Une page dont les gestionnaires ont été
+ générés à partir d'expressions vérifiées est exercée par un pilote dont les localisateurs
+ sont des sélecteurs vérifiés. L'aller-retour complet, qui consiste à générer, servir,
+ piloter et vérifier, reste à l'intérieur du système de types, le navigateur étant le seul
+ composant non vérifié.

- ### Related
+ ### Voir aussi

- - [HTML](html.md) and [CSS](css.md) describe the typed documents, selectors and identifiers
-   which locators and handlers share.
- - [Browser DOM](dom.md) describes scripting which runs in the browser itself, under Scala.js.
- - [Foreign interoperability](foreign-interop.md) explains how the WebIDL definitions are read.
- - [Testing](testing.md) hosts the end-to-end tests which drive a browser.
- - [Concurrency](concurrency.md#retrying) describes the `Tenacity` which governs waiting.
- - [Images](images.md) describes the `Raster in Png` which a screenshot gives.
- - [Filesystem](filesystem.md), [terminal](terminal.md) and [XML](xml.md) describe the handles, key presses and XPath locators which a browser session uses.
+ - [HTML](html.md) et [CSS](css.md) décrivent les documents, sélecteurs et identifiants typés
+   que partagent localisateurs et gestionnaires.
+ - [DOM du navigateur](dom.md) décrit le scripting qui s'exécute dans le navigateur lui-même,
+   sous Scala.js.
+ - [Interopérabilité étrangère](foreign-interop.md) explique comment les définitions WebIDL
+   sont lues.
+ - [Tests](testing.md) accueille les tests de bout en bout qui pilotent un navigateur.
+ - [Concurrence](concurrency.md#retrying) décrit le `Tenacity` qui régit l'attente.
+ - [Images](images.md) décrit le `Raster in Png` que donne une capture d'écran.
+ - [Système de fichiers](filesystem.md), [terminal](terminal.md) et [XML](xml.md) décrivent
+   les poignées, les frappes de touches et les localisateurs XPath qu'utilise une session de
+   navigateur.
