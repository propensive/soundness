- ## Mathematical Markup
+ ## Balisage mathématique

- ### About
+ ### Présentation

- Mathematics on a web page is [MathML](https://www.w3.org/TR/MathML-Core/): a presentation tree
- of rows, fractions, scripts and roots, which browsers render natively. That tree is modeled
- here as typed values, read back from XML, and embedded in [HTML](html.md). Writing MathML by
- hand is unbearable, so *Ergo* is provided too: a one-line shorthand which parses to the same
- tree, and serializes back out of it.
+ Les mathématiques sur une page web, c'est [MathML](https://www.w3.org/TR/MathML-Core/) : un arbre
+ de présentation fait de lignes, de fractions, d'indices et exposants et de racines, que les
+ navigateurs rendent nativement. Cet arbre est modélisé ici sous forme de valeurs typées, relu
+ depuis du XML, et incorporé dans du [HTML](html.md). Écrire du MathML à la main est
+ insupportable : *Ergo* est donc fourni aussi, une notation abrégée sur une ligne qui s'analyse
+ en le même arbre, et qui s'en resérialise.

- ### On writing mathematics
+ ### Écrire des mathématiques

- An equation is a tree, and MathML says so honestly. But it says so in perhaps ten times the
- characters of the equation itself. `x²` is `<msup><mi>x</mi><mn>2</mn></msup>`, and a quadratic
- formula runs to several lines which no one can read as mathematics. The alternatives are worse
- in a different way. TeX is compact and expressive, but needs a large renderer and produces
- something the browser cannot introspect. An image loses the content entirely.
+ Une équation est un arbre, et MathML le dit honnêtement. Mais il le dit en peut-être dix fois
+ plus de caractères que l'équation elle-même. `x²` s'écrit `<msup><mi>x</mi><mn>2</mn></msup>`,
+ et une formule quadratique s'étend sur plusieurs lignes que personne ne peut lire comme des
+ mathématiques. Les alternatives sont pires d'une autre manière. TeX est compact et expressif,
+ mais nécessite un gros moteur de rendu et produit quelque chose que le navigateur ne peut pas
+ inspecter. Une image perd entièrement le contenu.

- Ergo takes the middle path. It is a compact linear notation, with one glyph for each structural
- role, which parses to MathML and serializes back. The source stays readable, the output is a
- real document tree, and the round trip is exact. Markup built from typed values, and not from
- strings, is [safety by construction](../../philosophy/safety-by-construction.md) for a notation
- which is easy to get subtly wrong.
+ Ergo emprunte la voie médiane. C'est une notation linéaire compacte, avec un glyphe par rôle
+ structurel, qui s'analyse en MathML et se resérialise. Le code source reste lisible, la sortie
+ est un véritable arbre de document, et l'aller-retour est exact. Un balisage construit à partir
+ de valeurs typées, et non de chaînes, c'est la
+ [sûreté par construction](../../philosophy/safety-by-construction.md) pour une notation dans
+ laquelle il est facile de commettre des erreurs subtiles.

- Everything comes from the `soundness` package. The other imports choose a [strategy] for errors
- and an [XML schema](xml.md) for reading, and let the samples print their results:
+ Tout vient du package `soundness`. Les autres imports choisissent une [stratégie] pour les
+ erreurs et un [schéma XML](xml.md) pour la lecture, et permettent aux exemples d'afficher leurs
+ résultats :

```scala
import soundness.*
import Mathml.*
import errorDiagnostics.stackTracesDiagnostics
import strategies.throwUnsafely
import xmlSchemas.freeformXmlSchema
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### The MathML tree
+ ### L'arbre MathML

- The elements are values named after the tags, and `Math` is the root. These are the ones which
- Ergo writes:
+ Les éléments sont des valeurs nommées d'après les balises, et `Math` est la racine. Voici celles
+ qu'écrit Ergo :

```tabular
- column  Value
- column  Element
- column  Meaning
- row  `Mi`  `<mi>`  identifier
- row  `Mn`  `<mn>`  number
- row  `Mo`  `<mo>`  operator
- row  `Mrow`  `<mrow>`  group
- row  `Mfrac`  `<mfrac>`  fraction
- row  `Msqrt`  `<msqrt>`  square root
- row  `Mroot`  `<mroot>`  nth root
- row  `Msup`  `<msup>`  superscript
- row  `Msub`  `<msub>`  subscript
- row  `Msubsup`  `<msubsup>`  subscript and superscript
- row  `Mover`  `<mover>`  overscript
- row  `Munder`  `<munder>`  underscript
- row  `Munderover`  `<munderover>`  underscript and overscript
- row  `Mtable`, `Mtr`, `Mtd`  `<mtable>`, `<mtr>`, `<mtd>`  table, row, cell
+ column  Valeur
+ column  Élément
+ column  Signification
+ row  `Mi`  `<mi>`  identifiant
+ row  `Mn`  `<mn>`  nombre
+ row  `Mo`  `<mo>`  opérateur
+ row  `Mrow`  `<mrow>`  groupe
+ row  `Mfrac`  `<mfrac>`  fraction
+ row  `Msqrt`  `<msqrt>`  racine carrée
+ row  `Mroot`  `<mroot>`  racine n-ième
+ row  `Msup`  `<msup>`  exposant
+ row  `Msub`  `<msub>`  indice
+ row  `Msubsup`  `<msubsup>`  indice et exposant
+ row  `Mover`  `<mover>`  suscrit
+ row  `Munder`  `<munder>`  souscrit
+ row  `Munderover`  `<munderover>`  souscrit et suscrit
+ row  `Mtable`, `Mtr`, `Mtd`  `<mtable>`, `<mtr>`, `<mtd>`  tableau, ligne, cellule
```

- The other elements, such as `Mtext`, `Mspace`, `Mstyle` and `Mpadded`, are modeled the same
- way. `in[Xml]` renders a `Math`:
+ Les autres éléments, tels que `Mtext`, `Mspace`, `Mstyle` et `Mpadded`, sont modélisés de la
+ même façon. `in[Xml]` rend un `Math` :

```scala fuel
run
##
Out.println(Math(Msup(Mi(t"x"), Mn(t"2"))).in[Xml].show)
```

- MathML text reads back with `read[Math]`, and a document round-trips unchanged. An attribute
- which the model does not interpret is preserved, not dropped:
+ Le texte MathML se relit avec `read[Math]`, et un document fait l'aller-retour sans changement.
+ Un attribut que le modèle n'interprète pas est préservé, et non abandonné :

```scala fuel
run
##
val square = Math(Msup(Mi(t"x"), Mn(t"2")))

Out.println((square.in[Xml].show.read[Math] == square).inspect)
```

- A root element which is not `<math>` raises a `Mathml.Error`, which names what was found
- instead:
+ Un élément racine qui n'est pas `<math>` lève une `Mathml.Error`, qui nomme ce qui a été trouvé
+ à la place :

```scala fuel
run
errors show
##
t"<p>not mathematics</p>".read[Math]
```

- `html` embeds an expression as a foreign element in an HTML tree, and `Mathml.Reader.read`
- extracts it again.
+ `html` incorpore une expression comme élément étranger dans un arbre HTML, et
+ `Mathml.Reader.read` l'en extrait à nouveau.

- A `Math` also has a `display`, which is block or inline. It decides whether the browser sets the
- expression as a displayed equation, or within running text.
+ Un `Math` a aussi un `display`, qui est bloc ou en ligne. Il détermine si le navigateur compose
+ l'expression comme une équation centrée, ou au sein du texte courant.

- ### Ergo
+ ### Ergo

- An Ergo expression is delimited by a pair of brackets, and the **first character** chooses which
- pair, `(`, `[`, `{` or `⟨`, acts as grouping throughout. Every *other* bracket is then a literal
- operator. A grouped run is one `<mrow>`, and the operands of an operator are uniformly "an atom
- or a grouped unit". This is the quadratic formula:
+ Une expression Ergo est délimitée par une paire de parenthèses, et le **premier caractère**
+ choisit quelle paire, `(`, `[`, `{` ou `⟨`, sert au regroupement dans toute l'expression. Toute
+ *autre* parenthèse est alors un opérateur littéral. Une suite regroupée est un `<mrow>`, et les
+ opérandes d'un opérateur sont uniformément « un atome ou une unité regroupée ». Voici la formule
+ quadratique :

```scala fuel
run
##
val quadratic = Ergo.parse(t"(x = (-b ± √(b↗2 - 4 a c))/(2 a))")

Out.println(quadratic.in[Xml].show)
```

- Tokens follow three rules, and a space is a separator which renders as nothing:
+ Les jetons suivent trois règles, et un espace est un séparateur qui ne se rend par rien :

```tabular
- column  A run of
- column  Becomes
- column  Example
- row  letters  one identifier, `<mi>`  `sin` is `<mi>sin</mi>`; `x y` means *x·y*
- row  digits, with an interior `.` allowed  one number, `<mn>`  `3.14`
- row  any other character  an operator, `<mo>`  `=`
+ column  Une suite de
+ column  Devient
+ column  Exemple
+ row  lettres  un identifiant, `<mi>`  `sin` donne `<mi>sin</mi>` ; `x y` signifie *x·y*
+ row  chiffres, un `.` intérieur étant permis  un nombre, `<mn>`  `3.14`
+ row  tout autre caractère  un opérateur, `<mo>`  `=`
```

- ### Structural glyphs
+ ### Glyphes structurels

- Scripts and limits attach to the atom on their left, and a fraction binds more loosely than the
- scripts. An index atom immediately before `√`, with no space, makes an nth root. The three table
- introducers are each self-delimiting, and their body is one group whose children are the
- elements.
+ Les indices, exposants et bornes s'attachent à l'atome situé à leur gauche, et une fraction lie
+ moins fortement que les indices et exposants. Un atome d'indice placé juste avant `√`, sans
+ espace, forme une racine n-ième. Les trois introducteurs de tableau se délimitent chacun
+ eux-mêmes, et leur corps est un groupe dont les enfants sont les éléments.

```tabular
- column  Glyph
- column  Role
- column  Element
- column  Example
- row  `↗`  superscript  `<msup>`  `x↗2` is *x²*
- row  `↘`  subscript  `<msub>`  `x↘i` is *xᵢ*
- row  `↘` and `↗`  both, on one base  `<msubsup>`  `x↘i↗2` is *xᵢ²*
- row  `↑`  overscript or upper limit  `<mover>`
- row  `↓`  underscript or lower limit  `<munder>`
- row  `↓` and `↑`  both, on one base  `<munderover>`  `∑↓(i = 1)↑n`
- row  `/`  fraction  `<mfrac>`  `a/b`
- row  `√`  square root  `<msqrt>`  `√x`
- row  `√` after an index  nth root  `<mroot>`  `3√x` is *∛x*
- row  `⋱`  matrix  `<mtable>`  `⋱(((1)(2))((3)(4)))` is a 2×2 matrix
- row  `⋯`  row vector  a single-row `<mtable>`
- row  `⋮`  column vector  a single-column `<mtable>`
+ column  Glyphe
+ column  Rôle
+ column  Élément
+ column  Exemple
+ row  `↗`  exposant  `<msup>`  `x↗2` donne *x²*
+ row  `↘`  indice  `<msub>`  `x↘i` donne *xᵢ*
+ row  `↘` et `↗`  les deux, sur une même base  `<msubsup>`  `x↘i↗2` donne *xᵢ²*
+ row  `↑`  suscrit ou borne supérieure  `<mover>`
+ row  `↓`  souscrit ou borne inférieure  `<munder>`
+ row  `↓` et `↑`  les deux, sur une même base  `<munderover>`  `∑↓(i = 1)↑n`
+ row  `/`  fraction  `<mfrac>`  `a/b`
+ row  `√`  racine carrée  `<msqrt>`  `√x`
+ row  `√` après un indice  racine n-ième  `<mroot>`  `3√x` donne *∛x*
+ row  `⋱`  matrice  `<mtable>`  `⋱(((1)(2))((3)(4)))` donne une matrice 2×2
+ row  `⋯`  vecteur ligne  un `<mtable>` d'une seule ligne
+ row  `⋮`  vecteur colonne  un `<mtable>` d'une seule colonne
```

- Large operators need no special syntax. `∑` is an ordinary operator, and `∑↓(i = 1)↑n` gives it
- limits below and above:
+ Les grands opérateurs ne nécessitent aucune syntaxe particulière. `∑` est un opérateur
+ ordinaire, et `∑↓(i = 1)↑n` lui donne des bornes en dessous et au-dessus :

```scala fuel
run
##
Out.println(Ergo.parse(t"(∑↓(i = 1)↑n i↗2)").in[Xml].show)
```

- An operator glyph with a missing operand degrades to a literal operator, so `(↗)` writes a
- literal ↗ rather than failing.
+ Un glyphe d'opérateur auquel il manque un opérande se dégrade en opérateur littéral : `(↗)`
+ écrit donc un ↗ littéral au lieu d'échouer.

- ### Attribute directives
+ ### Directives d'attributs

- Each MathML Core presentation attribute has one directive glyph. Directives are *postfix*, and
- bind to the atom or bracketed group immediately to their left. Several simply juxtapose: `=◆⇿`
- sets both `largeop` and `stretchy` on the `=`.
+ Chaque attribut de présentation de MathML Core a un glyphe de directive. Les directives sont
+ *postfixes*, et se lient à l'atome ou au groupe entre parenthèses situé juste à leur gauche.
+ Plusieurs se juxtaposent simplement : `=◆⇿` définit à la fois `largeop` et `stretchy` sur le
+ `=`.

- Enumerated and boolean attributes have one bare glyph for each value, and take no parameter. A
- boolean is `⇿` for true and `↮` for false. Because they never take a group, `=◆(a)` is `=` with
- `largeop="true"`, *times* `(a)`. Open-valued attributes, which are lengths, colors and integers,
- take their value in the active grouping bracket, read verbatim. Under `(` grouping, `x●(red)`
- sets `mathcolor="red"`.
+ Les attributs énumérés et booléens ont un glyphe nu par valeur, et ne prennent aucun paramètre.
+ Un booléen s'écrit `⇿` pour vrai et `↮` pour faux. Comme ils ne prennent jamais de groupe,
+ `=◆(a)` est `=` avec `largeop="true"`, *multiplié par* `(a)`. Les attributs à valeur ouverte,
+ c'est-à-dire les longueurs, les couleurs et les entiers, prennent leur valeur dans la parenthèse
+ de regroupement active, lue telle quelle. Avec le regroupement `(`, `x●(red)` définit
+ `mathcolor="red"`.

- Grouping decides what a directive applies to. `(x↗2)●(red)` colors the whole superscript, while
- `x↗2●(red)` colors only the `2`:
+ Le regroupement détermine ce à quoi s'applique une directive. `(x↗2)●(red)` colore tout
+ l'exposant, tandis que `x↗2●(red)` ne colore que le `2` :

```scala fuel
run
##
Out.println(Ergo.parse(t"((x↗2)●(red))").in[Xml].show)
Out.println(Ergo.parse(t"(x↗2●(red))").in[Xml].show)
```

```tabular
- column  Glyphs
- column  Attribute
- column  Meaning
- row  `⧆` / `⧄`  `displaystyle`  display style, or inline/text style
- row  `⌄[±n]`  `scriptlevel`  relative script size; `+n` shrinks
- row  `◻` / `▭`  `display`  block or inline, on the root
- row  `●[colour]`  `mathcolor`  foreground color
- row  `▨[colour]`  `mathbackground`  background color
- row  `⟑[length]`  `mathsize`  font size
- row  `⦱`  `mathvariant`  upright, canceling automatic italicisation
- row  `⊩` / `⫣`  `dir`  left-to-right or right-to-left
- row  `⊰` / `⊹` / `⊱`  `form`  prefix, infix or postfix operator
- row  `∥` / `∤`  `fence`  mark as a fence
- row  `▮` / `▯`  `separator`  mark as a separator
- row  `⇿` / `↮`  `stretchy`  allow stretching to surrounding content
- row  `⋈` / `⋊`  `symmetric`  stretch symmetrically about the axis
- row  `◆` / `◇`  `largeop`  treat as a large operator in display style
- row  `⧳` / `⧯`  `movablelimits`  limits over/under in display, scripts inline
- row  `⧔[length]` / `⧕[length]`  `lspace` / `rspace`  space either side of an operator
- row  `⟰[length]` / `⟱[length]`  `maxsize` / `minsize`  stretch bounds
- row  `↔[length]`  `width`  advance width of a box
- row  `⍏[length]` / `⍖[length]`  `height` / `depth`  extent above and below the baseline
- row  `↕[length]`  `voffset`  vertical shift of padded content
- row  `═[length]`  `linethickness`  fraction bar thickness; `0` for none
- row  `◠` / `⌢`  `accent`  treat an overscript as a tight accent
- row  `◡` / `⌣`  `accentunder`  treat an underscript as a tight accent
- row  `⚙[type]`  `actiontype`  legacy; undefined in Core
+ column  Glyphes
+ column  Attribut
+ column  Signification
+ row  `⧆` / `⧄`  `displaystyle`  style centré, ou style en ligne/texte
+ row  `⌄[±n]`  `scriptlevel`  taille relative des indices ; `+n` réduit
+ row  `◻` / `▭`  `display`  bloc ou en ligne, sur la racine
+ row  `●[colour]`  `mathcolor`  couleur de premier plan
+ row  `▨[colour]`  `mathbackground`  couleur d'arrière-plan
+ row  `⟑[length]`  `mathsize`  taille de police
+ row  `⦱`  `mathvariant`  droit, annulant la mise en italique automatique
+ row  `⊩` / `⫣`  `dir`  de gauche à droite ou de droite à gauche
+ row  `⊰` / `⊹` / `⊱`  `form`  opérateur préfixe, infixe ou postfixe
+ row  `∥` / `∤`  `fence`  marquer comme délimiteur
+ row  `▮` / `▯`  `separator`  marquer comme séparateur
+ row  `⇿` / `↮`  `stretchy`  permettre l'étirement selon le contenu environnant
+ row  `⋈` / `⋊`  `symmetric`  étirer symétriquement par rapport à l'axe
+ row  `◆` / `◇`  `largeop`  traiter comme un grand opérateur en style centré
+ row  `⧳` / `⧯`  `movablelimits`  bornes dessus/dessous en style centré, indices en ligne
+ row  `⧔[length]` / `⧕[length]`  `lspace` / `rspace`  espace de part et d'autre d'un opérateur
+ row  `⟰[length]` / `⟱[length]`  `maxsize` / `minsize`  limites d'étirement
+ row  `↔[length]`  `width`  largeur d'avance d'une boîte
+ row  `⍏[length]` / `⍖[length]`  `height` / `depth`  étendue au-dessus et au-dessous de la ligne de base
+ row  `↕[length]`  `voffset`  décalage vertical d'un contenu rembourré
+ row  `═[length]`  `linethickness`  épaisseur de la barre de fraction ; `0` pour aucune
+ row  `◠` / `⌢`  `accent`  traiter un suscrit comme un accent serré
+ row  `◡` / `⌣`  `accentunder`  traiter un souscrit comme un accent serré
+ row  `⚙[type]`  `actiontype`  hérité ; non défini dans Core
```

- The scope is MathML Core, so the MathML 3 families which it dropped have no glyphs: the table
- attributes, such as `columnalign`, `rowspan` and `frame`, since Core keeps the table *elements*
- but defines none of these; `<menclose>` and its notations; the quote attributes of `<ms>`; and
- the elementary-mathematics elements `<mstack>`, `<mlongdiv>` and the rest.
+ Le périmètre est MathML Core : les familles de MathML 3 qu'il a abandonnées n'ont donc pas de
+ glyphes : les attributs de tableau, tels que `columnalign`, `rowspan` et `frame`, puisque Core
+ conserve les *éléments* de tableau mais ne définit aucun de ces attributs ; `<menclose>` et ses
+ notations ; les attributs de guillemets de `<ms>` ; et les éléments de mathématiques
+ élémentaires `<mstack>`, `<mlongdiv>` et les autres.

- ### Serializing back
+ ### Resérialiser

- `Ergo.serialize` turns a MathML tree back into Ergo. The two are inverse over the Ergo subset: a
- parsed expression serializes and parses again to the same tree:
+ `Ergo.serialize` retransforme un arbre MathML en Ergo. Les deux sont inverses l'un de l'autre
+ sur le sous-ensemble Ergo : une expression analysée se sérialise et s'analyse à nouveau en le
+ même arbre :

```scala fuel
run
##
Out.println(Ergo.serialize(quadratic))
```

- An element outside that subset, such as `<mtext>`, is rejected with an `Ergo.Error`, and is not
- serialized approximately, so the round trip is a guarantee and not a hope:
+ Un élément hors de ce sous-ensemble, tel que `<mtext>`, est rejeté avec une `Ergo.Error`, et
+ n'est pas sérialisé approximativement : l'aller-retour est donc une garantie et non un espoir :

```scala fuel
run
errors show
##
Ergo.serialize(Math(Mtext(t"hello")))
```

- ### Related
+ ### Voir aussi

- - [HTML](html.md) describes the documents which a `Math` is embedded in.
- - [XML](xml.md) describes the `Xml` which MathML renders to and reads from.
- - [Chemistry](chemistry.md) typesets formulas of another kind.
- - [Mathematics](mathematics.md) computes with the quantities that the markup displays.
+ - [HTML](html.md) décrit les documents dans lesquels un `Math` est incorporé.
+ - [XML](xml.md) décrit le `Xml` vers lequel MathML se rend et depuis lequel il se lit.
+ - [Chimie](chemistry.md) compose des formules d'un autre genre.
+ - [Mathématiques](mathematics.md) calcule avec les quantités qu'affiche le balisage.
