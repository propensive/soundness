- ## Charts
+ ## Graphiques

- ### About
+ ### Présentation

- A chart turns a run of numbers into a picture: bars for categories, a line for a trend, wedges
- for the parts of a whole, a histogram for a distribution. Charts are drawn here as [SVG](svg.md)
- from typed data, in four separable layers: the shape of the data, the kind of chart, the fit of
- one to the other, and the style. Each can be chosen without disturbing the rest, and a chart can
- be revised as new data arrives.
+ Un graphique transforme une suite de nombres en image : des barres pour des catégories, une
+ ligne pour une tendance, des secteurs pour les parties d'un tout, un histogramme pour une
+ distribution. Les graphiques sont dessinés ici en [SVG](svg.md) à partir de données typées, en
+ quatre couches séparables : la forme des données, le genre de graphique, l'ajustement de l'une
+ à l'autre, et le style. Chacune peut être choisie sans perturber les autres, et un graphique
+ peut être révisé à mesure que de nouvelles données arrivent.

- ### On charting
+ ### À propos des graphiques

- Charting libraries usually take a bag of numbers and a bag of options, and discover at [runtime]
- that the two do not go together: a pie chart of three series, a line through category labels, a
- logarithmic axis over a range which includes zero. The decisions which ought to be separate,
- which are what the data is, how it is drawn, how its axes are chosen and what it looks like,
- arrive tangled in one call.
+ Les bibliothèques de graphiques prennent habituellement un sac de nombres et un sac d'options,
+ et découvrent [à l'exécution][exécution] que les deux ne vont pas ensemble : un camembert de
+ trois séries, une ligne passant par des étiquettes de catégories, un axe logarithmique sur une
+ plage qui comprend zéro. Les décisions qui devraient être séparées, à savoir ce que sont les
+ données, comment elles sont dessinées, comment leurs axes sont choisis et à quoi elles
+ ressemblent, arrivent enchevêtrées dans un seul appel.

- Here the four decisions are kept apart. The *shape* of the data is in its types. A series over
- `Text` is categorical, and a series over `Double` or a `Duration` is continuous. A chart *kind*
- is compatible with some shapes and not others, and the compiler checks the pairing. The *fit* of
- the data to the axes, which is linear or logarithmic, padded to round numbers or tight, anchored
- at zero or not, is a calibration chosen for each axis type, or passed explicitly. Everything
- about the *style*, which is size, colors, type and legend, is a separate value. This is
- [decoupling](../../philosophy/decoupling.md) of meaning from rendering.
+ Ici, les quatre décisions sont maintenues séparées. La *forme* des données est dans leurs
+ types. Une série sur `Text` est catégorielle, et une série sur `Double` ou sur une `Duration`
+ est continue. Un *genre* de graphique est compatible avec certaines formes et pas avec
+ d'autres, et le compilateur vérifie l'association. L'*ajustement* des données aux axes,
+ linéaire ou logarithmique, élargi jusqu'à des nombres ronds ou serré, ancré à zéro ou non, est
+ un calibrage choisi pour chaque type d'axe, ou passé explicitement. Tout ce qui relève du
+ *style*, à savoir la taille, les couleurs, la typographie et la légende, est une valeur
+ distincte. C'est le [découplage](../../philosophy/decoupling.md) du sens et du rendu.

- Everything comes from the `soundness` package. The second import chooses a [strategy] for
- errors, and the third lets the samples print their results:
+ Tout provient du package `soundness`. Le deuxième import choisit une [stratégie] pour les
+ erreurs, et le troisième permet aux exemples d'afficher leurs résultats :

```scala
import soundness.*
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### Series
+ ### Séries

- A `Series` is a named run of points, each an abscissa paired with an ordinate:
+ Une `Series` est une suite nommée de points, chacun étant une abscisse associée à une
+ ordonnée :

```scala
val north = Series(t"north")((t"jan", 3.0), (t"feb", 5.0), (t"mar", 4.0))
val south = Series(t"south")((t"jan", 2.0), (t"feb", 1.0), (t"mar", 6.0))
```

- The type of the abscissa decides the axis. A `Text`, an [enumeration] or a `Category` is
- `Categorical`: its values are distinct labels, in the order of their first appearance. A number,
- a `Duration`, an `Instant` or any `Quantity` is `Continuous`: its values have positions on a
- numeric axis. Either [typeclass] can be given for a type of your own.
+ Le type de l'abscisse détermine l'axe. Un `Text`, une [énumération] ou une `Category` est
+ `Categorical` : ses valeurs sont des étiquettes distinctes, dans l'ordre de leur première
+ apparition. Un nombre, une `Duration`, un `Instant` ou toute `Quantity` est `Continuous` : ses
+ valeurs ont des positions sur un axe numérique. L'une ou l'autre [classe de types] peut être
+ fournie pour un type de votre cru.

- A `Chart.Estimate` is a continuous value with the interval it lies in, which a chart draws as an
- error bar or a band:
+ Un `Chart.Estimate` est une valeur continue accompagnée de l'intervalle dans lequel elle se
+ trouve, qu'un graphique dessine sous forme de barre d'erreur ou de bande :

```scala
val timings =
  Series(t"sort")((1000, Chart.Estimate(2.1, 1.9, 2.4)), (10000, Chart.Estimate(24.0, 22.5, 26.1)))
```

- A value may also carry a note, as `Noted(3.0, t"Paris")`, which a scatter plot or a line chart
- sets beside the point. The name of a series, like that of a sample set or a `Category`, is any
- showable value, such as text, an enumeration case or a number, rendered once when it is made.
+ Une valeur peut aussi porter une note, comme `Noted(3.0, t"Paris")`, qu'un nuage de points ou
+ un graphique en lignes place à côté du point. Le nom d'une série, comme celui d'un ensemble
+ d'échantillons ou d'une `Category`, est n'importe quelle valeur affichable, comme du texte, un
+ cas d'énumération ou un nombre, rendue une seule fois à sa création.

- A series is [immutable], and `add` appends a point in constant amortized time, which is what a
- chart fed one measurement at a time needs. Several series are any collection of them, such as a
- `List`, a `Sequence` or a `Set`, since compatibility is stated for anything traversable by
- series.
+ Une série est [immuable], et `add` ajoute un point en temps constant amorti, ce dont a besoin
+ un graphique alimenté une mesure à la fois. Plusieurs séries forment n'importe quelle
+ collection de séries, comme une `List`, une `Sequence` ou un `Set`, car la compatibilité est
+ déclarée pour tout ce qui se parcourt par séries.

- ### Chart kinds
+ ### Genres de graphiques

- A chart kind is a value, and each draws data of its own shape:
+ Un genre de graphique est une valeur, et chacun dessine des données de sa propre forme :

```tabular
- column  Kind
- column  Draws
- column  Abscissa
- column  Ordinate
- column  Style
- row  `Bars()`  a series, or a collection of series  categorical  continuous  `Bars.Style`
- row  `StackedBars()`  a collection of series  categorical  continuous  `StackedBars.Style`
- row  `Lines()`  a series, or a collection of series  continuous  continuous  `Lines.Style`
- row  `Scatter()`  a series, or a collection of series  continuous  continuous  `Scatter.Style`
- row  `Pie()`  exactly one series  categorical  continuous  `Pie.Style`
- row  `Histogram()`  `Samples`, or a collection of them  continuous  counts  `Histogram.Style`
- row  `Boxes()`  `Samples`, or a collection of them  one box per set  continuous  `Boxes.Style`
- row  `Doughnut()`  a `Dag` of `Weighted`, `Progressive` nodes  none  none  `Doughnut.Style`
+ column  Genre
+ column  Dessine
+ column  Abscisse
+ column  Ordonnée
+ column  Style
+ row  `Bars()`  une série, ou une collection de séries  catégorielle  continue  `Bars.Style`
+ row  `StackedBars()`  une collection de séries  catégorielle  continue  `StackedBars.Style`
+ row  `Lines()`  une série, ou une collection de séries  continue  continue  `Lines.Style`
+ row  `Scatter()`  une série, ou une collection de séries  continue  continue  `Scatter.Style`
+ row  `Pie()`  exactement une série  catégorielle  continue  `Pie.Style`
+ row  `Histogram()`  des `Samples`, ou une collection de ceux-ci  continue  effectifs  `Histogram.Style`
+ row  `Boxes()`  des `Samples`, ou une collection de ceux-ci  une boîte par ensemble  continue  `Boxes.Style`
+ row  `Doughnut()`  un `Dag` de nœuds `Weighted`, `Progressive`  aucune  aucune  `Doughnut.Style`
```

- The `chart` method pairs data with a kind, and compiles only where the kind can draw the shape
- of the data:
+ La méthode `chart` associe des données à un genre, et ne compile que là où le genre sait
+ dessiner la forme des données :

```scala
val sales = List(north, south).chart(Bars())
```

- A histogram and a box plot take `Samples`, which are raw measurements, rather than points. A
- pairing which makes no sense is a compile error, not a surprise at runtime. A categorical series
- has no numeric abscissa for a line:
+ Un histogramme et une boîte à moustaches prennent des `Samples`, qui sont des mesures brutes,
+ plutôt que des points. Une association absurde est une erreur de compilation, et non une
+ surprise à l'exécution. Une série catégorielle n'a pas d'abscisse numérique pour une ligne :

```scala fuel
compile
errors show
##
north.chart(Lines())
```

- A pie is one series:
+ Un camembert est une seule série :

```scala fuel
compile
errors show
##
List(north, south).chart(Pie())
```

- The pairing is a `Plottable` instance. A new kind of chart is a new value with instances for the
- shapes it can draw, and nothing else has to change.
+ L'association est une instance de `Plottable`. Un nouveau genre de graphique est une nouvelle
+ valeur dotée d'instances pour les formes qu'il sait dessiner, et rien d'autre n'a à changer.

- ### Fitting
+ ### Ajustement

- Fitting chooses the range and gradations of each axis from the data. The chart kind decides what
- the axis must show. The ordinate of a bar chart is anchored at zero, and that of a line chart is
- not. A `Calibration` decides how: linear or logarithmic positions, padded outward to whole steps
- or tight to the data. The default is linear.
+ L'ajustement choisit la plage et les graduations de chaque axe à partir des données. Le genre
+ de graphique décide de ce que l'axe doit montrer. L'ordonnée d'un graphique en barres est
+ ancrée à zéro, et celle d'un graphique en lignes ne l'est pas. Un `Calibration` décide de la
+ manière : positions linéaires ou logarithmiques, élargies vers l'extérieur jusqu'à des pas
+ entiers ou serrées sur les données. La valeur par défaut est linéaire.

- A calibration is keyed on the type of the values on the axis, so a scoped given changes every
- axis of that type, and an import changes every axis:
+ Un calibrage est indexé sur le type des valeurs de l'axe ; ainsi un given limité à une portée
+ modifie chaque axe de ce type, et un import modifie chaque axe :

```scala
val sizes = Series(t"sort")((100, 1.2), (1000, 14.0), (10000, 160.0), (100000, 1900.0))
```

```scala fuel
run
##
val logged =
  given Int is Calibration = calibrations.logarithmicCalibration
  sizes.chart(Lines())

Out.println(logged.fit.abscissa.transform.inspect)
Out.println(sizes.chart(Lines()).fit.abscissa.transform.inspect)
```

- A calibration can also be passed to a chart kind for one axis, when both axes share a type:
+ Un calibrage peut aussi être passé à un genre de graphique pour un seul axe, quand les deux
+ axes partagent un type :

```scala
val scaled = sizes.chart(Lines(ordinate = calibrations.adaptiveCalibration))
```

- The calibrations are givens in the `calibrations` package:
+ Les calibrages sont des givens du package `calibrations` :

```tabular
- column  Calibration
- column  Positions
- column  Range
- row  `linearCalibration`  linear  padded outward to whole gradations; the default
- row  `logarithmicCalibration`  logarithmic, or linear when the data reaches zero  whole decades
- row  `adaptiveCalibration`  logarithmic when the range spans three orders of magnitude, else linear  padded
- row  `tightCalibration`  linear  exactly the extent of the data
+ column  Calibrage
+ column  Positions
+ column  Plage
+ row  `linearCalibration`  linéaires  élargie vers l'extérieur jusqu'à des graduations entières ; par défaut
+ row  `logarithmicCalibration`  logarithmiques, ou linéaires quand les données atteignent zéro  décades entières
+ row  `adaptiveCalibration`  logarithmiques quand la plage couvre trois ordres de grandeur, linéaires sinon  élargie
+ row  `tightCalibration`  linéaires  exactement l'étendue des données
```

- The adaptive calibration is what benchmark timings over input sizes usually want. The `Scale`
- which a fit produces is plain data: its bounds, its transform and its gradations. Gradations are
- spaced at multiples of one, two and five for numbers, and at clock steps, such as seconds,
- half-minutes and hours, for durations and instants.
+ Le calibrage adaptatif est ce que veulent habituellement des durées de benchmark selon la
+ taille des entrées. La `Scale` que produit un ajustement est une simple donnée : ses bornes, sa
+ transformation et ses graduations. Les graduations sont espacées de multiples de un, deux et
+ cinq pour les nombres, et de pas d'horloge, comme les secondes, les demi-minutes et les heures,
+ pour les durées et les instants.

- ### Style
+ ### Style

- Everything about the appearance of a chart which is not data is a *style*: a trait with a method
- for each component the chart draws, such as a tick, a tick label, a bar, a wedge or a legend
- swatch. Each method takes the geometry which the chart computed, and returns the figures to
- draw. `Chart.Style` holds what every kind shares: the canvas, the typeface, the legend and how
- text is set. `Chart.Cartesian` adds axes, gradations, grid, titles, markers and error bars. Each
- kind adds its own: `Bars.Style` has `bar`, `Lines.Style` has `line` and `band`, and `Pie.Style`
- has `wedge`. A chart asks for the style of its kind, so a bar chart compiles against
- `Bars.Style`.
+ Tout ce qui, dans l'apparence d'un graphique, n'est pas une donnée relève du *style* : un trait
+ doté d'une méthode pour chaque composant que dessine le graphique, comme une graduation, une
+ étiquette de graduation, une barre, un secteur ou une pastille de légende. Chaque méthode prend la
+ géométrie que le graphique a calculée, et renvoie les figures à dessiner. `Chart.Style` contient
+ ce que partagent tous les genres : le canevas, la famille de polices, la légende et la façon de
+ composer le texte. `Chart.Cartesian` ajoute les axes, les graduations, la grille, les titres,
+ les marqueurs et les barres d'erreur. Chaque genre ajoute les siens : `Bars.Style` a `bar`,
+ `Lines.Style` a `line` et `band`, et `Pie.Style` a `wedge`. Un graphique demande le style de
+ son genre ; ainsi un graphique en barres compile par rapport à `Bars.Style`.

- `Chart.Standard` implements all of them. It is a case class, so its parameters are named
- arguments, and any component is an override:
+ `Chart.Standard` les implémente tous. C'est une case class ; ses paramètres sont donc des
+ arguments nommés, et tout composant peut être redéfini :

```scala
given Chart.Standard = new Chart.Standard(width = 800.0, ordinateTitle = t"units sold"):
  override def bar(corner: Point, width: Double, height: Double, color: Color in Srgb, series: Int)
  :   List[Figure] =
    List(Rectangle(corner, width.toFloat, height.toFloat, style = Css.Style(fill = color.to[Srgb])))

  override def arrowhead(tip: Point, axis: Chart.Axis, color: Color in Srgb): List[Figure] =
    List(Polyline(List(tip, Point(tip.x - 6, tip.y - 3), Point(tip.x - 6, tip.y + 3)), closed = true))
```

- The hooks cover the details on which a chart is usually judged:
+ Les points d'accroche couvrent les détails sur lesquels on juge habituellement un graphique :

```tabular
- column  Hook
- column  Draws
- column  Defined on
- row  `fontStyle`, `lettering`  the size and colour of text; every run of text  `Chart.Style`
- row  `backdrop`  the canvas behind the chart  `Chart.Style`
- row  `swatch`, `legendLabel`  a legend entry  `Chart.Style`
- row  `leader`  the line from a moved label back to its point  `Chart.Style`
- row  `axisLine`  the stroke of each axis  `Chart.Cartesian`
- row  `axisBreak`  a break mark where a linear axis starts away from zero  `Chart.Cartesian`
- row  `arrowhead`  the end of an axis  `Chart.Cartesian`
- row  `tick`, `gridLine`  a gradation and its grid line  `Chart.Cartesian`
- row  `tickLabel`  a tick's label, where it sits and whether it is rotated  `Chart.Cartesian`
- row  `axisTitle`  an axis's title  `Chart.Cartesian`
- row  `marker`, `errorBar`  a point and its interval  `Chart.Cartesian`
- row  `pointLabel`  the note of a `Noted` point  `Chart.Cartesian`
- row  `bar`  a bar  `Bars.Style`
- row  `line`, `band`  a series line and an estimate's band  `Lines.Style`
- row  `box`, `whisker`, `median`  a box plot's parts  `Boxes.Style`
- row  `wedge`, `wedgeLabel`  a wedge and its percentage  `Pie.Style`
- row  `shade`, `sector`, `remainder`  a sector's colour and its two parts  `Doughnut.Style`
+ column  Point d'accroche
+ column  Dessine
+ column  Défini dans
+ row  `fontStyle`, `lettering`  la taille et la couleur du texte ; chaque segment de texte  `Chart.Style`
+ row  `backdrop`  le canevas derrière le graphique  `Chart.Style`
+ row  `swatch`, `legendLabel`  une entrée de légende  `Chart.Style`
+ row  `leader`  la ligne qui relie une étiquette déplacée à son point  `Chart.Style`
+ row  `axisLine`  le trait de chaque axe  `Chart.Cartesian`
+ row  `axisBreak`  une marque de rupture là où un axe linéaire ne commence pas à zéro  `Chart.Cartesian`
+ row  `arrowhead`  l'extrémité d'un axe  `Chart.Cartesian`
+ row  `tick`, `gridLine`  une graduation et sa ligne de grille  `Chart.Cartesian`
+ row  `tickLabel`  l'étiquette d'une graduation, son emplacement et sa rotation éventuelle  `Chart.Cartesian`
+ row  `axisTitle`  le titre d'un axe  `Chart.Cartesian`
+ row  `marker`, `errorBar`  un point et son intervalle  `Chart.Cartesian`
+ row  `pointLabel`  la note d'un point `Noted`  `Chart.Cartesian`
+ row  `bar`  une barre  `Bars.Style`
+ row  `line`, `band`  la ligne d'une série et la bande d'une estimation  `Lines.Style`
+ row  `box`, `whisker`, `median`  les parties d'une boîte à moustaches  `Boxes.Style`
+ row  `wedge`, `wedgeLabel`  un secteur et son pourcentage  `Pie.Style`
+ row  `shade`, `sector`, `remainder`  la couleur d'un secteur et ses deux parties  `Doughnut.Style`
```

- `labelRoom` tells the layout how much space the tick labels take, so a style which rotates them
- overrides it beside `tickLabel`.
+ `labelRoom` indique à la mise en page combien de place occupent les étiquettes des graduations ;
+ ainsi un style qui les fait pivoter le redéfinit à côté de `tickLabel`.

- Colors are a `ChartPalette`, named by role: the series ramp, the axes, the grid, the lettering.
- One style therefore renders under any palette, and a palette is chosen by importing one:
+ Les couleurs forment une `ChartPalette`, nommées par rôle : la gamme des séries, les axes, la
+ grille, le lettrage. Un même style se rend donc sous n'importe quelle palette, et on choisit
+ une palette en l'important :

```scala
import palettes.solarizedDarkChartPalette
```

- The `font` of a style is a `Font in Web`: a face paired with a provision for its typeface, as
- [fonts](fonts.md#providing-a-typeface) describes. A chart can therefore be set only in a
- typeface which its SVG will carry a `@font-face` for, or in a generic family, which is the
- default. A provision is declared once, and the font is chosen in the style:
+ La `font` d'un style est une `Font in Web` : un style de police associé à une fourniture pour sa
+ famille, comme le décrit la page [polices](fonts.md#providing-a-typeface). Un graphique ne peut
+ donc être composé que dans une famille pour laquelle son SVG transportera un `@font-face`, ou
+ dans une famille générique, ce qui est le cas par défaut. Une fourniture est déclarée une seule
+ fois, et la police est choisie dans le style :

```scala
given menloProvision: ((Typeface of "Menlo") is Typesettable in Web) = Web.local()

val monospaced = Chart.Standard(font = Web.font(Typeface["Menlo"].face))
```

- Text is measured to lay out the axis labels and the legend. The glyph advances of an embedded
- font are used, so the face measured is the face rendered, and the margins fit their labels
- exactly. For a font without a file to hand, an average width for each character is assumed.
- Either can be overridden by importing a metric:
+ Le texte est mesuré pour disposer les étiquettes des axes et la légende. Les chasses des glyphes
+ d'une police incorporée sont utilisées ; ainsi le style mesuré est le style rendu, et les
+ marges s'ajustent exactement à leurs étiquettes. Pour une police dont le fichier n'est pas
+ disponible, une largeur moyenne par caractère est supposée. L'une ou l'autre peut être
+ remplacée en important une métrique :

```scala
import fontMetrics.averageFontMetric
```

- An axis over a [quantity](quantities.md) is titled by what it measures and its unit, taken from
- the type. A series of `Quantity[Metres[1]]` values has the ordinate title `distance / m`, and
- one of `Metre/Second` values has `velocity / m·s¯¹`, with the units rendered as they are
- everywhere else. A title given in the style replaces the name but keeps the unit. The style
- given above titles the ordinate `units sold`, so this chart of heights in meters is titled
- `units sold / m`:
+ Un axe sur une [quantité](quantities.md) a pour titre ce qu'il mesure et son unité, tirés du
+ type. Une série de valeurs `Quantity[Metres[1]]` a pour titre d'ordonnée `distance / m`, et une
+ série de valeurs `Metre/Second` a `velocity / m·s¯¹`, les unités étant rendues comme partout
+ ailleurs. Un titre donné dans le style remplace le nom mais conserve l'unité. Le style donné
+ ci-dessus intitule l'ordonnée `units sold` ; ce graphique de hauteurs en mètres a donc pour
+ titre `units sold / m` :

```scala
val tides = Series(t"tide")((0.0, 1.2*Metre), (6.0, 4.6*Metre), (12.0, 1.1*Metre))
```

```scala fuel
run
##
Out.println(tides.chart(Lines()).in[Svg].in[Xml].show.contains(t"units sold / m").inspect)
```

- A `Duration` axis is titled `time`, and its gradations carry their own units, such as `250ms`
- and `1m30s`, since clock steps are not decimal.
+ Un axe de `Duration` a pour titre `time`, et ses graduations portent leurs propres unités,
+ comme `250ms` et `1m30s`, car les pas d'horloge ne sont pas décimaux.

- ### Placing labels
+ ### Placement des étiquettes

- The text on a chart is placed by [label placement](label-placement.md), so labels do not
- collide. Tick labels, axis titles and legend entries hold still, and a tick label which would
- overlap its neighbor is left out. The note beside a `Noted` point, and the percentage in the
- wedge of a pie, move. A point's note is set to its right when that side is clear of other
- notes, markers and lines, on another side when it is not, and further off, with a leader line
- back to the point, when every side is taken. A wedge's percentage sits in its wedge, or is moved
- outward with a leader when the label of a neighboring wedge is in the way.
+ Le texte d'un graphique est placé par le [placement des étiquettes](label-placement.md), si bien
+ que les étiquettes ne se chevauchent pas. Les étiquettes de graduations, les titres d'axes et les
+ entrées de légende restent immobiles, et une étiquette de graduation qui chevaucherait sa voisine
+ est omise. La note à côté d'un point `Noted`, et le pourcentage dans le secteur d'un camembert,
+ se déplacent. La note d'un point est placée à sa droite quand ce côté est libre d'autres notes,
+ de marqueurs et de lignes, d'un autre côté quand il ne l'est pas, et plus loin, avec une ligne
+ de rappel vers le point, quand tous les côtés sont pris. Le pourcentage d'un secteur se place
+ dans son secteur, ou est déplacé vers l'extérieur avec une ligne de rappel quand l'étiquette d'un
+ secteur voisin gêne.

- The `Arranger` in scope decides the arrangement. The default places each label in turn. For a
- crowded chart, simulated annealing considers the labels together, and is chosen by importing
- it:
+ L'`Arranger` dans la portée décide de la disposition. Celui par défaut place chaque étiquette à
+ tour de rôle. Pour un graphique encombré, le recuit simulé considère les étiquettes ensemble, et
+ on le choisit en l'important :

```scala
import arrangers.annealingArranger
```

- A style draws each label where the arrangement put it. `tickLabel`, `pointLabel` and
- `wedgeLabel` take a `Chart.Anchoring`, which is the point and the alignment which sets the text
- on the right side of it, and `leader` draws the line from a moved label back to its point.
+ Un style dessine chaque étiquette là où la disposition l'a placée. `tickLabel`, `pointLabel` et
+ `wedgeLabel` prennent un `Chart.Anchoring`, c'est-à-dire le point et l'alignement qui place le
+ texte du bon côté de celui-ci, et `leader` dessine la ligne qui relie une étiquette déplacée à son
+ point.

- ### Rendering
+ ### Rendu

- A chart renders to an `Svg`, which serializes to XML text like any other:
+ Un graphique se rend en un `Svg`, qui se sérialise en texte XML comme n'importe quel autre :

```scala fuel
run
##
val drawing = sales.in[Svg]

Out.println(drawing.in[Xml].show.keep(70))
```

- Every part of the drawing is a group with a stable identifier, which is what makes the chart
- revisable:
+ Chaque partie du dessin est un groupe doté d'un identifiant stable, et c'est ce qui rend le
+ graphique révisable :

```tabular
- column  Identifier
- column  Holds
- column  In
- row  `backdrop`  the canvas, in the palette's background colour  every chart
- row  `grid`  the grid lines  cartesian charts
- row  `abscissa`, `ordinate`  each axis, its gradations and its title  cartesian charts
- row  `legend`  the legend  charts with a legend
- row  `series-0`, `series-1`, …  each series  cartesian charts
- row  `wedges`  every wedge and its label  `Pie`
- row  `rank-0`, `rank-1`, …  each band  `Doughnut`
+ column  Identifiant
+ column  Contient
+ column  Dans
+ row  `backdrop`  le canevas, dans la couleur de fond de la palette  tout graphique
+ row  `grid`  les lignes de la grille  les graphiques cartésiens
+ row  `abscissa`, `ordinate`  chaque axe, ses graduations et son titre  les graphiques cartésiens
+ row  `legend`  la légende  les graphiques dotés d'une légende
+ row  `series-0`, `series-1`, …  chaque série  les graphiques cartésiens
+ row  `wedges`  chaque secteur et son étiquette  `Pie`
+ row  `rank-0`, `rank-1`, …  chaque bande  `Doughnut`
```

- ### Revision
+ ### Révision

- A chart is immutable. Given new data, `revise` gives the next chart and a list of what changed
- on the page. If the fit still accommodates the new data, the axes hold still, and only the parts
- which differ are replaced. If a point falls outside them, the whole chart is redrawn:
+ Un graphique est immuable. À partir de nouvelles données, `revise` donne le graphique suivant
+ et une liste de ce qui a changé sur la page. Si l'ajustement accueille encore les nouvelles
+ données, les axes restent immobiles, et seules les parties qui diffèrent sont remplacées. Si un
+ point tombe en dehors, le graphique entier est redessiné :

```scala fuel
run
##
import dysasymptotics.linearSize

val latency = Series(t"p99")((0.0, 12.0), (10.0, 14.0)).chart(Lines())
val (next, revisions) = latency.revise(latency.data.add((5.0, 13.0)))

Out.println(revisions.size.show)
```

- The one revision is a `Chart.Revision.Replace` of `series-0`. A page which holds the SVG needs
- only two operations, which are to replace an element by its identifier and to replace the whole
- drawing, so a chart can be kept current over a WebSocket by sending each revision as it is
- produced.
+ L'unique révision est un `Chart.Revision.Replace` de `series-0`. Une page qui contient le SVG
+ n'a besoin que de deux opérations, à savoir remplacer un élément par son identifiant et
+ remplacer le dessin entier ; ainsi un graphique peut être maintenu à jour via un WebSocket en
+ envoyant chaque révision à mesure qu'elle est produite.

- ### Distributions
+ ### Distributions

- Raw measurements are `Samples`. A `Histogram` cuts their range into equal bins, by Sturges' rule
- unless a count of bins is given, and counts the samples in each. `Boxes` summarizes each set of
- samples as a box between its quartiles, with whiskers to its extremes:
+ Les mesures brutes sont des `Samples`. Un `Histogram` découpe leur plage en classes égales,
+ selon la règle de Sturges sauf si un nombre de classes est donné, et compte les échantillons de
+ chacune. `Boxes` résume chaque ensemble d'échantillons par une boîte entre ses quartiles, avec
+ des moustaches jusqu'à ses extrêmes :

```scala fuel
run
##
import dysasymptotics.linearSize

val runs = List
  ( Samples(t"before")(21.0, 22.5, 23.1, 22.8, 24.0),
    Samples(t"after")(18.2, 18.9, 19.5, 18.1, 19.0) )

Out.println(runs.chart(Histogram()).fit.edges.size.show)
Out.println(runs.chart(Boxes()).fit.summaries.size.show)
```

- Both accept any collection of sample sets, as the other kinds accept collections of series.
+ Tous deux acceptent n'importe quelle collection d'ensembles d'échantillons, comme les autres
+ genres acceptent des collections de séries.

- ### Doughnuts
+ ### Graphiques en anneau

- A `Doughnut` draws a dependency graph, a [`Dag`](graphs.md), as concentric bands. The nodes
- which depend on nothing are in the innermost band, and each further band holds the nodes one
- rank further out, so a build fills in from the center outwards. Each node is a sector of its
- band, which sweeps an angle proportional to its size within the rank, and a band is as thick as
- its rank's share of the total size.
+ Un `Doughnut` dessine un graphe de dépendances, un [`Dag`](graphs.md), sous forme de bandes
+ concentriques. Les nœuds qui ne dépendent de rien sont dans la bande la plus intérieure, et
+ chaque bande suivante contient les nœuds d'un rang plus loin ; ainsi un build se remplit du
+ centre vers l'extérieur. Chaque nœud est un secteur de sa bande, qui balaie un angle
+ proportionnel à sa taille au sein du rang, et une bande est aussi épaisse que la part de son
+ rang dans la taille totale.

- The size of a node and its progress, from `0.0` to `1.0`, come from two typeclasses on the node
- type, `Weighted` and `Progressive`. The sector fills clockwise as far as the node is complete,
- with the remainder in a subdued tint of the sector's color. That color is the first color of the
- palette, shaded by where the sector lies: its hue turns through thirty degrees from left to
- right, and it is darker below the middle and lighter above. A doughnut has no labels and no
- legend. Each band is a `rank-0`, `rank-1`, … group, each sector a `sector-0`, `sector-1`, …
- path, and the unfinished part of a sector is `sector-0-rest`.
+ La taille d'un nœud et sa progression, de `0.0` à `1.0`, proviennent de deux classes de types
+ sur le type des nœuds, `Weighted` et `Progressive`. Le secteur se remplit dans le sens des
+ aiguilles d'une montre à proportion de l'achèvement du nœud, le reste étant dans une teinte
+ atténuée de la couleur du secteur. Cette couleur est la première couleur de la palette, nuancée
+ selon l'emplacement du secteur : sa teinte tourne de trente degrés de gauche à droite, et elle
+ est plus sombre sous le milieu et plus claire au-dessus. Un graphique en anneau n'a ni étiquettes
+ ni légende. Chaque bande est un groupe `rank-0`, `rank-1`, …, chaque secteur un chemin
+ `sector-0`, `sector-1`, …, et la partie inachevée d'un secteur est `sector-0-rest`.

- A node carries its progress, so a doughnut is revised like any chart, with new data. The fit
- survives any graph of the same shape and sizes, and only the bands whose sectors have filled
- further are replaced:
+ Un nœud porte sa progression ; un graphique en anneau se révise donc comme n'importe quel
+ graphique, avec de nouvelles données. L'ajustement survit à tout graphe de même forme et de
+ mêmes tailles, et seules les bandes dont les secteurs se sont davantage remplis sont
+ remplacées :

```scala
case class Step(name: Text, size: Double, done: Double)
given stepWeighted: (Step is Weighted) = _.size
given stepProgressive: (Step is Progressive) = _.done

val parse = Step(t"parse", 2.0, 1.0)
val typecheck = Step(t"typecheck", 3.0, 0.5)
val emit = Step(t"emit", 1.0, 0.0)
val doughnut = Dag(typecheck -> parse, emit -> typecheck).chart(Doughnut())
```

```scala fuel
run
##
import dysasymptotics.linearSize

Out.println(doughnut.fit.bands.size.show)

val typechecked = typecheck.copy(done = 1.0)
Out.println(doughnut.revise(Dag(typechecked -> parse, emit -> typechecked))(1).size.show)
```

- There are three bands, one for each rank. Finishing `typecheck` replaces only `rank-1`.
+ Il y a trois bandes, une par rang. Terminer `typecheck` ne remplace que `rank-1`.

- `Chart.Standard(hole = 0.3)` hollows the center. `sectorGap` and `bandGap` are the widths of the
- grout between sectors and between bands. `Doughnut.Style` has `shade` for the color of a sector
- from its position, and `sector` and `remainder` for its two parts. The ranking is the one
- [graphs](graphs.md#layering) describes: `import rankings.balancedRanking` draws each node as
- close to its dependents as it can be, rather than as close to the center.
+ `Chart.Standard(hole = 0.3)` évide le centre. `sectorGap` et `bandGap` sont les largeurs du
+ joint entre les secteurs et entre les bandes. `Doughnut.Style` a `shade` pour la couleur d'un
+ secteur selon sa position, et `sector` et `remainder` pour ses deux parties. Le classement est
+ celui que décrit la page [graphes](graphs.md#layering) : `import rankings.balancedRanking`
+ dessine chaque nœud aussi près que possible de ses dépendants, plutôt que du centre.

- ### Related
+ ### Voir aussi

- - [SVG](svg.md) describes the drawing which a chart renders to.
- - [Label placement](label-placement.md) describes how the labels of a chart avoid each other.
- - [Fonts](fonts.md) describes the typefaces a chart may be set in.
- - [Quantities](quantities.md) describes the units which title an axis.
- - [Graphs](graphs.md) describes the `Dag` which a doughnut draws.
- - [Colors](colors.md) describes the palette's colors.
- - [Time](time.md) describes the durations and instants which a continuous axis may hold.
- - [Gauges](gauges.md) and [tables](tables.md) present values in the terminal, and [testing](testing.md) charts benchmark results.
+ - [SVG](svg.md) décrit le dessin en lequel se rend un graphique.
+ - [Placement des étiquettes](label-placement.md) décrit comment les étiquettes d'un graphique
+   s'évitent.
+ - [Polices](fonts.md) décrit les familles de polices dans lesquelles un graphique peut être
+   composé.
+ - [Quantités](quantities.md) décrit les unités qui donnent son titre à un axe.
+ - [Graphes](graphs.md) décrit le `Dag` que dessine un graphique en anneau.
+ - [Couleurs](colors.md) décrit les couleurs de la palette.
+ - [Temps](time.md) décrit les durées et les instants que peut contenir un axe continu.
+ - [Jauges](gauges.md) et [tableaux](tables.md) présentent des valeurs dans le terminal, et
+   [tests](testing.md) représente les résultats de benchmark sous forme de graphiques.
