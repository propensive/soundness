- ## Gauges
+ ## Jauges

- ### About
+ ### Présentation

- A long-running program has something to say while it works: how far through it is, that it
- is still alive, how fast bytes are moving, and which step of a pipeline is running. These
- are shown as *gauges*: spinners, progress bars, meters, sparklines, counters and step
- indicators. A gauge is either embedded in a [terminal layout](terminal.md#layout), or drawn
- on its own at the cursor.
+ Un programme qui s'exécute longtemps a quelque chose à dire pendant qu'il travaille : où il
+ en est, qu'il est toujours en vie, à quelle vitesse les octets circulent, et quelle étape
+ d'un pipeline est en cours. Ces informations s'affichent sous forme de *jauges* : spinners,
+ barres de progression, indicateurs de niveau, sparklines, compteurs et indicateurs d'étapes.
+ Une jauge est soit intégrée dans une [mise en page de terminal](terminal.md#layout), soit
+ dessinée seule au curseur.

- A gauge is chosen by the *type* of the thing which it displays. What it looks like, what
- colors it uses, and which characters it may draw with are three separate decisions. Each is
- made by one import, and none of them disturbs the other two.
+ Une jauge est choisie selon le *type* de ce qu'elle affiche. Son apparence, les couleurs
+ qu'elle utilise, et les caractères avec lesquels elle peut dessiner sont trois décisions
+ distinctes. Chacune se prend par un import, et aucune ne perturbe les deux autres.

- ### On progress display
+ ### À propos de l'affichage de la progression

- A display of progress is usually written twice: once as the drawing, and again as the
- arithmetic which decides what fits. The two are kept consistent by hand. The result is a bar
- which is correct at eighty columns and corrupt at thirty, with a row which overruns, a label
- which pushes the bar off the edge, or a percentage which disagrees with the bar beside it.
+ Un affichage de progression s'écrit généralement deux fois : une fois pour le dessin, et une
+ autre pour l'arithmétique qui décide de ce qui tient. Les deux sont maintenus cohérents à la
+ main. Le résultat est une barre correcte à quatre-vingts colonnes et corrompue à trente, avec
+ une ligne qui déborde, une étiquette qui pousse la barre hors du bord, ou un pourcentage qui
+ contredit la barre d'à côté.

- Separating the two removes the duplication. A design says how to draw a status at whatever
- width it is given, and how narrow it can usefully go. The layout says how much room there
- is. Neither needs to know what the other decided. A bar which is handed six cells is drawn
- in six. Handed three, it becomes a percentage, and handed one, it becomes a single shade. A
- design which is chosen by import, independently of the status it draws, is
- [decoupling](../../philosophy/decoupling.md) between meaning and appearance.
+ Séparer les deux supprime la duplication. Un design indique comment dessiner un état à la
+ largeur qu'on lui donne, quelle qu'elle soit, et jusqu'à quelle étroitesse il reste utile. La
+ mise en page indique combien de place il y a. Aucun des deux n'a besoin de savoir ce que
+ l'autre a décidé. Une barre à laquelle on donne six cellules est dessinée en six. Avec trois,
+ elle devient un pourcentage, et avec une, une seule nuance. Un design choisi par import,
+ indépendamment de l'état qu'il dessine, relève du
+ [découplage](../../philosophy/decoupling.md) entre le sens et l'apparence.

- Everything comes from the `soundness` package, and several choices are in scope:
+ Tout provient du package `soundness`, et plusieurs choix sont dans la portée :

```scala
import soundness.*
import textMetrics.uniformMetric
import palettes.solarizedDarkGaugePalette
import probates.awaitProbate
import stdios.javaLangSystemStdio
import termcapDefinitions.xtermTrueColorTermcap
import threads.virtualThreads
import Tui.{Captioned, Countdown, Fraction, Meter, Reading, Reckoning, Standing, Step, Transfer}
```

```tabular
- column  Import
- column  Decides
- row  `textMetrics.uniformMetric`  how the width of text is measured, since a gauge is laid out by the width of what it draws
- row  `palettes.solarizedDarkGaugePalette`  the colors; with none, a palette is chosen from what the terminal reports
- row  `stdios`, `termcapDefinitions`  where the samples print, and what the terminal can display
- row  `threads`, `probates`  the [concurrency](concurrency.md) context in which an animated gauge redraws itself
+ column  Import
+ column  Décide
+ row  `textMetrics.uniformMetric`  la mesure de la largeur du texte, car une jauge se dispose selon la largeur de son dessin
+ row  `palettes.solarizedDarkGaugePalette`  les couleurs ; sans elle, une palette est choisie d'après ce qu'indique le terminal
+ row  `stdios`, `termcapDefinitions`  où écrivent les exemples, et ce que sait afficher le terminal
+ row  `threads`, `probates`  le contexte de [concurrence](concurrency.md) dans lequel une jauge animée se redessine
```

- ### Statuses
+ ### États

- What a gauge displays is a value of a specific type, and that type is what selects the
- design. The status types are nested in `Tui`, and imported from it above:
+ Ce qu'affiche une jauge est une valeur d'un type précis, et c'est ce type qui sélectionne le
+ design. Les types d'état sont imbriqués dans `Tui`, et importés depuis celui-ci ci-dessus :

```tabular
- column  Status
- column  Displays
- column  Default design
- row  `Fraction`  a proportion in `[0, 1]`, clamped  smooth block bar
- row  `Reckoning`  `done` against a total that may be unknown  `17/120`
- row  `Standing`  how one unit of work turned out  `✓ ✗ ‑`
- row  `Duration`  time spent  `2m41s`
- row  `Countdown`  time left, clamped at zero  `2m41s`
- row  `Captioned`  any status, with a label  derived
- row  `Transfer`  bytes moved, with rate and estimate  *none*
- row  `Meter`  a reading on a bounded scale  *none*
- row  `Sequence[Double]`  samples over time  *none*
- row  `Sequence[Step]`  an ordered run of steps  *none*
+ column  État
+ column  Affiche
+ column  Design par défaut
+ row  `Fraction`  une proportion dans `[0, 1]`, bornée  barre de blocs lisse
+ row  `Reckoning`  `done` par rapport à un total qui peut être inconnu  `17/120`
+ row  `Standing`  l'issue d'une unité de travail  `✓ ✗ ‑`
+ row  `Duration`  le temps écoulé  `2m41s`
+ row  `Countdown`  le temps restant, borné à zéro  `2m41s`
+ row  `Captioned`  n'importe quel état, avec une étiquette  dérivé
+ row  `Transfer`  les octets transférés, avec débit et estimation  *aucun*
+ row  `Meter`  une mesure sur une échelle bornée  *aucun*
+ row  `Sequence[Double]`  des échantillons au fil du temps  *aucun*
+ row  `Sequence[Step]`  une suite ordonnée d'étapes  *aucun*
```

- Most of these are types which a program already has. A new type is introduced only where it
- has to be *distinct*, because it is the key by which the design is looked up. A proportion
- cannot be a plain `Double` without claiming every `Double` in the program for progress bars.
- Elapsed time is a [`Duration`](time.md#durations-and-timespans), samples are a
- `Sequence[Double]`, and a run of steps is a `Sequence[Step]`. `Countdown` has a type of its
- own only so that it can sit beside an elapsed time, with a different design.
+ La plupart de ces types sont des types qu'un programme possède déjà. Un nouveau type n'est
+ introduit que là où il doit être *distinct*, parce que c'est la clé par laquelle le design est
+ recherché. Une proportion ne peut pas être un simple `Double` sans réquisitionner chaque
+ `Double` du programme pour des barres de progression. Le temps écoulé est une
+ [`Duration`](time.md#durations-and-timespans), les échantillons sont une
+ `Sequence[Double]`, et une suite d'étapes est une `Sequence[Step]`. `Countdown` n'a son
+ propre type que pour pouvoir figurer à côté d'un temps écoulé, avec un design différent.

- Any status may be [`Optional`](optional-values.md), and every design is lifted to that
- automatically. `Optional[Fraction]` is progress which may not be known yet, so a job which
- starts out unmeasurable, and later learns its total, does not change type half way through.
- A design says how to draw when there is no value. A bar sweeps, because an empty bar claims
- "no progress" where the truth is "not measured", and a spinner draws its frames. Nothing has
- to be written twice for the optional case.
+ N'importe quel état peut être [`Optional`](optional-values.md), et chaque design y est
+ automatiquement étendu. `Optional[Fraction]` est une progression qui peut ne pas encore être
+ connue ; une tâche qui commence sans pouvoir être mesurée, et apprend plus tard son total, ne
+ change donc pas de type en cours de route. Un design indique comment dessiner en l'absence
+ de valeur. Une barre balaie, parce qu'une barre vide affirmerait « aucune progression » là où
+ la vérité est « non mesurée », et un spinner dessine ses images. Rien n'a besoin d'être écrit
+ deux fois pour le cas optionnel.

- The four statuses with no default are deliberate. A default exists only where every candidate
- design is the same height, and says the same thing. A battery, a thermometer and a dial are
- not interchangeable. Block and braille sparklines differ in vertical resolution, and so in
- what the data shows. A checklist is one row for each step, where a breadcrumb is one row
- altogether. And whether a transfer counts in `MB` or `MiB` is an editorial decision. Choosing
- silently in those cases would be choosing how much of the caller's layout to occupy, or what
- their numbers mean.
+ Les quatre états sans design par défaut le sont délibérément. Un design par défaut n'existe
+ que là où tous les designs candidats ont la même hauteur et disent la même chose. Une
+ batterie, un thermomètre et un cadran ne sont pas interchangeables. Les sparklines en blocs
+ et en braille diffèrent par leur résolution verticale, et donc par ce que montrent les
+ données. Une liste de contrôle occupe une ligne par étape, là où un fil d'Ariane occupe une
+ seule ligne en tout. Et le fait qu'un transfert compte en `MB` ou en `MiB` est une décision
+ éditoriale. Choisir silencieusement dans ces cas reviendrait à choisir quelle part de la mise
+ en page de l'appelant occuper, ou ce que signifient ses nombres.

- ### Choosing a design
+ ### Choisir un design

- With no design imported, the defaults apply. `gaugeLine` draws one frame, at a width which
- the caller chooses:
+ Sans design importé, les valeurs par défaut s'appliquent. `gaugeLine` dessine une image, à
+ une largeur que choisit l'appelant :

```scala fuel
run
##
Out.println(gaugeLine(Fraction(0.42), 40))
Out.println(gaugeLine(Reckoning(17, 120), 7))
Out.println(e"${gaugeLine(Standing.Succeeded, 1)} built")
```

- A gauge always needs a width. There is no drawing of a bar which does not know how wide it
- is, so a status cannot be printed on its own.
+ Une jauge a toujours besoin d'une largeur. Il n'existe pas de dessin d'une barre qui ignore
+ sa propre largeur ; un état ne peut donc pas être affiché seul.

- Importing a design by name replaces the default, and nothing else changes:
+ Importer un design par son nom remplace celui par défaut, et rien d'autre ne change :

```scala fuel
run
##
locally:
  import bars.arrowheadBar
  Out.println(gaugeLine(Fraction(0.42), 40))
```

- A spinner is what to import when the figure is not worth showing, or is not known. It is
- keyed on `Fraction`, as a bar is, and ignores the value. A spinner and a bar are therefore
- two designs for the same status, and only one of them can be imported at a time. A layout
- which wants both takes one from a [given] in a local scope:
+ Un spinner est ce qu'il faut importer lorsque le chiffre ne vaut pas la peine d'être montré,
+ ou n'est pas connu. Il est indexé sur `Fraction`, comme une barre, et ignore la valeur. Un
+ spinner et une barre sont donc deux designs pour le même état, et un seul des deux peut être
+ importé à la fois. Une mise en page qui veut les deux en prend un depuis un [given] dans une
+ portée locale :

```scala
val resolving =
  given spinner: (Fraction is Gaugeable) = spinners.brailleDotsSpinner
  gauge(Reading(Fraction.indeterminate))
```

- The designs are grouped into eight families, each of which is a package to import from:
+ Les designs sont regroupés en huit familles, chacune étant un package depuis lequel importer :

```tabular
- column  Family
- column  For
- column  Designs
- column  Range
- row  `spinners`  `Fraction`  42  single-cell braille and block animations to bouncing bars and emoji moon phases
- row  `bars`  `Fraction`  18  the smooth eighth-block default through segmented pips and gradient fills to `[###---]`
- row  `meters`  `Meter`  6  battery, thermometer, needle, bullet, column and ASCII
- row  `sparklines`  `Sequence[Double]`  4  block, tall, dot and ASCII
- row  `counters`  `Reckoning`, `Transfer`  9  plain, padded, worded, scaled and percentage counts; transfer reports
- row  `standings`  `Standing`  5  ASCII, tick, heavy, square and word markers
- row  `processions`  `Sequence[Step]`  5  checklist, breadcrumb, beads, numbered and ribbon
- row  `timers`  `Duration`, `Countdown`  5  compact and digital elapsed times; compact, digital and urgent countdowns
+ column  Famille
+ column  Pour
+ column  Designs
+ column  Éventail
+ row  `spinners`  `Fraction`  42  de l'animation braille d'une cellule aux barres rebondissantes et aux phases de lune en emoji
+ row  `bars`  `Fraction`  18  du huitième de bloc lisse par défaut aux pastilles segmentées et remplissages en dégradé, jusqu'à `[###---]`
+ row  `meters`  `Meter`  6  batterie, thermomètre, aiguille, puce, colonne et ASCII
+ row  `sparklines`  `Sequence[Double]`  4  blocs, haute, points et ASCII
+ row  `counters`  `Reckoning`, `Transfer`  9  comptes simples, complétés, en lettres, à l'échelle, en pourcentage ; transferts
+ row  `standings`  `Standing`  5  marqueurs ASCII, coche, gras, carré et en mots
+ row  `processions`  `Sequence[Step]`  5  liste de contrôle, fil d'Ariane, perles, numéroté, ruban
+ row  `timers`  `Duration`, `Countdown`  5  temps écoulé compact, numérique ; compte à rebours compact, numérique, urgent
```

- ### Colors and glyphs
+ ### Couleurs et glyphes

- Color is an independent choice. A `GaugePalette` names its colors by *role*, so one design
- is drawn under any palette:
+ La couleur est un choix indépendant. Une `GaugePalette` nomme ses couleurs par *rôle* ; un
+ même design se dessine donc sous n'importe quelle palette :

```tabular
- column  Role
- column  Colors
- row  `fill`  the filled part of a bar
- row  `track`  the unfilled part of a bar, hollow pips, the arc a dial's needle sweeps
- row  `leadingEdge`  the boundary cell of a bar and the head of a marker
- row  `caption`  labels and captions
- row  `muted`  completed steps, elapsed times, units
- row  `success`  a succeeded step; the low end of a severity scale
- row  `warning`  a warned step; the middle of a severity scale
- row  `danger`  a failed step; the top of a severity scale
+ column  Rôle
+ column  Colore
+ row  `fill`  la partie remplie d'une barre
+ row  `track`  le vide d'une barre, les pastilles creuses, l'arc que balaie l'aiguille
+ row  `leadingEdge`  la cellule de bordure d'une barre et la tête d'un marqueur
+ row  `caption`  les étiquettes et légendes
+ row  `muted`  les étapes terminées, les temps écoulés, les unités
+ row  `success`  une étape réussie ; le bas d'une échelle de gravité
+ row  `warning`  une étape avec avertissement ; le milieu d'une échelle de gravité
+ row  `danger`  une étape échouée ; le haut d'une échelle de gravité
```

- A palette is chosen by import, as `palettes.solarizedDarkGaugePalette` was above. Ten
- palettes are provided. They include `monochromeGaugePalette`, which has no hues, and
- `ansiSixteenGaugePalette`, whose colors are the canonical values which a sixteen-color
- terminal actually has. With no import, an adaptive palette picks by what the terminal
- reports. [Colors](colors.md#themes-and-palettes) describes palettes generally.
+ Une palette se choisit par import, comme `palettes.solarizedDarkGaugePalette` ci-dessus. Dix
+ palettes sont fournies. Parmi elles figurent `monochromeGaugePalette`, qui n'a pas de
+ teintes, et `ansiSixteenGaugePalette`, dont les couleurs sont les valeurs canoniques que
+ possède réellement un terminal à seize couleurs. Sans import, une palette adaptative choisit
+ d'après ce qu'indique le terminal. [Couleurs](colors.md#themes-and-palettes) décrit les
+ palettes en général.

- The repertoire of characters is a third choice, and it degrades every design at once:
+ Le répertoire de caractères est un troisième choix, et il dégrade tous les designs à la
+ fois :

```scala fuel
run
##
locally:
  import gaugeGlyphs.asciiGlyphs

  Out.println(gaugeLine(Fraction(0.42), 40))
  Out.println(e"${gaugeLine(Standing.Succeeded, 1)} built")
```

- Every design has an ASCII form, and every design which prefers exotic glyphs declares what
- to fall back to. Emoji designs become their plainer siblings where the terminal cannot show
- them, and under `asciiGlyphs` everything emits seven-bit output. No design carries its
- meaning in color alone, so a gauge which is written to a file, or read by someone who cannot
- tell red from green, still says what it means.
+ Chaque design a une forme ASCII, et chaque design qui préfère des glyphes exotiques déclare
+ vers quoi se replier. Les designs en emoji deviennent leurs équivalents plus sobres là où le
+ terminal ne peut pas les afficher, et sous `asciiGlyphs` tout produit une sortie sur sept
+ bits. Aucun design ne porte son sens par la seule couleur ; une jauge écrite dans un fichier,
+ ou lue par quelqu'un qui ne distingue pas le rouge du vert, dit donc toujours ce qu'elle
+ signifie.

- ### Degradation
+ ### Dégradation

- Every design is drawn at exactly the width which it is given, at every width, and says less
- as it narrows, instead of overrunning. A bar drops its end caps below eight cells, becomes a
- percentage at four, and becomes a single shade at one:
+ Chaque design est dessiné exactement à la largeur qui lui est donnée, à toute largeur, et en
+ dit moins à mesure qu'il se rétrécit, au lieu de déborder. Une barre perd ses embouts en
+ dessous de huit cellules, devient un pourcentage à quatre, et une seule nuance à une :

```scala fuel
run
##
List(40, 20, 8, 4, 1).each: width =>
  Out.println(gaugeLine(Fraction(0.42), width))
```

- A row which is composed of several parts sheds them in a declared order. A transfer which
- reads
+ Une ligne composée de plusieurs parties s'en défait dans un ordre déclaré. Un transfert qui
+ se lit

```
14.2 MiB/512 MiB · 3.1 MiB/s · 2m41s left
```

- at eighty cells keeps only the figures at thirty. A caption is shortened before it squeezes
- the gauge which it labels, and is allowed at most half the row before it begins to cost the
- gauge cells.
+ à quatre-vingts cellules ne garde que les chiffres à trente. Une légende est raccourcie avant
+ de comprimer la jauge qu'elle étiquette, et a droit à au plus la moitié de la ligne avant de
+ commencer à coûter des cellules à la jauge.

- A sparkline's scale is a choice of design, not part of the data.
- `Sparkline.Blocks.scaled(0, 100)` fixes it, so that a steady signal does not look erratic as
- the bounds move under it. A sparkline which is narrower than its series is *decimated*: each
- cell shows the maximum of the samples which it covers. It is not truncated. A truncated
- sparkline would show only the oldest samples, while looking like the whole series, which is
- worse than showing less.
+ L'échelle d'une sparkline est un choix de design, et ne fait pas partie des données.
+ `Sparkline.Blocks.scaled(0, 100)` la fixe, afin qu'un signal stable ne paraisse pas erratique
+ lorsque les bornes bougent sous lui. Une sparkline plus étroite que sa série est *décimée* :
+ chaque cellule montre le maximum des échantillons qu'elle couvre. Elle n'est pas tronquée.
+ Une sparkline tronquée ne montrerait que les échantillons les plus anciens, tout en ayant
+ l'air de représenter toute la série, ce qui est pire que d'en montrer moins.

- ### In a layout
+ ### Dans une mise en page

- `gauge` builds a pane. It takes a `Reading`, which is a mutable cell that holds the current
- status. Assigning to the cell publishes the value, and repaints, so a gauge which is driven
- from a background task updates itself. An application's [`Cli`](cli.md) provides the console:
+ `gauge` construit un volet. Il prend un `Reading`, une cellule mutable qui contient l'état
+ courant. Affecter la cellule publie la valeur et redessine ; une jauge pilotée depuis une
+ tâche d'arrière-plan se met donc à jour d'elle-même. Le [`Cli`](cli.md) d'une application
+ fournit la console :

```scala fuel
typecheck
##
import dysasymptotics.linearSize
import environments.javaBaseEnvironment
import strategies.throwUnsafely

given console: Console = Console(summon[Stdio])

val progress = Reading(Fraction(0.0))
val work = List(1, 2, 3, 4, 5)

supervise:
  async:
    work.each: item =>
      snooze(0.5*Second)
      progress() = Fraction.of(item, work.size)

  interactive: terminal ?=>
    conduct(Tui.Occupancy.Inline)(stack(gauge(progress)))
```

- A gauge reports the intrinsic size of its design each time the layout is solved, so it takes
- part in the layout, and does not assume a width. An elastic design fills what it is given.
- An inelastic one, such as a spinner or a status glyph, is held to its own width. A design of
- several rows, such as a checklist, grows the layout to the number of its steps. A gauge
- accepts no input, so Tab skips over it.
+ Une jauge indique la taille intrinsèque de son design chaque fois que la mise en page est
+ résolue ; elle participe donc à la mise en page, et ne présume pas d'une largeur. Un design
+ élastique remplit ce qu'on lui donne. Un design inélastique, comme un spinner ou un glyphe
+ d'état, est maintenu à sa propre largeur. Un design de plusieurs lignes, comme une liste de
+ contrôle, agrandit la mise en page jusqu'au nombre de ses étapes. Une jauge n'accepte aucune
+ saisie ; Tab la saute donc.

- A design which animates declares how often it wants to be redrawn. The layout takes the
- shortest period among the gauges on screen, and runs one timer for all of them, and none at
- all when nothing is animating.
+ Un design animé déclare à quelle fréquence il veut être redessiné. La mise en page prend la
+ période la plus courte parmi les jauges à l'écran, et exécute un seul minuteur pour toutes,
+ et aucun lorsque rien n'est animé.

- ### Standalone
+ ### Jauges autonomes

- Outside a layout, `whilst` shows a gauge at the cursor for the duration of a block, and
- erases it afterwards. Its animation runs as a background task, so it is called under
- `supervise`:
+ Hors d'une mise en page, `whilst` affiche une jauge au curseur pendant la durée d'un bloc, et
+ l'efface ensuite. Son animation s'exécute comme tâche d'arrière-plan ; elle s'appelle donc
+ sous `supervise` :

```scala fuel
typecheck
##
supervise:
  whilst(Reading(Fraction.indeterminate)):
    snooze(2.0*Second)
```

- `gaugeLine` draws a single frame, for a caller which does its own drawing. It is the form to
- use when the redrawing is already handled:
+ `gaugeLine` dessine une seule image, pour un appelant qui fait son propre dessin. C'est la
+ forme à utiliser lorsque le redessin est déjà pris en charge :

```scala
def frame(done: Int, total: Int)(using Stdio): Unit =
  Out.print(e"\r${gaugeLine(Fraction.of(done, total), 40)} $done/$total${csi.el()}")
```

- ### Composing
+ ### Composer

- `Captioned` labels any status. It derives its design from whichever design the underlying
- status already uses, so it is not a choice of style, and needs no import:
+ `Captioned` étiquette n'importe quel état. Il dérive son design de celui qu'utilise déjà
+ l'état sous-jacent ; ce n'est donc pas un choix de style, et il n'exige aucun import :

```scala fuel
run
##
Out.println(gaugeLine(Captioned(Fraction(0.42), t"resolving dependencies"), 60))
```

- A `Sequence[Step]` has five designs, which differ in height:
+ Une `Sequence[Step]` a cinq designs, qui diffèrent par leur hauteur :

```tabular
- column  Import
- column  Draws
- column  Height
- row  `processions.checklistProcession`  a checklist  one row per step
- row  `processions.breadcrumbProcession`  a breadcrumb  one row
- row  `processions.beadProcession`  a chain of beads  one row
- row  `processions.numberedProcession`  a numbered position  one row
- row  `processions.ribbonProcession`  a powerline ribbon  one row
+ column  Import
+ column  Dessine
+ column  Hauteur
+ row  `processions.checklistProcession`  une liste de contrôle  une ligne par étape
+ row  `processions.breadcrumbProcession`  un fil d'Ariane  une ligne
+ row  `processions.beadProcession`  une chaîne de perles  une ligne
+ row  `processions.numberedProcession`  une position numérotée  une ligne
+ row  `processions.ribbonProcession`  un ruban façon powerline  une ligne
```

- A checklist, for example, is drawn in three rows for three steps:
+ Une liste de contrôle, par exemple, se dessine en trois lignes pour trois étapes :

```scala fuel
typecheck
##
import processions.checklistProcession

val steps: Sequence[Step] =
  Sequence
    ( Step(t"resolve", Standing.Succeeded),
      Step(t"compile", Standing.Running),
      Step(t"publish", Standing.Pending) )

val checklist = gauge(Reading(steps))
```

```
✓ resolve
⠋ compile
· publish
```

- The running step animates, and each step's marker comes from the same `Standing` vocabulary
- which a status glyph of one cell uses.
+ L'étape en cours est animée, et le marqueur de chaque étape provient du même vocabulaire
+ `Standing` qu'utilise un glyphe d'état d'une seule cellule.

- ### Related
+ ### Voir aussi

- - [Terminal](terminal.md#layout) arranges gauges with other panes, and handles input.
- - [Colors](colors.md) describes the colors which a palette names.
- - [Concurrency](concurrency.md) runs the work which a gauge reports on.
- - [Charts](charts.md) draws data which is already complete, where a gauge draws work in
-   progress.
+ - [Terminal](terminal.md#layout) dispose les jauges avec d'autres volets, et gère la
+   saisie.
+ - [Couleurs](colors.md) décrit les couleurs que nomme une palette.
+ - [Concurrence](concurrency.md) exécute le travail dont une jauge rend compte.
+ - [Graphiques](charts.md) dessine des données déjà complètes, là où une jauge dessine un
+   travail en cours.
