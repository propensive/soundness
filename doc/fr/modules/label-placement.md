- ## Label placement
+ ## Placement des étiquettes

- ### About
+ ### Présentation

- Text on a picture is set beside the thing it names: a label by a point, a name by a town, a
- percentage in a wedge. When two things are close, their labels collide, and the picture is worse
- than it would be with one of them moved a little.
+ Le texte d'une image se place à côté de ce qu'il désigne : une étiquette près d'un point, un nom
+ près d'une ville, un pourcentage dans une part de camembert. Quand deux éléments sont proches,
+ leurs étiquettes se chevauchent, et l'image est moins bonne qu'elle ne le serait si l'une
+ d'elles était légèrement déplacée.

- Cartouche decides where each piece of text goes. It takes every label with its measured size,
- the point it describes and the sides it may lie on, together with everything else on the page
- which text must not cover, and finds a position for each one. [Charts](charts.md) use it for
- their labels. A map would use it for its place names.
+ Cartouche décide où va chaque morceau de texte. Il prend chaque étiquette avec sa taille
+ mesurée, le point qu'elle décrit et les côtés où elle peut se placer, ainsi que tout ce que le
+ texte ne doit pas recouvrir sur la page, et trouve une position pour chacune. Les
+ [graphiques](charts.md) l'utilisent pour leurs étiquettes. Une carte l'utiliserait pour ses noms
+ de lieux.

- ### On placement
+ ### Le placement

- The code which builds a picture knows what each label says and what it describes, but not where
- the other labels will end up, and so it cannot avoid them. Cartouche resolves this by running
- the code which builds the picture twice. The first run describes the labels. The engine arranges
- them. The second run builds the picture with the answers.
+ Le code qui construit une image sait ce que dit chaque étiquette et ce qu'elle décrit, mais pas
+ où les autres étiquettes finiront, et ne peut donc pas les éviter. Cartouche résout ce problème
+ en exécutant deux fois le code qui construit l'image. La première exécution décrit les
+ étiquettes. Le moteur les dispose. La seconde exécution construit l'image avec les réponses.

- The body of `arrange` is that code, and `position` is how it asks for the place of a label.
- Everything comes from the `soundness` package. The second import lets the samples print their
- results, and the `Decimalizer` fixes how many significant figures a coordinate is printed with:
+ Le corps de `arrange` est ce code, et `position` est le moyen par lequel il demande la place
+ d'une étiquette. Tout vient du package `soundness`. Le second import permet aux exemples
+ d'afficher leurs résultats, et le `Decimalizer` fixe le nombre de chiffres significatifs avec
+ lequel une coordonnée est affichée :

```scala
import soundness.*
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

given decimalizer: Decimalizer = Decimalizer(5)
```

- Here two labels of the same size ask for the same point:
+ Ici, deux étiquettes de même taille demandent le même point :

```scala fuel
run
##
val picture = arrange:
  val first = position(60.0, 12.0, 100.0, 100.0)
  val second = position(60.0, 12.0, 100.0, 100.0)
  (first.attachment, second.attachment)

Out.println(picture.inspect)
```

- The first is set to the east of the point, as labels are by preference. The second would cover
- it, so it is set to the west.
+ La première est placée à l'est du point, comme les étiquettes le sont de préférence. La seconde
+ la recouvrirait ; elle est donc placée à l'ouest.

- In the first run of the body, each `position` answers provisionally, with the label where it was
- asked for, and the result of that run is thrown away. The engine then solves the whole
- arrangement, and in the second run each call answers with its solved position. The value of
- `arrange` is the value of the body's second run, so a chart built inside `arrange` is what comes
- out of it.
+ Lors de la première exécution du corps, chaque `position` répond provisoirement, avec
+ l'étiquette là où elle a été demandée, et le résultat de cette exécution est jeté. Le moteur
+ résout alors la disposition entière, et lors de la seconde exécution chaque appel répond avec sa
+ position résolue. La valeur de `arrange` est celle de la seconde exécution du corps : un
+ graphique construit dans `arrange` est donc ce qui en sort.

- The body is run twice, so it must be repeatable: the same `position` calls, in the same order,
- each time, and nothing done which should not happen twice. A counter which numbers the labels
- belongs inside the body, where it starts again with the second run.
+ Le corps est exécuté deux fois ; il doit donc être reproductible : les mêmes appels à
+ `position`, dans le même ordre, à chaque fois, et rien de ce qui ne devrait pas se produire deux
+ fois. Un compteur qui numérote les étiquettes a sa place dans le corps, où il repart de zéro
+ lors de la seconde exécution.

- ### Captions and targets
+ ### Légendes et cibles

- A label is a *caption*: a width and height, the point it describes, and how it may be set
- relative to that point. The engine sees only geometry. Whoever draws the text measures it.
- `position` takes the parts of the caption directly, or a `Caption` built beforehand:
+ Une étiquette est une *légende* : une largeur et une hauteur, le point qu'elle décrit, et la
+ manière dont elle peut être placée par rapport à ce point. Le moteur ne voit que de la
+ géométrie. Celui qui dessine le texte le mesure. `position` prend directement les éléments de
+ la légende, ou une `Caption` construite au préalable :

```scala
val caption = Caption(60.0, 12.0, 100.0, 100.0, standoff = 4.0, reach = 24.0)
val placed = arrange(position(caption))
```

- The answer is a `Caption.Position`: the point to set the text at, the side of the target it lies
- on, the box it occupies, a leader line if it needed one, and whether it is to be drawn at all:
+ La réponse est une `Caption.Position` : le point où placer le texte, le côté de la cible où il
+ se trouve, la boîte qu'il occupe, une ligne de rappel s'il en a eu besoin, et s'il doit être
+ dessiné ou non :

```scala fuel
run
##
Out.println(placed.attachment.inspect)
Out.println(placed.x.show)
Out.println(placed.y.show)
Out.println(placed.visible.inspect)
```

- The side is an `Attachment`, which is `East`, `West`, `North`, `South`, one of the four
- diagonals, or `Center`. The point is the anchor of the label on that side. For `East`, it is the
- middle of the left edge of the label, so text set there with its start at the point lies to the
- east of the target. Here the `standoff` of `4.0` puts the point four units east of the target. A
- renderer turns the attachment into its own alignment. An SVG `text` element uses `text-anchor`
- and `dominant-baseline`.
+ Le côté est un `Attachment`, qui vaut `East`, `West`, `North`, `South`, l'une des quatre
+ diagonales, ou `Center`. Le point est l'ancre de l'étiquette de ce côté. Pour `East`, c'est le
+ milieu du bord gauche de l'étiquette : un texte placé là, son début au point, se trouve donc à
+ l'est de la cible. Ici, le `standoff` de `4.0` place le point quatre unités à l'est de la cible.
+ Un moteur de rendu traduit l'ancrage en son propre alignement. Un élément SVG `text` utilise
+ `text-anchor` et `dominant-baseline`.

- `attachments` lists the sides the label may lie on, best first. The default tries east, then
- west, then the diagonals, then north and south. A label which may lie on one side only is given
- just that side. `standoff` is the gap between the target and the near edge of the label, for a
- label which sits beside a marker rather than on its center.
+ `attachments` énumère les côtés où l'étiquette peut se placer, du meilleur au moins bon. Par
+ défaut, on essaie l'est, puis l'ouest, puis les diagonales, puis le nord et le sud. Une
+ étiquette qui ne peut se placer que d'un côté ne reçoit que ce côté. `standoff` est l'écart
+ entre la cible et le bord proche de l'étiquette, pour une étiquette placée à côté d'un marqueur
+ plutôt que sur son centre.

- The rest of the parameters of a caption are described in the sections which follow. All of them
- are:
+ Les autres paramètres d'une légende sont décrits dans les sections suivantes. Les voici tous :

```tabular
- column  Parameter
- column  Default
- column  Meaning
- row  `width`, `height`  required  the label's measured size
- row  `x`, `y`  required  the target point
- row  `attachments`  east, west, the diagonals, north, south  the sides it may lie on, best first
- row  `standoff`  `0.0`  the gap between the target and the label's near edge
- row  `reach`  `0.0`  how far it may be moved from the target
- row  `padding`  `0.0`  clearance demanded beyond its own box
- row  `priority`  `0`  higher is placed first
- row  `fallback`  `Caption.Fallback.Overlap`  what to do when no position is clear
+ column  Paramètre
+ column  Défaut
+ column  Signification
+ row  `width`, `height`  obligatoire  la taille mesurée de l'étiquette
+ row  `x`, `y`  obligatoire  le point cible
+ row  `attachments`  est, ouest, les diagonales, nord, sud  côtés possibles, du meilleur au pire
+ row  `standoff`  `0.0`  l'écart entre la cible et le bord proche de l'étiquette
+ row  `reach`  `0.0`  la distance maximale dont elle peut être éloignée de la cible
+ row  `padding`  `0.0`  le dégagement exigé au-delà de sa propre boîte
+ row  `priority`  `0`  la plus haute est placée en premier
+ row  `fallback`  `Caption.Fallback.Overlap`  que faire quand aucune position n'est libre
```

- ### Obstacles
+ ### Obstacles

- Labels avoid each other, and they avoid whatever the body declares with `avoid`:
+ Les étiquettes s'évitent entre elles, et elles évitent tout ce que le corps déclare avec
+ `avoid` :

```tabular
- column  Obstacle
- column  Parameters
- column  Covers
- row  `Obstacle.Box`  `left`, `top`, `width`, `height`  a bar
- row  `Obstacle.Line`  `x1`, `y1`, `x2`, `y2`, and a `width` defaulting to `1.0`  a plotted line
- row  `Obstacle.Disc`  `x`, `y`, `radius`  a marker
+ column  Obstacle
+ column  Paramètres
+ column  Recouvre
+ row  `Obstacle.Box`  `left`, `top`, `width`, `height`  une barre
+ row  `Obstacle.Line`  `x1`, `y1`, `x2`, `y2`, et une `width` (`1.0` par défaut)  une ligne
+ row  `Obstacle.Disc`  `x`, `y`, `radius`  un marqueur
```

```scala fuel
run
##
val kept = arrange:
  avoid(Obstacle.Box(100.0, 90.0, 40.0, 20.0))
  avoid(Obstacle.Line(0.0, 100.0, 300.0, 100.0), Obstacle.Disc(100.0, 100.0, 3.0))
  position(60.0, 12.0, 100.0, 100.0)

Out.println(kept.attachment.inspect)
```

- The box lies to the east of the point, and the line runs through it, so a label set to the west
- would touch the line. The label is set to the northwest instead. A polyline is its segments, each an `Obstacle.Line`. The `width` of a line is its stroke, and a label may not
- touch it.
+ La boîte se trouve à l'est du point, et la ligne le traverse : une étiquette placée à l'ouest
+ toucherait donc la ligne. L'étiquette est placée au nord-ouest à la place. Une polyligne est
+ constituée de ses segments, chacun étant une `Obstacle.Line`. La `width` d'une ligne est son
+ trait, et une étiquette ne peut pas la toucher.

- `canvas` declares the area which labels must stay within, which is otherwise unbounded. Here the
- label would run off the right edge, so it is set to the west:
+ `canvas` déclare la zone dont les étiquettes ne doivent pas sortir, et qui est sinon illimitée.
+ Ici, l'étiquette déborderait du bord droit ; elle est donc placée à l'ouest :

```scala fuel
run
##
val inside = arrange:
  canvas(Obstacle.Box(0.0, 0.0, 120.0, 120.0))
  position(60.0, 12.0, 110.0, 100.0)

Out.println(inside.attachment.inspect)
```

- ### Displacement and leaders
+ ### Déplacement et lignes de rappel

- When every side of the target is blocked, a label may be moved away from it, up to its `reach`,
- which is zero unless given. A label moved further than a short threshold gets a *leader*: a line
- from its edge back to the target, so what it names stays clear:
+ Quand tous les côtés de la cible sont bloqués, une étiquette peut en être éloignée, jusqu'à sa
+ `reach`, qui vaut zéro sauf indication contraire. Une étiquette déplacée au-delà d'un petit
+ seuil reçoit une *ligne de rappel* : un trait de son bord jusqu'à la cible, pour que ce qu'elle
+ désigne reste clair :

```scala fuel
run
##
val led = arrange:
  avoid(Obstacle.Box(0.0, 0.0, 200.0, 110.0))
  position(60.0, 12.0, 100.0, 100.0, attachments = List(Caption.Attachment.South), reach = 40.0)

Out.println(led.attachment.inspect)
Out.println(led.leader.present.inspect)
```

- The label is set below the box, and its leader runs up to the point. A leader is part of the
- arrangement. Later labels avoid it, as they avoid the label it belongs to, and the leader of a
- label may not cross another label. It may cross a fixed obstacle, since pointing past a bar or a
- marker to the target is what a leader is for.
+ L'étiquette est placée sous la boîte, et sa ligne de rappel remonte jusqu'au point. Une ligne
+ de rappel fait partie de la disposition. Les étiquettes suivantes l'évitent, comme elles
+ évitent l'étiquette à laquelle elle appartient, et la ligne de rappel d'une étiquette ne peut
+ pas traverser une autre étiquette. Elle peut traverser un obstacle fixe, puisque désigner la
+ cible par-delà une barre ou un marqueur est précisément le rôle d'une ligne de rappel.

- `padding` is clearance demanded around a label beyond its own box, so neighboring labels are
- not set touching.
+ `padding` est le dégagement exigé autour d'une étiquette au-delà de sa propre boîte, pour que
+ des étiquettes voisines ne soient pas placées bord à bord.

- ### Priority and hiding
+ ### Priorité et masquage

- Labels are placed in order of `priority`, highest first, and otherwise in the order the body
- asked for them. A label placed earlier keeps its target, and those after it move. A label with a
- single side and no reach is placed exactly where it was asked for, whatever is there, and
- everything after it steers around it. That is how the tick labels and legend of a chart hold
- still while its point labels move.
+ Les étiquettes sont placées par ordre de `priority`, la plus haute en premier, et sinon dans
+ l'ordre où le corps les a demandées. Une étiquette placée plus tôt conserve sa cible, et celles
+ qui suivent se déplacent. Une étiquette n'ayant qu'un seul côté et aucune portée de déplacement
+ est placée exactement là où elle a été demandée, quoi qu'il s'y trouve, et tout ce qui suit la
+ contourne. C'est ainsi que les étiquettes des graduations et la légende d'un graphique restent
+ immobiles pendant que ses étiquettes de points se déplacent.

- A label which collides wherever it is put is drawn at the position where it overlaps least. A
- caption whose `fallback` is `Caption.Fallback.Hide` is instead not drawn. Its position reports
- `visible` as false, and the body draws nothing for it. A hidden label blocks nothing:
+ Une étiquette qui entre en collision où qu'on la place est dessinée à la position où elle
+ chevauche le moins. Une légende dont le `fallback` est `Caption.Fallback.Hide` n'est au
+ contraire pas dessinée. Sa position indique `visible` à faux, et le corps ne dessine rien pour
+ elle. Une étiquette masquée ne bloque rien :

```scala fuel
run
##
val hidden = arrange:
  avoid(Obstacle.Box(0.0, 0.0, 300.0, 300.0))
  position(60.0, 12.0, 100.0, 100.0, fallback = Caption.Fallback.Hide)

Out.println(hidden.visible.inspect)
```

- ### Choosing an arranger
+ ### Choisir un arrangeur

- The algorithm which arranges is the `Arranger` in scope. The default is greedy. Each label, in
- priority order, takes the least displaced position which is clear of everything placed before
- it, or the least collision if nothing is clear. It is deterministic, and its work is bounded by
- the number of positions a label may take. A label placed early is never moved to make room for
- one placed later, which on a dense picture leaves collisions which a different order would have
- avoided.
+ L'algorithme de disposition est l'`Arranger` dans la portée. Celui par défaut est glouton.
+ Chaque étiquette, par ordre de priorité, prend la position la moins déplacée qui soit libre de
+ tout ce qui a été placé avant elle, ou celle de moindre collision si rien n'est libre. Il est
+ déterministe, et son travail est borné par le nombre de positions qu'une étiquette peut
+ prendre. Une étiquette placée tôt n'est jamais déplacée pour faire de la place à une étiquette
+ placée plus tard, ce qui, sur une image dense, laisse des collisions qu'un autre ordre aurait
+ évitées.

- Simulated annealing starts from the greedy arrangement, and then moves labels among their
- positions at random. It keeps a move which lowers the total collision and displacement, and
- sometimes one which raises it, less often as it cools. It is chosen by importing it:
+ Le recuit simulé part de la disposition gloutonne, puis déplace les étiquettes parmi leurs
+ positions au hasard. Il conserve un déplacement qui diminue le total des collisions et des
+ déplacements, et parfois un qui l'augmente, de moins en moins souvent à mesure qu'il refroidit.
+ On le choisit en l'important :

```scala fuel
run
##
import arrangers.annealingArranger

val annealed = arrange:
  val first = position(60.0, 12.0, 100.0, 100.0, reach = 20.0)
  val second = position(60.0, 12.0, 102.0, 101.0, reach = 20.0)
  val third = position(60.0, 12.0, 98.0, 99.0, reach = 20.0)
  List(first.attachment, second.attachment, third.attachment)

Out.println(annealed.inspect)
```

- Its random moves are drawn from a fixed seed, so the same input arranges the same way every
- time, and it stops after a set number of moves. Both arrangers are case classes,
- `Arranger.Greedy` and `Arranger.Annealing`, whose parameters can be given in place of the
- defaults:
+ Ses déplacements aléatoires sont tirés d'une graine fixe : la même entrée est donc disposée de
+ la même façon à chaque fois, et il s'arrête après un nombre fixé de déplacements. Les deux
+ arrangeurs sont des case classes, `Arranger.Greedy` et `Arranger.Annealing`, dont les
+ paramètres peuvent être fournis à la place des valeurs par défaut :

```tabular
- column  Parameter
- column  Default
- column  Arranger
- column  Meaning
- row  `step`  `4.0`  both  the step between candidate positions
- row  `rings`  `8`  both  how many steps out to search
- row  `leaderThreshold`  `6.0`  both  the displacement beyond which a leader is drawn
- row  `leaderWidth`  `1.0`  both  the stroke width of a leader
- row  `seed`  `Seed(0L)`  annealing  the seed its random moves are drawn from
- row  `iterations`  `4000`  annealing  the number of moves
- row  `temperature`  `1.0`  annealing  the starting temperature
- row  `cooling`  `0.995`  annealing  the factor the temperature is multiplied by at each move
- row  `displacementWeight`  `0.05`  annealing  the cost of displacement against that of collision
+ column  Paramètre
+ column  Défaut
+ column  Arrangeur
+ column  Signification
+ row  `step`  `4.0`  les deux  le pas entre positions candidates
+ row  `rings`  `8`  les deux  le nombre de pas de recherche vers l'extérieur
+ row  `leaderThreshold`  `6.0`  les deux  le déplacement au-delà duquel tracer une ligne de rappel
+ row  `leaderWidth`  `1.0`  les deux  l'épaisseur du trait d'une ligne de rappel
+ row  `seed`  `Seed(0L)`  recuit  la graine dont sont tirés ses déplacements aléatoires
+ row  `iterations`  `4000`  recuit  le nombre de déplacements
+ row  `temperature`  `1.0`  recuit  la température de départ
+ row  `cooling`  `0.995`  recuit  le facteur appliqué à la température à chaque déplacement
+ row  `displacementWeight`  `0.05`  recuit  le coût du déplacement face à celui de la collision
```

```scala
given arranger: Arranger = Arranger.Greedy(step = 2.0, rings = 12)
```

- Any other algorithm is an `Arranger` too: a value with one method, which takes the captions,
- the obstacles and the canvas, and answers with a position for each caption in turn.
+ Tout autre algorithme est aussi un `Arranger` : une valeur dotée d'une méthode, qui prend les
+ légendes, les obstacles et le canevas, et répond par une position pour chaque légende à tour de
+ rôle.

- ### Related
+ ### Voir aussi

- - [Charts](charts.md) place their labels through `arrange`.
- - [SVG](svg.md) describes the `text` elements which a renderer sets at the answered positions.
- - [Randomness](randomness.md) describes the `Seed` which the annealing arranger draws from.
+ - Les [graphiques](charts.md) placent leurs étiquettes au moyen de `arrange`.
+ - [SVG](svg.md) décrit les éléments `text` qu'un moteur de rendu place aux positions obtenues.
+ - [Aléatoire](randomness.md) décrit la `Seed` dont tire l'arrangeur par recuit.
