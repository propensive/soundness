- ## Graphs
+ ## Graphes

- ### About
+ ### Présentation

- A [directed acyclic graph](https://en.wikipedia.org/wiki/Directed_acyclic_graph) is a set of
- nodes with dependencies between them, and no cycles. Build systems, task schedules, type
- hierarchies and package dependencies all have this shape.
+ Un [graphe orienté acyclique](https://en.wikipedia.org/wiki/Directed_acyclic_graph) est un
+ ensemble de nœuds avec des dépendances entre eux, et sans cycle. Les systèmes de build, les
+ plannings de tâches, les hiérarchies de types et les dépendances de packages ont tous cette
+ forme.

- A `Dag` represents one as an [immutable] value. It has the operations which such graphs need:
- topological ordering, reachability, transitive closure and reduction, and editing which keeps
- the graph acyclic. Two other graph types sit beside it:
+ Un `Dag` en représente un sous forme de valeur [immuable]. Il dispose des opérations dont ces
+ graphes ont besoin : ordre topologique, accessibilité, fermeture et réduction transitives, et
+ modification qui garde le graphe acyclique. Deux autres types de graphes se tiennent à ses
+ côtés :

```tabular
- column  Type
- column  Cycles
- column  Editing
- column  Suited to
- row  `Dag`  never  returns a new value  dependency structures as values
- row  `Digraph`  allowed  returns a new value  graphs that may hold a cycle
- row  `Topology[node]^`  never  in place  graphs built up edge by edge
- row  `Topology[node]^{}`  never  none: frozen  graphs built once and queried often
+ column  Type
+ column  Cycles
+ column  Modification
+ column  Adapté à
+ row  `Dag`  jamais  renvoie une nouvelle valeur  les structures de dépendances en tant que valeurs
+ row  `Digraph`  autorisés  renvoie une nouvelle valeur  les graphes qui peuvent contenir un cycle
+ row  `Topology[node]^`  jamais  en place  les graphes construits arête par arête
+ row  `Topology[node]^{}`  jamais  aucune : figé  les graphes construits une fois et souvent interrogés
```

- All of these share one vocabulary through the `Nodal` [typeclass], so the same operations apply
- to each. So does anything else with the shape of a graph, such as a `Hasse` diagram, or a `Map`
- from nodes to their successors.
+ Tous partagent un même vocabulaire par la [classe de types] `Nodal` ; les mêmes opérations
+ s'appliquent donc à chacun. De même pour tout ce qui a la forme d'un graphe, comme un diagramme
+ de `Hasse`, ou une `Map` des nœuds vers leurs successeurs.

- ### On acyclic graphs
+ ### Sur les graphes acycliques

- Dependency structures have the same needs wherever they appear: an order in which to process the
- nodes, the set which a change can reach, the removal of redundant edges. A general-purpose graph
- library carries the weight of arbitrary graphs, cycles included. More often, a project writes
- its own topological sort over a `Map`, and inherits its edge cases.
+ Les structures de dépendances ont les mêmes besoins où qu'elles apparaissent : un ordre dans
+ lequel traiter les nœuds, l'ensemble qu'une modification peut atteindre, la suppression des
+ arêtes redondantes. Une bibliothèque de graphes généraliste porte le poids des graphes
+ arbitraires, cycles compris. Plus souvent, un projet écrit son propre tri topologique sur une
+ `Map`, et en hérite les cas limites.

- A `Dag` cannot be constructed with a cycle. A cyclic dependency graph is an
- [impossible state](../../philosophy/impossible-states.md), ruled out where the graph is built, and
- so the topological order of a `Dag` has no way to fail.
+ Un `Dag` ne peut pas être construit avec un cycle. Un graphe de dépendances cyclique est un
+ [état impossible](../../philosophy/impossible-states.md), exclu là où le graphe est construit ;
+ l'ordre topologique d'un `Dag` n'a donc aucun moyen d'échouer.

- Everything comes from the `soundness` package. The other imports choose a
- [strategy](errors.md#choosing-a-strategy) for errors, and let the samples print their results:
+ Tout provient du package `soundness`. Les autres imports choisissent une
+ [stratégie](errors.md#choosing-a-strategy) pour les erreurs, et permettent aux exemples
+ d'afficher leurs résultats :

```scala fuel
feature experimental.captureChecking
##
import soundness.*
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### Building a graph
+ ### Construire un graphe

- A `Dag` is built from nodes with their sets of dependencies:
+ Un `Dag` se construit à partir de nœuds avec leurs ensembles de dépendances :

```scala
val dag = Dag(8 -> Set(4, 6), 6 -> Set(3, 2), 4 -> Set(2), 3 -> Set(), 2 -> Set())
```

- An edge `a -> b` means that `a` depends on `b`. A `Dag` is also built from edges alone, and both
- ends of every edge become nodes:
+ Une arête `a -> b` signifie que `a` dépend de `b`. Un `Dag` se construit aussi à partir des
+ seules arêtes, et les deux extrémités de chaque arête deviennent des nœuds :

```scala
val edges = Dag(8 -> 4, 8 -> 6, 6 -> 3)
```

- A third form takes a set of nodes, and a function which gives the dependencies of each.
+ Une troisième forme prend un ensemble de nœuds, et une fonction qui donne les dépendances de
+ chacun.

- Each of these checks for a cycle, and raises a `Dag.Error` if it finds one:
+ Chacune de ces formes vérifie l'absence de cycle, et lève une `Dag.Error` si elle en trouve un :

```scala fuel
run
errors show
##
Dag(1 -> 2, 2 -> 3, 3 -> 1)
```

- A graph which may hold a cycle is a `Digraph`. It is built in the same ways, without the check.
- `cycle` finds a cycle, as a list of nodes which ends where it began:
+ Un graphe qui peut contenir un cycle est un `Digraph`. Il se construit des mêmes façons, sans la
+ vérification. `cycle` trouve un cycle, sous forme de liste de nœuds qui se termine là où elle a
+ commencé :

```scala
val graph = Digraph(1 -> 2, 2 -> 3, 3 -> 1)
```

```scala fuel
run
##
Out.println(graph.cycle.inspect)
```

- `acyclic` is the one checked conversion from a `Digraph` to a `Dag`:
+ `acyclic` est l'unique conversion vérifiée d'un `Digraph` en `Dag` :

```scala
val checked = Digraph(1 -> 2).acyclic
```

- `explore` builds a `Digraph` by following a dependency function outward from one node. The
- result is a `Digraph` because the function may lead back on itself. This is the graph of
- divisors beneath 12:
+ `explore` construit un `Digraph` en suivant une fonction de dépendance vers l'extérieur à partir
+ d'un nœud. Le résultat est un `Digraph` car la fonction peut revenir sur elle-même. Voici le
+ graphe des diviseurs sous 12 :

```scala fuel
run
##
val divisors = 12.explore(n => (1 until n).filter(n % _ == 0)).acyclic
Out.println(divisors.linearized.inspect)
```

- ### Ordering and reachability
+ ### Ordre et accessibilité

- `linearized` gives a [topological order](https://en.wikipedia.org/wiki/Topological_sorting).
- Every node comes after its dependencies, and the nodes are otherwise in the order they were
- given:
+ `linearized` donne un [ordre topologique](https://en.wikipedia.org/wiki/Topological_sorting).
+ Chaque nœud vient après ses dépendances, et les nœuds sont par ailleurs dans l'ordre où ils ont
+ été donnés :

```scala fuel
run
##
Out.println(dag.linearized.inspect)
```

- `reachable` gives a node and everything it depends on, directly or through other nodes:
+ `reachable` donne un nœud et tout ce dont il dépend, directement ou par d'autres nœuds :

```scala fuel
run
##
Out.println(dag.reachable(8).inspect)
Out.println(dag.reachable(4).inspect)
```

- `reachable` raises a `Dag.Error` for a node which the graph does not have. It is defined on a
- `Dag` and a `Digraph` alike, since reachability is well defined whether or not there is a cycle:
+ `reachable` lève une `Dag.Error` pour un nœud que le graphe ne contient pas. Il est défini aussi
+ bien sur un `Dag` que sur un `Digraph`, puisque l'accessibilité est bien définie qu'il y ait un
+ cycle ou non :

```scala fuel
run
errors show
##
dag.reachable(5)
```

- These are the queries which a graph answers:
+ Voici les requêtes auxquelles répond un graphe :

```tabular
- column  Query
- column  Answers
- column  On a `Dag`
- row  `sources`  the nodes depending on nothing, where a build starts
- row  `sinks`  the nodes nothing depends on
- row  `successors(n)`  the dependencies of `n`
- row  `predecessors(n)`  the dependants of `n`  needs `linearScan`
- row  `reachable(n)`  `n` and everything it depends on, transitively
- row  `descendants(n)`  the sub-graph beneath `n`
- row  `ancestors(n)`  the sub-graph that depends on `n`  needs `linearScan`
- row  `lineage(n)`  ancestors and descendants together  needs `linearScan`
- row  `linearized`  a topological order
- row  `invert`  the graph with every edge reversed
- row  `reaches(a, b)`  whether `a` depends on `b`, transitively  frozen `Topology` only
+ column  Requête
+ column  Répond
+ column  Sur un `Dag`
+ row  `sources`  les nœuds qui ne dépendent de rien, là où commence un build
+ row  `sinks`  les nœuds dont rien ne dépend
+ row  `successors(n)`  les dépendances de `n`
+ row  `predecessors(n)`  les dépendants de `n`  requiert `linearScan`
+ row  `reachable(n)`  `n` et tout ce dont il dépend, transitivement
+ row  `descendants(n)`  le sous-graphe sous `n`
+ row  `ancestors(n)`  le sous-graphe qui dépend de `n`  requiert `linearScan`
+ row  `lineage(n)`  ancêtres et descendants ensemble  requiert `linearScan`
+ row  `linearized`  un ordre topologique
+ row  `invert`  le graphe avec toutes les arêtes inversées
+ row  `reaches(a, b)`  si `a` dépend de `b`, transitivement  `Topology` figée uniquement
```

- As a collection, a graph is its nodes, and those of a `Dag` are in topological order. The
- [collection](collections.md) vocabulary applies through the usual typeclasses:
+ En tant que collection, un graphe est l'ensemble de ses nœuds, et ceux d'un `Dag` sont dans
+ l'ordre topologique. Le vocabulaire des [collections](collections.md) s'applique par les classes
+ de types habituelles :

```scala fuel
run
##
Out.println(dag.has(4).inspect)
Out.println(dag.size.show)
```

- `dag.map(_.toString)` gives a `Digraph`, not a `Dag`. Two nodes may map to the same value, and
- merging them could close a cycle.
+ `dag.map(_.toString)` donne un `Digraph`, et non un `Dag`. Deux nœuds peuvent avoir la même
+ image, et les fusionner pourrait fermer un cycle.

- A `Dag` stores only the forward direction. Finding the predecessors of a node means scanning
- every edge, which is a cost out of proportion to the question. The operations which need it are
- `predecessors`, `ancestors`, `lineage`, `bypass` and `-`. Without an acknowledgement of the
- cost, they do not compile:
+ Un `Dag` ne stocke que le sens direct. Trouver les prédécesseurs d'un nœud implique de parcourir
+ toutes les arêtes, un coût sans proportion avec la question. Les opérations qui en ont besoin
+ sont `predecessors`, `ancestors`, `lineage`, `bypass` et `-`. Sans reconnaissance de ce coût,
+ elles ne compilent pas :

```scala fuel
compile
errors show
##
dag.predecessors(2)
```

- An import gives the acknowledgement:
+ Un import apporte la reconnaissance :

```scala
import dysasymptotics.linearScan
```

```scala fuel
run
##
Out.println(dag.predecessors(2).inspect)
Out.println(dag.ancestors(2).linearized.inspect)
Out.println(dag.lineage(4).linearized.inspect)
```

- ### Folding over a graph
+ ### Plier un graphe

- A traversal computes a value for every node from the values of the nodes it depends on. The
- dependencies are always computed first. This traversal gives each node one more than the sum of
- the values beneath it:
+ Un parcours calcule une valeur pour chaque nœud à partir des valeurs des nœuds dont il dépend.
+ Les dépendances sont toujours calculées en premier. Ce parcours donne à chaque nœud un de plus
+ que la somme des valeurs situées en dessous :

```scala
val counts = dag.traversal[Int]((childValues, node) => childValues.total + 1)
```

- Most real work over a dependency graph has this shape: the cost of a build, a version constraint
- passed along, a transitive set gathered up. A traversal expresses it once, without recursion
- written by hand or a set of visited nodes to keep.
+ La plupart des vrais traitements sur un graphe de dépendances ont cette forme : le coût d'un
+ build, une contrainte de version transmise, un ensemble transitif rassemblé. Un parcours
+ l'exprime une seule fois, sans récursion écrite à la main ni ensemble de nœuds visités à tenir.

- ### Closure and reduction
+ ### Fermeture et réduction

- The transitive `closure` adds an edge wherever a path exists. The transitive `reduction` removes
- every edge which a path already implies. The reduction is the smallest graph with the same
- reachability, and is the form in which a dependency graph is usually drawn:
+ La `closure` transitive ajoute une arête partout où existe un chemin. La `reduction` transitive
+ supprime toute arête qu'un chemin implique déjà. La réduction est le plus petit graphe ayant la
+ même accessibilité, et c'est la forme sous laquelle on dessine habituellement un graphe de
+ dépendances :

```scala
val closed = dag.closure
val reduced = dag.reduction
```

- Both are answered from the reachability matrix of a frozen topology, which the next section
- describes. A graph which is queried this way more than once is best frozen once.
+ Toutes deux sont obtenues à partir de la matrice d'accessibilité d'une topologie figée, que
+ décrit la section suivante. Un graphe interrogé ainsi plus d'une fois gagne à être figé une seule
+ fois.

- ### Freezing
+ ### Figer

- Whether a `Topology` can be edited is a matter of its capture set, as it is for arrays. A
- `Topology[node]^` can be edited. A `Topology[node]^{}` cannot, since the editing methods need an
- exclusive handle. `dag.freeze` gives the frozen form, as does `Topology.freeze`, which consumes
- an editable topology.
+ Qu'une `Topology` puisse être modifiée dépend de son ensemble de captures, comme pour les
+ tableaux. Une `Topology[node]^` peut être modifiée. Une `Topology[node]^{}` ne le peut pas,
+ puisque les méthodes de modification exigent une poignée exclusive. `dag.freeze` donne la forme
+ figée, tout comme `Topology.freeze`, qui consomme une topologie modifiable.

- The frozen form is laid out for querying. It holds both directions of adjacency in dense arrays,
- and maintains its sources and sinks. It builds a bit matrix of reachability on first use, so
- whether one node reaches another is a single bit:
+ La forme figée est organisée pour l'interrogation. Elle contient les deux sens d'adjacence dans
+ des tableaux denses, et tient à jour ses sources et ses puits. Elle construit à la première
+ utilisation une matrice de bits d'accessibilité ; savoir si un nœud en atteint un autre se
+ résume donc à un seul bit :

```scala
val frozen = dag.freeze
```

```scala fuel
run
##
Out.println(frozen.reaches(8, 2).inspect)
Out.println(frozen.reaches(2, 8).inspect)
Out.println(frozen.predecessors(2).inspect)
```

- `predecessors` needs no acknowledgement here. `frozen.snapshot` gives a `Dag` back.
+ `predecessors` n'exige ici aucune reconnaissance. `frozen.snapshot` redonne un `Dag`.

- `closure`, `reduction` and `reaches` read the matrix. They are offered only on the frozen form,
- since an edit would make the matrix stale. To edit, take an editable copy with
- `Topology(frozen)`, edit it, and freeze again.
+ `closure`, `reduction` et `reaches` lisent la matrice. Elles ne sont proposées que sur la forme
+ figée, puisqu'une modification rendrait la matrice obsolète. Pour modifier, prenez une copie
+ modifiable avec `Topology(frozen)`, modifiez-la, et figez-la de nouveau.

- ### Editing
+ ### Modifier

- An edit to a `Dag` gives a new `Dag` where the edit cannot create a cycle, and a `Digraph` where
- it might:
+ Une modification d'un `Dag` donne un nouveau `Dag` là où elle ne peut créer de cycle, et un
+ `Digraph` là où elle le pourrait :

```tabular
- column  Edit
- column  Returns
- column  Acknowledgement
- row  `add(a, b)`  a `Dag`, or raises `Dag.Error`
- row  `remove(a, b)`  a `Dag`
- row  `- n`  a `Dag`  `linearScan`
- row  `bypass(n)`  a `Dag`  `linearScan`
- row  `bypassAll(nodes)`  a `Dag`  `linearScan`
- row  `subgraph(nodes)`  a `Dag`  `linearSize`
- row  `+ (a -> b)`  a `Digraph`
- row  `++ other`  a `Digraph`
- row  `map(f)`  a `Digraph`
+ column  Modification
+ column  Renvoie
+ column  Reconnaissance
+ row  `add(a, b)`  un `Dag`, ou lève `Dag.Error`
+ row  `remove(a, b)`  un `Dag`
+ row  `- n`  un `Dag`  `linearScan`
+ row  `bypass(n)`  un `Dag`  `linearScan`
+ row  `bypassAll(nodes)`  un `Dag`  `linearScan`
+ row  `subgraph(nodes)`  un `Dag`  `linearSize`
+ row  `+ (a -> b)`  un `Digraph`
+ row  `++ other`  un `Digraph`
+ row  `map(f)`  un `Digraph`
```

- `add` adds a dependency, and `remove` takes one away:
+ `add` ajoute une dépendance, et `remove` en retire une :

```scala fuel
run
##
Out.println(dag.add(8, 3).reachable(8).inspect)
Out.println(dag.remove(8, 4).reachable(8).inspect)
```

- `add` refuses an edge which would close a cycle:
+ `add` refuse une arête qui fermerait un cycle :

```scala fuel
run
errors show
##
dag.add(2, 8)
```

- `+` adds the same edge without complaint, because its result is a `Digraph`:
+ `+` ajoute la même arête sans protester, car son résultat est un `Digraph` :

```scala fuel
run
##
Out.println((dag + (2 -> 8)).cycle.inspect)
```

- `bypass` drops a node, and reroutes each of its dependants to each of its dependencies. Every
- path through the node survives. `-` drops the node and its edges, and the paths with them:
+ `bypass` retire un nœud, et redirige chacun de ses dépendants vers chacune de ses dépendances.
+ Tout chemin passant par le nœud survit. `-` retire le nœud et ses arêtes, et les chemins avec :

```scala fuel
run
##
Out.println(dag.bypass(6).reachable(8).inspect)
Out.println((dag - 6).reachable(8).inspect)
```

- `bypassAll` bypasses a set of nodes, one after another. `subgraph` keeps only the edges among
- the chosen nodes. It rebuilds the whole map, which is the cost that `linearSize` acknowledges:
+ `bypassAll` contourne un ensemble de nœuds, l'un après l'autre. `subgraph` ne garde que les
+ arêtes entre les nœuds choisis. Il reconstruit toute la map, ce qui est le coût que reconnaît
+ `linearSize` :

```scala
import dysasymptotics.linearSize

val part = dag.subgraph(Set(8, 4, 2))
```

- ### Editing in place
+ ### Modifier en place

- A graph is sometimes built up one edge at a time, as when a build system discovers tasks or a
- scheduler accepts jobs. A `Topology` keeps its topological order valid as edges arrive. A new
- edge disturbs only the nodes between its two ends.
+ Un graphe se construit parfois une arête à la fois, comme lorsqu'un système de build découvre
+ des tâches ou qu'un ordonnanceur accepte des travaux. Une `Topology` maintient son ordre
+ topologique valide à mesure que les arêtes arrivent. Une nouvelle arête ne perturbe que les
+ nœuds situés entre ses deux extrémités.

- A `Topology` is a mutable value with one exclusive owner. It is edited through its methods,
- which do not produce new values:
+ Une `Topology` est une valeur mutable avec un seul propriétaire exclusif. Elle se modifie par
+ ses méthodes, qui ne produisent pas de nouvelles valeurs :

```scala
val topology = Topology[Text]()
topology.add(t"app", t"lib")
topology.add(t"lib", t"core")
```

```scala fuel
run
##
Out.println(topology.linearized.inspect)
```

- An edge which would close a cycle is refused with a `Dag.Error`, and the graph is left as it
- was:
+ Une arête qui fermerait un cycle est refusée avec une `Dag.Error`, et le graphe reste tel qu'il
+ était :

```scala fuel
typecheck
##
topology.add(t"core", t"app")
```

- When the editing is done, the topology is given up. `Topology.freeze(topology)` and
- `Dag(topology)` both consume it, so the result can never be edited behind its back. A `Dag`
- thaws into a `Topology` with `thaw`, and `snapshot` takes a `Dag` without giving up the handle.
+ Une fois les modifications terminées, on renonce à la topologie. `Topology.freeze(topology)` et
+ `Dag(topology)` la consomment tous deux ; le résultat ne peut donc jamais être modifié à son
+ insu. Un `Dag` se dégèle en une `Topology` avec `thaw`, et `snapshot` prend un `Dag` sans
+ renoncer à la poignée.

- ### Partial orders
+ ### Ordres partiels

- A `Hasse` diagram takes a set and an ordering relation, and computes the covering structure: for
- each element, the elements immediately above and below it, with every implied comparison
- dropped. These are the divisors of 12, ordered by divisibility:
+ Un diagramme de `Hasse` prend un ensemble et une relation d'ordre, et calcule la structure de
+ couverture : pour chaque élément, les éléments immédiatement au-dessus et en dessous de lui,
+ toute comparaison impliquée étant écartée. Voici les diviseurs de 12, ordonnés par
+ divisibilité :

```scala
val divisors = Hasse(Set(1, 2, 3, 4, 6, 12))((a, b) => b % a == 0)
```

```scala fuel
run
##
Out.println(divisors.parents(2).inspect)
Out.println(divisors.children(6).inspect)
Out.println(divisors.maxima.inspect)
```

- A `Hasse` diagram is itself a graph, in which each element points at the elements it covers. It
- linearizes from its bottom to its top, and `divisors.dag` is the same relation as a `Dag`.
+ Un diagramme de `Hasse` est lui-même un graphe, dans lequel chaque élément pointe vers les
+ éléments qu'il couvre. Il se linéarise de son bas vers son haut, et `divisors.dag` est la même
+ relation sous forme de `Dag`.

- ### Layering
+ ### Disposition en couches

- `layered` arranges a `Dag` for drawing. Each node is assigned to a layer, with dependencies
- above dependents. The nodes within each layer are ordered so that the lines which join adjacent
- layers cross as rarely as possible. This is the layered approach of Sugiyama, which Graphviz's
- `dot` uses.
+ `layered` dispose un `Dag` pour le dessiner. Chaque nœud est affecté à une couche, les
+ dépendances au-dessus des dépendants. Les nœuds de chaque couche sont ordonnés de sorte que les
+ lignes qui joignent des couches adjacentes se croisent le moins possible. C'est l'approche en
+ couches de Sugiyama, qu'utilise `dot` de Graphviz.

```scala
val layering = dag.layered
```

- A node with no dependencies is in the first layer. Node `8` is two layers below, one for each
- dependency on its longest path. No link in this graph crosses another:
+ Un nœud sans dépendances est dans la première couche. Le nœud `8` est deux couches plus bas, une
+ pour chaque dépendance de son plus long chemin. Aucun lien de ce graphe n'en croise un autre :

```scala fuel
run
##
Out.println(layering.rank(2).show)
Out.println(layering.rank(8).show)
Out.println(layering.crossings.show)
```

- `layering.layers(1)` gives the vertices of the second layer in their drawn order, and
- `layering.links(0)` gives the positions joined between the first two layers. An edge which spans
- more than one layer passes through a `Layering.Vertex.Virtual` in each layer between, so every
- link joins adjacent layers.
+ `layering.layers(1)` donne les sommets de la deuxième couche dans leur ordre de dessin, et
+ `layering.links(0)` donne les positions reliées entre les deux premières couches. Une arête qui
+ s'étend sur plus d'une couche passe par un `Layering.Vertex.Virtual` dans chaque couche
+ intermédiaire ; chaque lien joint donc des couches adjacentes.

- By default a node sits one layer below its deepest dependency, so every node without
- dependencies shares the first layer:
+ Par défaut, un nœud se trouve une couche sous sa dépendance la plus profonde ; tous les nœuds
+ sans dépendances partagent donc la première couche :

```scala
val deep = Dag(1 -> Set(), 5 -> Set(), 2 -> Set(1), 3 -> Set(2, 5))
```

```scala fuel
run
##
Out.println(deep.layered.rank(5).show)
```

- `rankings.balancedRanking` instead pulls each node which has dependents down to the layer above
- its shallowest dependent. That shortens the long edge from a source which feeds only something
- deep. Node `5` feeds only node `3`, two layers down, so it sinks to the layer above:
+ `rankings.balancedRanking` tire plutôt chaque nœud qui a des dépendants vers le bas, jusqu'à la
+ couche située au-dessus de son dépendant le moins profond. Cela raccourcit la longue arête issue
+ d'une source qui n'alimente qu'un élément profond. Le nœud `5` n'alimente que le nœud `3`, deux
+ couches plus bas ; il descend donc à la couche au-dessus :

```scala fuel
run
##
locally:
  import rankings.balancedRanking
  Out.println(deep.layered.rank(5).show)
```

- ### Drawing
+ ### Dessin

- [Trees](trees.md#graphs) draws a graph as text, in lanes, in layers or as a dense grid.
+ [Arbres](trees.md#graphs) dessine un graphe sous forme de texte, en couloirs, en couches ou en
+ grille dense.

- A `Dag` of text is also rendered as
- [DOT](https://en.wikipedia.org/wiki/DOT_(graph_description_language)), ready for
- [Graphviz](https://graphviz.org/):
+ Un `Dag` de texte se rend aussi en
+ [DOT](https://en.wikipedia.org/wiki/DOT_(graph_description_language)), prêt pour
+ [Graphviz](https://graphviz.org/) :

```scala fuel
run
##
Out.println(dag.map(_.show).acyclic.dot.serialize)
```

- ### Related
+ ### Voir aussi

- - [Trees](trees.md) draws trees and graphs as text.
- - [Collections](collections.md) describes the vocabulary which a graph shares, and the
-   acknowledgements of cost in `dysasymptotics`.
- - [Concurrency](concurrency.md) runs the tasks which a dependency graph orders.
- - [Impossible states](../../philosophy/impossible-states.md) explains why a `Dag` cannot hold a
-   cycle.
- - [Charts](charts.md) draws a graph as a doughnut, and [compiler](compiler.md) builds a toolchain as one.
+ - [Arbres](trees.md) dessine des arbres et des graphes sous forme de texte.
+ - [Collections](collections.md) décrit le vocabulaire que partage un graphe, et les
+   reconnaissances de coût de `dysasymptotics`.
+ - [Concurrence](concurrency.md) exécute les tâches qu'ordonne un graphe de dépendances.
+ - [États impossibles](../../philosophy/impossible-states.md) explique pourquoi un `Dag` ne peut
+   contenir de cycle.
+ - [Graphiques](charts.md) dessine un graphe en anneau, et [compilateur](compiler.md) construit
+   une chaîne d'outils sous forme de graphe.
