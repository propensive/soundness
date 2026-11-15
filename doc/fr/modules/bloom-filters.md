- ## Bloom Filters
+ ## Filtres de Bloom

- ### About
+ ### Présentation

- A [Bloom filter](https://en.wikipedia.org/wiki/Bloom_filter) answers the question "have I seen
- this before?" in constant space, at the price of an occasional false positive. A *no* is always
- right. A *yes* is right with a probability which the filter's size controls.
+ Un [filtre de Bloom](https://en.wikipedia.org/wiki/Bloom_filter) répond à la question « ai-je
+ déjà vu cela ? » en espace constant, au prix d'un faux positif occasionnel. Un *non* est
+ toujours juste. Un *oui* est juste avec une probabilité que contrôle la taille du filtre.

- A filter is constructed from the two numbers which actually matter: roughly how many elements
- it will hold, and the rate of false positives to tolerate. The size of the bit array and the
- number of hashes are worked out from them.
+ Un filtre se construit à partir des deux nombres qui comptent réellement : à peu près combien
+ d'éléments il contiendra, et le taux de faux positifs à tolérer. La taille du tableau de bits
+ et le nombre de hachages en sont déduits.

- Any [digestible](hashing.md#hashing-your-own-types) value can be added, and the hash algorithm
- is chosen as a type. A filter is [immutable]: adding an element gives a new filter, so filters
- are shared and accumulated as safely as any other value.
+ Toute valeur [hachable](hashing.md#hashing-your-own-types) peut être ajoutée, et
+ l'algorithme de hachage se choisit comme un type. Un filtre est [immuable] : ajouter un
+ élément donne un nouveau filtre ; les filtres se partagent et s'accumulent donc aussi
+ sûrement que n'importe quelle autre valeur.

- ### On probabilistic membership
+ ### À propos de l'appartenance probabiliste

- Storing every element of a large set, just to answer whether something is a member, is often a
- waste. A cache of URLs which have already been fetched, a guard against sending a notification
- twice, and a check before an expensive lookup all survive the occasional false *yes*.
+ Stocker chaque élément d'un grand ensemble, simplement pour savoir si quelque chose en fait
+ partie, est souvent un gaspillage. Un cache d'URL déjà récupérées, une protection contre
+ l'envoi d'une notification en double, et une vérification avant une recherche coûteuse
+ s'accommodent tous d'un *oui* erroné occasionnel.

- A Bloom filter hashes each element to a handful of positions in an array of bits, and
- remembers only the bits. That is a few bits for each element, whatever the size of the
- elements. But tuning a filter means choosing the size of the bit array and the number of hash
- functions, by a formula which most code copies from a textbook, sometimes wrongly.
+ Un filtre de Bloom hache chaque élément vers une poignée de positions dans un tableau de
+ bits, et ne retient que les bits. Cela représente quelques bits par élément, quelle que soit
+ la taille des éléments. Mais régler un filtre implique de choisir la taille du tableau de
+ bits et le nombre de fonctions de hachage, selon une formule que la plupart des programmes
+ recopient d'un manuel, parfois de travers.

- Here the filter is asked for by intent: the expected size, and the target error rate. The
- parameters are computed. The error rate is a [bounded number](numbers.md#bounded-numbers), so
- a rate outside `[0, 1]` does not compile. The filter's error rate is fixed by how it is
- constructed, and is not checked at each use, which is
- [safety by construction](../../philosophy/safety-by-construction.md).
+ Ici, on demande le filtre par son intention : la taille attendue, et le taux d'erreur visé.
+ Les paramètres sont calculés. Le taux d'erreur est un
+ [nombre borné](numbers.md#bounded-numbers) ; un taux hors de `[0, 1]` ne compile donc pas.
+ Le taux d'erreur du filtre est fixé par la manière dont il est construit, et n'est pas
+ vérifié à chaque utilisation, ce qui relève de la
+ [sûreté par construction](../../philosophy/safety-by-construction.md).

- Everything comes from the `soundness` package. A provider of hash algorithms is in scope, and
- the last import lets the samples print their results:
+ Tout provient du package `soundness`. Un fournisseur d'algorithmes de hachage est dans la
+ portée, et le dernier import permet aux exemples d'afficher leurs résultats :

```scala
import soundness.*
import providers.soundnessProvider
import Blake3.hash
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### Creating and adding
+ ### Créer et ajouter

- A filter is created for a type of element, an expected size and an error rate. `+` adds one
- element, and `++` adds many:
+ Un filtre se crée pour un type d'élément, une taille attendue et un taux d'erreur. `+`
+ ajoute un élément, et `++` en ajoute plusieurs :

```scala
val bloom = BloomFilter[Text](100, 0.001) + t"Hello world"

val fuller = bloom ++ List(t"hello", t"world")
```

- Anything with a `Digestible` instance can be an element, including text, numbers and case
- classes, since elements enter the filter through the same hashing as everything else.
+ Tout ce qui a une instance de `Digestible` peut être un élément, y compris le texte, les
+ nombres et les case classes, puisque les éléments entrent dans le filtre au moyen du même
+ hachage que tout le reste.

- An error rate which is not a probability does not compile:
+ Un taux d'erreur qui n'est pas une probabilité ne compile pas :

```scala fuel
compile
errors show
##
BloomFilter[Text](100, 1.5)
```

- ### Querying
+ ### Interroger

- `hits` asks whether an element may be present. A `false` is definitive: the element was never
- added. A `true` means *probably*, with the probability of a false positive which the filter
- was built for:
+ `hits` demande si un élément est peut-être présent. Un `false` est définitif : l'élément n'a
+ jamais été ajouté. Un `true` signifie *probablement*, avec la probabilité de faux positif
+ pour laquelle le filtre a été construit :

```scala fuel
run
##
Out.println(fuller.hits(t"hello").inspect)
Out.println(fuller.hits(t"goodbye").inspect)
Out.println(bloom.hits(t"hello").inspect)
```

- The last line shows that `bloom` itself was not changed when `fuller` was made from it.
+ La dernière ligne montre que `bloom` lui-même n'a pas été modifié lorsque `fuller` en a été
+ dérivé.

- The asymmetry is the contract. A Bloom filter suits exactly those places where a false *yes*
- costs a redundant check, and a false *no* would be a bug.
+ L'asymétrie est le contrat. Un filtre de Bloom convient exactement aux endroits où un *oui*
+ erroné coûte une vérification redondante, et où un *non* erroné serait un bogue.

- ### Choosing the parameters
+ ### Choisir les paramètres

- The two numbers given at construction are the expected number of elements and the tolerable
- rate of false positives. The size of the bit array and the number of hash functions follow
- from them, by the standard formulae. Stating the intent, and not the parameters, means that
- the two cannot disagree: a filter sized for a hundred elements at one in a thousand is exactly
- that.
+ Les deux nombres donnés à la construction sont le nombre d'éléments attendu et le taux de
+ faux positifs tolérable. La taille du tableau de bits et le nombre de fonctions de hachage en
+ découlent, selon les formules standard. Énoncer l'intention, et non les paramètres, signifie
+ que les deux ne peuvent pas être en désaccord : un filtre dimensionné pour cent éléments à un
+ sur mille est exactement cela.

- It is worth knowing what happens when the *expected size* is wrong. A filter which is given
- far more elements than it was built for keeps working, but its rate of false positives rises
- above the rate requested. The rate is a promise about a filter used as intended, not a
- guarantee which holds however the filter is filled.
+ Il est utile de savoir ce qui se passe lorsque la *taille attendue* est fausse. Un filtre
+ qui reçoit bien plus d'éléments que prévu continue de fonctionner, mais son taux de faux
+ positifs dépasse le taux demandé. Le taux est une promesse concernant un filtre utilisé comme
+ prévu, et non une garantie valable quelle que soit la manière dont le filtre est rempli.

- ### Where the bits come from
+ ### D'où viennent les bits

- The hash algorithm is part of the filter's type, and every bit position is derived from a
- single digest of the element. The digest's first two 64-bit words, `h₁` and `h₂`, give
- position *i* as `h₁ + i·h₂`, modulo the size of the bit array. This is the double-hashing
- scheme which Guava also uses, and it costs one digest for each element, however many positions
- are needed. An algorithm whose digest is shorter than sixteen bytes, such as a CRC, is applied
- again under a counter until enough bytes have been gathered.
+ L'algorithme de hachage fait partie du type du filtre, et chaque position de bit est dérivée
+ d'une unique empreinte de l'élément. Les deux premiers mots de 64 bits de l'empreinte, `h₁`
+ et `h₂`, donnent la position *i* sous la forme `h₁ + i·h₂`, modulo la taille du tableau de
+ bits. C'est le schéma de double hachage qu'utilise aussi Guava, et il coûte une empreinte par
+ élément, quel que soit le nombre de positions nécessaires. Un algorithme dont l'empreinte
+ fait moins de seize octets, comme un CRC, est appliqué de nouveau avec un compteur jusqu'à ce
+ que suffisamment d'octets aient été rassemblés.

- The algorithm in scope therefore decides the filter's behavior, and two filters over the same
- elements agree only if they agree on the algorithm.
+ L'algorithme dans la portée décide donc du comportement du filtre, et deux filtres sur les
+ mêmes éléments ne concordent que s'ils s'accordent sur l'algorithme.

- A Bloom filter has no adversary to resist, only a distribution to spread, so the cryptographic
- BLAKE3 is more hash than it needs. `Murmur3`, the 128-bit MurmurHash3, costs about a fifth as
- much for each element, and is the algorithm which Guava's filters use. It is chosen by naming
- it, behind the same [permit](hashing.md#fast-hashing) as the checksums:
+ Un filtre de Bloom n'a aucun adversaire à qui résister, seulement une distribution à
+ étaler ; BLAKE3, cryptographique, est donc plus de hachage qu'il n'en faut. `Murmur3`, le
+ MurmurHash3 sur 128 bits, coûte environ un cinquième par élément, et c'est l'algorithme
+ qu'utilisent les filtres de Guava. On le choisit en le nommant, derrière le même
+ [permis](hashing.md#fast-hashing) que les sommes de contrôle :

```scala
import cryptoPermits.permitNonCryptographicHashes

val fast = BloomFilter[Text](10_000, 0.01)[Murmur3]
```

- ### Filling a filter in place
+ ### Remplir un filtre sur place

- A filter fresh from `BloomFilter[Text](n, p)` is *exclusive*. It belongs to the code which
- made it, and that code fills it in place, with `add` and `addAll`, with no copying at all, and
- then freezes it:
+ Un filtre tout juste issu de `BloomFilter[Text](n, p)` est *exclusif*. Il appartient au code
+ qui l'a créé, et ce code le remplit sur place, avec `add` et `addAll`, sans aucune copie,
+ puis le fige :

```scala
val filter = BloomFilter[Text](10_000, 0.01)
filter.add(t"first")
filter.addAll(List(t"second", t"third"))
val frozen = BloomFilter.freeze(filter)
```

```scala fuel
run
##
Out.println(frozen.hits(t"second").inspect)
Out.println(frozen.hits(t"fourth").inspect)
```

- `freeze` consumes the exclusive filter, and the compiler's separation checking rejects any
- later use of `filter`. What it gives is the frozen form, which may be shared across threads,
- held in a data structure and queried from anywhere, but never written to. This is the same
- discipline as freezing an array, and it is what makes the immutability below free, not copied.
+ `freeze` consomme le filtre exclusif, et le contrôle de séparation du compilateur rejette
+ toute utilisation ultérieure de `filter`. Ce qu'il donne est la forme figée, qui peut être
+ partagée entre threads, conservée dans une structure de données et interrogée de n'importe
+ où, mais jamais modifiée. C'est la même discipline que pour figer un tableau, et c'est ce
+ qui rend l'immuabilité décrite ci-dessous gratuite, et non copiée.

- ### Immutability
+ ### Immuabilité

- `+` and `++` give new filters, and do not change the original, at the price of one copy of
- the bits each. Building a filter from a large collection with `++` is a single pass and a
- single copy, not the sequence of copies which repeated `+` would make. Filling an exclusive
- filter with `addAll`, before freezing it, makes no copy at all.
+ `+` et `++` donnent de nouveaux filtres, et ne modifient pas l'original, au prix d'une copie
+ des bits chacun. Construire un filtre à partir d'une grande collection avec `++` se fait en
+ une seule passe et une seule copie, et non par la suite de copies que ferait un `+` répété.
+ Remplir un filtre exclusif avec `addAll`, avant de le figer, ne fait aucune copie.

- The operations on a filter, side by side:
+ Les opérations sur un filtre, côte à côte :

```tabular
- column  Operation
- column  Applies to
- column  Yields
- column  Copies the bits
- row  `filter + element`  any filter  a new frozen filter  once
- row  `filter ++ elements`  any filter  a new frozen filter  once
- row  `filter.add(element)`  an exclusive filter  nothing; fills it in place  never
- row  `filter.addAll(elements)`  an exclusive filter  nothing; fills it in place  never
- row  `BloomFilter.freeze(filter)`  an exclusive filter, consumed  the frozen filter  never
- row  `filter.hits(element)`  any filter  `false` if absent, `true` if probably present  never
+ column  Opération
+ column  S'applique à
+ column  Produit
+ column  Copie les bits
+ row  `filter + element`  tout filtre  un nouveau filtre figé  une fois
+ row  `filter ++ elements`  tout filtre  un nouveau filtre figé  une fois
+ row  `filter.add(element)`  un filtre exclusif  rien ; le remplit sur place  jamais
+ row  `filter.addAll(elements)`  un filtre exclusif  rien ; le remplit sur place  jamais
+ row  `BloomFilter.freeze(filter)`  un filtre exclusif, consommé  le filtre figé  jamais
+ row  `filter.hits(element)`  tout filtre  `false` s'il est absent, `true` s'il est probablement présent  jamais
```

- [Caching](caching.md) covers the case where the answer itself must be remembered, not only
- whether it has been seen.
+ [Cache](caching.md) couvre le cas où la réponse elle-même doit être mémorisée, et pas
+ seulement le fait de l'avoir vue.
