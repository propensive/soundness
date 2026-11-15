- ## Generic Data
+ ## Données génériques

- ### About
+ ### Présentation

- A value sometimes has to leave the world where its class is known, and arrive intact: in
- another classloader, in another process, or across a staging boundary. The `Pojo`
- representation carries any Scala value using only the types which every JVM context shares.
- Those are boxed primitives, strings, and arrays of objects.
+ Une valeur doit parfois quitter le monde où sa classe est connue, et arriver intacte : dans un
+ autre classloader, dans un autre processus, ou de l'autre côté d'une frontière de mise en
+ étapes. La représentation `Pojo` transporte n'importe quelle valeur Scala en n'utilisant que
+ les types que partagent tous les contextes de la JVM : les primitifs boxés, les chaînes, et
+ les tableaux d'objets.

- A case class or an enumeration converts to a `Pojo` and back with [derived][derivation] codecs,
- so the value survives the crossing without its class traveling with it.
+ Une case class ou une énumération se convertit en `Pojo` et inversement avec des codecs
+ [dérivés][dérivation] ; la valeur survit donc à la traversée sans que sa classe voyage avec
+ elle.

- ### On classloader-neutral data
+ ### À propos des données indépendantes du classloader

- A Scala object is welded to its class, and its class to the classloader which defined it. Hand
- the object to code which was loaded elsewhere, such as an isolated plugin or a computation
- staged on another machine, and the receiving side cannot cast it back. The "same" class from
- another loader is a different class.
+ Un objet Scala est soudé à sa classe, et sa classe au classloader qui l'a définie. Remettez
+ l'objet à du code chargé ailleurs, comme un plugin isolé ou un calcul mis en étapes sur une
+ autre machine, et le côté récepteur ne peut pas le caster à nouveau. La « même » classe
+ provenant d'un autre classloader est une classe différente.

- Full serialization solves this with heavy machinery. Passing strings solves it by giving up
- structure.
+ La sérialisation complète résout ce problème au prix d'une lourde mécanique. Transmettre des
+ chaînes le résout en renonçant à la structure.

- A `Pojo` keeps the structure and drops the class. A product becomes an array of its fields, a
- variant becomes a labeled pair, and a primitive becomes its boxed self. All of these are types
- defined by the JDK, and mean the same on both sides of any boundary. This is the transport
- beneath [staged](staging.md) remote execution, and working on a generic representation in this
- way is what [composability](../../philosophy/composability.md) between formats rests on.
+ Un `Pojo` conserve la structure et abandonne la classe. Un produit devient un tableau de ses
+ champs, un variant devient une paire étiquetée, et un primitif devient sa propre version
+ boxée. Ce sont tous des types définis par le JDK, qui signifient la même chose des deux côtés
+ de toute frontière. C'est le transport sous-jacent à l'exécution distante
+ [mise en étapes](staging.md), et travailler ainsi sur une représentation générique est ce
+ sur quoi repose la [composabilité](../../philosophy/composability.md) entre formats.

- Everything comes from the `soundness` package. The samples throw any error as an exception, and
- the last import lets them print their results:
+ Tout vient du package `soundness`. Les exemples lèvent toute erreur sous forme d'exception,
+ et le dernier import leur permet d'afficher leurs résultats :

```scala
import soundness.*
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### Encoding and decoding
+ ### Encodage et décodage

- `pojo` encodes a value. The result holds only arrays, strings and boxed primitives:
+ `pojo` encode une valeur. Le résultat ne contient que des tableaux, des chaînes et des
+ primitifs boxés :

```scala
case class Person(name: Text, age: Int)
case class Troupe(players: List[Person], size: Int)

val troupe = Troupe(List(Person(t"Quince", 40), Person(t"Bottom", 35)), 2)

val transportable = troupe.pojo
```

- `as` decodes it. The receiving side must state the type it expects, and the data may not match
- that type, so decoding can fail, and needs a [strategy]:
+ `as` la décode. Le côté récepteur doit indiquer le type qu'il attend, et les données peuvent
+ ne pas correspondre à ce type ; le décodage peut donc échouer, et requiert une [stratégie] :

```scala fuel
run
##
Out.println(transportable.as[Troupe].inspect)
```

- Data which is not shaped like the expected type raises a `Pojo.Error`. A string, for
- instance, is not a `Person`:
+ Des données qui n'ont pas la forme du type attendu lèvent une `Pojo.Error`. Une chaîne, par
+ exemple, n'est pas une `Person` :

```scala fuel
run
errors show
##
t"Bottom".pojo.as[Person]
```

- Nested case classes, collections and enumerations all derive their conversions, so any value
- which is shaped like data crosses without preparation.
+ Les case classes imbriquées, les collections et les énumérations dérivent toutes leurs
+ conversions ; toute valeur qui a la forme de données traverse donc sans préparation.

- ### The representation
+ ### La représentation

- The encoding is deliberately transparent:
+ L'encodage est délibérément transparent :

```tabular
- column  Scala value
- column  Pojo form
- row  `Text` or `String`  a `String`
- row  `Int`, `Long`, `Float`, `Double`, `Char`, `Boolean` or `Byte`  its boxed `java.lang` form
- row  a collection: `List`, `Set`, `Sequence` or other `Iterable`  an `Array[Object]` of its elements
- row  a case class  an `Array[Object]` of its fields in declaration order
- row  an enumeration case  an `Array[Object]` pair of its name and its payload
+ column  Valeur Scala
+ column  Forme Pojo
+ row  `Text` ou `String`  une `String`
+ row  `Int`, `Long`, `Float`, `Double`, `Char`, `Boolean` ou `Byte`  sa forme boxée `java.lang`
+ row  une collection : `List`, `Set`, `Sequence` ou autre `Iterable`  un `Array[Object]` de ses éléments
+ row  une case class  un `Array[Object]` de ses champs dans l'ordre de déclaration
+ row  un cas d'énumération  une paire `Array[Object]` formée de son nom et de sa charge utile
```

- So `Person(t"Bottom", 35).pojo` is an array of the string `"Bottom"` and the boxed integer
- `35`, and the `Green` case of an enumeration is an array of the string `"Green"` and an empty
- array:
+ Ainsi, `Person(t"Bottom", 35).pojo` est un tableau formé de la chaîne `"Bottom"` et de
+ l'entier boxé `35`, et le cas `Green` d'une énumération est un tableau formé de la chaîne
+ `"Green"` et d'un tableau vide :

```scala
enum Light:
  case Red, Amber, Green

val person = Person(t"Bottom", 35).pojo
val light = (Light.Green: Light).pojo
```

- Both sides derive the codec from the same type definition, so the encoding needs no schema on
- the wire. It does mean that both sides must agree on that definition.
+ Les deux côtés dérivent le codec de la même définition de type ; l'encodage n'a donc besoin
+ d'aucun schéma sur le fil. Cela implique toutefois que les deux côtés s'accordent sur cette
+ définition.

- `Pojo` is an [opaque type] over the JDK types it permits. A `Pojo` cannot be confused with an
- arbitrary `Object`, and the only way to produce one is through an encoder.
+ `Pojo` est un [type opaque] sur les types du JDK qu'il autorise. Un `Pojo` ne peut pas être
+ confondu avec un `Object` arbitraire, et la seule façon d'en produire un passe par un
+ encodeur.

- ### Where it is used
+ ### Où on l'utilise

- The obvious case is a boundary between classloaders. Code which is loaded in an isolated
- loader, such as a plugin, a compiled fragment or a sandboxed extension, shares the JDK's
- classes with its host, and nothing else. The JDK's classes are therefore exactly the vocabulary
- the two can speak. [Classpath](classpath.md) describes classloaders.
+ Le cas évident est une frontière entre classloaders. Le code chargé dans un classloader
+ isolé, comme un plugin, un fragment compilé ou une extension en bac à sable, partage avec
+ son hôte les classes du JDK, et rien d'autre. Les classes du JDK sont donc exactement le
+ vocabulaire que les deux peuvent parler. [Classpath](classpath.md) décrit les classloaders.

- The same reasoning applies across a boundary between processes, which is what
- [staged](staging.md) remote execution uses. The arguments to a staged computation are encoded
- on one side, sent, and decoded on the far side against the type which the staged code expects.
+ Le même raisonnement s'applique à travers une frontière entre processus, ce qu'utilise
+ l'exécution distante [mise en étapes](staging.md). Les arguments d'un calcul mis en étapes
+ sont encodés d'un côté, envoyés, et décodés de l'autre côté selon le type qu'attend le code
+ mis en étapes.

- Nothing about the representation is specific to either use. Any boundary which can carry boxed
- primitives, strings and arrays of objects can carry a Scala value.
+ Rien dans la représentation n'est propre à l'un ou l'autre usage. Toute frontière capable
+ de transporter des primitifs boxés, des chaînes et des tableaux d'objets peut transporter
+ une valeur Scala.

- ### Related
+ ### Voir aussi

- - [Derivation](derivation.md) explains the engine which generates the codecs.
+ - [Dérivation](derivation.md) explique le moteur qui génère les codecs.
