- ## Bit Flags
+ ## Indicateurs binaires

- ### About
+ ### Présentation

- A `Flags[Color]` is a set of on-or-off switches, one for each case of the `Color`
- [enumeration], held in the bits of a single `Long`. A flag is set and tested by naming its case,
- never by a bit number. The value stays one machine word, so it is as cheap to copy and store as
- a number, but it reads like a set.
+ Un `Flags[Color]` est un ensemble d'interrupteurs marche-arrêt, un pour chaque cas de
+ l'[énumération] `Color`, contenus dans les bits d'un seul `Long`. Un indicateur se positionne et
+ se teste en nommant son cas, jamais par un numéro de bit. La valeur reste un seul mot machine :
+ elle est donc aussi peu coûteuse à copier et à stocker qu'un nombre, mais se lit comme un
+ ensemble.

- ### On flag sets
+ ### Les ensembles d'indicateurs

- Bit flags are one of the oldest data structures in systems programming. A program writes
- `flags | 0x10` to switch an option on and `flags & 0x04` to test another, and the meaning of each
- bit is recorded in a comment, if anywhere. The compact representation is worth keeping: a
- `Set[Color]` costs an allocation and several pointers to hold what fits in one word.
+ Les indicateurs binaires sont l'une des plus anciennes structures de données de la programmation
+ système. Un programme écrit `flags | 0x10` pour activer une option et `flags & 0x04` pour en
+ tester une autre, et le sens de chaque bit est consigné dans un commentaire, s'il l'est quelque
+ part. La représentation compacte vaut la peine d'être conservée : un `Set[Color]` coûte une
+ allocation et plusieurs pointeurs pour contenir ce qui tient dans un seul mot.

- The untyped access is not worth keeping. Nothing stops a program testing a bit that belongs to a
- different set of options, or a bit that means nothing at all.
+ L'accès non typé ne vaut pas la peine d'être conservé. Rien n'empêche un programme de tester un
+ bit qui appartient à un autre ensemble d'options, ou un bit qui ne signifie rien du tout.

- `Flags` keeps the word and gives the bits types. Each case of an enumeration owns the bit at its
- ordinal: the first case owns bit 0, the second bit 1, and so on. A flag can only be named by a
- case of the right enumeration, so a flag from another set of options is an
- [impossible state](../../philosophy/impossible-states.md), rejected at [compiletime]. Everything
- comes from the `soundness` package. The second import lets the samples print their results:
+ `Flags` conserve le mot et donne des types aux bits. Chaque cas d'une énumération possède le bit
+ situé à son ordinal : le premier cas possède le bit 0, le deuxième le bit 1, et ainsi de suite.
+ Un indicateur ne peut être nommé que par un cas de la bonne énumération : un indicateur issu d'un
+ autre ensemble d'options est donc un [état impossible](../../philosophy/impossible-states.md),
+ rejeté [à la compilation][compilation]. Tout provient du package `soundness`. Le deuxième import
+ permet aux exemples d'afficher leurs résultats :

```scala
import soundness.*
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### Setting and testing flags
+ ### Positionner et tester des indicateurs

- `Flags[Color]()` makes a flag set for the enumeration `Color`, with every flag off:
+ `Flags[Color]()` crée un ensemble d'indicateurs pour l'énumération `Color`, avec tous les
+ indicateurs désactivés :

```scala
enum Color:
  case Red, Orange, Yellow, Green, Blue, Indigo, Violet

val none = Flags[Color]()
```

- Assigning `true` to a case switches its flag on. A `Flags` value is [immutable], so the assignment
- is an expression whose value is a new flag set, and `none` is left as it was:
+ Affecter `true` à un cas active son indicateur. Une valeur `Flags` est [immuable] : l'affectation
+ est donc une expression dont la valeur est un nouvel ensemble d'indicateurs, et `none` reste tel
+ qu'il était :

```scala fuel
highlight none(Color.Orange) = true  a new `Flags[Color]`, with Orange on
##
val warm = none(Color.Orange) = true
```

- Applying a flag set to a case tests its flag:
+ Appliquer un ensemble d'indicateurs à un cas teste son indicateur :

```scala fuel
run
##
Out.println(warm(Color.Orange).inspect)
Out.println(warm(Color.Red).inspect)
Out.println(none(Color.Orange).inspect)
```

- A program that switches several flags usually keeps the current set in a `var`, and assigns each
- new set back to it. Assigning `false` switches a flag off again:
+ Un programme qui bascule plusieurs indicateurs conserve en général l'ensemble courant dans une
+ `var`, et lui réaffecte chaque nouvel ensemble. Affecter `false` désactive de nouveau un
+ indicateur :

```scala
var flags = Flags[Color]()
flags = flags(Color.Orange) = true
flags = flags(Color.Green) = true
flags = flags(Color.Violet) = true
flags = flags(Color.Green) = false
```

- Because every step produces a new value, a flag set can be shared between threads, or stored in
- another data structure, as safely as the `Long` it is.
+ Comme chaque étape produit une nouvelle valeur, un ensemble d'indicateurs peut être partagé entre
+ threads, ou stocké dans une autre structure de données, avec autant de sûreté que le `Long` qu'il
+ est.

- ### Flags as a set
+ ### Les indicateurs comme ensemble

- `set` reads the flags that are on as an ordinary `Set` of the enumeration's cases, ready for
- iteration and the [set operations](collections.md#sets-and-maps):
+ `set` lit les indicateurs activés sous forme d'un `Set` ordinaire des cas de l'énumération, prêt
+ pour l'itération et les [opérations ensemblistes](collections.md#sets-and-maps) :

```scala fuel
run
##
Out.println(flags.set.show)
Out.println(none.set.show)
```

- ### What the compiler rejects
+ ### Ce que rejette le compilateur

- A flag set accepts only the cases of its own enumeration. A case of another enumeration is a type
- error, where a raw bit mask would have silently tested whichever bit had the same position:
+ Un ensemble d'indicateurs n'accepte que les cas de sa propre énumération. Un cas d'une autre
+ énumération est une erreur de type, là où un masque de bits brut aurait testé sans rien dire le
+ bit qui se trouvait à la même position :

```scala
enum Weekday:
  case Monday, Tuesday, Wednesday
```

```scala fuel
compile
errors show
##
flags(Weekday.Monday)
```

- ### How many flags
+ ### Combien d'indicateurs

- A flag occupies the bit at its case's ordinal, and a `Long` has 64 bits, so an enumeration of up
- to 64 cases fits in one flag set. The cases, their names and their ordinals come from the
- enumeration's `Enumerable` instance, which is [derived][derivation] from the `enum` declaration;
- [Enumerations by name and position](decoding.md#enumerations-by-name-and-position) shows what
- else that instance provides.
+ Un indicateur occupe le bit situé à l'ordinal de son cas, et un `Long` a 64 bits : une
+ énumération d'au plus 64 cas tient donc dans un seul ensemble d'indicateurs. Les cas, leurs noms
+ et leurs ordinaux proviennent de l'instance d'`Enumerable` de l'énumération, qui est
+ [dérivée][dérivation] de la déclaration `enum` ;
+ [Énumérations par nom et par position](decoding.md#enumerations-by-name-and-position) montre ce
+ que fournit encore cette instance.

- ### Related
+ ### Voir aussi

- - [Numbers](numbers.md) and [binary data](binary-data.md) describe the bits beneath a set of flags.
+ - [Nombres](numbers.md) et [données binaires](binary-data.md) décrivent les bits sous-jacents à
+   un ensemble d'indicateurs.
