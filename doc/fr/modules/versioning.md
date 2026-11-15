- ## Versioning
+ ## Gestion des versions

- ### About
+ ### Présentation

- A [semantic version](https://semver.org/) is a structured claim: this major version, this
- minor, this patch, and perhaps a prerelease. The specification has precise rules for validity
- and for ordering, and `Semver` implements them exactly. A version literal is checked at
- [compiletime], versions compare by the specification's rules of precedence, and text is parsed
- with a typed error.
+ Une [version sémantique](https://semver.org/) est une affirmation structurée : telle version
+ majeure, telle version mineure, tel correctif, et éventuellement une préversion. La
+ spécification fixe des règles précises de validité et d'ordre, et `Semver` les implémente
+ exactement. Un littéral de version est vérifié [à la compilation][compilation], les versions se
+ comparent selon les règles de précédence de la spécification, et l'analyse d'un texte signale
+ une erreur typée.

- JAR *manifests* are handled here too. They are read and written as typed values, whose standard
- attributes have real types: a `Main-Class` is an [Fqcn](stack-traces.md), not a string.
+ Les *manifestes* JAR sont également pris en charge ici. Ils sont lus et écrits comme des valeurs
+ typées, dont les attributs standard ont de vrais types : un `Main-Class` est un
+ [Fqcn](stack-traces.md), et non une chaîne.

- ### On version numbers
+ ### À propos des numéros de version

- Version strings invite thinking in strings: split on dots, compare the characters, and
- `1.10.0` sorts before `1.9.0`. The semantic versioning specification pins down both the grammar
- and the rules of precedence, including the subtle ones:
+ Les chaînes de version incitent à raisonner en chaînes : découper sur les points, comparer les
+ caractères, et `1.10.0` se retrouve classé avant `1.9.0`. La spécification de la gestion
+ sémantique des versions définit précisément la grammaire et les règles de précédence, y compris
+ les plus subtiles :

- - Leading zeros are illegal, and a prerelease identifier may contain only certain characters.
- - Numeric identifiers compare numerically.
- - `1.0.0-alpha` precedes `1.0.0`.
- - Build metadata never affects ordering.
+ - Les zéros non significatifs sont interdits, et un identifiant de préversion ne peut contenir
+   que certains caractères.
+ - Les identifiants numériques se comparent numériquement.
+ - `1.0.0-alpha` précède `1.0.0`.
+ - Les métadonnées de build n'influent jamais sur l'ordre.

- Rules that precise belong in a type. A version which cannot be constructed invalid, and which
- compares as the specification says, is
- [safety by construction](../../philosophy/safety-by-construction.md).
+ Des règles aussi précises ont leur place dans un type. Une version qu'on ne peut pas construire
+ invalide, et qui se compare comme le dit la spécification, relève de la
+ [sûreté par construction](../../philosophy/safety-by-construction.md).

- Everything comes from the `soundness` package. The samples throw any error as an exception, and
- the last import lets them print their results:
+ Tout provient du package `soundness`. Les exemples lèvent toute erreur sous forme d'exception,
+ et le dernier import leur permet d'afficher leurs résultats :

```scala
import soundness.*
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### Versions
+ ### Versions

- The `v"…"` [interpolator] writes a version, and checks it at compiletime:
+ L'[interpolateur] `v"…"` écrit une version, et la vérifie à la compilation :

```scala fuel
compile
##
val version = v"1.4.2"
```

- A literal which is not a semantic version does not compile:
+ Un littéral qui n'est pas une version sémantique ne compile pas :

```scala fuel
compile
errors show
##
v"1.2"
```

- Text is [decoded](decoding.md#decoding-a-value) to the same type with `as[Semver]`, and a
- malformed version raises a `Semver.Error`:
+ Un texte est [décodé](decoding.md#decoding-a-value) vers le même type avec `as[Semver]`, et une
+ version mal formée lève une `Semver.Error` :

```scala fuel
run
##
Out.println(t"2.0.0-rc.1".as[Semver].show)
```

```scala fuel
run
errors show
##
t"2.0".as[Semver]
```

- ### Ordering
+ ### Ordre

- Ordering follows the specification exactly. A longer prerelease follows a shorter one, numeric
- identifiers compare as numbers, and a release outranks its own prereleases:
+ L'ordre suit exactement la spécification. Une préversion plus longue suit une plus courte, les
+ identifiants numériques se comparent comme des nombres, et une version finale l'emporte sur ses
+ propres préversions :

```scala fuel
compile
run
##
Out.println((v"1.0.0-alpha" < v"1.0.0-alpha.1").inspect)
Out.println((v"1.0.0-beta.2" < v"1.0.0-beta.11").inspect)
Out.println((v"1.0.0-rc.1" < v"1.0.0").inspect)
Out.println((v"1.9.0" < v"1.10.0").inspect)
```

- A version knows whether it is still in initial `development`, with a major version of zero,
- when anything may change. `release` strips the prerelease and build parts.
+ Une version sait si elle est encore en phase initiale de `development`, avec une version majeure
+ égale à zéro, où tout peut changer. `release` retire les parties de préversion et de build.

- ### Manifests
+ ### Manifestes

- A JAR's `META-INF/MANIFEST.MF` is read and written as a `Manifest` of typed attributes. The
- standard attributes are named values, and reading one gives a value of its proper type:
+ Le fichier `META-INF/MANIFEST.MF` d'un JAR est lu et écrit comme un `Manifest` d'attributs
+ typés. Les attributs standard sont des valeurs nommées, et la lecture de l'un d'eux donne une
+ valeur du type qui lui convient :

```scala
import manifestAttributes.*

val manifest = Manifest
  ( ManifestVersion(()),
    MainClass(fqcn"com.example.Main"),
    CreatedBy(t"Soundness") )

val main: Optional[Fqcn] = manifest(MainClass)
```

- A manifest read from a JAR is parsed through the same type. Tools which inspect or rewrite
- manifests, as [Packaging](packaging.md) does, therefore work with values, not with the format's
- fussy rules for wrapping lines. [Archives](archives.md) reads and writes the JAR itself.
+ Un manifeste lu depuis un JAR est analysé à travers le même type. Les outils qui inspectent ou
+ réécrivent des manifestes, comme le fait [Packaging](packaging.md), manipulent donc des valeurs,
+ et non les règles tatillonnes du format sur le retour à la ligne. [Archives](archives.md) lit
+ et écrit le JAR lui-même.
