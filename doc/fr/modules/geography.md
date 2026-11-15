- ## Geography
+ ## Géographie

- ### About
+ ### Présentation

- Angles, positions on the Earth, and the compass are typed values.
+ Les angles, les positions sur la Terre et la boussole sont des valeurs typées.

- An `Angle` is a real angular quantity. It is constructed in degrees or in radians, is added
- and scaled with wrapping, and is normalized to a principal value. It is not a bare number
- whose unit is a matter of memory.
+ Un `Angle` est une véritable grandeur angulaire. Il se construit en degrés ou en radians,
+ s'additionne et se met à l'échelle en rebouclant, et se normalise en une valeur principale. Ce
+ n'est pas un simple nombre dont l'unité est affaire de mémoire.

- A `Location` is a latitude and a longitude, packed into a single value. Bearings, angular
- distances and [geohashes](https://en.wikipedia.org/wiki/Geohash) are computed from it, and a
- bearing is shown as a point of a
- [compass rose](https://en.wikipedia.org/wiki/Compass_rose) with 4, 8 or 16 points.
+ Une `Location` est une latitude et une longitude, regroupées en une seule valeur. Les relèvements,
+ les distances angulaires et les [geohashes](https://en.wikipedia.org/wiki/Geohash) se calculent à
+ partir d'elle, et un relèvement s'affiche comme un point d'une
+ [rose des vents](https://en.wikipedia.org/wiki/Compass_rose) à 4, 8 ou 16 points.

- ### On angles and positions
+ ### Les angles et les positions

- The oldest bug in geometric code is the unit of an angle: a `Double` which one function
- treats as degrees, and another as radians, with nothing in the type to decide between them.
- Angles also wrap, since 370° is 10°, and code which forgets that accumulates rotations which
- compare as unequal when they should not. Positions inherit both problems twice over, once
- for each coordinate.
+ Le plus vieux bogue du code géométrique est l'unité d'un angle : un `Double` qu'une fonction
+ traite comme des degrés, et une autre comme des radians, sans rien dans le type pour trancher.
+ Les angles rebouclent aussi, puisque 370° valent 10°, et un code qui l'oublie accumule des
+ rotations qui se comparent comme différentes alors qu'elles ne devraient pas l'être. Les
+ positions héritent des deux problèmes en double, une fois pour chaque coordonnée.

- Here the angle is a type of its own. It is constructed through its unit, and its arithmetic
- wraps, so a bare number never stands for an angle. Angles and locations which are
- dimensioned values, not bare doubles, rule out
- [impossible states](../../philosophy/impossible-states.md): a latitude cannot be added to a
- distance.
+ Ici, l'angle est un type à part entière. Il se construit au moyen de son unité, et son
+ arithmétique reboucle : un simple nombre ne tient donc jamais lieu d'angle. Des angles et des
+ positions qui sont des valeurs dimensionnées, et non de simples doubles, excluent des
+ [états impossibles](../../philosophy/impossible-states.md) : une latitude ne peut pas être
+ ajoutée à une distance.

- Everything comes from the `soundness` package. An angle's textual form, which is degrees
- with a `°`, is a named given, imported beside it, and the last import lets the samples print
- their results:
+ Tout provient du package `soundness`. La forme textuelle d'un angle, en degrés avec un `°`, est
+ un given nommé, importé à côté, et le dernier import permet aux exemples d'afficher leurs
+ résultats :

```scala
import soundness.*
import soundness.angleShowable
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### Angles
+ ### Angles

- An angle is written through its unit, by multiplying the constant `Degree`, or with the
- extension `.deg`, or with `.rad` for radians. It is read back in either unit:
+ Un angle s'écrit au moyen de son unité, en multipliant la constante `Degree`, ou avec
+ l'extension `.deg`, ou avec `.rad` pour les radians. Il se relit dans l'une ou l'autre unité :

```scala
val bearing = 45.0.deg
```

```scala fuel
run
##
Out.println(bearing.show)
Out.println(bearing.radians.inspect)
```

- Arithmetic wraps, as angles do. Two normal forms are available: `principal`, in
- `[0°, 360°)`, and `canonical`, in `[−180°, 180°)`:
+ L'arithmétique reboucle, comme le font les angles. Deux formes normales sont disponibles :
+ `principal`, dans `[0°, 360°)`, et `canonical`, dans `[−180°, 180°)` :

```scala fuel
run
##
Out.println(Angle.degrees(375).principal.show)
Out.println(Angle.degrees(355).canonical.show)
```

- [SVG](svg.md#transforms) rotates figures by these angles, and
- [Colors](colors.md#adjusting) turns hues by them.
+ [SVG](svg.md#transforms) fait tourner des figures selon ces angles, et
+ [Couleurs](colors.md#adjusting) fait pivoter des teintes selon eux.

- ### Compass points
+ ### Points cardinaux

- An angle resolves to the nearest point of a compass rose. The number of points is chosen as
- a type argument:
+ Un angle se résout en le point le plus proche d'une rose des vents. Le nombre de points se
+ choisit comme argument de type :

```scala fuel
run
##
Out.println(Compass[4](Angle.degrees(315)).inspect)
Out.println(Compass[8](Angle.degrees(315)).inspect)
Out.println(Compass[16](Angle.degrees(330)).inspect)
```

- The winds are ordinary enumerations, so a bearing can be matched, stored and shown like any
- other value:
+ Les vents sont des énumérations ordinaires : un relèvement peut donc être filtré, stocké et
+ affiché comme n'importe quelle autre valeur :

```tabular
- column  Rose
- column  Resolves to
- column  Bearing given
- row  `Compass[4]`  `CardinalWind`: `North`, `East`, `South`, `West`  `compassBearings.fourPointCompassBearing`
- row  `Compass[8]`  `CardinalWind` or `IntercardinalWind`, adding `Northeast`, `Southeast`, `Southwest`, `Northwest`  `compassBearings.eightPointCompassBearing`
- row  `Compass[16]`  any of those or a `HalfWind`, adding `NorthNortheast`, `EastNortheast` and the other six  `compassBearings.sixteenPointCompassBearing`
+ column  Rose
+ column  Se résout en
+ column  Given de relèvement
+ row  `Compass[4]`  `CardinalWind` : `North`, `East`, `South`, `West`  `compassBearings.fourPointCompassBearing`
+ row  `Compass[8]`  `CardinalWind` ou `IntercardinalWind`, qui ajoute `Northeast`, `Southeast`, `Southwest`, `Northwest`  `compassBearings.eightPointCompassBearing`
+ row  `Compass[16]`  l'un de ceux-là ou un `HalfWind`, qui ajoute `NorthNortheast`, `EastNortheast` et les six autres  `compassBearings.sixteenPointCompassBearing`
```

- The number of points is a type argument, not a parameter at runtime, so the *type* of a
- resolved bearing says which rose it came from. A `CardinalWind` cannot be passed where a
- `HalfWind` of sixteen points is expected, and a match on a bearing of four points is
- exhaustive over four cases, not sixteen.
+ Le nombre de points est un argument de type, et non un paramètre à l'exécution : le *type* d'un
+ relèvement résolu dit donc de quelle rose il provient. Un `CardinalWind` ne peut pas être passé
+ là où un `HalfWind` à seize points est attendu, et un filtrage sur un relèvement à quatre points
+ est exhaustif sur quatre cas, et non seize.

- A rose with a different number of points is added by implementing `Directional`, which has a
- single method, from an angle to a value. A domain with sectors of its own, such as a
- wind-rose with twelve points, or a radar with sixty, therefore uses the same `Compass`
- machinery.
+ Une rose avec un autre nombre de points s'ajoute en implémentant `Directional`, qui a une seule
+ méthode, d'un angle vers une valeur. Un domaine qui a ses propres secteurs, comme une rose des
+ vents à douze points, ou un radar à soixante, utilise donc la même machinerie `Compass`.

- ### Locations
+ ### Positions

- A `Location` pairs a latitude and a longitude, constructed from angles. It answers
- geographic questions: the initial bearing toward another location, on whichever compass is
- asked for, the angular distance between two points along the great circle, and the geohash
- of the position, to a chosen precision:
+ Une `Location` associe une latitude et une longitude, construites à partir d'angles. Elle répond
+ à des questions géographiques : le relèvement initial vers une autre position, sur la boussole
+ demandée, la distance angulaire entre deux points le long du grand cercle, et le geohash de la
+ position, à une précision choisie :

```scala
import compassBearings.eightPointCompassBearing

val here = Location(51.5.deg, 0.1.deg)
val there = Location(48.9.deg, 2.4.deg)
```

```scala fuel
run
##
Out.println(here.bearing[Compass[8]](there).inspect)
Out.println(here.surfaceDistance(there).show)
Out.println(here.geohash(6).inspect)
```

- The surface distance is itself an `Angle`: the fraction of the great circle which lies
- between the points. It multiplies by the radius of a planet to give a
- [length](quantities.md), which keeps the geometry separate from the choice of sphere. A
- calculation on Earth, and the same calculation on Mars, differ only in the radius by which
- they are multiplied.
+ La distance en surface est elle-même un `Angle` : la fraction du grand cercle qui se trouve entre
+ les points. Elle se multiplie par le rayon d'une planète pour donner une
+ [longueur](quantities.md), ce qui sépare la géométrie du choix de la sphère. Un calcul sur la
+ Terre, et le même calcul sur Mars, ne diffèrent que par le rayon par lequel on les multiplie.

- A `Location` is stored as a packed pair of fixed-point angles, not as two boxed doubles, so
- a collection of positions costs one machine word each. A type which has a position, such as
- a city, a station or a photograph, supplies a `Locatable` instance, and is then accepted
- wherever a location is. The geographic operations therefore apply to values of the domain
- directly, without their coordinates being extracted first.
+ Une `Location` est stockée sous forme d'une paire compacte d'angles à virgule fixe, et non de
+ deux doubles boxés : une collection de positions coûte donc un mot machine chacune. Un type
+ qui a une position, comme une ville, une station ou une photographie, fournit une instance de
+ `Locatable`, et est alors accepté partout où une position l'est. Les opérations géographiques
+ s'appliquent donc directement aux valeurs du domaine, sans qu'il faille d'abord en extraire les
+ coordonnées.

- Geohashes are the usual base-32 encoding, and share the property which matters. Two
- locations which are close together usually share a prefix, so a query by prefix is a coarse
- search by proximity. "Usually" is the honest word. The boundaries of the encoding's cells
- mean that two points on either side of one share nothing, which is why a geohash prefix is a
- first filter, not an answer.
+ Les geohashes sont l'encodage habituel en base 32, et partagent la propriété qui compte. Deux
+ positions proches l'une de l'autre partagent en général un préfixe : une requête par préfixe est
+ donc une recherche grossière par proximité. « En général » est le mot honnête. Les frontières
+ des cellules de l'encodage font que deux points situés de part et d'autre de l'une d'elles ne
+ partagent rien, et c'est pourquoi un préfixe de geohash est un premier filtre, et non une
+ réponse.

- ### Geo URIs
+ ### URI geo

- A location travels in text as a `geo:` URI, of
- [RFC 5870](https://datatracker.ietf.org/doc/html/rfc5870), with an optional altitude and
- uncertainty. A `Geolocation` is decoded from such text, and encoded back. A malformed URI
- raises a typed error which names the fault:
+ Une position circule dans du texte sous forme d'URI `geo:`, selon la
+ [RFC 5870](https://datatracker.ietf.org/doc/html/rfc5870), avec une altitude et une incertitude
+ optionnelles. Une `Geolocation` se décode à partir d'un tel texte, et se réencode. Une URI mal
+ formée lève une erreur typée qui nomme le défaut :

```scala
import strategies.throwUnsafely

val place = t"geo:51.5,0.1".as[Geolocation].location
```

```scala fuel
run
errors show
##
t"geo:north".as[Geolocation]
```

- ### Related
+ ### Voir aussi

- - [Quantities](quantities.md) describes the lengths which a distance is measured in.
- - [Network addresses](network-addresses.md) describes the other kinds of URI.
- - [Base encoding](base-encoding.md) lists the Geohash alphabet among its Base32 alphabets.
+ - [Grandeurs](quantities.md) décrit les longueurs dans lesquelles se mesure une distance.
+ - [Adresses réseau](network-addresses.md) décrit les autres types d'URI.
+ - [Encodage en base](base-encoding.md) range l'alphabet Geohash parmi ses alphabets Base32.
