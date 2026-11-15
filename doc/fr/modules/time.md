- ## Time
+ ## Temps

- ### About
+ ### Présentation

- Time is represented and manipulated in all its forms: dates on a calendar, times on a clock,
- absolute instants on a physical timeline, the spans between them, the time zones which connect
- human time to physical time, and the rules by which events recur.
+ Le temps est représenté et manipulé sous toutes ses formes : les dates d'un calendrier, les
+ heures d'une horloge, les instants absolus sur une ligne de temps physique, les intervalles qui
+ les séparent, les fuseaux horaires qui relient le temps humain au temps physique, et les règles
+ selon lesquelles les événements se répètent.

- Wherever a value can be known when the code is written, it is checked at [compiletime]. A date
- which cannot exist, or a time zone which was never defined, is a compile error, not a surprise
- at [runtime]. That is [safety by construction](../../philosophy/safety-by-construction.md),
- applied to a domain in which almost every value has a rule.
+ Partout où une valeur peut être connue au moment où le code est écrit, elle est vérifiée
+ [à la compilation][compilation]. Une date qui ne peut pas exister, ou un fuseau horaire qui n'a
+ jamais été défini, est une erreur de compilation, et non une surprise
+ [à l'exécution][exécution]. C'est la
+ [sûreté par construction](../../philosophy/safety-by-construction.md), appliquée à un domaine
+ dans lequel presque chaque valeur obéit à une règle.

- ### On time
+ ### Sur le temps

- Time looks simple, and is not.
+ Le temps a l'air simple, et ne l'est pas.

- A physical instant, such as the striking of a bell or the arrival of a photon, can be ordered
- before or after another, and measured against a regular reference. That much is unambiguous,
- *provided that the two happen in the same place*. Events at a distance are only
- [partially ordered](https://en.wikipedia.org/wiki/Special_relativity), related to one another
- through the speed of light, and not by a universal clock.
+ Un instant physique, comme le tintement d'une cloche ou l'arrivée d'un photon, peut être placé
+ avant ou après un autre, et mesuré par rapport à une référence régulière. Cela du moins est
+ sans ambiguïté, *pourvu que les deux se produisent au même endroit*. Des événements distants ne
+ sont que [partiellement ordonnés](https://en.wikipedia.org/wiki/Special_relativity), liés les
+ uns aux autres par la vitesse de la lumière, et non par une horloge universelle.

- Everything which people build on top of it is less certain still. Calendars divide the year
- into months of unequal length, weeks which ignore months entirely, and days which are
- themselves divided in ways which are
- [not decimal](https://en.wikipedia.org/wiki/12-hour_clock), and were
- [negotiated over history](https://en.wikipedia.org/wiki/Thirty_Days_Hath_September). The
- twelves and sixties are
- [highly composite numbers](https://en.wikipedia.org/wiki/Highly_composite_number), chosen
- because they divide well, long before anything needed to be divided by ten.
+ Tout ce que l'on construit par-dessus est encore moins certain. Les calendriers divisent
+ l'année en mois de longueurs inégales, en semaines qui ignorent complètement les mois, et en
+ jours eux-mêmes divisés de façons qui ne sont
+ [pas décimales](https://en.wikipedia.org/wiki/12-hour_clock), et qui ont été
+ [négociées au fil de l'histoire](https://en.wikipedia.org/wiki/Thirty_Days_Hath_September).
+ Les douze et les soixante sont des
+ [nombres hautement composés](https://en.wikipedia.org/wiki/Highly_composite_number), choisis
+ parce qu'ils se divisent bien, bien avant qu'il faille diviser quoi que ce soit par dix.

- The relationship between physical time and civil time is worse again:
+ La relation entre temps physique et temps civil est pire encore :

- - The length of a day drifts as the Earth's rotation changes, so civil time is corrected with
-   [leap seconds](https://en.wikipedia.org/wiki/Leap_second), which cannot be predicted years
-   ahead.
- - A single reading of a wall clock can name two distinct instants, or none at all, when
-   [daylight-saving time](https://en.wikipedia.org/wiki/Daylight_saving_time) shifts the
-   clocks.
- - The same moment has different labels in different places, and which labels apply has
-   depended, at times, on the outcome of wars.
+ - La durée d'un jour dérive à mesure que la rotation de la Terre change, si bien que le temps
+   civil est corrigé par des
+   [secondes intercalaires](https://en.wikipedia.org/wiki/Leap_second), qu'on ne peut pas
+   prévoir des années à l'avance.
+ - Une même lecture d'une horloge murale peut désigner deux instants distincts, ou aucun,
+   lorsque l'[heure d'été](https://en.wikipedia.org/wiki/Daylight_saving_time) décale les
+   horloges.
+ - Le même moment porte des étiquettes différentes en des lieux différents, et les étiquettes
+   qui s'appliquent ont parfois dépendu de l'issue de guerres.

- As much of this as can be made systematic is made systematic, by encoding the distinctions in
- types. A date knows which calendar it belongs to. An instant knows which timeline it is
- measured on. A moment knows its zone. And the awkward cases are resolved by policies which are
- chosen in context, and are not left to chance: a 31st which some months lack, a 29th of
- February which most years lack, and a clock reading which falls in a daylight-saving gap.
+ Tout ce qui peut en être rendu systématique l'est, en encodant les distinctions dans les types.
+ Une date sait à quel calendrier elle appartient. Un instant sait sur quelle ligne de temps il est
+ mesuré. Un moment connaît son fuseau. Et les cas délicats sont résolus par des politiques
+ choisies selon le contexte, et non laissés au hasard : un 31 que certains mois n'ont pas, un
+ 29 février que la plupart des années n'ont pas, et une lecture d'horloge qui tombe dans le
+ trou d'un passage à l'heure d'été.

- The sections below begin with the everyday types, and build toward the rare ones:
+ Les sections ci-dessous commencent par les types de tous les jours, puis progressent vers les
+ types rares :

```tabular
- column  Type
- column  Represents
- row  `Date`  a day on a calendar
- row  `Clockface`  a time of day
- row  `Timestamp`  a date with a time of day
- row  `Month`  a month of the year, `Jan` to `Dec`
- row  `Year`  a calendar year
- row  `Monthstamp`  a month of a particular year
- row  `Anniversary`  a month and a day, recurring every year
- row  `Duration`  a physical quantity of seconds
- row  `Timespan`  a calendar quantity: years, months, weeks, days, hours, minutes
- row  `Period`  a half-open interval between two points
- row  `Instant`  a moment in physical time, on a timeline
- row  `Timezone`  a place's rules for civil time
- row  `Moment`  a timestamp anchored to a zone
- row  `Recurrence`  an event repeating at a fixed interval
- row  `Rrule`  an event repeating by a calendar pattern
+ column  Type
+ column  Représente
+ row  `Date`  un jour d'un calendrier
+ row  `Clockface`  une heure de la journée
+ row  `Timestamp`  une date avec une heure de la journée
+ row  `Month`  un mois de l'année, de `Jan` à `Dec`
+ row  `Year`  une année civile
+ row  `Monthstamp`  un mois d'une année donnée
+ row  `Anniversary`  un mois et un jour, qui reviennent chaque année
+ row  `Duration`  une quantité physique de secondes
+ row  `Timespan`  une quantité calendaire : années, mois, semaines, jours, heures, minutes
+ row  `Period`  un intervalle semi-ouvert entre deux points
+ row  `Instant`  un moment du temps physique, sur une ligne de temps
+ row  `Timezone`  les règles d'un lieu pour le temps civil
+ row  `Moment`  un horodatage ancré dans un fuseau
+ row  `Recurrence`  un événement qui se répète à intervalle fixe
+ row  `Rrule`  un événement qui se répète selon un motif calendaire
```

- Everything comes from the `soundness` package:
+ Tout provient du package `soundness` :

```scala
import soundness.*
```

- Several further choices are made by import:
+ Plusieurs autres choix se font par import :

```scala
import strategies.throwUnsafely
import chronometries.unixChronometry
import soundness.{tsInterpolator, recInterpolator}
import calendars.gregorianCalendar
import dateFormats.iso8601DateFormat
import timeFormats.iso8601TimeFormat
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

```tabular
- column  Import
- column  Decides
- row  `strategies.throwUnsafely`  that an error, such as text which is not a date, is thrown as an exception; [Errors](errors.md#choosing-a-strategy) has the other strategies
- row  `chronometries.unixChronometry`  that the current time is read from the Unix clock
- row  `calendars.gregorianCalendar`  the calendar which a date literal, and date arithmetic, are read in; [Calendars](#calendars) has the others
- row  `dateFormats`, `timeFormats`  how a date and a time are written out; [Formatting dates and times](#formatting-dates-and-times) has the others
- row  `stdios`, `termcapDefinitions`  where the samples print their results
+ column  Import
+ column  Décide
+ row  `strategies.throwUnsafely`  qu'une erreur, comme un texte qui n'est pas une date, est levée comme exception ; [Erreurs](errors.md#choosing-a-strategy) présente les autres stratégies
+ row  `chronometries.unixChronometry`  que l'heure courante est lue sur l'horloge Unix
+ row  `calendars.gregorianCalendar`  le calendrier dans lequel sont lus un littéral de date et l'arithmétique sur les dates ; [Calendriers](#calendars) présente les autres
+ row  `dateFormats`, `timeFormats`  comment une date et une heure sont écrites ; [Formater les dates et les heures](#formatting-dates-and-times) présente les autres
+ row  `stdios`, `termcapDefinitions`  où les exemples affichent leurs résultats
```

- ### Dates
+ ### Dates

- A date is written as a year, an abbreviated month and a day, joined by hyphens:
+ Une date s'écrit sous la forme d'une année, d'un mois abrégé et d'un jour, reliés par des
+ tirets :

```scala
val date = 2024-Jan-15
```

- This is not a string to be parsed at runtime. It is a literal which the compiler understands,
- and an impossible date does not compile. The thirtieth of February, the thirty-first of April,
- and the twenty-ninth of February in a year which is not a leap year, are all rejected before
- the program runs:
+ Ce n'est pas une chaîne à analyser à l'exécution. C'est un littéral que le compilateur
+ comprend, et une date impossible ne compile pas. Le trente février, le trente et un avril, et
+ le vingt-neuf février d'une année non bissextile sont tous rejetés avant que le programme ne
+ s'exécute :

```scala fuel
compile
errors show
##
2012-Feb-30
```

```scala fuel
compile
errors show
##
2012-Apr-31
```

```scala fuel
compile
errors show
##
2011-Feb-29
```

- Catching these mistakes at compiletime, instead of trusting a date to be valid because it
- parsed, is the recurring habit which shapes the whole design.
+ Intercepter ces erreurs à la compilation, au lieu de tenir une date pour valide parce qu'elle
+ a été analysée, est l'habitude récurrente qui façonne toute la conception.

- A date is read from text with [`as`](decoding.md#decoding-a-value). The `ts` [interpolator]
- does the same for a literal which is known at compiletime, and validates its digits just as
- the hyphenated form does:
+ Une date se lit à partir d'un texte avec [`as`](decoding.md#decoding-a-value). L'[interpolateur]
+ `ts` fait de même pour un littéral connu à la compilation, et valide ses chiffres exactement
+ comme le fait la forme à tirets :

```scala
val parsed = t"2011-12-13".as[Date]
val literal = ts"2024-12-31"
```

```scala fuel
run
##
Out.println(date.show)
Out.println(parsed.show)
```

- Text which is not a date raises an error:
+ Un texte qui n'est pas une date lève une erreur :

```scala fuel
run
errors show
##
t"2011-02-30".as[Date]
```

- The fields of a date are read back as a year, a month and a day. A date also knows its day of
- the week, and its position in the year. Dates compare, and shift by whole days:
+ Les champs d'une date se relisent sous forme d'année, de mois et de jour. Une date connaît
+ aussi son jour de la semaine, et sa position dans l'année. Les dates se comparent, et se
+ décalent de jours entiers :

```scala fuel
run
##
Out.println((2016-Jul-11).year.inspect)
Out.println((2016-Jul-11).month.inspect)
Out.println((2024-Jan-1).weekday.inspect)
Out.println((2024-Dec-31).yearDay.show)
Out.println(((2024-Jan-1) < (2024-Jan-2)).inspect)
Out.println((2024-Jan-1).addDays(31).show)
```

- A `Date` is a `Timestamp` with a precision of one day. The two are distinguished by a type
- parameter, not by a separate class, so a date may be used wherever a timestamp is expected.
+ Une `Date` est un `Timestamp` d'une précision d'un jour. Les deux se distinguent par un
+ paramètre de type, et non par une classe distincte, si bien qu'une date peut être utilisée
+ partout où un horodatage est attendu.

- ### Times of day
+ ### Heures de la journée

- A time of day is a `Clockface`, given as hours, minutes and seconds:
+ Une heure de la journée est une `Clockface`, donnée en heures, minutes et secondes :

```scala
val time = Clockface(14, 30, 59)
```

- For the common case of an exact minute, a time can be written as a number, with `am` or `pm`
- attached. The minutes always take two digits:
+ Pour le cas courant d'une minute exacte, une heure peut s'écrire comme un nombre, suivi de `am`
+ ou `pm`. Les minutes prennent toujours deux chiffres :

```scala fuel
run
##
Out.println(time.show)
Out.println(2.01.am.show)
Out.println(7.25.pm.show)
Out.println(12.00.pm.show)
Out.println(12.00.am.show)
```

- These literals are checked too. An hour above twelve does not compile:
+ Ces littéraux sont vérifiés eux aussi. Une heure supérieure à douze ne compile pas :

```scala fuel
compile
errors show
##
13.00.am
```

- ### Timestamps
+ ### Horodatages

- A `Timestamp` pairs a date with a time of day. The `on` operator joins the two, and reads
- naturally, from clock to calendar:
+ Un `Timestamp` associe une date à une heure de la journée. L'opérateur `on` relie les deux, et
+ se lit naturellement, de l'horloge au calendrier :

```scala
val appointment = 5.25.pm on 2018-Aug-11
```

- A timestamp can also be built directly, parsed from text, or written as a `ts` literal:
+ Un horodatage peut aussi être construit directement, analysé à partir d'un texte, ou écrit
+ comme littéral `ts` :

```scala
val built = Timestamp(2023-May-28, Clockface(14, 30, 59))
val decoded = t"2024-01-15T14:30:59".as[Timestamp]
val written = ts"2023-05-28T14:30:59"
```

```scala fuel
run
##
Out.println(appointment.inspect)
Out.println(decoded.inspect)
```

- Its fields are read back individually:
+ Ses champs se relisent individuellement :

```scala fuel
run
##
Out.println(built.month.inspect)
Out.println(built.hour.show)
Out.println(built.minute.show)
Out.println(built.second.show)
```

- Subtracting one timestamp from another gives the span between them, broken down into days,
- hours, minutes and seconds:
+ Soustraire un horodatage d'un autre donne l'intervalle qui les sépare, décomposé en jours,
+ heures, minutes et secondes :

```scala
val a = Timestamp(2024-Jan-10, Clockface(8, 0, 0))
val b = Timestamp(2024-Jan-12, Clockface(11, 30, 15))
val elapsed = b - a
```

```scala fuel
run
##
Out.println(elapsed.days.show)
Out.println(elapsed.hours.show)
Out.println(elapsed.minutes.show)
```

- ### Months, years and anniversaries
+ ### Mois, années et anniversaires

- The months are an enumeration, from `Jan` to `Dec`. A month converts to and from its text and
- its number, and the whole set is available as a list:
+ Les mois forment une énumération, de `Jan` à `Dec`. Un mois se convertit vers et depuis son
+ texte et son numéro, et l'ensemble complet est disponible sous forme de liste :

```scala fuel
run
##
Out.println(Month(t"Jan").inspect)
Out.println(Month(7).inspect)
Out.println(Dec.numerical.show)
```

- A `Year` wraps a calendar year. Years add and subtract whole numbers, compare with each other,
- and are parsed from text:
+ Une `Year` enveloppe une année civile. Les années s'additionnent et se soustraient avec des
+ nombres entiers, se comparent entre elles, et s'analysent à partir d'un texte :

```scala fuel
run
##
Out.println((Year(2024) + 5).inspect)
Out.println((Year(1999) < Year(2000)).inspect)
Out.println(t"2024".as[Year].inspect)
```

- A year and a month together form a `Monthstamp`, written with `-`. Subtracting a day from one
- gives a date. Adding months and years moves it through the calendar, and rolls across the
- boundaries of years as needed:
+ Une année et un mois forment ensemble un `Monthstamp`, écrit avec `-`. Lui soustraire un jour
+ donne une date. Ajouter des mois et des années le déplace dans le calendrier, en franchissant
+ au besoin les limites des années :

```scala
val month = 2024 - Jan
```

```scala fuel
run
##
Out.println(((2024 - Feb) - 29).show)
Out.println(((2024 - Nov) + 3*Month).inspect)
Out.println(((2024 - Jun) + 1*Year).inspect)
```

- A month and a day, with no year, form an `Anniversary`: a birthday, or a holiday, which recurs
- every year. Applying it to a year gives that year's date:
+ Un mois et un jour, sans année, forment un `Anniversary` : un anniversaire, ou un jour férié,
+ qui revient chaque année. L'appliquer à une année donne la date de cette année-là :

```scala
val birthday = Mar - 14
```

- The twenty-ninth of February has no counterpart in most years, so applying such an anniversary
- needs a policy for the missing day. The policy is a [given], selected by what is in scope, and
- is not passed at each call. Rounding down lands on the twenty-eighth, and rounding up lands on
- the first of March:
+ Le vingt-neuf février n'a pas d'équivalent la plupart des années, si bien qu'appliquer un tel
+ anniversaire nécessite une politique pour le jour manquant. La politique est un [given],
+ sélectionné par ce qui est dans la portée, et n'est pas passée à chaque appel. Arrondir vers le
+ bas tombe sur le vingt-huit, et arrondir vers le haut tombe sur le premier mars :

```scala fuel
run
##
locally:
  given leapDay: Anniversary.NonexistentLeapDay = nonexistentLeapDays.roundDownLeapDay

  Out.println((Feb - 29)(Year(2023)).show)
  Out.println((Feb - 29)(Year(2024)).show)
```

- A leap year is unaffected.
+ Une année bissextile n'est pas concernée.

- ### Durations and timespans
+ ### Durées et durées calendaires

- A firm line is drawn between two kinds of elapsed time, and the distinction is deliberate.
+ Une ligne ferme sépare deux sortes de temps écoulé, et la distinction est délibérée.

- A `Duration` is a physical quantity of seconds, the same everywhere and always. A `Timespan`
- is a quantity on the calendar, of years, months, weeks, days, hours and minutes. Its meaning
- depends on where it is anchored, because a month is not a fixed number of seconds, and a
- calendar day is not always twenty-four hours. Keeping the two apart in the types stops a
- number of months from ever being mistaken for a number of seconds:
+ Une `Duration` est une quantité physique de secondes, la même partout et toujours. Un
+ `Timespan` est une quantité sur le calendrier, en années, mois, semaines, jours, heures et
+ minutes. Son sens dépend de l'endroit où il est ancré, car un mois n'est pas un nombre fixe de
+ secondes, et un jour calendaire ne dure pas toujours vingt-quatre heures. Séparer les deux dans
+ les types empêche qu'un nombre de mois soit jamais pris pour un nombre de secondes :

```tabular
- column  Aspect
- column  `Duration`
- column  `Timespan`
- row  Measures  seconds  years, months, weeks, days, hours and minutes
- row  Means  the same everywhere and always  what its anchor makes of it
- row  Written  `Duration(1000L)`, from milliseconds  `2*Year + 3*Month`
- row  As text  a compact form such as `1h 0m 5s`, with `compactDurationFormat`  an ISO 8601 duration such as `P1Y2M3DT4H5M6S`
+ column  Aspect
+ column  `Duration`
+ column  `Timespan`
+ row  Mesure  des secondes  des années, mois, semaines, jours, heures et minutes
+ row  Signifie  la même chose partout et toujours  ce qu'en fait son ancrage
+ row  S'écrit  `Duration(1000L)`, à partir de millisecondes  `2*Year + 3*Month`
+ row  En texte  une forme compacte telle que `1h 0m 5s`, avec `compactDurationFormat`  une durée ISO 8601 telle que `P1Y2M3DT4H5M6S`
```

- A duration is a [quantity](quantities.md) of seconds. One which is constructed from
- milliseconds reports its length in seconds:
+ Une durée est une [quantité](quantities.md) de secondes. Une durée construite à partir de
+ millisecondes indique sa longueur en secondes :

```scala fuel
run
##
Out.println(Duration(1000L).value.inspect)
```

- A timespan is built by multiplying a number by a unit, and several units combine into one
- span. Its components are read back by name:
+ Une durée calendaire se construit en multipliant un nombre par une unité, et plusieurs
+ unités se combinent en une seule durée. Ses composantes se relisent par leur nom :

```scala
val span = 2*Year + 3*Month + 14*Day
```

```scala fuel
run
##
Out.println(span.years.show)
Out.println(span.months.show)
Out.println(span.days.show)
```

- A timespan is encoded as an
- [ISO 8601 duration](https://en.wikipedia.org/wiki/ISO_8601#Durations), and is parsed back
- from one. The `dur` interpolator writes such a duration as a checked literal:
+ Une durée calendaire est encodée comme une
+ [durée ISO 8601](https://en.wikipedia.org/wiki/ISO_8601#Durations), et se relit à partir d'une
+ telle durée. L'interpolateur `dur` écrit une telle durée comme un littéral vérifié :

```scala
val precise = Timespan(years = 1, months = 2, days = 3, hours = 4, minutes = 5,
    seconds = Quantity(6.0))

val fromText = t"P1Y2M3DT4H5M6S".as[Timespan]
val fromLiteral = dur"P1Y2M3DT4H5M6S"
```

```scala fuel
run
##
Out.println(precise.encode)
```

- #### Compact durations
+ #### Durées compactes

- A tool reads a duration from a [flag](cli.md), and shows one in a report, and both have a
- compact form. With `durationDecodables.compactDurationDecodable` in scope, a number followed by
- a unit is decoded to a `Duration`:
+ Un outil lit une durée à partir d'une [option](cli.md), et en affiche une dans un rapport, et
+ les deux ont une forme compacte. Avec `durationDecodables.compactDurationDecodable` dans la
+ portée, un nombre suivi d'une unité est décodé en une `Duration` :

```tabular
- column  Suffix
- column  Unit
- column  Example
- row  `ms`  milliseconds  `250ms`
- row  `s`  seconds  `30s`
- row  `m`  minutes  `30m`
- row  `h`  hours  `1.5h`
- row  `d`  days  `2d`
- row  none  seconds  `90`
+ column  Suffixe
+ column  Unité
+ column  Exemple
+ row  `ms`  millisecondes  `250ms`
+ row  `s`  secondes  `30s`
+ row  `m`  minutes  `30m`
+ row  `h`  heures  `1.5h`
+ row  `d`  jours  `2d`
+ row  aucun  secondes  `90`
```

- Anything else, such as a negative number or a word, raises a `Duration.Error`. With
- `durationFormats.compactDurationFormat`, a duration is shown as its coarsest unit, followed by
- every unit down to seconds, so that a column of them lines up. Below a second, it is shown as
- milliseconds alone:
+ Tout le reste, comme un nombre négatif ou un mot, lève une `Duration.Error`. Avec
+ `durationFormats.compactDurationFormat`, une durée s'affiche dans son unité la plus grossière,
+ suivie de chaque unité jusqu'aux secondes, de sorte qu'une colonne de durées s'aligne. En
+ dessous d'une seconde, elle s'affiche en millisecondes seules :

```scala
import durationDecodables.compactDurationDecodable
import durationFormats.compactDurationFormat
```

```scala fuel
run
##
Out.println(t"1.5h".as[Duration].value.inspect)
Out.println(Duration(3605000L).show)
Out.println(Duration(420L).show)
```

```scala fuel
run
errors show
##
t"soon".as[Duration]
```

- ### Arithmetic on dates
+ ### Arithmétique sur les dates

- Adding a span to a date moves it forward. Days and weeks are regular. They need no policy,
- because a week is always seven days:
+ Ajouter un intervalle à une date la fait avancer. Les jours et les semaines sont réguliers. Ils
+ n'ont besoin d'aucune politique, car une semaine compte toujours sept jours :

```scala fuel
run
##
Out.println((2024-Jan-1 + 3*Day).show)
Out.println((2024-Jan-1 + 2*Week).show)
```

- Months and years are irregular. Adding a month to the thirty-first of January asks for a
- thirty-first of February, which does not exist, so the result depends on a policy. No policy
- is picked silently. Without one in scope, the addition does not compile:
+ Les mois et les années sont irréguliers. Ajouter un mois au trente et un janvier demande un
+ trente et un février, qui n'existe pas, si bien que le résultat dépend d'une politique. Aucune
+ politique n'est choisie en silence. Sans politique dans la portée, l'addition ne compile pas :

```scala fuel
compile
errors show
##
2024-Jan-31 + 1*Month
```

- The policy is a given, which the calling code must supply. *Clamping* lands on the last valid
- day, and *overflowing* spills into the next month:
+ La politique est un given, que le code appelant doit fournir. *Borner* tombe sur le dernier
+ jour valide, et *déborder* déverse le surplus dans le mois suivant :

```scala fuel
run
##
locally:
  import monthEnds.clampMonthEnd

  Out.println((2024-Jan-31 + 1*Month).show)
  Out.println((2024-Feb-29 + 1*Year).show)

locally:
  import monthEnds.overflowMonthEnd

  Out.println((2024-Jan-31 + 1*Month).show)
  Out.println((2024-Feb-29 + 1*Year).show)
```

- Subtracting one date from another counts the days between them:
+ Soustraire une date d'une autre compte les jours qui les séparent :

```scala
val gap = 2018-Nov-19 - (2017-Sep-1)
```

- The days of the week can be counted across a range, which answers questions such as how many
- Saturdays fall in a month:
+ Les jours de la semaine peuvent se compter sur un intervalle, ce qui répond à des questions
+ comme le nombre de samedis que compte un mois :

```scala fuel
run
##
Out.println(Weekday.count(2025-Mar-1, 2025-Apr-1, Sat).show)
```

- Arithmetic in working days skips weekends and holidays. It needs to know which days form the
- week, which is a `Hebdomad`, and which dates are holidays. Both are supplied from context:
+ L'arithmétique en jours ouvrés saute les week-ends et les jours fériés. Elle doit savoir quels
+ jours forment la semaine, ce qu'est un `Hebdomad`, et quelles dates sont fériées. Les deux sont
+ fournis par le contexte :

```scala fuel
run
##
locally:
  import hebdomads.europeanHebdomad

  given holidays: Holidays = Holidays(List
    ( Holiday(2025-Dec-25, t"Christmas Day"),
      Holiday(2025-Dec-26, t"Boxing Day") ))

  Out.println((2025-Aug-11 + WorkingDays(5)).show)
  Out.println((2025-Dec-24 + WorkingDays(1)).show)
```

- The first skips a weekend, and the second skips the two holidays and the weekend which
- follows them.
+ Le premier saute un week-end, et le second saute les deux jours fériés et le week-end qui les
+ suit.

- ### Instants
+ ### Instants

- An instant is a moment in physical time, with no reference to any calendar or clock. An
- `Instant` is measured along a timeline, and the timeline is part of its type. The `Unix`
- timeline, which is POSIX time, counts milliseconds from the
- [Unix epoch](https://en.wikipedia.org/wiki/Unix_time), the start of 1970:
+ Un instant est un moment du temps physique, sans référence à aucun calendrier ni aucune
+ horloge. Un `Instant` est mesuré le long d'une ligne de temps, et la ligne de temps fait partie de
+ son type. La ligne de temps `Unix`, qui est le temps POSIX, compte les millisecondes depuis
+ l'[époque Unix](https://en.wikipedia.org/wiki/Unix_time), le début de 1970 :

```scala
val epoch = Instant(0L)
```

- The current instant comes from a clock in scope:
+ L'instant courant provient d'une horloge dans la portée :

```tabular
- column  Call
- column  Reads
- column  Needs
- row  `now()`  the wall clock, as an `Instant over Unix`  a `Clock`
- row  `today()`  the current date  a `Clock`, a calendar and a `Timezone`
- row  `monotonic()`  a high-resolution elapsed-time counter, as an `Instant over Monotonic`  a `MonotonicClock`
+ column  Appel
+ column  Lit
+ column  Nécessite
+ row  `now()`  l'horloge murale, sous forme d'`Instant over Unix`  une `Clock`
+ row  `today()`  la date du jour  une `Clock`, un calendrier et un `Timezone`
+ row  `monotonic()`  un compteur de temps écoulé à haute résolution, sous forme d'`Instant over Monotonic`  une `MonotonicClock`
```

- The clock is itself a given. Supplying a fixed one makes time deterministic, which is what
- [tests](testing.md) want:
+ L'horloge est elle-même un given. En fournir une fixe rend le temps déterministe, ce que
+ veulent les [tests](testing.md) :

```scala fuel
run
##
locally:
  given clock: Clock = Clock.fixed(Instant(99999L))

  Out.println(now().inspect)
```

- Instants move by physical quantities, and one subtracted from another gives a duration, in
- seconds:
+ Les instants se déplacent de quantités physiques, et la soustraction de deux instants donne une
+ durée, en secondes :

```scala fuel
run
##
Out.println((Instant(0L) + 1*Hour).inspect)
Out.println((Instant(3600000L) - Instant(0L)).inspect)
```

- ### Periods
+ ### Périodes

- Two points of the same kind bound a `Period`, written with `~`. A period is a half-open
- interval: its start is included, and its finish is not. Consecutive periods therefore tile a
- timeline, without overlapping where they meet.
+ Deux points de même sorte délimitent une `Period`, écrite avec `~`. Une période est un
+ intervalle semi-ouvert : son début est inclus, et sa fin ne l'est pas. Des périodes
+ consécutives pavent donc une ligne de temps, sans se chevaucher là où elles se rejoignent.

- A period reports its duration, and tests whether a point lies within it:
+ Une période indique sa durée, et teste si un point s'y trouve :

```scala
val period = Instant(0L) ~ Instant(3_600_000L)
```

```scala fuel
run
##
Out.println(period.duration.value.inspect)
Out.println(period.has(Instant(0L)).inspect)
Out.println(period.has(Instant(3_600_000L)).inspect)
```

- Periods also intersect and unite.
+ Les périodes se croisent et s'unissent également.

- `by` walks a period at a step, and gives each point. `segments` divides it into sub-periods
- instead of points, which is what a view of a calendar needs, or a report divided into
- buckets. A step which does not divide the period evenly drops the remainder by default, or
- keeps it as a short final segment on request:
+ `by` parcourt une période selon un pas, et donne chaque point. `segments` la divise plutôt en
+ sous-périodes qu'en points, ce dont a besoin une vue de calendrier, ou un rapport découpé en
+ tranches. Un pas qui ne divise pas exactement la période abandonne le reste par défaut, ou le
+ garde, sur demande, comme un segment final plus court :

```scala
import dysasymptotics.linearSize
```

```scala fuel
run
##
Out.println(period.by(15*Minute).to[List].size.show)
Out.println(period.segments(15*Minute).size.show)
Out.println(period.segments(25*Minute, partial = true).size.show)
Out.println(Period(2024-Jan-1, 2024-Jan-5).by(1*Day).map(_.show).to[List].join(t", "))
```

- The sampling is lazy, so a range which spans years costs nothing until its values are
- demanded:
+ L'échantillonnage est paresseux, si bien qu'un intervalle qui s'étend sur des années ne coûte
+ rien tant que ses valeurs ne sont pas demandées :

```scala fuel
run
##
Out.println(Period(2024-Jan-1, 2030-Jan-1).by(1*Day).keep(3).map(_.show).to[List].join(t", "))
```

- A period is generic in the type of its points. The same operations serve instants, dates and
- timestamps, with the step given in whatever units that kind of point moves by.
+ Une période est générique dans le type de ses points. Les mêmes opérations servent pour les
+ instants, les dates et les horodatages, le pas étant donné dans les unités dont se déplace
+ cette sorte de point.

- ### Time zones and moments
+ ### Fuseaux horaires et moments

- A time zone names a place's rules for civil time. The `tz` interpolator checks, at
- compiletime, that the name exists in the
- [time-zone database](https://en.wikipedia.org/wiki/Tz_database). A misspelled zone therefore
- never reaches a running program:
+ Un fuseau horaire désigne les règles d'un lieu pour le temps civil. L'interpolateur `tz`
+ vérifie, à la compilation, que le nom existe dans la
+ [base de données des fuseaux horaires](https://en.wikipedia.org/wiki/Tz_database). Un fuseau
+ mal orthographié n'atteint donc jamais un programme en cours d'exécution :

```scala
val london = tz"Europe/London"
```

```scala fuel
compile
errors show
##
tz"NotARealZone"
```

- A `Moment` is a timestamp anchored to a zone: a reading of a wall clock, somewhere in
- particular. `instant` resolves it to physical time, and `in` places any instant into a zone:
+ Un `Moment` est un horodatage ancré dans un fuseau : la lecture d'une horloge murale, quelque
+ part en particulier. `instant` le ramène au temps physique, et `in` place n'importe quel
+ instant dans un fuseau :

```scala
val moment = Moment(2024-Jan-1, Clockface(1, 0, 0), tz"UTC")
```

```scala fuel
run
##
Out.println(moment.instant.inspect)
Out.println(Instant(0L).in(tz"UTC").date.show)
```

- A zone's offset changes with the season. The same instant reads as one hour later in London
- in summer than in winter, because British Summer Time is in force:
+ Le décalage d'un fuseau change avec la saison. Le même instant se lit une heure plus tard à
+ Londres en été qu'en hiver, car l'heure d'été britannique est en vigueur :

```scala
import instantDecodables.iso8601InstantDecodable

val summer = t"2024-07-15T12:00:00Z".as[Instant over Unix]
```

```scala fuel
run
##
Out.println(summer.in(london).time.show)
Out.println(summer.in(tz"UTC").time.show)
```

- #### Daylight-saving transitions
+ #### Passages à l'heure d'été

- Daylight-saving transitions make some readings of a wall clock ambiguous, and others
- impossible. Neither case is papered over:
+ Les passages à l'heure d'été et à l'heure d'hiver rendent certaines lectures d'une horloge
+ murale ambiguës, et d'autres impossibles. Aucun de ces cas n'est passé sous silence :

```tabular
- column  Transition
- column  A reading in it
- column  By default
- column  Otherwise
- row  clocks spring forward  falls in a *gap*, and never happens  is pushed forward  `gapPolicies.pushBackwardGapPolicy` pushes it backward; `gapPolicies.rejectGapPolicy` raises a `Moment.Error`
- row  clocks fall back  falls in an *overlap*, and names two instants  is the first occurrence  `Occurrence.Second` names the later one
+ column  Passage
+ column  Une lecture qui y tombe
+ column  Par défaut
+ column  Sinon
+ row  les horloges avancent  tombe dans un *trou*, et n'a jamais lieu  est repoussée en avant  `gapPolicies.pushBackwardGapPolicy` la repousse en arrière ; `gapPolicies.rejectGapPolicy` lève une `Moment.Error`
+ row  les horloges reculent  tombe dans un *chevauchement*, et désigne deux instants  est la première occurrence  `Occurrence.Second` désigne la plus tardive
```

- A moment in the gap is pushed forward by default. On 31 March 2024, London's clocks went from
- 01:00 straight to 02:00, so 01:30 never happened:
+ Un moment situé dans le trou est repoussé en avant par défaut. Le 31 mars 2024, les horloges
+ de Londres sont passées directement de 01:00 à 02:00, si bien que 01:30 n'a jamais eu lieu :

```scala
val spring = Moment(2024-Mar-31, Clockface(1, 30, 0), london)
```

```scala fuel
run
##
Out.println(spring.instant.in(london).time.show)
```

- With the rejecting policy, the same reading is an error:
+ Avec la politique de rejet, la même lecture est une erreur :

```scala fuel
run
errors show
##
locally:
  import gapPolicies.rejectGapPolicy

  Moment(2024-Mar-31, Clockface(1, 30, 0), london).instant
```

- When the clocks go back, an hour repeats. The two instants which a reading in the overlap
- names are an hour apart:
+ Quand les horloges reculent, une heure se répète. Les deux instants que désigne une lecture
+ dans le chevauchement sont séparés d'une heure :

```scala
val clock = Clockface(1, 30, 0)
val earlier = Moment(2024-Oct-27, clock, london)
val later = Moment(2024-Oct-27, clock, london, Occurrence.Second)
```

```scala fuel
run
##
Out.println((later.instant.long - earlier.instant.long).show)
```

- ### The zone database
+ ### La base de données des fuseaux

- The rules behind all of this come from the [tz database](https://www.iana.org/time-zones). Its
- source files are parsed directly, and are not reached through a platform's API. The parser
- understands the format's own vocabulary: `Rule` lines, with day specifications such as
- `Sun>=8`, `Zone` lines, with their offsets and formats, and `Link` aliases. A database which
- is newer than the platform's own can therefore be loaded, which matters when a government
- changes its rules at short notice:
+ Les règles derrière tout cela proviennent de la [base tz](https://www.iana.org/time-zones). Ses
+ fichiers source sont analysés directement, et non atteints par l'API d'une plateforme.
+ L'analyseur comprend le vocabulaire propre au format : les lignes `Rule`, avec des
+ spécifications de jour telles que `Sun>=8`, les lignes `Zone`, avec leurs décalages et leurs
+ formats, et les alias `Link`. On peut donc charger une base plus récente que celle de la
+ plateforme, ce qui compte lorsqu'un gouvernement change ses règles à bref délai :

```scala
val lines = Chain(t"Zone\tAmerica/Nowhere\t-5:00\t-\tEST")
val zones = Tzdb.parse(t"northamerica", lines)
```

- A file which does not exist, or a line which the format does not permit, raises a
- `Tzdb.Error` which names the fault. It never silently produces a zone with the wrong rules.
+ Un fichier qui n'existe pas, ou une ligne que le format ne permet pas, lève une `Tzdb.Error`
+ qui nomme le défaut. Cela ne produit jamais en silence un fuseau aux règles erronées.

- ### Calendars
+ ### Calendriers

- The dates so far have been Gregorian, which is the modern civil default. Many other calendars
- are supported, and a date literal is interpreted by whichever calendar is in scope. The same
- written date can name a different day under a different calendar, which shows in its
- [Julian day number](https://en.wikipedia.org/wiki/Julian_day):
+ Les dates vues jusqu'ici étaient grégoriennes, ce qui est le choix civil moderne par défaut.
+ Beaucoup d'autres calendriers sont pris en charge, et un littéral de date est interprété par le
+ calendrier qui se trouve dans la portée. La même date écrite peut désigner un jour différent
+ dans un autre calendrier, ce que montre son
+ [jour julien](https://en.wikipedia.org/wiki/Julian_day) :

```scala
val gregorian = { import calendars.gregorianCalendar; (1900-Mar-10).jdn }
val julian = { import calendars.julianCalendar; (1900-Mar-10).jdn }
```

```scala fuel
run
##
Out.println((gregorian == julian).inspect)
```

- A calendar reads the year, month and day of a date in its own terms. Converting a Gregorian
- date into the [Coptic](https://en.wikipedia.org/wiki/Coptic_calendar) calendar reveals an
- entirely different year and month:
+ Un calendrier lit l'année, le mois et le jour d'une date dans ses propres termes. Convertir une
+ date grégorienne dans le calendrier [copte](https://en.wikipedia.org/wiki/Coptic_calendar) fait
+ apparaître une année et un mois entièrement différents :

```scala
val millennium = 2000-Jan-1
val coptic = calendars.copticCalendar
```

```scala fuel
run
##
Out.println(coptic.annual(millennium)().show)
Out.println(coptic.mensual(millennium).inspect)
Out.println(coptic.diurnal(millennium)().show)
```

- The other calendars follow the same shape. Each is a given in the package `calendars`:
+ Les autres calendriers suivent la même forme. Chacun est un given du package `calendars` :

```tabular
- column  Calendar
- column  Given
- column  Months
- row  Gregorian  `gregorianCalendar`  `Jan` to `Dec`
- row  Julian  `julianCalendar`  `Jan` to `Dec`
- row  Coptic  `copticCalendar`  `CopticMonth`
- row  Ethiopian  `ethiopianCalendar`  `EthiopianMonth`
- row  Islamic  `islamicCalendar`  `IslamicMonth`
- row  Persian  `persianCalendar`  `PersianMonth`
- row  Hebrew  `hebrewCalendar`  `HebrewMonth`
- row  Indian national  `indianCalendar`  `IndianMonth`
- row  French Republican  `frenchRepublicanCalendar`  `FrenchRepublicanMonth`
- row  Buddhist  `buddhistCalendar`  Gregorian, with years offset by 543
- row  Minguo  `minguoCalendar`  Gregorian, with years offset by -1911
- row  Ordinal  `ordinalCalendar`  none: a year and a day of the year
+ column  Calendrier
+ column  Given
+ column  Mois
+ row  grégorien  `gregorianCalendar`  de `Jan` à `Dec`
+ row  julien  `julianCalendar`  de `Jan` à `Dec`
+ row  copte  `copticCalendar`  `CopticMonth`
+ row  éthiopien  `ethiopianCalendar`  `EthiopianMonth`
+ row  musulman  `islamicCalendar`  `IslamicMonth`
+ row  persan  `persianCalendar`  `PersianMonth`
+ row  hébraïque  `hebrewCalendar`  `HebrewMonth`
+ row  national indien  `indianCalendar`  `IndianMonth`
+ row  républicain français  `frenchRepublicanCalendar`  `FrenchRepublicanMonth`
+ row  bouddhiste  `buddhistCalendar`  grégoriens, avec des années décalées de 543
+ row  Minguo  `minguoCalendar`  grégoriens, avec des années décalées de -1911
+ row  ordinal  `ordinalCalendar`  aucun : une année et un jour de l'année
```

- A calendar formats a date with its own names for the months:
+ Un calendrier formate une date avec ses propres noms de mois :

```scala fuel
run
##
locally:
  import calendars.islamicCalendar

  Out.println(islamicCalendar.format(Date(Year(1445), IslamicMonth.Ramadan, Day(15))))
```

- When one calendar replaced another, the change left a gap of dates which never occurred. A
- `Regime` models such a change. It applies the old calendar before the switch, and the new one
- after it, and rejects dates which fall in the gap, at compiletime when the date is a literal.
- The package `calendars` provides the papal and the British changes from the Julian calendar to
- the Gregorian, as `papalCutover` and `britishCutover`. Under the papal one, 4 October 1582 was
- followed directly by 15 October, and the days between do not exist:
+ Quand un calendrier en a remplacé un autre, le changement a laissé un trou de dates qui n'ont
+ jamais existé. Un `Regime` modélise un tel changement. Il applique l'ancien calendrier avant
+ le basculement, et le nouveau après, et rejette les dates qui tombent dans le trou, à la
+ compilation lorsque la date est un littéral. Le package `calendars` fournit les passages
+ papal et britannique du calendrier julien au grégorien, sous les noms `papalCutover` et
+ `britishCutover`. Selon le passage papal, le 4 octobre 1582 fut directement suivi du
+ 15 octobre, et les jours intermédiaires n'existent pas :

```scala
val cutover: Regime = calendars.papalCutover
```

- Dates also have an ordinal form, which is the number of the day within its year, and an
- [ISO week-date](https://en.wikipedia.org/wiki/ISO_week_date) form, in which a date belongs to
- a year numbered by weeks, which need not match its calendar year. The first day of 2000 falls
- in the last week of 1999:
+ Les dates ont aussi une forme ordinale, qui est le numéro du jour dans son année, et une forme
+ [date de semaine ISO](https://en.wikipedia.org/wiki/ISO_week_date), dans laquelle une date
+ appartient à une année numérotée par semaines, qui ne coïncide pas nécessairement avec son
+ année civile. Le premier jour de 2000 tombe dans la dernière semaine de 1999 :

```scala fuel
run
##
locally:
  import calendars.gregorianCalendar

  Out.println(WeekDate.weekYear(2000-Jan-1)().show)
  Out.println(WeekDate.weekOfYear(2000-Jan-1).show)
```

- ### Recurrences
+ ### Récurrences

- A recurrence describes an event which repeats at a fixed interval. In its
- [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601#Repeating_intervals) form it is a start, a
- period, and an optional number of repetitions. Its occurrences form a lazy
- [chain](collections.md#lazy-chains):
+ Une récurrence décrit un événement qui se répète à intervalle fixe. Dans sa forme
+ [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601#Repeating_intervals), c'est un début, une
+ période, et un nombre optionnel de répétitions. Ses occurrences forment une
+ [chaîne](collections.md#lazy-chains) paresseuse :

```scala
def dates(chain: Chain[Date]): Text = chain.map(_.show).to[List].join(t", ")
```

```scala fuel
run
##
Out.println(dates(Recurrence(2024-Jan-1, 1*Day, 3).occurrences))
```

- A recurrence with no number of repetitions runs forever, so it is consumed with a bound:
- every occurrence up to a date, or those which fall within a window:
+ Une récurrence sans nombre de répétitions se poursuit indéfiniment, si bien qu'on la consomme
+ avec une borne : toutes les occurrences jusqu'à une date, ou celles qui tombent dans une
+ fenêtre :

```scala fuel
run
##
Out.println(dates(Recurrence(2024-Jan-1, 1*Week).until(2024-Jan-22)))
Out.println(dates(Recurrence(2024-Jan-1, 1*Week).within(Period(2024-Jan-5, 2024-Jan-20))))
```

- A recurrence is encoded to its ISO 8601 form, and decoded from it, and the `rec` interpolator
- writes one as a checked literal:
+ Une récurrence est encodée dans sa forme ISO 8601, et décodée à partir de celle-ci, et
+ l'interpolateur `rec` en écrit une comme littéral vérifié :

```scala
val monthly =
  import monthEnds.clampMonthEnd

  val recurrence: Recurrence of Date by (Timespan of Month.type) = rec"R3/2024-01-01/P1M"
  recurrence.occurrences
```

```scala fuel
run
##
Out.println(Recurrence(2024-Jan-1, 1*Day, 5).encode)
Out.println(dates(monthly))
```

- ### Recurrence rules
+ ### Règles de récurrence

- The patterns on the calendar which most software needs do not fit a fixed interval: "the
- third Monday of every month", "the last Friday", "the fourth Thursday of November". These are
- described by a recurrence rule, modeled on the `RRULE` of
- [RFC 5545](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10), from iCalendar. A
- rule has a start, a frequency, and optional fields which select within each period:
+ Les motifs calendaires dont a besoin la plupart des logiciels ne correspondent pas à un
+ intervalle fixe : « le troisième lundi de chaque mois », « le dernier vendredi », « le
+ quatrième jeudi de novembre ». Ils sont décrits par une règle de récurrence, calquée sur la
+ `RRULE` de la [RFC 5545](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10),
+ issue d'iCalendar. Une règle a un début, une fréquence, et des champs optionnels qui
+ sélectionnent à l'intérieur de chaque période :

```tabular
- column  Field
- column  Type
- column  Default
- column  Gives
- row  `start`  the point type  none  the first point, whose fields the occurrences take by default
- row  `frequency`  `Frequency`  none  `Secondly`, `Minutely`, `Hourly`, `Daily`, `Weekly`, `Monthly` or `Yearly`
- row  `interval`  `Int`  `1`  how many periods of the frequency each step spans
- row  `count`  `Optional[Int]`  `Unset`  how many occurrences there are
- row  `until`  `Optional` point  `Unset`  the point the occurrences end at
- row  `byMonth`  `List[Month]`  `Nil`  the months selected
- row  `byMonthDay`  `List[Int]`  `Nil`  the days of the month selected
- row  `byDay`  `List[WeekdayOrdinal]`  `Nil`  the weekdays selected, optionally by ordinal
- row  `byYearDay`  `List[Int]`  `Nil`  the days of the year selected
- row  `byWeekNo`  `List[Int]`  `Nil`  the weeks of the year selected
- row  `byHour`, `byMinute`, `bySecond`  `List[Int]`  `Nil`  the hours, minutes and seconds selected
- row  `bySetPos`  `List[Int]`  `Nil`  positions within each period's expansion
- row  `weekStart`  `Weekday`  `Mon`  the first day of the week
+ column  Champ
+ column  Type
+ column  Défaut
+ column  Donne
+ row  `start`  le type de point  aucun  le premier point, dont les occurrences reprennent les champs par défaut
+ row  `frequency`  `Frequency`  aucun  `Secondly`, `Minutely`, `Hourly`, `Daily`, `Weekly`, `Monthly` ou `Yearly`
+ row  `interval`  `Int`  `1`  combien de périodes de la fréquence couvre chaque pas
+ row  `count`  `Optional[Int]`  `Unset`  combien il y a d'occurrences
+ row  `until`  point `Optional`  `Unset`  le point auquel les occurrences s'arrêtent
+ row  `byMonth`  `List[Month]`  `Nil`  les mois sélectionnés
+ row  `byMonthDay`  `List[Int]`  `Nil`  les jours du mois sélectionnés
+ row  `byDay`  `List[WeekdayOrdinal]`  `Nil`  les jours de la semaine sélectionnés, éventuellement par rang
+ row  `byYearDay`  `List[Int]`  `Nil`  les jours de l'année sélectionnés
+ row  `byWeekNo`  `List[Int]`  `Nil`  les semaines de l'année sélectionnées
+ row  `byHour`, `byMinute`, `bySecond`  `List[Int]`  `Nil`  les heures, minutes et secondes sélectionnées
+ row  `bySetPos`  `List[Int]`  `Nil`  des positions dans le développement de chaque période
+ row  `weekStart`  `Weekday`  `Mon`  le premier jour de la semaine
```

- The simplest rule repeats at its frequency, and takes the day from its start:
+ La règle la plus simple se répète à sa fréquence, et reprend le jour de son début :

```scala fuel
run
##
Out.println(dates(Rrule(2024-Jan-15, Frequency.Monthly, count = 4).occurrences))
```

- The field `byDay` selects days of the week, optionally by their position in the period. The
- third Monday of a month is `WeekdayOrdinal(Mon, 3)`. The last Friday counts back from the end,
- with `-1`:
+ Le champ `byDay` sélectionne des jours de la semaine, éventuellement par leur position dans la
+ période. Le troisième lundi d'un mois est `WeekdayOrdinal(Mon, 3)`. Le dernier vendredi se
+ compte à rebours depuis la fin, avec `-1` :

```scala fuel
run
##
Out.println(dates(Rrule(2024-Jan-1, Frequency.Monthly, byDay = List(WeekdayOrdinal(Mon, 3)), count = 2).occurrences))
Out.println(dates(Rrule(2024-Jan-1, Frequency.Monthly, byDay = List(WeekdayOrdinal(Fri, -1)), count = 2).occurrences))
```

- Fields combine. The fourth Thursday of November is a yearly rule, narrowed to one month and
- one day of the week:
+ Les champs se combinent. Le quatrième jeudi de novembre est une règle annuelle, restreinte à un
+ mois et à un jour de la semaine :

```scala fuel
run
##
Out.println(dates(Rrule(2024-Jan-1, Frequency.Yearly, byMonth = List(Nov),
    byDay = List(WeekdayOrdinal(Thu, 4)), count = 2).occurrences))
```

- A frequency can have an interval, such as every second week, or every third day. A rule can
- also run weekly across several days of the week:
+ Une fréquence peut avoir un intervalle, comme une semaine sur deux, ou un jour sur trois. Une
+ règle peut aussi être hebdomadaire et porter sur plusieurs jours de la semaine :

```scala fuel
run
##
Out.println(dates(Rrule(2024-Jan-1, Frequency.Weekly, interval = 2, count = 3).occurrences))

Out.println(dates(Rrule(2024-Jan-1, Frequency.Weekly, byDay = List(WeekdayOrdinal(Mon),
    WeekdayOrdinal(Wed), WeekdayOrdinal(Fri)), count = 4).occurrences))
```

- The field `bySetPos` picks by position from the dates which each period expands to. "The last
- working day of the month" is the last of the candidates from Monday to Friday:
+ Le champ `bySetPos` choisit par position parmi les dates en lesquelles se développe chaque
+ période. « Le dernier jour ouvré du mois » est le dernier des candidats du lundi au vendredi :

```scala fuel
run
##
Out.println(dates(Rrule(2024-Jan-1, Frequency.Monthly, byDay = List(WeekdayOrdinal(Mon),
    WeekdayOrdinal(Tue), WeekdayOrdinal(Wed), WeekdayOrdinal(Thu),
    WeekdayOrdinal(Fri)), bySetPos = List(-1), count = 2).occurrences))
```

- Frequencies below a day apply to timestamps, in which `byHour` and `byMinute` expand within
- each day. This rule gives nine o'clock and five o'clock each day:
+ Les fréquences inférieures à un jour s'appliquent aux horodatages, dans lesquels `byHour` et
+ `byMinute` se développent à l'intérieur de chaque jour. Cette règle donne neuf heures et
+ cinq heures chaque jour :

```scala
val shifts = Rrule(Timestamp(2024-Jan-1, Clockface(0, 0, 0)), Frequency.Daily,
    byHour = List(9, 17), count = 3).occurrences
```

- A rule is written as an RFC 5545 string, and is parsed back from one:
+ Une règle s'écrit sous forme de chaîne RFC 5545, et s'analyse à partir d'une telle chaîne :

```scala
val lastFridays: Chain[Date] = Rrule.parse(t"FREQ=MONTHLY;BYDAY=-1FR", 2024-Jan-1).occurrences
```

```scala fuel
run
##
Out.println(Rrule(2024-Jan-1, Frequency.Monthly, interval = 2, count = 10,
    byDay = List(WeekdayOrdinal(Mon, 3))).encode)

Out.println(dates(lastFridays.keep(2)))
```

- Several rules, and individual dates, combine into a `RecurrenceSet`. It merges its sources
- into one ascending sequence, adds explicit dates, and removes exceptions:
+ Plusieurs règles, et des dates individuelles, se combinent en un `RecurrenceSet`. Celui-ci
+ fusionne ses sources en une seule suite croissante, ajoute des dates explicites, et retire des
+ exceptions :

```scala
val weekly = Rrule(2024-Jan-1, Frequency.Weekly, count = 4)
```

```scala fuel
run
##
Out.println(dates(RecurrenceSet(include = List(weekly.occurrences), rdates = List(2024-Jan-10),
    exdates = List(2024-Jan-15)).occurrences))
```

- ### Describing recurrences
+ ### Décrire les récurrences

- A rule, a recurrence or a timespan describes itself in words. The description is localized,
- and the language is enforced by the types. It needs a `Locale` in scope, and the names of the
- months and of the days of the week for that language, or it does not compile:
+ Une règle, une récurrence ou une durée calendaire se décrit en mots. La description est
+ localisée, et la langue est imposée par les types. Elle nécessite une `Locale` dans la portée,
+ ainsi que les noms des mois et des jours de la semaine dans cette langue, faute de quoi elle
+ ne compile pas :

```scala
val english =
  import monthFormats.englishMonths
  import weekdays.englishWeekdays
  given locale: Locale[en] = Locale(en)

  Rrule(2024-Jan-1, Frequency.Yearly, byMonth = List(Nov),
      byDay = List(WeekdayOrdinal(Thu, 4))).show
```

- Another language follows from a different locale, and its names:
+ Une autre langue découle d'une autre locale, et de ses noms :

```scala
val french =
  import monthFormats.frenchMonths
  import weekdays.frenchWeekdays
  given locale: Locale[fr] = Locale(fr)

  Rrule(2024-Jan-1, Frequency.Monthly, byDay = List(WeekdayOrdinal(Mon, 3))).show
```

```scala fuel
run
##
Out.println(english)
Out.println(french)
```

- A timespan describes itself relative to now, and reads forward or backward according to its
- sign:
+ Une durée calendaire se décrit par rapport à maintenant, et se lit vers l'avant ou vers
+ l'arrière selon son signe :

```scala fuel
run
##
locally:
  import timespanFormats.englishRelativeTimespan
  given locale: Locale[en] = Locale(en)

  Out.println(Timespan(minutes = 18).show)
  Out.println(Timespan(minutes = -8).show)
  Out.println(Timespan().show)
```

- [Internationalization](internationalization.md) describes locales.
+ [Internationalisation](internationalization.md) décrit les locales.

- ### Formatting dates and times
+ ### Formater les dates et les heures

- A date or a time is displayed through a chosen format. Importing one of the named formats
- fixes the convention for a whole scope:
+ Une date ou une heure s'affiche selon un format choisi. Importer l'un des formats nommés fixe
+ la convention pour toute une portée :

```scala
val april = 2025-Apr-7
val afternoon = Clockface(14, 30, 59)
```

```scala fuel
run
##
Out.println({ import dateFormats.iso8601DateFormat; april.show })
Out.println({ import dateFormats.americanDateFormat; april.show })
Out.println({ import dateFormats.europeanDateFormat; april.show })
```

- Times offer the same range, from the twenty-four-hour form to the twelve-hour form:
+ Les heures offrent le même éventail, de la forme sur vingt-quatre heures à la forme sur douze
+ heures :

```scala fuel
run
##
Out.println({ import timeFormats.militaryTimeFormat; afternoon.show })
Out.println({ import timeFormats.civilianTimeFormat; afternoon.show })
Out.println({ import timeFormats.railwayTimeFormat; afternoon.show })
```

- For finer control, a format is composed from independent choices, each brought in on its own:
- the order of the fields, the separator between them, the width of each number, and the length
- of the year. Assembling a format from independent pieces, instead of choosing from a fixed
- menu, lets an unusual convention be expressed without a formatter written for it:
+ Pour un contrôle plus fin, un format se compose de choix indépendants, chacun introduit
+ séparément : l'ordre des champs, le séparateur entre eux, la largeur de chaque nombre, et la
+ longueur de l'année. Assembler un format à partir de pièces indépendantes, au lieu de choisir
+ dans un menu fixe, permet d'exprimer une convention inhabituelle sans écrire de formateur
+ dédié :

```scala
import dateEndianness.bigEndian
import dateNumerics.variableWidthDateNumerics
import dateSeparators.hyphenDateSeparator
import yearFormats.fullYears
```

- Together these four give a date with the year first, hyphens between the fields, numbers
- without leading zeros, and all four digits of the year. A named format in scope, such as
- `iso8601DateFormat`, takes precedence over a composed one.
+ Ensemble, ces quatre choix donnent une date avec l'année en premier, des tirets entre les
+ champs, des nombres sans zéros de tête, et les quatre chiffres de l'année. Un format nommé dans
+ la portée, comme `iso8601DateFormat`, a précédence sur un format composé.

- ### Leap seconds and atomic time
+ ### Secondes intercalaires et temps atomique

- Civil time and physical time disagree by a whole number of seconds, and the number grows.
- Since 1972, [leap seconds](https://en.wikipedia.org/wiki/Leap_second) have been inserted into
- [UTC](https://en.wikipedia.org/wiki/Coordinated_Universal_Time), to keep clocks aligned with
- the Earth's slowing rotation. A `Unix` instant ignores them, so the elapsed seconds between
- two `Unix` instants can be short by the leap seconds which fell between them.
+ Le temps civil et le temps physique diffèrent d'un nombre entier de secondes, et ce nombre
+ croît. Depuis 1972, des [secondes intercalaires](https://en.wikipedia.org/wiki/Leap_second)
+ sont insérées dans l'[UTC](https://en.wikipedia.org/wiki/Coordinated_Universal_Time), pour
+ maintenir les horloges alignées sur la rotation de la Terre, qui ralentit. Un instant `Unix`
+ les ignore, si bien que les secondes écoulées entre deux instants `Unix` peuvent être en deçà
+ du nombre de secondes intercalaires survenues entre eux.

- Each insertion is a correction. It is decided from observations of the small variations in
- the Earth's rotation over the preceding months, and is made to preserve one invariant: that
- solar noon at the Greenwich Meridian, when the sun is highest, stays within a second of the
- moment when a clock reads 12:00:00. That is the whole reason the discontinuities exist, and
- why they cannot be predicted years in advance.
+ Chaque insertion est une correction. Elle est décidée d'après les observations des petites
+ variations de la rotation de la Terre au cours des mois précédents, et sert à préserver un
+ invariant : que le midi solaire au méridien de Greenwich, lorsque le soleil est au plus haut,
+ reste à moins d'une seconde du moment où une horloge indique 12:00:00. C'est l'unique raison
+ d'être de ces discontinuités, et la raison pour laquelle on ne peut pas les prévoir des années
+ à l'avance.

- [International Atomic Time](https://en.wikipedia.org/wiki/International_Atomic_Time) (TAI) has
- no such discontinuities. `over[Tai]` converts a `Unix` instant to the TAI timeline, which
- accounts for the leap seconds which had accumulated by that instant. Converting back gives the
- original instant exactly:
+ Le [temps atomique international](https://en.wikipedia.org/wiki/International_Atomic_Time)
+ (TAI) n'a pas de telles discontinuités. `over[Tai]` convertit un instant `Unix` vers la
+ ligne de temps TAI, en tenant compte des secondes intercalaires accumulées jusqu'à cet instant. La
+ conversion inverse redonne exactement l'instant d'origine :

```scala
val instant = t"2017-06-15T00:00:00Z".as[Instant over Unix]
```

```scala fuel
run
##
Out.println((instant.over[Tai].over[Unix] == instant).inspect)
```

- The timelines are distinct types, so subtracting an instant on one from an instant on the
- other is a compile error. A whole class of bugs, which are wrong by the leap seconds, simply
- cannot be written:
+ Les lignes de temps sont des types distincts, si bien que soustraire un instant de l'une d'un
+ instant de l'autre est une erreur de compilation. Toute une catégorie de bogues, faux du nombre
+ de secondes intercalaires, ne peut tout simplement pas s'écrire :

```scala fuel
compile
errors show
##
Instant(0L).over[Tai] - Instant(0L)
```

- By default, arithmetic on moments is lenient. It works in civil time, and steps over leap
- seconds as though they were not there. Choosing the exact mode counts them. Adding two hours
- across the leap second at the end of 2016 then lands one second short of the lenient answer:
+ Par défaut, l'arithmétique sur les moments est tolérante. Elle travaille en temps civil, et
+ enjambe les secondes intercalaires comme si elles n'existaient pas. Choisir le mode exact les
+ compte. Ajouter deux heures en traversant la seconde intercalaire de la fin de 2016 tombe
+ alors une seconde avant la réponse tolérante :

```scala fuel
run
##
val start = Moment(2016-Dec-31, Clockface(23, 0, 0), tz"UTC")
val lenient = (start + 2*Hour).instant

val exact =
  import leapModes.exactLeapMode
  (start + 2*Hour).instant

Out.println((lenient.long - exact.long).show)
```

- A leap second itself is the rare reading `23:59:60`. A `ts` literal with a zone accepts it,
- marks the moment as having an inserted second, and resolves it to the second which follows:
+ Une seconde intercalaire est elle-même la lecture rare `23:59:60`. Un littéral `ts` avec un
+ fuseau l'accepte, marque le moment comme ayant une seconde insérée, et le résout vers la
+ seconde qui suit :

```scala
val inserted = ts"2016-12-31T23:59:60Z"
```

```scala fuel
run
##
Out.println(inserted.leap.inspect)
```

- ### Related
+ ### Voir aussi

- - [Quantities](quantities.md) describes the physical units which a `Duration` is measured in.
- - [JSON](json.md#dates-and-times) encodes instants and durations.
- - [Decoding](decoding.md) reads a date from a command-line argument or a configuration file.
- - [Concurrency](concurrency.md) waits for a duration, and sets timeouts.
- - [Internationalization](internationalization.md) covers the locales used to describe
-   recurrences.
- - [Caching](caching.md), [charts](charts.md), [cryptography](cryptography.md), [gauges](gauges.md), [numbers](numbers.md) and [foreign interoperability](foreign-interop.md) measure, plot, stamp and convert times.
+ - [Quantités](quantities.md) décrit les unités physiques dans lesquelles se mesure une
+   `Duration`.
+ - [JSON](json.md#dates-and-times) encode les instants et les durées.
+ - [Décodage](decoding.md) lit une date à partir d'un argument de ligne de commande ou d'un
+   fichier de configuration.
+ - [Concurrence](concurrency.md) attend pendant une durée, et fixe des délais d'expiration.
+ - [Internationalisation](internationalization.md) couvre les locales utilisées pour décrire
+   les récurrences.
+ - La [mise en cache](caching.md), les [graphiques](charts.md), la
+   [cryptographie](cryptography.md), les [jauges](gauges.md), les [nombres](numbers.md) et
+   l'[interopérabilité étrangère](foreign-interop.md) mesurent, tracent, horodatent et
+   convertissent des heures.
