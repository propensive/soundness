- ## Characters
+ ## Caractères

- ### About
+ ### Présentation

- The boundary between text and bytes is a
- [character encoding](https://en.wikipedia.org/wiki/Character_encoding), and here the choice of
- encoding is explicit and typed. A `Codepage` turns text into bytes, and a `Charset` turns bytes
- back into text. Each is a [contextual value], chosen by import, so no conversion ever happens
- under an assumed default. What to do with bytes which cannot be decoded is a separate choice,
- made just as explicitly.
+ La frontière entre texte et octets est un
+ [encodage de caractères](https://en.wikipedia.org/wiki/Character_encoding), et ici le choix
+ de l'encodage est explicite et typé. Un `Codepage` transforme du texte en octets, et un
+ `Charset` retransforme des octets en texte. Chacun est une [valeur contextuelle], choisie
+ par import ; aucune conversion n'a donc jamais lieu sous un défaut supposé. Ce qu'il faut
+ faire des octets qui ne peuvent pas être décodés est un choix distinct, fait tout aussi
+ explicitement.

- A program sometimes needs facts about single characters too:
+ Un programme a parfois aussi besoin d'informations sur des caractères isolés :

- - how many columns a character occupies in a terminal, where an "East Asian wide" character
-   takes two;
- - a character's Unicode name and properties;
- - its superscript and subscript forms;
- - the [grapheme cluster](https://unicode.org/reports/tr29/) boundaries, which say where one
-   character, as a reader perceives it, ends and the next begins.
+ - combien de colonnes un caractère occupe dans un terminal, où un caractère « large
+   d'Asie de l'Est » en prend deux ;
+ - le nom et les propriétés Unicode d'un caractère ;
+ - ses formes en exposant et en indice ;
+ - les frontières des [grappes de graphèmes](https://unicode.org/reports/tr29/), qui
+   indiquent où un caractère, tel que le perçoit un lecteur, se termine et où le suivant
+   commence.

- ### On characters
+ ### À propos des caractères

- Encodings fail, and most APIs hide the failure. Decoding bytes with the platform's default
- charset silently replaces anything malformed. Corrupt input turns into question marks somewhere
- downstream, and nothing in the code records which encoding was assumed.
+ Les encodages échouent, et la plupart des API masquent l'échec. Décoder des octets avec le
+ jeu de caractères par défaut de la plateforme remplace silencieusement tout ce qui est mal
+ formé. Une entrée corrompue se transforme en points d'interrogation quelque part en aval,
+ et rien dans le code n'enregistre quel encodage a été supposé.

- Meanwhile "one character" is a slippery idea. A flag emoji is two code points. An accented
- letter may be one or two. A wide ideograph occupies two columns of a terminal. These details
- matter as soon as text is measured or split.
+ Par ailleurs, « un caractère » est une idée glissante. Un émoji de drapeau est formé de deux
+ points de code. Une lettre accentuée peut en compter un ou deux. Un idéogramme large occupe
+ deux colonnes d'un terminal. Ces détails comptent dès que le texte est mesuré ou découpé.

- Here the choices are separate, and each is stated:
+ Ici, les choix sont distincts, et chacun est énoncé :

```tabular
- column  Choice
- column  Made by
- row  the encoding  a `Codepage` or `Charset` given, imported where it is used
- row  what happens to undecodable bytes  a `TextSanitizer` given
- row  how wide a character is  a metric, chosen for the display
+ column  Choix
+ column  Fait par
+ row  l'encodage  un given `Codepage` ou `Charset`, importé là où il est utilisé
+ row  ce qu'il advient des octets indécodables  un given `TextSanitizer`
+ row  la largeur d'un caractère  une métrique, choisie pour l'affichage
```

- Naming each as a [given], instead of defaulting silently, is
- [declarative context](../../philosophy/declarative-context.md) at the boundary between bytes and
- text. Everything comes from the `soundness` package, and the second import lets the samples
- print their results:
+ Nommer chacun comme un [given], au lieu de recourir silencieusement à un défaut, relève du
+ [contexte déclaratif](../../philosophy/declarative-context.md) à la frontière entre octets
+ et texte. Tout provient du package `soundness`, et le second import permet aux exemples
+ d'afficher leurs résultats :

```scala
import soundness.*
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### Encoding and decoding
+ ### Encodage et décodage

- Text cannot become bytes until an encoding has been chosen. There is no default to fall back
- on, so without a `Codepage` in scope the conversion does not compile:
+ Un texte ne peut pas devenir des octets tant qu'un encodage n'a pas été choisi. Il n'y a
+ aucun défaut sur lequel se replier ; sans `Codepage` dans la portée, la conversion ne
+ compile donc pas :

```scala fuel
compile
errors show
##
t"café".in[Data]
```

- An encoding is brought into scope by import. The conversion then happens where text meets
- bytes, either explicitly, as here, or wherever a [stream](streams.md) crosses the boundary:
+ Un encodage est introduit dans la portée par un import. La conversion a alors lieu là où le
+ texte rencontre les octets, soit explicitement, comme ici, soit partout où un
+ [flux](streams.md) franchit la frontière :

```scala
import codepages.utf8Codepage
import charsets.utf8Charset
import textSanitizers.strictSanitizer

val bytes = t"café".in[Data]
```

```scala fuel
run
##
Out.println(bytes.length.show)
Out.println(bytes.utf8)
```

- The four characters are five bytes, since `é` takes two bytes in UTF-8.
+ Les quatre caractères font cinq octets, puisque `é` occupe deux octets en UTF-8.

- UTF-8, UTF-16 in both byte orders, ASCII and ISO-8859-1 are provided. Any encoding the JVM
- knows can be named with the `enc"…"` [interpolator], which checks at [compiletime] that the
- encoding exists. When an encoder is asked for, it also checks that the encoding can encode as
- well as decode:
+ UTF-8, UTF-16 dans les deux ordres d'octets, ASCII et ISO-8859-1 sont fournis. Tout encodage
+ que connaît la JVM peut être nommé avec l'[interpolateur] `enc"…"`, qui vérifie
+ [à la compilation][compilation] que l'encodage existe. Lorsqu'on demande un encodeur, il
+ vérifie aussi que l'encodage permet d'encoder aussi bien que de décoder :

```scala
val encoder = enc"UTF-8".encoder
```

```scala fuel
compile
errors show
##
enc"ABCDEF"
```

- ### Bad input
+ ### Entrées invalides

- When a decoder meets bytes which are not valid in its encoding, it consults the `TextSanitizer`
- in scope. Three sanitizers decide between failing, dropping and replacing:
+ Lorsqu'un décodeur rencontre des octets qui ne sont pas valides dans son encodage, il
+ consulte le `TextSanitizer` dans la portée. Trois assainisseurs choisissent entre échouer,
+ ignorer et remplacer :

```tabular
- column  Sanitizer
- column  On bad bytes
- row  `strictSanitizer`  raises a `Charset.Error` which names the position of the fault
- row  `skipSanitizer`  drops them
- row  `substituteSanitizer`  replaces them with `?`
+ column  Assainisseur
+ column  Face à des octets invalides
+ row  `strictSanitizer`  lève une `Charset.Error` qui indique la position de la faute
+ row  `skipSanitizer`  les ignore
+ row  `substituteSanitizer`  les remplace par `?`
```

- These four bytes are a hyphen, the first byte of a two-byte sequence whose second byte is
- missing, and the digits `1` and `0`:
+ Ces quatre octets sont un tiret, le premier octet d'une séquence de deux octets dont le
+ second octet manque, et les chiffres `1` et `0` :

```scala
import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics

val badUtf8 = Data(45, -62, 49, 48)
```

```scala fuel
run
##
locally:
  import textSanitizers.skipSanitizer
  Out.println(charsets.utf8Charset.decoded(badUtf8))

locally:
  import textSanitizers.substituteSanitizer
  Out.println(charsets.utf8Charset.decoded(badUtf8))

locally:
  import textSanitizers.strictSanitizer
  Out.println(capture[Charset.Error](charsets.utf8Charset.decoded(badUtf8)).message.show)
```

- Which behavior is right depends on the data. Strictness suits input which should be trusted
- absolutely, and tolerance suits text recovered from a lossy source. Either way, the choice is
- visible at the import.
+ Le bon comportement dépend des données. La rigueur convient aux entrées auxquelles on doit
+ pouvoir se fier absolument, et la tolérance au texte récupéré d'une source avec pertes. Dans
+ les deux cas, le choix est visible à l'import.

- A fourth sanitizer keeps both. `accrueSanitizer` carries on decoding, as the skipping one does,
- but records each fault. The whole of a damaged input is recovered, *and* every bad sequence is
- reported, with the position at which it occurred.
+ Un quatrième assainisseur conserve les deux. `accrueSanitizer` poursuit le décodage, comme
+ celui qui ignore, mais enregistre chaque faute. L'intégralité d'une entrée endommagée est
+ récupérée, *et* chaque séquence invalide est signalée, avec la position où elle s'est
+ produite.

- The faults [accumulate](errors.md#accumulating-failures) into a value of the caller's choosing,
- here an error which collects positions and faults. `validate` does the collecting: its first
- block says how each fault joins the accumulation, and its `protect` block does the decoding.
+ Les fautes s'[accumulent](errors.md#accumulating-failures) dans une valeur choisie par
+ l'appelant, ici une erreur qui rassemble des positions et des fautes. `validate` effectue le
+ rassemblement : son premier bloc indique comment chaque faute rejoint l'accumulation, et son
+ bloc `protect` effectue le décodage.

```scala
import dysasymptotics.linearSize

case class DecodeIssues(items: List[(Int, Charset.Error)] = Nil)(using Diagnostics)
extends Error(m"${items.size} decoding issues"):
  def +(position: Int, error: Charset.Error): DecodeIssues =
    DecodeIssues(items :+ (position, error))

val issues =
  validate[Charset.Focus](DecodeIssues()):
    case error: Charset.Error => accrual + (prior.let(_.position).or(0), error)
  . protect:
      import textSanitizers.accrueSanitizer
      charsets.utf8Charset.decoded(badUtf8)
```

- The result of `validate` is the accumulation, with one entry for the one bad sequence:
+ Le résultat de `validate` est l'accumulation, avec une entrée pour l'unique séquence
+ invalide :

```scala fuel
run
##
issues.items.each: (position, error) =>
  Out.println(t"at byte $position: ${error.message}")
```

- This is what a tool wants when it imports a file of uncertain origin: the text, and a list of
- where it was wrong, not a choice between the two.
+ C'est ce que souhaite un outil lorsqu'il importe un fichier d'origine incertaine : le texte,
+ et une liste des endroits où il était erroné, et non un choix entre les deux.

- ### Encodings
+ ### Encodages

- The packages `codepages` and `charsets` provide the encodings a program is likely to need, each
- as a named given:
+ Les packages `codepages` et `charsets` fournissent les encodages dont un programme est
+ susceptible d'avoir besoin, chacun sous forme de given nommé :

```tabular
- column  Encoding
- column  Codepage, for encoding
- column  Charset, for decoding
- row  UTF-8  `utf8Codepage`  `utf8Charset`
- row  UTF-16  `utf16Codepage`  `utf16Charset`
- row  UTF-16, little-endian  `utf16LeCodepage`  `utf16LeCharset`
- row  UTF-16, big-endian  `utf16BeCodepage`  `utf16BeCharset`
- row  ASCII  `asciiCodepage`  `asciiCharset`
- row  ISO-8859-1  `iso88591Codepage`  `iso88591Charset`, which always skips bad bytes
+ column  Encodage
+ column  Codepage, pour encoder
+ column  Charset, pour décoder
+ row  UTF-8  `utf8Codepage`  `utf8Charset`
+ row  UTF-16  `utf16Codepage`  `utf16Charset`
+ row  UTF-16, petit-boutiste  `utf16LeCodepage`  `utf16LeCharset`
+ row  UTF-16, gros-boutiste  `utf16BeCodepage`  `utf16BeCharset`
+ row  ASCII  `asciiCodepage`  `asciiCharset`
+ row  ISO-8859-1  `iso88591Codepage`  `iso88591Charset`, qui ignore toujours les octets invalides
```

- Every other charset uses the sanitizer in scope.
+ Tous les autres jeux de caractères utilisent l'assainisseur dans la portée.

- Encoding and decoding also work as stages of a [stream](streams.md#composing-a-pipeline). A
- multi-byte character which is split across two chunks is carried over the boundary, not
- corrupted. That includes a surrogate pair split between chunks, which is where a naive
- implementation loses a character.
+ L'encodage et le décodage fonctionnent aussi comme étapes d'un
+ [flux](streams.md#composing-a-pipeline). Un caractère multi-octets coupé entre deux morceaux
+ est reporté par-delà la frontière, et non corrompu. Cela inclut une paire de substitution
+ coupée entre deux morceaux, là où une implémentation naïve perd un caractère.

- ### Character widths
+ ### Largeur des caractères

- A character is not always one column wide in a monospaced display. The `metrics` extension
- measures a character, or a text, through the metric in scope. There are three, in the package
- `textMetrics`:
+ Un caractère n'occupe pas toujours une seule colonne dans un affichage à chasse fixe.
+ L'extension `metrics` mesure un caractère, ou un texte, au moyen de la métrique dans la
+ portée. Il y en a trois, dans le package `textMetrics` :

```tabular
- column  Metric
- column  Two columns
- column  No columns
- column  One column
- row  `uniformMetric`  none  none  every character
- row  `eastAsianScriptsMetric`  wide and full-width characters  none  every other character
- row  `wideCharacterWidthMetric`  wide and full-width characters  control characters, combining and enclosing marks, format characters  every other character
+ column  Métrique
+ column  Deux colonnes
+ column  Aucune colonne
+ column  Une colonne
+ row  `uniformMetric`  aucun  aucun  tous les caractères
+ row  `eastAsianScriptsMetric`  les caractères larges et pleine chasse  aucun  tous les autres caractères
+ row  `wideCharacterWidthMetric`  les caractères larges et pleine chasse  les caractères de contrôle, les marques combinantes et englobantes, les caractères de formatage  tous les autres caractères
```

```scala
import textMetrics.eastAsianScriptsMetric
```

```scala fuel
run
##
Out.println('a'.metrics.show)
Out.println('身'.metrics.show)
```

- Text [padding and fitting](text.md#padding-and-fitting) measure with the same metric, so a
- column of Japanese text aligns correctly where counting characters would not.
+ Le [remplissage et l'ajustement](text.md#padding-and-fitting) du texte mesurent avec la même
+ métrique ; une colonne de texte japonais s'aligne donc correctement là où compter les
+ caractères échouerait.

- The choice affects speed as well as correctness, which is why the most thorough metric is not
- simply the default. Measuring a text under `uniformMetric` is reading its length, in constant
- time. Measuring it under `eastAsianScriptsMetric` means inspecting every character and adding up
- the widths. Where a text is known to hold nothing wide, the uniform metric gives the same answer
- for nothing.
+ Le choix affecte la vitesse autant que la justesse, c'est pourquoi la métrique la plus
+ complète n'est pas simplement le défaut. Mesurer un texte avec `uniformMetric` revient à
+ lire sa longueur, en temps constant. Le mesurer avec `eastAsianScriptsMetric` implique
+ d'inspecter chaque caractère et d'additionner les largeurs. Lorsqu'on sait qu'un texte ne
+ contient rien de large, la métrique uniforme donne la même réponse pour rien.

- ### Unicode properties
+ ### Propriétés Unicode

- A character answers questions about itself. It reports its Unicode name, and gives its
- superscript and subscript forms where Unicode defines them:
+ Un caractère répond à des questions sur lui-même. Il indique son nom Unicode, et donne ses
+ formes en exposant et en indice lorsqu'Unicode les définit :

```tabular
- column  Member
- column  Returns
- row  `whitespace`  whether it is whitespace
- row  `control`  whether it is an ISO control character
- row  `printable`  whether it is neither whitespace nor a control character
- row  `letter`, `digit`, `alphanumeric`  whether it is a letter, a digit, or either
- row  `majuscular`, `minuscular`  whether it is upper-case or lower-case
- row  `majuscule`, `minuscule`  its upper-case or lower-case form
- row  `description`, `designation`  its Unicode name, an `Optional[Text]`
- row  `superscript`, `subscript`  its superscript or subscript form, an `Optional[Char]`
+ column  Membre
+ column  Renvoie
+ row  `whitespace`  s'il s'agit d'un espace
+ row  `control`  s'il s'agit d'un caractère de contrôle ISO
+ row  `printable`  s'il n'est ni un espace ni un caractère de contrôle
+ row  `letter`, `digit`, `alphanumeric`  s'il s'agit d'une lettre, d'un chiffre, ou de l'un ou l'autre
+ row  `majuscular`, `minuscular`  s'il est en majuscule ou en minuscule
+ row  `majuscule`, `minuscule`  sa forme majuscule ou minuscule
+ row  `description`, `designation`  son nom Unicode, un `Optional[Text]`
+ row  `superscript`, `subscript`  sa forme en exposant ou en indice, un `Optional[Char]`
```

```scala fuel
run
##
Out.println('é'.description.inspect)
Out.println('\t'.control.inspect)
Out.println('2'.superscript.inspect)
Out.println('é'.superscript.inspect)
```

- The last three are [optional](optional-values.md) values. `inspect` displays a present one
- between corner brackets, and an absent one, such as the superscript of `é`, as `○`.
+ Les trois derniers sont des valeurs [optionnelles](optional-values.md). `inspect` affiche une
+ valeur présente entre crochets d'angle, et une valeur absente, comme l'exposant de `é`, sous
+ la forme `○`.

- ### Grapheme clusters
+ ### Grappes de graphèmes

- What a reader perceives as one character may be several code points: a flag, an emoji sequence,
- a letter with a combining accent. `GraphemeBreak.boundaries` finds the positions where one such
- character ends and the next begins, following Unicode's segmentation rules. Two flags are four
- code points, and eight `Char`s, but two graphemes:
+ Ce qu'un lecteur perçoit comme un seul caractère peut être formé de plusieurs points de
+ code : un drapeau, une séquence d'émojis, une lettre avec un accent combinant.
+ `GraphemeBreak.boundaries` trouve les positions où un tel caractère se termine et où le
+ suivant commence, en suivant les règles de segmentation d'Unicode. Deux drapeaux font quatre
+ points de code, et huit `Char`, mais deux graphèmes :

```scala fuel
run
##
Out.println(GraphemeBreak.boundaries(t"🇬🇧🇫🇷").to[List].inspect)
```

- Code which truncates or reverses text at these boundaries never splits a character in half.
- [Text](text.md#graphemes-and-width) counts and measures graphemes with `Writing`.
+ Un code qui tronque ou inverse du texte à ces frontières ne coupe jamais un caractère en
+ deux. [Texte](text.md#graphemes-and-width) compte et mesure les graphèmes avec `Writing`.

- ### Related
+ ### Voir aussi

- - [Filesystem](filesystem.md) describes the encodings which file names and contents are read in.
+ - [Système de fichiers](filesystem.md) décrit les encodages dans lesquels sont lus les noms
+   et le contenu des fichiers.
