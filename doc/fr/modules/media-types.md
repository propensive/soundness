- ## Media Types
+ ## Types de médias

- ### About
+ ### Présentation

- A [media type](https://en.wikipedia.org/wiki/Media_type), such as `application/json` or
- `text/html; charset=UTF-8`, names what a stream of bytes means. It is represented
- structurally: its group, its subtype, its `+suffix`es and its parameters are each a typed
- part, not a substring.
+ Un [type de média](https://en.wikipedia.org/wiki/Media_type), tel que `application/json` ou
+ `text/html; charset=UTF-8`, désigne ce que signifie un flux d'octets. Il est représenté de
+ manière structurelle : son groupe, son sous-type, ses `+suffix`es et ses paramètres sont chacun
+ une partie typée, et non une sous-chaîne.

- The `media"…"` [interpolator] checks a literal against the
- [IANA registry](https://www.iana.org/assignments/media-types/media-types.xhtml) at
- [compiletime]. A misspelled media type, of the kind which browsers silently mishandle, is a
- compile error, with a suggestion of what was probably meant.
+ L'[interpolateur] `media"…"` vérifie un littéral par rapport au
+ [registre de l'IANA](https://www.iana.org/assignments/media-types/media-types.xhtml)
+ [à la compilation][compilation]. Un type de média mal orthographié, du genre de ceux que les
+ navigateurs traitent mal en silence, est une erreur de compilation, accompagnée d'une
+ suggestion de ce qui était probablement voulu.

- The `Media` [typeclass] lets any type declare its own media type. That is how HTTP responses,
- served files and email attachments label themselves. Multipart bodies, the format of file
- uploads, are parsed into their typed parts.
+ La [classe de types] `Media` permet à n'importe quel type de déclarer son propre type de média.
+ C'est ainsi que les réponses HTTP, les fichiers servis et les pièces jointes des e-mails
+ s'étiquettent eux-mêmes. Les corps multipart, le format des envois de fichiers, sont analysés en
+ leurs parties typées.

- ### On media types
+ ### Les types de médias

- Media types look like free text, and are not. The grammar distinguishes registered types from
- vendor trees. Suffixes carry meaning: `svg+xml` *is* XML. Parameters such as `charset` change
- how the bytes are interpreted.
+ Les types de médias ressemblent à du texte libre, et n'en sont pas. La grammaire distingue les
+ types enregistrés des arbres de fournisseurs. Les suffixes ont un sens : `svg+xml` *est* du XML.
+ Des paramètres tels que `charset` changent la manière dont les octets sont interprétés.

- Media types are usually handled as strings, so a typo produces a type which looks plausible,
- and which no consumer recognizes. Receivers tend to fall back instead of failing, so the
- mistake shows up as subtly wrong behavior, far away.
+ Les types de médias sont généralement manipulés comme des chaînes : une faute de frappe produit
+ donc un type qui semble plausible, et qu'aucun consommateur ne reconnaît. Les récepteurs ont
+ tendance à se rabattre sur un repli plutôt qu'à échouer : l'erreur se manifeste donc par un
+ comportement subtilement faux, loin de sa cause.

- Here media types are parsed fully, a literal is validated against the registry itself, and the
- value is typed. Checking a media type as the code compiles is
- [safety by construction](../../philosophy/safety-by-construction.md), applied to a string which
- is otherwise trusted.
+ Ici, les types de médias sont entièrement analysés, un littéral est validé par rapport au
+ registre lui-même, et la valeur est typée. Vérifier un type de média à la compilation, c'est la
+ [sûreté par construction](../../philosophy/safety-by-construction.md), appliquée à une chaîne à
+ laquelle on ferait sinon confiance.

- Everything comes from the `soundness` package. The samples throw any error as an exception,
- and the last import lets them print their results:
+ Tout vient du package `soundness`. Les exemples lèvent toute erreur sous forme d'exception, et
+ le dernier import leur permet d'afficher leurs résultats :

```scala
import soundness.*
import strategies.throwUnsafely
import codepages.utf8Codepage
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### Writing a media type
+ ### Écrire un type de média

- The `media"…"` interpolator checks its literal at compiletime. A registered type passes:
+ L'interpolateur `media"…"` vérifie son littéral à la compilation. Un type enregistré est
+ accepté :

```scala
val json = media"application/json"
val epub = media"application/epub+zip"
```

- An unregistered type in the standard tree fails, and a registered name close to it is
- suggested:
+ Un type non enregistré dans l'arbre standard échoue, et un nom enregistré proche est suggéré :

```scala fuel
compile
errors show
##
media"application/jsom"
```

- The other trees are accepted without being looked up in the registry, as the standard
- prescribes:
+ Les autres arbres sont acceptés sans être recherchés dans le registre, comme le prescrit la
+ norme :

```tabular
- column  Tree
- column  Subtype written
- column  As a `Media.Subtype`
- column  Checked against the registry
- row  standard  `json`  `Standard`  yes
- row  vendor  `vnd.…`  `Vendor`  no
- row  personal  `prs.…`  `Personal`  no
- row  experimental  `x-…`  `X`  no
+ column  Arbre
+ column  Sous-type écrit
+ column  En tant que `Media.Subtype`
+ column  Vérifié dans le registre
+ row  standard  `json`  `Standard`  oui
+ row  fournisseur  `vnd.…`  `Vendor`  non
+ row  personnel  `prs.…`  `Personal`  non
+ row  expérimental  `x-…`  `X`  non
```

- ### Structure
+ ### Structure

- A parsed `MediaType` exposes its parts:
+ Un `MediaType` analysé expose ses parties :

```scala
val mediaType = t"application/json; charset=UTF-8".as[MediaType]
```

```scala fuel
run
##
Out.println(mediaType.group.inspect)
Out.println(mediaType.basic)
Out.println(mediaType.at(t"charset").inspect)
```

- `basic` is the media type without its parameters. Parameters are added as named arguments:
+ `basic` est le type de média sans ses paramètres. Les paramètres s'ajoutent sous forme
+ d'arguments nommés :

```scala fuel
run
##
Out.println(media"text/html"(charset = t"UTF-8").show)
```

- The four parts are those which [RFC 2046](https://www.iana.org/go/rfc2046) defines:
+ Les quatre parties sont celles que définit la [RFC 2046](https://www.iana.org/go/rfc2046) :

```tabular
- column  Part
- column  Field
- column  Is
- row  group  `group`  one of eleven `Media.Group` values: `application`, `audio`, `image`, `message`, `multipart`, `text`, `video`, `font`, `example`, `model` and `haptics`
- row  subtype  `subtype`  registered, vendor, personal or experimental
- row  suffixes  `suffixes`  zero or more, such as `+xml`, `+json` and `+gzip`
- row  parameters  `parameters`  key–value pairs, `charset` being much the most common
+ column  Partie
+ column  Champ
+ column  Est
+ row  groupe  `group`  l'une des onze valeurs de `Media.Group` : `application`, `audio`, `image`, `message`, `multipart`, `text`, `video`, `font`, `example`, `model` et `haptics`
+ row  sous-type  `subtype`  enregistré, fournisseur, personnel ou expérimental
+ row  suffixes  `suffixes`  zéro ou plusieurs, tels que `+xml`, `+json` et `+gzip`
+ row  paramètres  `parameters`  paires clé–valeur, `charset` étant de loin le plus courant
```

- The group is called `group`, not `type`, only because `type` is a keyword in Scala. Which of
- the four trees a subtype belongs to is part of the value, and is not a matter of reading its
- prefix.
+ Le groupe s'appelle `group`, et non `type`, uniquement parce que `type` est un mot-clé en Scala.
+ L'arbre auquel appartient un sous-type fait partie de la valeur, et ne se déduit pas de la
+ lecture de son préfixe.

- Registration is checked against a copy of IANA's published list, carried on the classpath, so
- the check needs no network and no service. A newer list, or a list specific to a site, which
- is placed earlier on the classpath, supersedes it. The file is a list of media types, one on
- each line, with suffixes but without parameters.
+ L'enregistrement est vérifié par rapport à une copie de la liste publiée par l'IANA, présente
+ dans le classpath : la vérification ne nécessite donc ni réseau ni service. Une liste plus
+ récente, ou propre à un site, placée plus tôt dans le classpath, la remplace. Le fichier est une
+ liste de types de médias, un par ligne, avec leurs suffixes mais sans leurs paramètres.

- Text which is not a media type raises a `MediaType.Error`, which names the reason: a missing
- slash, an invalid character, or an unknown suffix.
+ Un texte qui n'est pas un type de média lève une `MediaType.Error`, qui en indique la raison :
+ une barre oblique manquante, un caractère invalide ou un suffixe inconnu.

```scala fuel
run
errors show
##
t"application".as[MediaType]
```

- ### Types that know their media type
+ ### Types qui connaissent leur type de média

- The `Media` typeclass gives a type its media type, so a value can be served, or attached,
- without anyone stating again what it is. Text is `text/plain`, and anything with a filename
- guesses from its extension. `ascribe` pairs any source of bytes with an explicit media type,
- where none is intrinsic. The result is a `Content`: bytes, together with their type.
+ La classe de types `Media` donne à un type son type de média : une valeur peut ainsi être
+ servie, ou jointe, sans que personne ait à redire ce qu'elle est. Le texte est `text/plain`, et
+ tout ce qui a un nom de fichier devine à partir de son extension. `ascribe` associe n'importe
+ quelle source d'octets à un type de média explicite, là où il n'y en a aucun d'intrinsèque. Le
+ résultat est un `Content` : des octets, accompagnés de leur type.

```scala
val data = t"raw bytes".in[Data]
val content = data.ascribe(media"application/octet-stream")
```

- This typeclass is what lets an [HTTP server](http-server.md#responses) return a value, and
- have the `Content-Type` header follow automatically. The [HTTP client](http-client.md#headers)
- takes a media type as the value of its `accept` header, and [email](email.md#composing)
- attachments carry one.
+ C'est cette classe de types qui permet à un [serveur HTTP](http-server.md#responses) de
+ renvoyer une valeur et d'avoir automatiquement l'en-tête `Content-Type` correspondant. Le
+ [client HTTP](http-client.md#headers) prend un type de média comme valeur de son en-tête
+ `accept`, et les pièces jointes des [e-mails](email.md#composing) en portent un.

- ### Detecting a media type
+ ### Détecter un type de média

- Content often arrives without a trustworthy label, and has to be recognised from its bytes. A
- `Media["…"]` pattern does that for a stream — a Zephyrine `Cursor` — and binds a `Detection`:
+ Le contenu arrive souvent sans étiquette fiable, et doit être reconnu à partir de ses octets. Un
+ motif `Media["…"]` le fait pour un flux — un `Cursor` de Zephyrine — et lie une `Detection` :

```scala
import detectables.{gifDetectable, pngDetectable}

val cursor = Cursor(hex"89504e470d0a1a0a0000000d49484452")

cursor match
  case Media["image/png"](png) => t"a PNG"
  case Media["image/gif"](gif) => t"a GIF"
  case _                       => t"something else"
```

- Each case reads as much of the stream as it needs and then returns the cursor to where it was
- found, whether or not it matched: a later case, and the code which follows the match, see the
- whole stream. A detector cannot break this promise, because it never holds the cursor. It reads
- through a `Probe`, which can only go forwards, and the pattern rewinds the cursor when the
- detector returns or throws.
+ Chaque cas lit autant du flux qu'il en a besoin, puis ramène le curseur là où il l'a trouvé,
+ qu'il ait correspondu ou non : un cas ultérieur, et le code qui suit la correspondance, voient
+ le flux entier. Un détecteur ne peut pas rompre cette promesse, car il ne détient jamais le
+ curseur. Il lit au moyen d'une `Probe`, qui ne peut qu'avancer, et le motif rembobine le curseur
+ quand le détecteur renvoie un résultat ou lève une exception.

- What a pattern can detect is decided by the `Detectable` instances in scope, each imported by
- name from the `detectables` family, as the two above are. The media type is checked against the
- registry like any other, and a pattern for which no detector has been imported is a compile
- error:
+ Ce qu'un motif peut détecter est déterminé par les instances de `Detectable` dans la portée,
+ chacune importée par son nom depuis la famille `detectables`, comme les deux ci-dessus. Le type
+ de média est vérifié dans le registre comme n'importe quel autre, et un motif pour lequel aucun
+ détecteur n'a été importé est une erreur de compilation :

```scala fuel
compile
errors show
##
cursor match
  case Media["image/jpeg"](jpeg) => t"a JPEG"
  case _                         => t"something else"
```

- #### How certain a detection is
+ #### Degré de certitude d'une détection

- A few opening bytes rarely prove what a stream is, and a `Detection` says in its type how much
- was established. `png` above is a `Detection of "image/png" under Likely to (Raster in Png)`:
- content detected as `image/png`, with a certainty of `Likely`, which is read as a `Raster in Png`.
- The certainty is one of three, fixed by what the detector examined:
+ Quelques octets initiaux prouvent rarement ce qu'est un flux, et une `Detection` indique dans son
+ type ce qui a été établi. `png` ci-dessus est une
+ `Detection of "image/png" under Likely to (Raster in Png)` : un contenu détecté comme
+ `image/png`, avec une certitude `Likely`, qui se lit comme un `Raster in Png`. La certitude est
+ l'une de trois, fixée par ce que le détecteur a examiné :

```tabular
- column  Certainty
- column  What was examined
- column  Example
- row  `Plausible`  something merely consistent with the format, which reserves no identifying structure or keeps it out of reach  a leading `{` for JSON; a leading `<` for XML
- row  `Likely`  the structure which the format's specification designates as identifying, found where the specification puts it  the eight-byte PNG signature; the index at the end of a ZIP file
- row  `Certain`  the whole of the content, validated  none is provided: it means reading the entire stream
+ column  Certitude
+ column  Ce qui a été examiné
+ column  Exemple
+ row  `Plausible`  quelque chose de simplement compatible avec le format, qui ne réserve aucune structure identifiante ou la garde hors de portée  un `{` initial pour JSON ; un `<` initial pour XML
+ row  `Likely`  la structure que la spécification du format désigne comme identifiante, trouvée là où la spécification la place  la signature PNG de huit octets ; l'index à la fin d'un fichier ZIP
+ row  `Certain`  la totalité du contenu, validée  aucun n'est fourni : cela suppose de lire le flux entier
```

- A `Likely` match is wrong only by coincidence or forgery, and says nothing about the rest of the
- content being well-formed. A `Plausible` match can be wrong by design: other formats legitimately
- begin the same way, so the order of `Plausible` cases matters where the order of `Likely` ones
- does not.
+ Une correspondance `Likely` n'est fausse que par coïncidence ou falsification, et ne dit rien de
+ la bonne formation du reste du contenu. Une correspondance `Plausible` peut être fausse par
+ nature : d'autres formats commencent légitimement de la même façon ; l'ordre des cas
+ `Plausible` importe donc, là où celui des cas `Likely` n'importe pas.

- Each level is a subtype of the one before it — `Certain <: Likely <: Plausible` — so a demand for
- at least a certainty is a bound. A second type argument to the pattern makes that demand of the
- detector, and a method makes it of a `Detection` it is given:
+ Chaque niveau est un sous-type du précédent — `Certain <: Likely <: Plausible` — : exiger au
+ moins une certitude revient donc à poser une borne. Un second argument de type du motif impose
+ cette exigence au détecteur, et une méthode l'impose à une `Detection` qu'elle reçoit :

```scala
def decode(detection: Detection of "image/png" under (? <: Likely)): Text = t"decoding"

cursor match
  case Media["image/png", Likely](png) => decode(png)
  case _                               => t"something else"
```

- The JSON detector is only `Plausible`, so demanding more of it is rejected as the code compiles:
+ Le détecteur JSON n'est que `Plausible` : exiger davantage de lui est donc rejeté à la
+ compilation du code :

```scala fuel
compile
errors show
##
import detectables.jsonDetectable

cursor match
  case Media["application/json", Likely](json) => t"JSON"
  case _                                       => t"something else"
```

- A detection is evidence, not a second handle on the stream: the cursor stays with the code that
- matched it, and `png.read(cursor)` reads it as the type the detection names.
+ Une détection est une preuve, et non une seconde poignée sur le flux : le curseur reste au code
+ qui l'a mis en correspondance, et `png.read(cursor)` le lit comme le type que nomme la
+ détection.

- #### Formats a stream cannot identify
+ #### Formats qu'un flux ne peut pas identifier

- Some formats are identified by something a forward read cannot reach — a ZIP file by the index
- at its end. The same patterns match content with random access, an `Expanse` or `Data` in memory,
- and there a detector may look anywhere. A detector declares which access it needs, and one media
- type may have both kinds, establishing different amounts. ZIP has: `zipDetectable` reads a stream,
- and can only see whether it opens as most archives do, while `zipIndexDetectable` finds the index:
+ Certains formats sont identifiés par quelque chose qu'une lecture vers l'avant ne peut atteindre
+ — un fichier ZIP par l'index situé à sa fin. Les mêmes motifs reconnaissent un contenu à accès
+ aléatoire, un `Expanse` ou des `Data` en mémoire, et là un détecteur peut regarder n'importe où.
+ Un détecteur déclare l'accès dont il a besoin, et un même type de média peut avoir les deux
+ sortes, établissant des choses différentes. C'est le cas de ZIP : `zipDetectable` lit un flux,
+ et ne peut voir que s'il commence comme la plupart des archives, tandis que `zipIndexDetectable`
+ trouve l'index :

```scala
import detectables.{zipDetectable, zipIndexDetectable}

def identify(content: Data): Text = content match
  case Media["application/zip", Likely](zip) => t"a ZIP archive"
  case _                                     => t"something else"
```

- A cursor can use only the first detector, so its `Media["application/zip"]` detection is
- `Plausible`, and `Media["application/zip", Likely]` does not compile for it. `Data` and an
- `Expanse` can use either, and the pattern takes whichever establishes more: there the same
- detection is `Likely`, and an archive which follows a program, as a self-extracting one does, is
- still found.
+ Un curseur ne peut utiliser que le premier détecteur : sa détection `Media["application/zip"]`
+ est donc `Plausible`, et `Media["application/zip", Likely]` ne compile pas pour lui. Des `Data`
+ et un `Expanse` peuvent utiliser l'un ou l'autre, et le motif prend celui qui établit le plus :
+ là, la même détection est `Likely`, et une archive qui suit un programme, comme le fait une
+ archive auto-extractible, est tout de même trouvée.

- #### Writing a detector
+ #### Écrire un détecteur

- A format which reserves a signature needs only its bytes, and the offset if it is not the start:
+ Un format qui réserve une signature n'a besoin que de ses octets, et du décalage s'il ne s'agit
+ pas du début :

```scala
given sqlite: ("application/vnd.sqlite3" is Detectable under Likely over Probe to Data) =
  Detectable.signature(0x53, 0x51, 0x4c, 0x69, 0x74, 0x65)
```

- Anything else is a function of a `Probe`, for a stream, or of an `Expanse`:
+ Tout le reste est une fonction d'une `Probe`, pour un flux, ou d'un `Expanse` :

```scala
given logfile: ("text/vnd.example" is Detectable under Plausible over Probe to Text) =
  Detectable: probe =>
    probe.skipWhitespace()
    probe.next().lay(false)(_ == '['.toByte)

given trailed: ("application/vnd.example" is Detectable under Likely over Expanse to Data) =
  Detectable.positional: expanse =>
    expanse.size >= 4 && expanse.read(expanse.size - 4, 4).utf8 == t"DONE"
```

- The type states the certainty, and the detector must earn it: `Likely` only for the structure the
- format's specification designates as identifying, and `Certain` only for one which reads and
- validates everything — which, on a stream, means holding all of it in memory until the detector
- returns.
+ Le type indique la certitude, et le détecteur doit la mériter : `Likely` seulement pour la
+ structure que la spécification du format désigne comme identifiante, et `Certain` seulement pour
+ un détecteur qui lit et valide tout — ce qui, sur un flux, signifie tout garder en mémoire
+ jusqu'à ce que le détecteur rende la main.

- ### Multipart
+ ### Multipart

- `multipart/form-data` is the format of file uploads. It is parsed into a `Multipart` of typed
- `Part`s. Each part has its disposition, its name, an optional filename, its headers, and a
- stream for its body. It is parsed incrementally, so a large upload streams, and does not
- accumulate:
+ `multipart/form-data` est le format des envois de fichiers. Il est analysé en un `Multipart` de
+ `Part`s typées. Chaque partie a sa disposition, son nom, un nom de fichier optionnel, ses
+ en-têtes, et un flux pour son corps. L'analyse est incrémentale : un envoi volumineux est donc
+ traité en flux, sans s'accumuler :

```scala
val body =
  t"--xyz\r\n" +
  t"Content-Disposition: form-data; name=\"avatar\"; filename=\"me.png\"\r\n" +
  t"\r\n" +
  t"pixels\r\n" +
  t"--xyz--\r\n"

val upload = Multipart.parse(Chain(body.in[Data]))
```

```scala fuel
run
##
Out.println(upload.at(t"avatar").let(_.filename).inspect)
```

- A part's body is lent from the input, without copying. It must be read, with
- `part.read[Data]`, or with `part.body()` for the stream, before moving on to the next part.
- After that, whatever remained of it has been skipped, and it reads as empty.
+ Le corps d'une partie est prêté par l'entrée, sans copie. Il doit être lu, avec
+ `part.read[Data]`, ou avec `part.body()` pour le flux, avant de passer à la partie suivante.
+ Ensuite, ce qui en restait a été sauté, et il se lit comme vide.

- ### Related
+ ### Voir aussi

- - [Audio](audio.md), [images](images.md), [interpolation](interpolation.md) and [OpenAPI](openapi.md) name their formats by media type.
+ - [Audio](audio.md), [images](images.md), [interpolation](interpolation.md) et
+   [OpenAPI](openapi.md) désignent leurs formats par leur type de média.
