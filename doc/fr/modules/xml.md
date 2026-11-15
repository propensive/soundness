- ## XML
+ ## XML

- ### About
+ ### Présentation

- [XML](https://en.wikipedia.org/wiki/XML) text parses into an `Xml` value. A case class converts
- to and from XML with an encoder and decoder [derived][derivation] from its shape, and a
- document's elements can be navigated and queried. XML written literally, with the `x"…"`
- [interpolator], is parsed and checked at [compiletime], so a malformed fragment does not
- compile.
+ Un texte [XML](https://en.wikipedia.org/wiki/XML) s'analyse en une valeur `Xml`. Une case class
+ se convertit vers et depuis XML avec un encodeur et un décodeur [dérivés][dérivation] de sa
+ forme, et les éléments d'un document peuvent être parcourus et interrogés. Le XML écrit
+ littéralement, avec l'[interpolateur] `x"…"`, est analysé et vérifié
+ [à la compilation][compilation], de sorte qu'un fragment mal formé ne compile pas.

- The design mirrors that of [JSON](json.md). Parsing keeps the structure and the types together.
- Conversions are derived, not written by hand. A conversion which cannot be made raises a typed
- error which names the reason.
+ La conception reflète celle de [JSON](json.md). L'analyse garde la structure et les types
+ ensemble. Les conversions sont dérivées, et non écrites à la main. Une conversion impossible
+ lève une erreur typée qui en nomme la raison.

- ### On XML
+ ### À propos de XML

- The usual API for XML is the [DOM](https://en.wikipedia.org/wiki/Document_Object_Model). It is
- an untyped tree, whose elements are fetched by string name, and whose text is pulled out and
- parsed by hand. Where the document does not match what the code expects, the mismatch surfaces
- as a `null` or a failed cast. XML is verbose already, and an interface which checks nothing
- makes it worse.
+ L'API habituelle pour XML est le [DOM](https://en.wikipedia.org/wiki/Document_Object_Model).
+ C'est un arbre non typé, dont on récupère les éléments par nom sous forme de chaîne, et dont on
+ extrait le texte pour l'analyser à la main. Lorsque le document ne correspond pas à ce que le
+ code attend, l'incohérence se manifeste par un `null` ou un cast raté. XML est déjà verbeux, et
+ une interface qui ne vérifie rien aggrave les choses.

- Here the conversion between XML and a Scala type is derived from the type. A case class becomes
- an element with a child for each field, and an enumeration becomes an element named for its
- case, so there is nothing to keep in step by hand. Literals are checked as they are written,
- and a failed conversion is a typed `Xml.Error`. An `Xml` value is [immutable], and every edit
- gives a new value, in keeping with [immutability](../../philosophy/immutability.md).
+ Ici, la conversion entre XML et un type Scala est dérivée du type. Une case class devient un
+ élément doté d'un enfant pour chaque champ, et une énumération devient un élément nommé d'après
+ son cas ; il n'y a donc rien à maintenir en phase à la main. Les littéraux sont vérifiés au
+ moment où ils sont écrits, et une conversion en échec est une `Xml.Error` typée. Une valeur
+ `Xml` est [immuable], et chaque modification donne une nouvelle valeur, conformément à
+ l'[immuabilité](../../philosophy/immutability.md).

- Everything comes from the `soundness` package. A schema and an error [strategy] are in scope,
- and the last import lets the samples print their results:
+ Tout provient du package `soundness`. Un schéma et une [stratégie] d'erreur sont dans la portée,
+ et le dernier import permet aux exemples d'afficher leurs résultats :

```scala
import soundness.*
import temporaryDirectories.javaBaseTemporaryDirectory
import strategies.throwUnsafely
import xmlSchemas.freeformXmlSchema
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- The schema is a choice, imported as a charset or a strategy is. `freeformXmlSchema` admits any
- element and any attribute. An [XSD](#typed-records-from-an-xml-schema) takes its place when a
- document's shape is known.
+ Le schéma est un choix, importé comme l'est un jeu de caractères ou une stratégie.
+ `freeformXmlSchema` admet tout élément et tout attribut. Un
+ [XSD](#typed-records-from-an-xml-schema) prend sa place lorsque la forme d'un document est
+ connue.

- ### Parsing
+ ### Analyse

- `read[Xml]` parses text into an `Xml` value, and `show` writes one back out:
+ `read[Xml]` analyse un texte en une valeur `Xml`, et `show` la réécrit :

```scala
val message = t"<message>hello world</message>".read[Xml]
```

```scala fuel
run
##
Out.println(message.show)
```

- Text which is not well-formed XML is an error, which says what was wrong and where:
+ Un texte qui n'est pas du XML bien formé est une erreur, qui dit ce qui n'allait pas et où :

```scala fuel
run
errors show
##
t"<message>hello world</mesage>".read[Xml]
```

- `load` reads a whole document, and keeps its `<?xml?>` header. Where `read` gives the content,
- `load` gives a `Document`, which pairs the root with its `Header`: the version, and the
- encoding and standalone declarations where they are given. A document is therefore written back
- with its declaration intact:
+ `load` lit un document entier, et conserve son en-tête `<?xml?>`. Là où `read` donne le contenu,
+ `load` donne un `Document`, qui associe la racine à son `Header` : la version, ainsi que les
+ déclarations d'encodage et de standalone lorsqu'elles sont présentes. Un document est donc
+ réécrit avec sa déclaration intacte :

```scala
import threads.platformThreads

val loaded = supervise:
  t"""<?xml version="1.0"?><root>content</root>""".load[Xml]
```

- Both `read` and `load` take a source of bytes as readily as text: a [file](filesystem.md), the
- body of an [HTTP response](http-client.md), a [classpath](classpath.md) resource. The bytes are
- parsed directly, with no `Charset` in scope, since the parser reads UTF-8 itself:
+ `read` et `load` acceptent tous deux une source d'octets aussi facilement que du texte : un
+ [fichier](filesystem.md), le corps d'une [réponse HTTP](http-client.md), une ressource du
+ [classpath](classpath.md). Les octets sont analysés directement, sans `Charset` dans la portée,
+ puisque l'analyseur lit lui-même l'UTF-8 :

```scala fuel
typecheck
##
val feed: Path on Linux = temporaryDirectory[Path on Linux] / "feed.xml"
val document = feed.load[Xml]
```

- Anything in the prolog, such as a comment, a processing instruction or a `<!DOCTYPE>`, is kept
- as a node before the root. A stylesheet instruction therefore survives being read and written.
+ Tout ce qui se trouve dans le prologue, comme un commentaire, une instruction de traitement ou
+ un `<!DOCTYPE>`, est conservé sous forme de nœud avant la racine. Une instruction de feuille de
+ style survit donc à la lecture et à l'écriture.

- ### The node types
+ ### Les types de nœuds

- An `Xml` value is one of the kinds of node which the specification defines. Each is a distinct
- type, in the `Xml` companion, and not a tagged string. Beyond elements and text, they hold what
- other libraries tend to flatten away:
+ Une valeur `Xml` est l'une des sortes de nœuds que définit la spécification. Chacune est un type
+ distinct, dans le compagnon `Xml`, et non une chaîne taguée. Au-delà des éléments et du
+ texte, elles contiennent ce que les autres bibliothèques ont tendance à aplatir :

```tabular
- column  Type
- column  Holds
- row  `Xml.Element`  a name, attributes and children
- row  `Xml.Text`  character data
- row  `Xml.Cdata`  a `<![CDATA[…]]>` section, kept as a section
- row  `Xml.Comment`  a `<!-- … -->` comment, wherever it appears
- row  `Xml.ProcessingInstruction`  a target and its data, including the empty-data case `<?target?>`
- row  `Xml.Doctype`  a `<!DOCTYPE>` declaration, which serializes back verbatim
- row  `Xml.Fragment`  a sequence of nodes with no single parent, such as a prolog plus a root
+ column  Type
+ column  Contient
+ row  `Xml.Element`  un nom, des attributs et des enfants
+ row  `Xml.Text`  des données textuelles
+ row  `Xml.Cdata`  une section `<![CDATA[…]]>`, conservée en tant que section
+ row  `Xml.Comment`  un commentaire `<!-- … -->`, où qu'il apparaisse
+ row  `Xml.ProcessingInstruction`  une cible et ses données, y compris le cas sans données `<?target?>`
+ row  `Xml.Doctype`  une déclaration `<!DOCTYPE>`, qui se resérialise mot pour mot
+ row  `Xml.Fragment`  une suite de nœuds sans parent unique, comme un prologue suivi d'une racine
```

- A CDATA section stays a section, so text which contains `<not a tag>` survives being read and
- written, without being escaped into something else.
+ Une section CDATA reste une section, de sorte qu'un texte contenant `<not a tag>` survit à la
+ lecture et à l'écriture, sans être échappé en autre chose.

- The predefined entities are resolved as a document is read, and escaped again as it is written.
- So are numeric character references, in both decimal and hexadecimal:
+ Les entités prédéfinies sont résolues à la lecture d'un document, et de nouveau échappées à son
+ écriture. Il en va de même des références numériques de caractères, en décimal comme en
+ hexadécimal :

```tabular
- column  Entity
- column  Character
- row  `&amp;`  `&`
- row  `&lt;`  `<`
- row  `&gt;`  `>`
- row  `&quot;`  `"`
- row  `&apos;`  `'`
- row  `&#NNN;`, `&#xHHH;`  the character with that code point
+ column  Entité
+ column  Caractère
+ row  `&amp;`  `&`
+ row  `&lt;`  `<`
+ row  `&gt;`  `>`
+ row  `&quot;`  `"`
+ row  `&apos;`  `'`
+ row  `&#NNN;`, `&#xHHH;`  le caractère ayant ce point de code
```

- ### Reading values
+ ### Lecture des valeurs

- `as` converts an `Xml` value to a Scala type. Content which cannot be read as that type raises
- an `Xml.Error`:
+ `as` convertit une valeur `Xml` en un type Scala. Un contenu qui ne peut pas être lu comme ce
+ type lève une `Xml.Error` :

```scala fuel
run
##
Out.println(x"<message>42</message>".as[Int].show)
```

```scala fuel
run
errors show
##
x"<message>forty-two</message>".as[Int]
```

- ### Case classes
+ ### Case classes

- A case class takes part in XML as it stands. Its encoder and decoder are derived from its
- fields. The value becomes an element named for the type, with a child element for each field,
- and is read back whatever the order of the children:
+ Une case class participe à XML telle quelle. Son encodeur et son décodeur sont dérivés de ses
+ champs. La valeur devient un élément nommé d'après le type, avec un élément enfant pour chaque
+ champ, et se relit quel que soit l'ordre des enfants :

```scala
case class Person(name: Text, age: Int)

val hamlet = t"<Person><age>30</age><name>Hamlet</name></Person>".read[Person in Xml]
```

```scala fuel
run
##
Out.println(Person(t"Hamlet", 30).in[Xml].show)
Out.println(hamlet.inspect)
```

- `in[Text over Xml]` encodes a value straight to its XML text, as the mirror of
- `read[Person in Xml]`, and `in[Data over Xml]` writes the same text as bytes, in the `Codepage`
- in scope.
+ `in[Text over Xml]` encode une valeur directement en son texte XML, symétrique de
+ `read[Person in Xml]`, et `in[Data over Xml]` écrit le même texte en octets, dans le `Codepage`
+ présent dans la portée.

- A field marked `@attribute` becomes an attribute instead of a child element, and is read back
- as one:
+ Un champ marqué `@attribute` devient un attribut au lieu d'un élément enfant, et se relit comme
+ tel :

```scala
case class Play(title: Text, @attribute year: Int)
```

```scala fuel
run
##
Out.println(Play(t"Hamlet", 1603).in[Xml].show)
```

- `@name` renames an element or attribute where the Scala name and the XML name should differ.
- `@name[Xml]` confines the rename to XML, and leaves other formats to their own. It also renames
- the element of an enumeration's *variant*, so a `Light.Stop` may be written as `<red>`:
+ `@name` renomme un élément ou un attribut lorsque le nom Scala et le nom XML doivent différer.
+ `@name[Xml]` limite le renommage à XML, et laisse les autres formats à leurs propres noms. Il
+ renomme aussi l'élément d'une *variante* d'énumération, de sorte qu'un `Light.Stop` peut
+ s'écrire `<red>` :

```scala
enum Light:
  @name[Xml](t"red") case Stop(seconds: Int)
  @name(t"green") case Go(seconds: Int)
  case Wait(seconds: Int)

val stop: Light = Light.Stop(30)
```

```scala fuel
run
##
Out.println(stop.in[Xml].show)
```

- A sum type is decoded by its element's name, which selects the variant. A name which matches no
- variant raises an `Xml.Error`, and never falls through to a default.
+ Un type somme se décode d'après le nom de son élément, qui sélectionne la variante. Un nom qui ne
+ correspond à aucune variante lève une `Xml.Error`, et ne retombe jamais sur une valeur par
+ défaut.

- An `Optional` field which is `Unset` is simply absent: there is no child element, or, for an
- `@attribute` field, no attribute. A document without the element decodes the field as `Unset`.
- A `Map` field becomes one child element for each entry, holding a `<key>` and a `<value>`, so
- a key need not be a valid element name, and may be of any type which can be encoded in XML:
+ Un champ `Optional` qui vaut `Unset` est simplement absent : il n'y a pas d'élément enfant, ou,
+ pour un champ `@attribute`, pas d'attribut. Un document sans l'élément décode le champ comme
+ `Unset`. Un champ `Map` devient un élément enfant pour chaque entrée, contenant un `<key>` et un
+ `<value>`, de sorte qu'une clé n'a pas besoin d'être un nom d'élément valide, et peut être de
+ n'importe quel type encodable en XML :

```scala
case class Profile(name: Text, nickname: Optional[Text], stock: Map[Text, Int])
```

```scala fuel
run
##
Out.println(Profile(t"Falstaff", Unset, Map(t"sack" -> 3)).in[Xml].show)
```

- In summary:
+ En résumé :

```tabular
- column  Scala
- column  XML
- row  a case class  an element named for the type
- row  a field  a child element named for the field
- row  an `@attribute` field  an attribute
- row  an `@name` or `@name[Xml]` label  the element or attribute renamed
- row  a variant of a sum type  an element named for the variant
- row  an `Optional` field that is `Unset`  no element, or no attribute
- row  a collection field  one element per item, named for the field, in order
- row  a `Map` field  one element per entry, holding a `<key>` and a `<value>`
+ column  Scala
+ column  XML
+ row  une case class  un élément nommé d'après le type
+ row  un champ  un élément enfant nommé d'après le champ
+ row  un champ `@attribute`  un attribut
+ row  un label `@name` ou `@name[Xml]`  l'élément ou l'attribut renommé
+ row  une variante d'un type somme  un élément nommé d'après la variante
+ row  un champ `Optional` qui vaut `Unset`  aucun élément, ou aucun attribut
+ row  un champ de collection  un élément par élément de la collection, nommé d'après le champ, dans l'ordre
+ row  un champ `Map`  un élément par entrée, contenant un `<key>` et un `<value>`
```

- [Annotations](annotations.md) describes `@name` and `@attribute` and how they are read.
+ [Annotations](annotations.md) décrit `@name` et `@attribute` et la manière dont ils sont lus.

- ### What decoding tolerates
+ ### Ce que tolère le décodage

- Real XML is untidy, and a decoder which insists on tidiness is of little use. Text, comments
- and processing instructions which sit between the children a type expects are ignored. A
- document with prose between its fields decodes as though the prose were not there:
+ Le XML réel est désordonné, et un décodeur qui exige de l'ordre est de peu d'utilité. Le texte,
+ les commentaires et les instructions de traitement qui se trouvent entre les enfants qu'attend
+ un type sont ignorés. Un document comportant de la prose entre ses champs se décode comme si la
+ prose n'était pas là :

```scala
val untidy =
  t"<root>hello<name>A</name><!--c--><?pi data?> <age>4</age>bye</root>".read[Person in Xml]
```

```scala fuel
run
##
Out.println(untidy.inspect)
```

- Entities in text are expanded as they are read. Repeated elements gather into a collection
- field, in document order, and they need not be next to each other: a `<songs>` before the
- `<name>` and another after it still form one list, in the order they appeared. A recursive type
- is handled through its own derivation, so a tree of any depth encodes and decodes without a
- hand-written codec.
+ Les entités dans le texte sont développées à la lecture. Les éléments répétés se rassemblent dans
+ un champ de collection, dans l'ordre du document, et n'ont pas besoin d'être adjacents : un
+ `<songs>` avant le `<name>` et un autre après forment tout de même une seule liste, dans l'ordre
+ où ils sont apparus. Un type récursif est traité par sa propre dérivation, de sorte qu'un arbre
+ de n'importe quelle profondeur s'encode et se décode sans codec écrit à la main.

- Where a nested value is missing altogether, a `Default` for its type turns what would be a
- cascade of errors about its fields into a single error, at the point where the value should
- have been. The default is used to carry on, which is what a validation pass reporting to a
- person wants.
+ Lorsqu'une valeur imbriquée manque entièrement, un `Default` pour son type transforme ce qui
+ serait une cascade d'erreurs sur ses champs en une seule erreur, à l'endroit où la valeur aurait
+ dû se trouver. La valeur par défaut sert à poursuivre, ce que veut une passe de validation qui
+ rend compte à une personne.

- A field which is required, and missing, is an error:
+ Un champ obligatoire et manquant est une erreur :

```scala fuel
run
errors show
##
t"<Person><name>Hamlet</name></Person>".read[Person in Xml]
```

- An `Optional` field has two further policies, from the package `optionalityOptions`, as
- [Decoding](decoding.md#policies-for-optional-fields) describes:
+ Un champ `Optional` a deux politiques supplémentaires, issues du package `optionalityOptions`,
+ comme le décrit [Décodage](decoding.md#policies-for-optional-fields) :

```tabular
- column  A field whose element or attribute is
- column  Reads as
- column  Unless
- row  omitted from the document  `Unset`  `strictXmlAbsence` makes it an error
- row  present but rejected by the inner decoder  an error  `lenientXmlFaults` reads it as `Unset`
+ column  Un champ dont l'élément ou l'attribut est
+ column  Se lit comme
+ column  Sauf si
+ row  omis du document  `Unset`  `strictXmlAbsence` en fait une erreur
+ row  présent mais rejeté par le décodeur interne  une erreur  `lenientXmlFaults` le lit comme `Unset`
```

- Under `strictXmlAbsence`, a missing element or attribute is an `Xml.Error.Reason.Missing`. XML
- has no null. An empty element is a present value, so there is no policy for nulls.
+ Sous `strictXmlAbsence`, un élément ou un attribut manquant est une
+ `Xml.Error.Reason.Missing`. XML n'a pas de null. Un élément vide est une valeur présente ; il
+ n'y a donc pas de politique pour les nulls.

```scala
case class Tally(name: Text, count: Optional[Int])

val tolerant =
  import optionalityOptions.lenientXmlFaults
  x"<root><name>Puck</name><count>many</count></root>".as[Tally]
```

```scala fuel
run
##
Out.println(tolerant.inspect)
```

- ### Writing XML literally
+ ### Écrire du XML littéralement

- The `x"…"` interpolator writes XML directly, and checks it at compiletime. Values are
- substituted into it:
+ L'interpolateur `x"…"` écrit directement du XML, et le vérifie à la compilation. Des valeurs y
+ sont substituées :

```scala
val name = t"Hamlet"
val user = x"<user>$name</user>"
```

```scala fuel
run
##
Out.println(user.show)
```

- A malformed fragment is rejected where it is written:
+ Un fragment mal formé est rejeté là où il est écrit :

```scala fuel
compile
errors show
##
x"<user>$name</usr>"
```

- ### Navigating
+ ### Navigation

- With dynamic access enabled, a child element is reached as though it were a member, and the
- steps chain. An index picks among repeated elements:
+ Lorsque l'accès dynamique est activé, un élément enfant s'atteint comme s'il était un membre, et
+ les étapes s'enchaînent. Un indice choisit parmi des éléments répétés :

```scala
import dynamicAccess.dynamicXml

val data = t"<a><b><c>42</c></b></a>".read[Xml]
val list = t"<r><x>1</x><x>2</x></r>".read[Xml]
```

```scala fuel
run
##
Out.println(data.b().c().as[Int].show)
Out.println(list.x(Sec).as[Int].show)
```

- The import grants the permission `Xml is Dynamical` to the rest of its scope.
- `dynamically[Xml]:` grants it to a single block instead, as
- [JSON](json.md#fields-as-members) describes.
+ L'import accorde la permission `Xml is Dynamical` au reste de sa portée.
+ `dynamically[Xml]:` l'accorde plutôt à un seul bloc, comme le décrit
+ [JSON](json.md#fields-as-members).

- ### Updating
+ ### Mise à jour

- An element is updated through a [lens](optics.md). A lens reaches through several levels, and
- may carry optics such as `Each`, to change every matching element at once. An update gives a
- new document, since `Xml` values are immutable. The lens and its optics are givens, imported by
- name from the package `optics`:
+ Un élément se met à jour par une [lentille](optics.md). Une lentille traverse plusieurs niveaux,
+ et peut porter des optiques comme `Each`, pour modifier d'un coup chaque élément correspondant.
+ Une mise à jour donne un nouveau document, puisque les valeurs `Xml` sont immuables. La lentille
+ et ses optiques sont des givens, importés par leur nom depuis le package `optics` :

```scala
import optics.{xmlEachOptical, xmlLens}

val numbers = t"<doc><x>1</x><x>2</x><x>3</x></doc>".read[Xml]
```

```scala fuel
run
##
Out.println(numbers.lens(_.x = x"<x>9</x>").show)
Out.println(numbers.lens(_(Each) = x"<x>0</x>").show)
```

- The value on the right of the `=` need not be XML already. Anything which can be encoded in
- XML, such as a case class, an `Int` or a `Text`, is encoded as it is assigned, with no import.
- The other formats ask for one, such as `conversions.encodableToJson`.
+ La valeur à droite du `=` n'a pas besoin d'être déjà du XML. Tout ce qui peut s'encoder en XML,
+ comme une case class, un `Int` ou un `Text`, est encodé au moment de l'affectation, sans import.
+ Les autres formats en demandent un, comme `conversions.encodableToJson`.

```scala
val court = t"<court><Person><name>x</name><age>1</age></Person></court>".read[Xml]
```

```scala fuel
run
##
Out.println(court.lens(_.Person = Person(t"Hamlet", 30)).show)
```

- ### Paths and queries
+ ### Chemins et requêtes

- An [XPath](https://en.wikipedia.org/wiki/XPath) names a location within a document. The `xp"…"`
- interpolator writes one, and checks it at compiletime. A step with no position means the first
- match, and `@` names an attribute:
+ Un [XPath](https://en.wikipedia.org/wiki/XPath) nomme un emplacement dans un document.
+ L'interpolateur `xp"…"` en écrit un, et le vérifie à la compilation. Une étape sans position
+ désigne la première correspondance, et `@` nomme un attribut :

```scala fuel
run
##
Out.println(xp"/root[1]/child[2]".encode)
Out.println(xp"/root[1]/@id".encode)
Out.println(xp"/root/child".encode)
```

- Positions and accumulated errors are reported as paths, so an error from decoding a large
- document names the element which caused it.
+ Les positions et les erreurs accumulées sont signalées sous forme de chemins, de sorte qu'une
+ erreur issue du décodage d'un grand document nomme l'élément qui l'a causée.

- The same interpolator writes a full [XPath 1.0](https://www.w3.org/TR/xpath-10/) expression,
- and a document answers it:
+ Le même interpolateur écrit une expression [XPath 1.0](https://www.w3.org/TR/xpath-10/)
+ complète, et un document y répond :

```tabular
- column  Method
- column  Returns
- row  `select`  the matching nodes, as a fragment
- row  `selectText`  the text of the first match
- row  `evaluate`  the value of any expression, whether a number, a boolean, a string or a node set, as an `XPath.Value`
+ column  Méthode
+ column  Renvoie
+ row  `select`  les nœuds correspondants, sous forme de fragment
+ row  `selectText`  le texte de la première correspondance
+ row  `evaluate`  la valeur de n'importe quelle expression, qu'il s'agisse d'un nombre, d'un booléen, d'une chaîne ou d'un ensemble de nœuds, sous forme de `XPath.Value`
```

- Axes, predicates on attributes and on text, and the core function library are all supported.
- That is what a locator for [browser automation](web-automation.md) needs, or a lookup in a
- configuration file:
+ Les axes, les prédicats sur les attributs et sur le texte, et la bibliothèque de fonctions de
+ base sont tous pris en charge. C'est ce dont a besoin un localisateur pour
+ l'[automatisation de navigateur](web-automation.md), ou une recherche dans un fichier de
+ configuration :

```scala
val page = t"""<app><div id="main"><button data-test="submit">Submit</button>
                 <ul><li>one</li><li>two</li><li>three</li></ul></div></app>""".read[Xml]
```

```scala fuel
run
##
Out.println(page.selectText(xp"//button[text()='Submit']/@data-test").inspect)
Out.println(page.selectText(xp"/app/div[1]/ul/li[2]").inspect)
Out.println(page.evaluate(xp"count(//li)").inspect)
```

- An expression which the engine does not support, or a variable it was not given, raises an
- `XPath.Error` which says so.
+ Une expression que le moteur ne prend pas en charge, ou une variable qu'on ne lui a pas fournie,
+ lève une `XPath.Error` qui le signale.

- ### Namespaces
+ ### Espaces de noms

- The parser resolves namespaces as it reads. A declaration, `xmlns="…"` or `xmlns:p="…"`, is
- kept as an attribute, so a document is written back exactly as it was read. The declaration
- also binds the prefix for the element and its descendants. Every `Xml.Element` carries the
- bindings in force at it, as its `scope`, and resolves its own name through them:
+ L'analyseur résout les espaces de noms au fil de la lecture. Une déclaration, `xmlns="…"` ou
+ `xmlns:p="…"`, est conservée comme attribut, de sorte qu'un document est réécrit exactement tel
+ qu'il a été lu. La déclaration lie aussi le préfixe pour l'élément et ses descendants. Chaque
+ `Xml.Element` porte les liaisons en vigueur à son niveau, sous forme de son `scope`, et résout
+ son propre nom par leur intermédiaire :

```scala
val prefixed = t"""<r xmlns:a="urn:a"><a:x>1</a:x></r>""".read[Xml]

val element = prefixed.`a:x`() match
  case Xml.Fragment(element: Xml.Element) => element
```

```scala fuel
run
##
Out.println(element.namespace.inspect)
Out.println(element.localName.inspect)
Out.println(element.qualified.inspect)
```

- Two names are the same name when their namespace URIs and their local parts agree, whatever
- prefixes bind them. A prefixed name selects by resolved name when its prefix is bound, whether
- by a declaration in the document or by an `Xml.Scope` given in scope. The prefix the code uses
- therefore need not be the one the document used:
+ Deux noms sont le même nom lorsque leurs URI d'espace de noms et leurs parties locales
+ concordent, quels que soient les préfixes qui les lient. Un nom préfixé sélectionne selon le nom
+ résolu lorsque son préfixe est lié, que ce soit par une déclaration dans le document ou par un
+ given `Xml.Scope` dans la portée. Le préfixe qu'utilise le code n'a donc pas besoin d'être celui
+ qu'utilisait le document :

```scala
given svg: ("svg" is Namespace of "http://www.w3.org/2000/svg") = Namespace()
given svgScope: Xml.Scope = Xml.Scope(t"svg" -> t"http://www.w3.org/2000/svg")

val picture = t"""<svg xmlns="http://www.w3.org/2000/svg"><rect/><rect/></svg>""".read[Xml]

val byPrefix = picture.`svg:rect`
val byName = picture.elements(Xml.Name(t"http://www.w3.org/2000/svg", t"rect"))
```

- A `Namespace` binds a prefix, its `Self`, to a URI, its `Topic`. Both are singleton types, so
- the binding is checked wherever it is used. An `x"…"` literal which uses a prefix it does not
- declare takes the binding from the givens in scope. An `xp"…"` path does the same for the names
- it tests, so `xp"//svg:rect"` matches by namespace. `XPath#in(scope)` rebinds a path's
- prefixes.
+ Un `Namespace` lie un préfixe, son `Self`, à une URI, son `Topic`. Tous deux sont des types
+ singletons, de sorte que la liaison est vérifiée partout où elle est utilisée. Un littéral
+ `x"…"` qui utilise un préfixe qu'il ne déclare pas prend la liaison dans les givens dans la
+ portée. Un chemin `xp"…"` fait de même pour les noms qu'il teste, de sorte que `xp"//svg:rect"`
+ correspond selon l'espace de noms. `XPath#in(scope)` lie à nouveau les préfixes d'un chemin.

- A prefix with no binding is an error, as the Namespaces recommendation requires. Parsing
- `<p:a/>` without a declaration of `p` raises a `Parse.Error`:
+ Un préfixe sans liaison est une erreur, comme l'exige la recommandation Namespaces. Analyser
+ `<p:a/>` sans déclaration de `p` lève une `Parse.Error` :

```scala fuel
run
errors show
##
t"<p:a/>".read[Xml]
```

- Writing `x"<p:a/>"` without a `Namespace` given for `p` does not compile. Importing
- `namespaceOptions.lenientNamespaces` makes an unbound prefix resolve to no namespace instead.
+ Écrire `x"<p:a/>"` sans given `Namespace` pour `p` ne compile pas. Importer
+ `namespaceOptions.lenientNamespaces` fait plutôt résoudre un préfixe non lié vers aucun espace
+ de noms.

- When an `Xml` tree is written, an element may use a prefix, or a default namespace, which its
- scope binds but which nothing above it has declared. The declaration is then written for it. A
- subtree which was built in code, or cut from a document by a selection, is therefore written
- with every namespace it needs. The scope takes no part in equality: a parsed element equals the
- same element built by hand.
+ Lorsqu'un arbre `Xml` est écrit, un élément peut utiliser un préfixe, ou un espace de noms par
+ défaut, que lie sa portée mais qu'aucun élément au-dessus de lui n'a déclaré. La déclaration est
+ alors écrite pour lui. Un sous-arbre construit dans le code, ou découpé d'un document par une
+ sélection, est donc écrit avec chaque espace de noms dont il a besoin. La portée ne participe
+ pas à l'égalité : un élément analysé est égal au même élément construit à la main.

- A derived codec puts a case class in a namespace with an annotation:
+ Un codec dérivé place une case class dans un espace de noms avec une annotation :

```scala
case class Item(sku: Text, quantity: Int)

@xmlns("urn:shop")
case class Order(id: Int, item: Item)
```

```scala fuel
run
##
Out.println(Order(1, Item(t"a", 2)).in[Xml].show)
```

- The fields' elements are in the same namespace as the class, and `as[Order]` reads a document
- whose elements resolve to that namespace, however it prefixes them. The alternatives are:
+ Les éléments des champs sont dans le même espace de noms que la classe, et `as[Order]` lit un
+ document dont les éléments se résolvent vers cet espace de noms, quels que soient les préfixes
+ qu'il leur donne. Les alternatives sont :

```tabular
- column  Declaration
- column  Effect
- row  `@xmlns("urn:shop")` on the class  the class's element and its fields' elements are in the namespace
- row  `@xmlns("urn:shop", qualified = false)` on the class  the fields' elements are in no namespace, as under `elementFormDefault="unqualified"`
- row  `@unqualified` on a field  that field's element is in no namespace
- row  `given Rect is Xml.Namespaced = Xml.Namespaced(t"…")`  a namespace for a type which cannot be annotated
- row  none  the fields' elements are matched by name alone, whatever namespace the document puts them in
+ column  Déclaration
+ column  Effet
+ row  `@xmlns("urn:shop")` sur la classe  l'élément de la classe et les éléments de ses champs sont dans l'espace de noms
+ row  `@xmlns("urn:shop", qualified = false)` sur la classe  les éléments des champs ne sont dans aucun espace de noms, comme sous `elementFormDefault="unqualified"`
+ row  `@unqualified` sur un champ  l'élément de ce champ n'est dans aucun espace de noms
+ row  `given Rect is Xml.Namespaced = Xml.Namespaced(t"…")`  un espace de noms pour un type qui ne peut pas être annoté
+ row  aucune  les éléments des champs sont mis en correspondance par leur seul nom, quel que soit l'espace de noms où le document les place
```

- ### Formatting
+ ### Formatage

- The output format is a given in scope. Compact formatting leaves out whitespace, and indented
- formatting adds newlines and indentation for a human reader:
+ Le format de sortie est un given dans la portée. Le formatage compact omet les espaces, et le
+ formatage indenté ajoute des sauts de ligne et une indentation pour un lecteur humain :

```scala fuel
run
##
locally:
  import formatting.indentedXmlFormatting

  Out.println(Person(t"Hamlet", 30).in[Xml].show)
```

- `show` renders the whole node into one `Text`. A large document need not be held in memory
- before it is sent:
+ `show` rend le nœud entier en un seul `Text`. Un grand document n'a pas besoin d'être tenu en
+ mémoire avant d'être envoyé :

```tabular
- column  Form
- column  Serializes on
- column  Hands out
- row  `show`  the caller's thread  one `Text`
- row  `Xml.emit[Text](document, f)`  the caller's thread  each block of text to `f`, as it fills
- row  `Xml.emit[Data](document, f)`  the caller's thread  each block as UTF-8 bytes to `f`, as it fills
- row  `Xml.lend(document)`  the caller's thread  each block lent to a function, not copied
- row  `Xml.emit(document)`  a fiber  the text, through an iterator
- row  `Streamable` by `Text`  a fiber  the text, as a stream
+ column  Forme
+ column  Sérialise sur
+ column  Fournit
+ row  `show`  le thread de l'appelant  un seul `Text`
+ row  `Xml.emit[Text](document, f)`  le thread de l'appelant  chaque bloc de texte à `f`, dès qu'il est rempli
+ row  `Xml.emit[Data](document, f)`  le thread de l'appelant  chaque bloc en octets UTF-8 à `f`, dès qu'il est rempli
+ row  `Xml.lend(document)`  le thread de l'appelant  chaque bloc prêté à une fonction, sans copie
+ row  `Xml.emit(document)`  une fibre  le texte, par un itérateur
+ row  `Streamable` par `Text`  une fibre  le texte, sous forme de flux
```

- The push form of `Xml.emit` serializes a `Document[Xml]`, with its header, and hands each block
- over as it fills. That suits writing to a file, a [socket](sockets.md) or an `OutputStream`:
+ La forme poussée de `Xml.emit` sérialise un `Document[Xml]`, avec son en-tête, et transmet
+ chaque bloc dès qu'il est rempli. Cela convient à l'écriture dans un fichier, une
+ [socket](sockets.md) ou un `OutputStream` :

```scala fuel
typecheck
##
Xml.emit[Data](document, chunk => socket.write(chunk))
```

- `Xml.lend` avoids even the copy into each `Data` block. It lends the consumer the writer's own
- buffer as a `Region`, together with an interval which proves that every index in it is in
- range. Both are valid only for the duration of the call, the same rule as a
- [stream's](streams.md) `lend`:
+ `Xml.lend` évite même la copie dans chaque bloc `Data`. Il prête au consommateur le tampon
+ propre de l'écrivain sous forme de `Region`, avec un intervalle qui prouve que chacun de ses
+ indices est dans les bornes. Tous deux ne sont valides que pendant la durée de l'appel, la même
+ règle que pour le `lend` d'un [flux](streams.md) :

```scala fuel
typecheck
##
Xml.lend(document): region =>
  interval => region.visit(interval)(index => sink.put(region(index)))
```

- Where the consumer pulls instead, `Xml.emit(document)` and the `Streamable` instance of a
- `Document[Xml]` serialize on a fiber, under a [`supervise`](concurrency.md) block:
+ Lorsque c'est le consommateur qui tire, `Xml.emit(document)` et l'instance `Streamable` d'un
+ `Document[Xml]` sérialisent sur une fibre, sous un bloc [`supervise`](concurrency.md) :

```scala fuel
typecheck
##
import probates.cancelProbate

supervise:
  Xml.emit(document).each(chunk => out.write(chunk))
```

- ### Skipping the tree
+ ### Se passer de l'arbre

- A record with an `Xml.Printable` instance is written directly, with no tree built, wherever
- the formatting is compact. Indented formatting still renders through the tree. The printer is
- derived at compiletime, for a record with no `@name` or `@attribute` annotations and no
- namespace, and writes the same text:
+ Un enregistrement doté d'une instance `Xml.Printable` est écrit directement, sans construire
+ d'arbre, partout où le formatage est compact. Le formatage indenté passe toujours par l'arbre.
+ L'afficheur est dérivé à la compilation, pour un enregistrement sans annotations `@name` ni
+ `@attribute` et sans espace de noms, et écrit le même texte :

```scala fuel
alternative Through a tree
  code
      Person(t"Hamlet", 30).in[Text over Xml]
alternative Direct
  code
      val direct =
        given Person is Xml.Printable = Xml.Direct.derived[Person].printable
        Person(t"Hamlet", 30).in[Text over Xml]
##
```

- `Xml.Printable.emit` writes the text a block at a time, handing each block to a function as it
- fills. A record can generate both a parser and a printer at once, into its companion, with
- `derives Xml.Direct`. A single import then selects the route for every record in scope, as it
- does for [JSON](json.md#skipping-the-tree):
+ `Xml.Printable.emit` écrit le texte un bloc à la fois, en confiant chaque bloc à une fonction
+ dès qu'il est rempli. Un enregistrement peut générer à la fois un analyseur et un afficheur,
+ dans son compagnon, avec `derives Xml.Direct`. Un seul import sélectionne alors le chemin pour
+ chaque enregistrement dans la portée, comme pour [JSON](json.md#skipping-the-tree) :

```tabular
- column  Import
- column  For a record
- column  For any other type
- row  `codecs.direct`  a generated parser and printer  the derived codecs, through the tree
- row  `codecs.derived`  the derived codecs, even where a direct instance is in scope  the derived codecs, through the tree
- row  `codecs.strict`  a generated parser and printer  a compile error naming the type
+ column  Import
+ column  Pour un enregistrement
+ column  Pour tout autre type
+ row  `codecs.direct`  un analyseur et un afficheur générés  les codecs dérivés, par l'arbre
+ row  `codecs.derived`  les codecs dérivés, même lorsqu'une instance directe est dans la portée  les codecs dérivés, par l'arbre
+ row  `codecs.strict`  un analyseur et un afficheur générés  une erreur de compilation nommant le type
```

- Under `codecs.strict`, the error arises wherever `read[Play in Xml]` or `in[Text over Xml]`
- would otherwise fall back to the tree. The modes are alternatives, so import one at a time.
+ Sous `codecs.strict`, l'erreur survient partout où `read[Play in Xml]` ou `in[Text over Xml]` se
+ replierait sinon sur l'arbre. Les modes sont des alternatives : importez-en un seul à la fois.

- ### Typed records from an XML Schema
+ ### Enregistrements typés à partir d'un XML Schema

- Sometimes a document's shape is given as an XML Schema (XSD), not as a Scala type. An
- `Xml.Provider` reads the schema at compiletime, and produces typed records from XML which
- matches it. A provider object holds the schema:
+ Il arrive que la forme d'un document soit donnée sous forme de XML Schema (XSD), et non de type
+ Scala. Un `Xml.Provider` lit le schéma à la compilation, et produit des enregistrements typés à
+ partir du XML qui s'y conforme. Un objet fournisseur contient le schéma :

```scala fuel
typecheck
##
import charsets.utf8Charset
import classloaders.threadContextClassloader
import textSanitizers.skipSanitizer

object PurchaseOrder extends Xml.Provider(cp"/xsd/po.xsd")
```

- Its `record` method turns an `Xml` value into a record. The record has one member for each
- child element and attribute of the schema's root element, and each has the type the schema
- declares:
+ Sa méthode `record` transforme une valeur `Xml` en enregistrement. L'enregistrement a un membre
+ pour chaque élément enfant et chaque attribut de l'élément racine du schéma, et chacun a le type
+ que déclare le schéma :

```scala fuel
typecheck
##
val order = PurchaseOrder.record(cp"/xsd/po.xml".read[Xml])

val city: Text = order.shipTo.city
val quantities: List[Long] = order.items.item.map(_.quantity)
val comment: Optional[Text] = order.comment
val parts: List[Text] = order.items.item.map(_.partNum)
```

- `quantity` is a restriction of `xs:positiveInteger`, `comment` has `minOccurs="0"`, and
- `partNum` is checked against the schema's pattern for a part number as it is read.
+ `quantity` est une restriction de `xs:positiveInteger`, `comment` a `minOccurs="0"`, et
+ `partNum` est vérifié à la lecture par rapport au motif de numéro de pièce du schéma.

- The schema may be an `Xsd` value, an `Xml` document, the schema's text, or anything which can
- be read as text, such as a classpath resource. Where a schema declares several global elements,
- the root is the one with complex content, or is chosen with `root = t"Envelope"`.
+ Le schéma peut être une valeur `Xsd`, un document `Xml`, le texte du schéma, ou tout ce qui peut
+ se lire comme du texte, comme une ressource du classpath. Lorsqu'un schéma déclare plusieurs
+ éléments globaux, la racine est celui dont le contenu est complexe, ou se choisit avec
+ `root = t"Envelope"`.

- Child elements and attributes become fields, by local name, shaped by the schema:
+ Les éléments enfants et les attributs deviennent des champs, par nom local, façonnés par le
+ schéma :

```tabular
- column  The schema has
- column  The field is
- row  an element with `minOccurs="0"`, or `nillable`  `Optional`
- row  an element whose `maxOccurs` allows more than one  a `List`
- row  an alternative of a `choice`  `Optional`
- row  an attribute sharing a name with a child element  reached as `` `@name` ``
- row  an element with simple content and attributes  its text reached as `text`, or `` `#text` `` if an attribute is called `text`
- row  an extension  preceded by its base type's fields
+ column  Le schéma contient
+ column  Le champ est
+ row  un élément avec `minOccurs="0"`, ou `nillable`  `Optional`
+ row  un élément dont `maxOccurs` autorise plus d'une occurrence  une `List`
+ row  une alternative d'un `choice`  `Optional`
+ row  un attribut portant le même nom qu'un élément enfant  atteint sous la forme `` `@name` ``
+ row  un élément à contenu simple doté d'attributs  son texte atteint sous la forme `text`, ou `` `#text` `` si un attribut s'appelle `text`
+ row  une extension  précédé des champs de son type de base
```

- Each field has the Scala type of its XSD type:
+ Chaque champ a le type Scala de son type XSD :

```tabular
- column  XSD type
- column  Scala type
- row  `xs:string` and the token, name and identifier types, `xs:QName`, the binary types, `xs:gYear` and relatives  `Text`
- row  `xs:boolean`  `Boolean`
- row  `xs:int`, `xs:short`, `xs:byte`  `Int`, `Short`, `Byte`
- row  `xs:integer`, `xs:long` and the unsigned, positive and negative integer types  `Long`, with the implied bounds checked
- row  `xs:decimal`, `xs:double`, `xs:float`  `Double`, `Double`, `Float`
- row  `xs:dateTime`, `xs:date`, `xs:time`, `xs:duration`, `xs:anyURI`  the type an interface in scope instantiates, else `Text`
- row  `xs:NMTOKENS`, `xs:IDREFS` and any `xs:list`  `List[Text]`
- row  `xs:anyType`, `xs:any`, a recursive type, an unresolved reference  `Xml`
+ column  Type XSD
+ column  Type Scala
+ row  `xs:string` et les types de jetons, de noms et d'identifiants, `xs:QName`, les types binaires, `xs:gYear` et ses semblables  `Text`
+ row  `xs:boolean`  `Boolean`
+ row  `xs:int`, `xs:short`, `xs:byte`  `Int`, `Short`, `Byte`
+ row  `xs:integer`, `xs:long` et les types d'entiers non signés, positifs et négatifs  `Long`, avec vérification des bornes implicites
+ row  `xs:decimal`, `xs:double`, `xs:float`  `Double`, `Double`, `Float`
+ row  `xs:dateTime`, `xs:date`, `xs:time`, `xs:duration`, `xs:anyURI`  le type qu'instancie une interface dans la portée, sinon `Text`
+ row  `xs:NMTOKENS`, `xs:IDREFS` et tout `xs:list`  `List[Text]`
+ row  `xs:anyType`, `xs:any`, un type récursif, une référence non résolue  `Xml`
```

- A restriction's facets are checked as the field is read: `enumeration`, `pattern`, the
- inclusive and exclusive bounds, the length facets and the digit facets. A failed check raises
- an `Xml.Provider.Error` with the reason, as does a missing required element, or a value which
- its type cannot read. Every error is raised through the [tactic] of the code which reads the
- field, so `validate`, with a `venture` for each field,
- [collects every problem](errors.md#ventures-and-guards) in a document.
+ Les facettes d'une restriction sont vérifiées à la lecture du champ : `enumeration`, `pattern`,
+ les bornes inclusives et exclusives, les facettes de longueur et les facettes de chiffres. Une
+ vérification en échec lève une `Xml.Provider.Error` avec la raison, de même qu'un élément
+ obligatoire manquant, ou une valeur que son type ne peut pas lire. Chaque erreur est levée par
+ la [tactique] du code qui lit le champ, de sorte que `validate`, avec un `venture` pour chaque
+ champ, [collecte tous les problèmes](errors.md#ventures-and-guards) d'un document.

- The provider matches a document's elements by resolved name. A child is expected in the
- schema's target namespace when the schema qualifies it, through `elementFormDefault`, a `form`,
- or a `ref` to a global element. Otherwise it is expected in no namespace, whatever prefixes the
- document uses. An element in another namespace is treated as absent.
+ Le fournisseur met en correspondance les éléments d'un document selon leur nom résolu. Un enfant
+ est attendu dans l'espace de noms cible du schéma lorsque le schéma le qualifie, par
+ `elementFormDefault`, un `form`, ou un `ref` vers un élément global. Sinon, il est attendu dans
+ aucun espace de noms, quels que soient les préfixes qu'utilise le document. Un élément dans un
+ autre espace de noms est traité comme absent.

- Substitution groups, `xsi:type`, identity constraints and mixed content are read as the schema
- declares without them. An `import` or `include` is recorded in the `Xsd`, but not fetched, so a
- type from another schema is read as raw `Xml`. The provider does not validate documents: it
- checks what a program reads. [JSON](json.md#typed-records-from-a-schema) has the same facility
- for JSON Schema, and [Records](records.md) describes the records themselves.
+ Les groupes de substitution, `xsi:type`, les contraintes d'identité et le contenu mixte sont lus
+ comme le schéma déclare les choses sans eux. Un `import` ou un `include` est enregistré dans le
+ `Xsd`, mais pas récupéré, de sorte qu'un type issu d'un autre schéma se lit comme du `Xml` brut.
+ Le fournisseur ne valide pas les documents : il vérifie ce que lit un programme.
+ [JSON](json.md#typed-records-from-a-schema) offre la même fonctionnalité pour JSON Schema, et
+ [Enregistrements](records.md) décrit les enregistrements eux-mêmes.

- ### Positions and errors
+ ### Positions et erreurs

- A malformed document raises a `Parse.Error`. Its position is not merely a line number but a
- range: the offset and length of the text at fault. A tool can therefore underline exactly the
- mismatched closing tag or the unterminated attribute, not the whole line.
+ Un document mal formé lève une `Parse.Error`. Sa position n'est pas un simple numéro de ligne
+ mais un intervalle : le décalage et la longueur du texte fautif. Un outil peut donc souligner
+ exactement la balise fermante discordante ou l'attribut non terminé, et non la ligne entière.

- The positions of *well-formed* content are recorded on request, in the same way as for
- [JSON](json.md#source-positions):
+ Les positions du contenu *bien formé* sont enregistrées sur demande, de la même manière que
+ pour [JSON](json.md#source-positions) :

```scala
import parsing.trackPositions

val source = t"<root><child/></root>"
val tracked = source.load[Xml]
```

- Under a strategy which [accumulates failures](errors.md#accumulating-failures), decoding
- reports every fault in the document at once, each with the path to the element which failed,
- instead of stopping at the first.
+ Sous une stratégie qui [accumule les échecs](errors.md#accumulating-failures), le décodage
+ signale en une fois chaque défaut du document, chacun avec le chemin vers l'élément en échec,
+ au lieu de s'arrêter au premier.

- ### XML over HTTP
+ ### XML sur HTTP

- An `Xml` value is a request body or a response body, with the media type `application/xml`,
- and a body is parsed back to `Xml` on arrival. An XML API is therefore consumed and offered
- with no glue between the XML and [HTTP](http-client.md) layers.
+ Une valeur `Xml` est un corps de requête ou un corps de réponse, avec le type de média
+ `application/xml`, et un corps est réanalysé en `Xml` à son arrivée. Une API XML se consomme et
+ se propose donc sans aucune colle entre les couches XML et [HTTP](http-client.md).

- ### Related
+ ### Voir aussi

- - [HTML](html.md) uses the same node types and interpolation for web pages.
- - [SVG](svg.md) and [mathematical markup](mathematical-markup.md) are XML vocabularies with
-   typed builders.
- - [JSON](json.md) and [YAML](yaml.md) share the vocabulary of `read`, `as` and `in`.
- - [Derivation](derivation.md), [interpolation](interpolation.md), [OpenAPI](openapi.md) and [TEL](tel.md) describe how XML codecs are made, checked and carried.
+ - [HTML](html.md) utilise les mêmes types de nœuds et la même interpolation pour les pages web.
+ - [SVG](svg.md) et le [balisage mathématique](mathematical-markup.md) sont des vocabulaires XML
+   dotés de constructeurs typés.
+ - [JSON](json.md) et [YAML](yaml.md) partagent le vocabulaire de `read`, `as` et `in`.
+ - [Dérivation](derivation.md), [interpolation](interpolation.md), [OpenAPI](openapi.md) et
+   [TEL](tel.md) décrivent comment les codecs XML sont créés, vérifiés et transportés.
