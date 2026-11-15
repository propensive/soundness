- ## OpenAPI
+ ## OpenAPI

- ### About
+ ### Présentation

- An [OpenAPI](https://www.openapis.org/) document describes a REST API: its paths, parameters,
- and request and response schemas. Here that description becomes a *typed client* as the code
- compiles. Pointing `Api` at a specification gives a value whose navigation mirrors the paths of
- the API. Each segment, each path parameter, each query or header parameter and each response
- type is checked against the specification. A call which the API does not offer, or a parameter
- of the wrong type, does not compile.
+ Un document [OpenAPI](https://www.openapis.org/) décrit une API REST : ses chemins, ses
+ paramètres, et les schémas de ses requêtes et de ses réponses. Ici, cette description devient un
+ *client typé* à la compilation du code. Diriger `Api` vers une spécification donne une valeur
+ dont la navigation reflète les chemins de l'API. Chaque segment, chaque paramètre de chemin,
+ chaque paramètre de requête ou d'en-tête et chaque type de réponse est vérifié par rapport à la
+ spécification. Un appel que l'API n'offre pas, ou un paramètre du mauvais type, ne compile pas.

- ### On API specifications
+ ### Sur les spécifications d'API

- An OpenAPI document is a machine-readable contract. The usual way to honor it is code
- generation: a build step emits a client, which is compiled, versioned and kept in step by
- tooling. The contract is enforced, at the price of generated sources and a build pipeline. When
- the generator is skipped, calls are made against remembered URLs and hoped-for schemas.
+ Un document OpenAPI est un contrat lisible par une machine. La façon habituelle de l'honorer est
+ la génération de code : une étape du build émet un client, qui est compilé, versionné et tenu à
+ jour par l'outillage. Le contrat est respecté, au prix de sources générées et d'une chaîne de
+ build. Quand on se passe du générateur, les appels sont faits sur des URL dont on se souvient et
+ des schémas qu'on espère justes.

- Here the specification is read during compilation instead. There is no generated code to
- maintain. The client *is* the specification, interpreted by the compiler, and a drift between
- code and contract is a compile error in the code. That is
- [correctness](../../philosophy/correctness.md) by construction, and not by review.
+ Ici, la spécification est plutôt lue pendant la compilation. Il n'y a aucun code généré à
+ maintenir. Le client *est* la spécification, interprétée par le compilateur, et un écart entre
+ le code et le contrat est une erreur de compilation dans le code. C'est la
+ [correction](../../philosophy/correctness.md) par construction, et non par relecture.

- Everything comes from the `soundness` package, with a few deliberate choices imported by name:
+ Tout provient du package `soundness`, avec quelques choix délibérés importés par leur nom :

```scala
import soundness.*
import strategies.throwUnsafely
import internetAccess.online
import httpBackends.javaNetHttp
import logging.silentLogging
import construables.jsonConstruable
import postables.jsonPostable
import codepages.utf8Codepage
import formatting.compactJsonFormatting
import errorDiagnostics.stackTracesDiagnostics
```

- The samples on this page call APIs over the network, so they are compiled but not run.
+ Les exemples de cette page appellent des API sur le réseau ; ils sont donc compilés mais pas
+ exécutés.

- ### A typed client
+ ### Un client typé

- `Api` reads a specification from the [classpath](classpath.md) as the code compiles. The
- specification may be JSON or YAML, and OpenAPI 3.0 or 3.1. The resulting value navigates by
- path:
+ `Api` lit une spécification depuis le [classpath](classpath.md) à la compilation du code. La
+ spécification peut être en JSON ou en YAML, et en OpenAPI 3.0 ou 3.1. La valeur obtenue se
+ parcourt par chemin :

```scala fuel
typecheck
##
val api = Api(cp"/openapi/local/petstore.json")

val pets = api.pets
val pet = api.pets(42)
```

- `api.pets` is the `/pets` path, and `api.pets(42)` is `/pets/{petId}`, with the parameter typed
- by the specification. A path which the specification does not have does not compile:
+ `api.pets` est le chemin `/pets`, et `api.pets(42)` est `/pets/{petId}`, avec le paramètre typé
+ par la spécification. Un chemin que la spécification ne contient pas ne compile pas :

```scala fuel
compile
errors show
##
api.unicorns
```

- A path parameter of the wrong type, such as text where the specification says integer, is also
- a compile error, caught where the call is written. A parameter of `type: integer` with
- `format: int64` is a `Long`, and an `Int` is accepted too. A segment which the specification
- writes with characters Scala does not allow in an identifier is written in backticks, as in
- `` api.`museum-hours` `` or `` api.users.`@me` ``.
+ Un paramètre de chemin du mauvais type, comme du texte là où la spécification indique un entier,
+ est aussi une erreur de compilation, détectée là où l'appel est écrit. Un paramètre de
+ `type: integer` avec `format: int64` est un `Long`, et un `Int` est accepté aussi. Un segment
+ que la spécification écrit avec des caractères que Scala n'autorise pas dans un identifiant
+ s'écrit entre backticks, comme dans `` api.`museum-hours` `` ou `` api.users.`@me` ``.

- The base URL comes from the first server of the specification, with its `{variables}` at their
- defaults, and is checked as a URL as the code compiles. A specification whose server is
- relative, such as `/api/v3`, or which declares none, must take its base from the caller. A
- relative server URL then extends that base:
+ L'URL de base provient du premier serveur de la spécification, avec ses `{variables}` à leurs
+ valeurs par défaut, et elle est vérifiée en tant qu'URL à la compilation du code. Une
+ spécification dont le serveur est relatif, comme `/api/v3`, ou qui n'en déclare aucun, doit
+ recevoir sa base de l'appelant. Une URL de serveur relative prolonge alors cette base :

```scala fuel
typecheck
##
val petstore = Api(cp"/openapi/swagger/petstore3.json", base = url"https://petstore3.swagger.io")
```

- What is about to be sent is a value too. `request` on any navigated `Api` or `Api.Response` is
- an `Api.Request`, whose fields are:
+ Ce qui est sur le point d'être envoyé est aussi une valeur. `request` sur n'importe quel `Api`
+ ou `Api.Response` parcouru est une `Api.Request`, dont les champs sont :

```tabular
- column  Field
- column  Holds
- row  `method`  the `Http.Method`
- row  `base`  the base `HttpUrl`
- row  `path`  the path template
- row  `substitutions`  the values for the template's variables
- row  `query`  the `Query`
- row  `headers`  the `Http.Header`s
- row  `body`  the body, with its `MediaType`
- row  `accept`  the media type asked for
+ column  Champ
+ column  Contient
+ row  `method`  la `Http.Method`
+ row  `base`  l'`HttpUrl` de base
+ row  `path`  le modèle de chemin
+ row  `substitutions`  les valeurs des variables du modèle
+ row  `query`  la `Query`
+ row  `headers`  les `Http.Header`
+ row  `body`  le corps, avec son `MediaType`
+ row  `accept`  le type de média demandé
```

- ### Calling
+ ### Appeler

- An endpoint is invoked with its method, with its query and header parameters as named arguments,
- and with a request body as a value. `call` executes the request and reads the response as the
- type asked for, which must conform to the response schema the specification declares:
+ Un endpoint est invoqué avec sa méthode, avec ses paramètres de requête et d'en-tête comme
+ arguments nommés, et avec un corps de requête sous forme de valeur. `call` exécute la requête et
+ lit la réponse dans le type demandé, qui doit être conforme au schéma de réponse que déclare la
+ spécification :

```scala
case class Pet(id: Int, name: Text, tag: Optional[Text] = Unset)
case class NewPet(name: Text, tag: Optional[Text] = Unset)
```

```scala fuel
typecheck
##
val one: Pet = api.pets(42).get.call[Pet]()
val some: List[Pet] = api.pets.get(limit = 10).call[List[Pet]]()
val created: Pet = api.pets.post(NewPet(t"Crab")).call[Pet]()
val deleted: Unit = api.sessions(t"abc").delete.call()
```

- The first is `GET /pets/42`, decoded according to the schema. The third sends a typed request
- body. The last answers `204` with no body, so its result is `Unit`.
+ Le premier est `GET /pets/42`, décodé selon le schéma. Le troisième envoie un corps de requête
+ typé. Le dernier répond `204` sans corps ; son résultat est donc `Unit`.

- A parameter declared on the path item applies to every operation on it. A parameter written as
- a `$ref` into the components of the document is followed, as are `$ref` responses and request
- bodies. Where an operation declares several successful responses, `200` is preferred, then
- `201`, then the lowest. Asking for a type which the response schema does not support is a
- compile error.
+ Un paramètre déclaré sur l'élément de chemin s'applique à toutes ses opérations. Un paramètre
+ écrit comme un `$ref` vers les composants du document est suivi, tout comme les réponses et
+ corps de requête `$ref`. Là où une opération déclare plusieurs réponses de succès, `200` est
+ préférée, puis `201`, puis la plus basse. Demander un type que le schéma de réponse ne prend pas
+ en charge est une erreur de compilation.

- ### Security
+ ### Sécurité

- The specification of an API names its security schemes, which may be an API key in a header,
- query parameter or cookie, HTTP authentication, OAuth 2 or OpenID Connect, and says which of
- them each operation requires. The credentials come from [OAuth](oauth.md). A `Credential` is a
- [contextual value] named for the scheme, which holds the value the kind of scheme calls for. An
- operation which requires credentials which nobody has provided does not compile, and the error
- names the givens which would satisfy each alternative:
+ La spécification d'une API nomme ses schémas de sécurité, qui peuvent être une clé d'API dans un
+ en-tête, un paramètre de requête ou un cookie, une authentification HTTP, OAuth 2 ou OpenID
+ Connect, et indique lesquels chaque opération exige. Les identifiants proviennent
+ d'[OAuth](oauth.md). Un `Credential` est une [valeur contextuelle] nommée d'après le schéma, qui
+ contient la valeur qu'exige ce type de schéma. Une opération qui exige des identifiants que
+ personne n'a fournis ne compile pas, et l'erreur nomme les givens qui satisferaient chaque
+ alternative :

```scala
val issuer = Issuer(url"https://issuer.example/token", t"client-1", t"s3cret")

given apiKey: ("api_key" is Credential to Text) = Credential(t"…")
given oauth: ("petstore_auth" is Credential to Authorization) =
  Credential(issuer.clientCredentials(t"read:pets", t"write:pets"))
```

```scala fuel
typecheck
##
petstore.pet(42L).get.call()
petstore.pet(42L).delete(api_key = t"…")
```

- The first call sends `api_key` as the specification dictates. In the second, `api_key` is a
- header parameter, and is sent as a header. Each kind of scheme takes its own credential, and
- sends it its own way:
+ Le premier appel envoie `api_key` comme le dicte la spécification. Dans le second, `api_key` est
+ un paramètre d'en-tête, et il est envoyé comme en-tête. Chaque type de schéma prend son propre
+ identifiant, et l'envoie à sa manière :

```tabular
- column  Scheme
- column  Credential
- column  Sent
- row  API key  `Credential to Text`  in the header, query parameter or cookie the scheme names
- row  HTTP authentication  `Credential to Auth`  in the `Authorization` header
- row  OAuth 2, OpenID Connect  `Credential to Authorization`, or `Credential to Auth` for a token obtained elsewhere  as a `Bearer` authorization
- row  mutual TLS  not supported
+ column  Schéma
+ column  Identifiant
+ column  Envoyé
+ row  clé d'API  `Credential to Text`  dans l'en-tête, le paramètre de requête ou le cookie que nomme le schéma
+ row  authentification HTTP  `Credential to Auth`  dans l'en-tête `Authorization`
+ row  OAuth 2, OpenID Connect  `Credential to Authorization`, ou `Credential to Auth` pour un jeton obtenu ailleurs  comme autorisation `Bearer`
+ row  TLS mutuel  non pris en charge
```

- An OAuth 2 or OpenID Connect token is checked, before the request goes, against the scopes which
- the requirement names. A token which lacks one raises `OAuth.Error`. Where an operation offers
- alternative requirements, the first of which every scheme has a credential in scope is used. An
- operation whose `security` is empty needs none.
+ Un jeton OAuth 2 ou OpenID Connect est vérifié, avant l'envoi de la requête, par rapport aux
+ portées que nomme l'exigence. Un jeton auquel il en manque une lève `OAuth.Error`. Là où une
+ opération offre des exigences alternatives, on utilise la première dont chaque schéma a un
+ identifiant dans la portée. Une opération dont la `security` est vide n'en exige aucun.

- ### Errors
+ ### Erreurs

- Every response which an operation declares outside the success range has its own error type,
- named for its status:
+ Chaque réponse qu'une opération déclare hors de la plage de succès a son propre type d'erreur,
+ nommé d'après son statut :

```tabular
- column  Declared response
- column  Error type
- row  a status telekinesis names, such as `404`, `400` or `429`  one named for it: `Api.NotFound`, `Api.BadRequest`, `Api.TooManyRequests`
- row  the `4XX` range  `Api.ClientError`
- row  the `5XX` range  `Api.ServerError`
- row  `default`  `Api.OtherError`
+ column  Réponse déclarée
+ column  Type d'erreur
+ row  un statut que nomme telekinesis, comme `404`, `400` ou `429`  un type nommé d'après lui : `Api.NotFound`, `Api.BadRequest`, `Api.TooManyRequests`
+ row  la plage `4XX`  `Api.ClientError`
+ row  la plage `5XX`  `Api.ServerError`
+ row  `default`  `Api.OtherError`
```

- Each carries the error body, construed as the type which the specification declares for it, by
- the first of these which applies:
+ Chacun porte le corps de l'erreur, interprété comme le type que la spécification déclare pour
+ lui, selon le premier des cas suivants qui s'applique :

```tabular
- column  The error body is
- column  Construed as
- row  a JSON object  a record typed by the error schema
- row  of a media type with a carrier  the media type's carrier
- row  anything else  `Text`
- row  not declared  `Unit`
+ column  Le corps de l'erreur est
+ column  Interprété comme
+ row  un objet JSON  un enregistrement typé par le schéma de l'erreur
+ row  d'un type de média doté d'un porteur  le porteur du type de média
+ row  autre chose  `Text`
+ row  non déclaré  `Unit`
```

- The `Api.Response` of the operation records the union of them as its `Failure`. So `attempt`
- gives an `Attempt` over exactly the declared errors, and the compiler checks a `match` over it
- for exhaustiveness:
+ L'`Api.Response` de l'opération enregistre leur union comme son `Failure`. Ainsi, `attempt`
+ donne un `Attempt` portant exactement sur les erreurs déclarées, et le compilateur vérifie
+ l'exhaustivité d'un `match` sur lui :

```scala fuel
typecheck
##
petstore.pet(42L).get.attempt[Pet]() match
  case Attempt.Success(pet)                        => pet.name
  case Attempt.Failure(Api.NotFound(()))           => t"no such pet"
  case Attempt.Failure(Api.BadRequest(()))         => t"bad id"

given basic: ("basicAuth" is Credential to Auth) = Credential(Auth.Basic(t"user", t"pass"))
val stripe = Api(cp"/openapi/stripe/spec3.json")

stripe.v1.customers(t"cus_1").get.attempt[Json]() match
  case Attempt.Success(customer)                       => customer
  case Attempt.Failure(Api.OtherError(status, problem)) => problem.error.message
```

- A case for a status which the specification does not declare, such as `Api.Conflict` for these
- operations, does not compile. `call()` itself raises the declared errors through
- [contingency](errors.md), with one [tactic] for each declared type, which a `recover` or
- `mitigate` supplies case by case. A handler which misses one is a compile error which names it.
+ Un cas pour un statut que la spécification ne déclare pas, comme `Api.Conflict` pour ces
+ opérations, ne compile pas. `call()` lui-même lève les erreurs déclarées par
+ [contingency](errors.md), avec une [tactique] pour chaque type déclaré, qu'un `recover` ou un
+ `mitigate` fournit cas par cas. Un gestionnaire qui en oublie une est une erreur de compilation
+ qui la nomme.

- Servers do not always keep to their specifications. A response with an undeclared status raises
- `Api.Violation`, which carries the status and the bytes of the body. Every call may meet it, and
- it is handled apart from the declared errors. It is the breach of the contract, not one of its
- cases.
+ Les serveurs ne respectent pas toujours leurs spécifications. Une réponse avec un statut non
+ déclaré lève `Api.Violation`, qui porte le statut et les octets du corps. Tout appel peut la
+ rencontrer, et elle est traitée à part des erreurs déclarées. C'est la rupture du contrat, et
+ non l'un de ses cas.

- ### Media types and their carriers
+ ### Types de média et leurs porteurs

- The specification names the media type of each body, and a `Construable` names the type which
- carries that media type: `("application/json" is Construable to Json)`, or
- `("image/png" is Construable to (Raster in Png))`. The [typeclass] carries no operations. The
- reader and writer of the carrier do the work. Every mapping is a contextual value in the
- `construables` family, imported by name, so nothing is construed without a deliberate choice:
+ La spécification nomme le type de média de chaque corps, et un `Construable` nomme le type qui
+ porte ce type de média : `("application/json" is Construable to Json)`, ou
+ `("image/png" is Construable to (Raster in Png))`. La [classe de types] ne porte aucune
+ opération. Le lecteur et l'écrivain du porteur font le travail. Chaque correspondance est une
+ valeur contextuelle de la famille `construables`, importée par son nom, de sorte que rien n'est
+ interprété sans un choix délibéré :

```scala fuel
typecheck
##
import construables.pngConstruable

given museumAuth: ("MuseumPlaceholderAuth" is Credential to Auth) =
  Credential(Auth.Basic(t"curator", t"…"))

val museum = Api(cp"/openapi/redocly/museum.yaml")
val ticket: Raster in Png = museum.tickets(t"a1").qr.get.call()
val inventory: Json = petstore.store.inventory.get.call()
```

- These are the `Construable`s provided:
+ Voici les `Construable` fournis :

```tabular
- column  Media type
- column  Carrier
- column  Given
- row  `application/json`  `Json`  `construables.jsonConstruable`
- row  `application/xml`  `Xml`  `construables.xmlConstruable`
- row  `text/xml`  `Xml`  `construables.textXmlConstruable`
- row  `application/yaml`  `Yaml`  `construables.yamlConstruable`
- row  `application/vnd.tel`  `Tel`  `construables.telConstruable`
- row  `application/cbor`  `Cbor`  `construables.cborConstruable`
- row  `application/protobuf`  `Protobuf`  `construables.protobufConstruable`
- row  `text/html`  `Document[Html]`  `construables.htmlConstruable`
- row  `text/css`  `Css`  `construables.cssConstruable`
- row  `image/svg+xml`  `Svg`  `construables.svgConstruable`
- row  `text/markdown`  `Markdown of Layout`  `construables.markdownConstruable`
- row  `text/csv`  `Sheet`  `construables.csvConstruable`
- row  `text/tab-separated-values`  `Sheet`  `construables.tsvConstruable`
- row  `text/plain`  `Text`  `construables.plainTextConstruable`
- row  `application/octet-stream`  `Data`  `construables.octetStreamConstruable`
- row  `application/x-www-form-urlencoded`  `Query`  `construables.formConstruable`
- row  `image/png`  `Raster in Png`  `construables.pngConstruable`
- row  `image/jpeg`  `Raster in Jpeg`  `construables.jpegConstruable`
- row  `image/gif`  `Raster in Gif`  `construables.gifConstruable`
- row  `image/bmp`  `Raster in Bmp`  `construables.bmpConstruable`
- row  `image/webp`  `Raster in Webp`  `construables.webpConstruable`
+ column  Type de média
+ column  Porteur
+ column  Given
+ row  `application/json`  `Json`  `construables.jsonConstruable`
+ row  `application/xml`  `Xml`  `construables.xmlConstruable`
+ row  `text/xml`  `Xml`  `construables.textXmlConstruable`
+ row  `application/yaml`  `Yaml`  `construables.yamlConstruable`
+ row  `application/vnd.tel`  `Tel`  `construables.telConstruable`
+ row  `application/cbor`  `Cbor`  `construables.cborConstruable`
+ row  `application/protobuf`  `Protobuf`  `construables.protobufConstruable`
+ row  `text/html`  `Document[Html]`  `construables.htmlConstruable`
+ row  `text/css`  `Css`  `construables.cssConstruable`
+ row  `image/svg+xml`  `Svg`  `construables.svgConstruable`
+ row  `text/markdown`  `Markdown of Layout`  `construables.markdownConstruable`
+ row  `text/csv`  `Sheet`  `construables.csvConstruable`
+ row  `text/tab-separated-values`  `Sheet`  `construables.tsvConstruable`
+ row  `text/plain`  `Text`  `construables.plainTextConstruable`
+ row  `application/octet-stream`  `Data`  `construables.octetStreamConstruable`
+ row  `application/x-www-form-urlencoded`  `Query`  `construables.formConstruable`
+ row  `image/png`  `Raster in Png`  `construables.pngConstruable`
+ row  `image/jpeg`  `Raster in Jpeg`  `construables.jpegConstruable`
+ row  `image/gif`  `Raster in Gif`  `construables.gifConstruable`
+ row  `image/bmp`  `Raster in Bmp`  `construables.bmpConstruable`
+ row  `image/webp`  `Raster in Webp`  `construables.webpConstruable`
```

- A structured-syntax suffix falls back to the type it names, so `application/problem+json` is
- construed by the JSON mapping. Where an operation offers several media types, the first with a
- mapping in scope is chosen, with `application/json` first.
+ Un suffixe de syntaxe structurée se replie sur le type qu'il nomme ; `application/problem+json`
+ est donc interprété par la correspondance JSON. Là où une opération offre plusieurs types de
+ média, on choisit le premier qui a une correspondance dans la portée, `application/json` en
+ premier.

- A request body is encoded to the carrier of the media type, and written by that carrier's
- `Postable`, which is likewise imported by name, as `postables.jsonPostable` or
- `postables.xmlPostable`. A value which already *is* the carrier, such as a `Query` for
- `application/x-www-form-urlencoded` or `Data` for `application/octet-stream`, is sent as it is.
+ Un corps de requête est encodé dans le porteur du type de média, et écrit par le `Postable` de
+ ce porteur, qui est lui aussi importé par son nom, comme `postables.jsonPostable` ou
+ `postables.xmlPostable`. Une valeur qui *est* déjà le porteur, comme une `Query` pour
+ `application/x-www-form-urlencoded` ou des `Data` pour `application/octet-stream`, est envoyée
+ telle quelle.

- ### Records from a response
+ ### Des enregistrements à partir d'une réponse

- A response is read in one of four ways:
+ Une réponse se lit de l'une de quatre façons :

```tabular
- column  Method
- column  Reads the response as
- row  `call[T]()`  a `T`, raising the declared errors
- row  `attempt[T]()`  an `Attempt` over a `T` and the declared errors
- row  `record()`  a record typed by the response schema
- row  `tuple()`  a named tuple typed by the response schema, read eagerly
+ column  Méthode
+ column  Lit la réponse comme
+ row  `call[T]()`  un `T`, en levant les erreurs déclarées
+ row  `attempt[T]()`  un `Attempt` portant sur un `T` et les erreurs déclarées
+ row  `record()`  un enregistrement typé par le schéma de réponse
+ row  `tuple()`  un tuple nommé typé par le schéma de réponse, lu immédiatement
```

- A JSON response can be read without a mirror class at all. `record()` gives a
- [record](records.md) typed by the response schema, with one member for each property, at the
- type the schema declares. Nested objects and references to component schemas are nested
- records, and an array of objects is a `List` of them:
+ Une réponse JSON peut se lire sans aucune classe miroir. `record()` donne un
+ [enregistrement](records.md) typé par le schéma de réponse, avec un membre pour chaque
+ propriété, au type que déclare le schéma. Les objets imbriqués et les références aux schémas de
+ composants sont des enregistrements imbriqués, et un tableau d'objets en est une `List` :

```scala fuel
typecheck
##
val found = petstore.pet(42L).get.record()
val name: Text = found.name
val photos: List[Text] = found.photoUrls
val sold = petstore.pet.findByStatus.get(status = t"sold").record().map(_.name)
```

- `tuple()` reads the same schema as a named tuple, eagerly. Both need a JSON response whose
- schema describes an object or an array of objects. Anything else is a compile error which
- suggests `call[T]()`.
+ `tuple()` lit le même schéma comme un tuple nommé, immédiatement. Tous deux exigent une réponse
+ JSON dont le schéma décrit un objet ou un tableau d'objets. Tout le reste est une erreur de
+ compilation qui suggère `call[T]()`.

- ### The document model
+ ### Le modèle de document

- The specification itself is also a value. `OpenApi` decodes an OpenAPI 3.x document from JSON or
- YAML into typed parts: info, servers with their variables, paths, operations, parameters,
- request bodies, responses, and component schemas, parameters, responses and request bodies.
- This serves tooling which inspects APIs rather than calling them. A YAML document is translated
- to JSON and read by the same decoder, so the two forms cannot drift.
+ La spécification elle-même est aussi une valeur. `OpenApi` décode un document OpenAPI 3.x depuis
+ du JSON ou du YAML en parties typées : informations, serveurs avec leurs variables, chemins,
+ opérations, paramètres, corps de requête, réponses, ainsi que schémas, paramètres, réponses et
+ corps de requête des composants. Cela sert l'outillage qui inspecte les API plutôt que de les
+ appeler. Un document YAML est traduit en JSON et lu par le même décodeur, de sorte que les deux
+ formes ne peuvent diverger.

- A parameter, response or request body written as a `$ref` is kept as an `OpenApi.Ref`, and
- resolved on demand by `apply()`, given the document. A schema reference resolves the same way,
- one hop at a time, so cyclic schemas stay finite. A document of an unsupported version, or one
- which will not parse, raises an `OpenApi.Error` which names the fault.
+ Un paramètre, une réponse ou un corps de requête écrit comme un `$ref` est conservé sous forme
+ d'`OpenApi.Ref`, et résolu à la demande par `apply()`, étant donné le document. Une référence de
+ schéma se résout de la même façon, un saut à la fois, de sorte que les schémas cycliques restent
+ finis. Un document d'une version non prise en charge, ou qui ne s'analyse pas, lève une
+ `OpenApi.Error` qui nomme le défaut.

- The model and the client are exercised against a corpus of real descriptions: the examples of
- the OpenAPI Initiative, Swagger's Petstore, Redocly's Museum, Twilio, Kubernetes, Discord, Box
- and Stripe. They are checked in under `lib/apoplexy/res/test/openapi` with their provenance.
+ Le modèle et le client sont éprouvés sur un corpus de descriptions réelles : les exemples de
+ l'OpenAPI Initiative, le Petstore de Swagger, le Museum de Redocly, Twilio, Kubernetes, Discord,
+ Box et Stripe. Ils sont versionnés sous `lib/apoplexy/res/test/openapi` avec leur provenance.

- ### Limitations
+ ### Limitations

- Cookie parameters, `callbacks`, `links`, `security` requirements, `webhooks` and references into
- other documents are read into the model where the model has a place for them, but the client
- does not act on them. A `multipart/form-data` body is not yet construed.
+ Les paramètres de cookie, les `callbacks`, les `links`, les exigences `security`, les `webhooks`
+ et les références vers d'autres documents sont lus dans le modèle là où le modèle leur a une
+ place, mais le client n'en tient pas compte. Un corps `multipart/form-data` n'est pas encore
+ interprété.

- ### Related
+ ### Voir aussi

- - [HTTP client](http-client.md) carries the requests, and describes the backends.
- - [OAuth](oauth.md) describes the `Credential` values and the token flows.
- - [Records](records.md) describes the typed records which `record()` gives.
- - [JSON](json.md), [XML](xml.md) and [YAML](yaml.md) describe the carriers of the common media
-   types.
- - [Errors](errors.md) describes `attempt`, `recover` and `mitigate`.
- - [Media types](media-types.md) describes the `MediaType` values which name each body.
+ - [Client HTTP](http-client.md) transporte les requêtes, et décrit les backends.
+ - [OAuth](oauth.md) décrit les valeurs `Credential` et les flux de jetons.
+ - [Enregistrements](records.md) décrit les enregistrements typés que donne `record()`.
+ - [JSON](json.md), [XML](xml.md) et [YAML](yaml.md) décrivent les porteurs des types de média
+   courants.
+ - [Erreurs](errors.md) décrit `attempt`, `recover` et `mitigate`.
+ - [Types de média](media-types.md) décrit les valeurs `MediaType` qui nomment chaque corps.
