## OpenAPI

### About

An [OpenAPI](https://www.openapis.org/) document describes a REST API: its paths, parameters,
and request and response schemas. Here that description becomes a *typed client* as the code
compiles. Pointing `Api` at a specification gives a value whose navigation mirrors the paths of
the API. Each segment, each path parameter, each query or header parameter and each response
type is checked against the specification. A call which the API does not offer, or a parameter
of the wrong type, does not compile.

### On API specifications

An OpenAPI document is a machine-readable contract. The usual way to honor it is code
generation: a build step emits a client, which is compiled, versioned and kept in step by
tooling. The contract is enforced, at the price of generated sources and a build pipeline. When
the generator is skipped, calls are made against remembered URLs and hoped-for schemas.

Here the specification is read during compilation instead. There is no generated code to
maintain. The client *is* the specification, interpreted by the compiler, and a drift between
code and contract is a compile error in the code. That is
[correctness](../philosophy/correctness.md) by construction, and not by review.

Everything comes from the `soundness` package, with a few deliberate choices imported by name:

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

The samples on this page call APIs over the network, so they are compiled but not run.

### A typed client

`Api` reads a specification from the [classpath](classpath.md) as the code compiles. The
specification may be JSON or YAML, and OpenAPI 3.0 or 3.1. The resulting value navigates by
path:

```scala fuel
typecheck
##
val api = Api(cp"/openapi/local/petstore.json")

val pets = api.pets
val pet = api.pets(42)
```

`api.pets` is the `/pets` path, and `api.pets(42)` is `/pets/{petId}`, with the parameter typed
by the specification. A path which the specification does not have does not compile:

```scala fuel
compile
errors show
##
api.unicorns
```

A path parameter of the wrong type, such as text where the specification says integer, is also
a compile error, caught where the call is written. A parameter of `type: integer` with
`format: int64` is a `Long`, and an `Int` is accepted too. A segment which the specification
writes with characters Scala does not allow in an identifier is written in backticks, as in
`` api.`museum-hours` `` or `` api.users.`@me` ``.

The base URL comes from the first server of the specification, with its `{variables}` at their
defaults, and is checked as a URL as the code compiles. A specification whose server is
relative, such as `/api/v3`, or which declares none, must take its base from the caller. A
relative server URL then extends that base:

```scala fuel
typecheck
##
val petstore = Api(cp"/openapi/swagger/petstore3.json", base = url"https://petstore3.swagger.io")
```

What is about to be sent is a value too. `request` on any navigated `Api` or `Api.Response` is
an `Api.Request`, whose fields are:

```tabular
column  Field
column  Holds
row  `method`  the `Http.Method`
row  `base`  the base `HttpUrl`
row  `path`  the path template
row  `substitutions`  the values for the template's variables
row  `query`  the `Query`
row  `headers`  the `Http.Header`s
row  `body`  the body, with its `MediaType`
row  `accept`  the media type asked for
```

### Calling

An endpoint is invoked with its method, with its query and header parameters as named arguments,
and with a request body as a value. `call` executes the request and reads the response as the
type asked for, which must conform to the response schema the specification declares:

```scala
case class Pet(id: Int, name: Text, tag: Optional[Text] = Unset)
case class NewPet(name: Text, tag: Optional[Text] = Unset)
```

```scala fuel
typecheck
##
val one: Pet = api.pets(42).get.call[Pet]()
val some: List[Pet] = api.pets.get(limit = 10).call[List[Pet]]()
val created: Pet = api.pets.post(NewPet(t"Rex")).call[Pet]()
val deleted: Unit = api.sessions(t"abc").delete.call()
```

The first is `GET /pets/42`, decoded according to the schema. The third sends a typed request
body. The last answers `204` with no body, so its result is `Unit`.

A parameter declared on the path item applies to every operation on it. A parameter written as
a `$ref` into the components of the document is followed, as are `$ref` responses and request
bodies. Where an operation declares several successful responses, `200` is preferred, then
`201`, then the lowest. Asking for a type which the response schema does not support is a
compile error.

### Security

The specification of an API names its security schemes, which may be an API key in a header,
query parameter or cookie, HTTP authentication, OAuth 2 or OpenID Connect, and says which of
them each operation requires. The credentials come from [OAuth](oauth.md). A `Credential` is a
[contextual value] named for the scheme, which holds the value the kind of scheme calls for. An
operation which requires credentials which nobody has provided does not compile, and the error
names the givens which would satisfy each alternative:

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

The first call sends `api_key` as the specification dictates. In the second, `api_key` is a
header parameter, and is sent as a header. Each kind of scheme takes its own credential, and
sends it its own way:

```tabular
column  Scheme
column  Credential
column  Sent
row  API key  `Credential to Text`  in the header, query parameter or cookie the scheme names
row  HTTP authentication  `Credential to Auth`  in the `Authorization` header
row  OAuth 2, OpenID Connect  `Credential to Authorization`, or `Credential to Auth` for a token obtained elsewhere  as a `Bearer` authorization
row  mutual TLS  not supported
```

An OAuth 2 or OpenID Connect token is checked, before the request goes, against the scopes which
the requirement names. A token which lacks one raises `OAuth.Error`. Where an operation offers
alternative requirements, the first of which every scheme has a credential in scope is used. An
operation whose `security` is empty needs none.

### Errors

Every response which an operation declares outside the success range has its own error type,
named for its status:

```tabular
column  Declared response
column  Error type
row  a status telekinesis names, such as `404`, `400` or `429`  one named for it: `Api.NotFound`, `Api.BadRequest`, `Api.TooManyRequests`
row  the `4XX` range  `Api.ClientError`
row  the `5XX` range  `Api.ServerError`
row  `default`  `Api.OtherError`
```

Each carries the error body, construed as the type which the specification declares for it, by
the first of these which applies:

```tabular
column  The error body is
column  Construed as
row  a JSON object  a record typed by the error schema
row  of a media type with a carrier  the media type's carrier
row  anything else  `Text`
row  not declared  `Unit`
```

The `Api.Response` of the operation records the union of them as its `Failure`. So `attempt`
gives an `Attempt` over exactly the declared errors, and the compiler checks a `match` over it
for exhaustiveness:

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

A case for a status which the specification does not declare, such as `Api.Conflict` for these
operations, does not compile. `call()` itself raises the declared errors through
[contingency](errors.md), with one [tactic] for each declared type, which a `recover` or
`mitigate` supplies case by case. A handler which misses one is a compile error which names it.

Servers do not always keep to their specifications. A response with an undeclared status raises
`Api.Violation`, which carries the status and the bytes of the body. Every call may meet it, and
it is handled apart from the declared errors. It is the breach of the contract, not one of its
cases.

### Media types and their carriers

The specification names the media type of each body, and a `Construable` names the type which
carries that media type: `("application/json" is Construable to Json)`, or
`("image/png" is Construable to (Raster in Png))`. The [typeclass] carries no operations. The
reader and writer of the carrier do the work. Every mapping is a contextual value in the
`construables` family, imported by name, so nothing is construed without a deliberate choice:

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

These are the `Construable`s provided:

```tabular
column  Media type
column  Carrier
column  Given
row  `application/json`  `Json`  `construables.jsonConstruable`
row  `application/xml`  `Xml`  `construables.xmlConstruable`
row  `text/xml`  `Xml`  `construables.textXmlConstruable`
row  `application/yaml`  `Yaml`  `construables.yamlConstruable`
row  `application/vnd.tel`  `Tel`  `construables.telConstruable`
row  `application/cbor`  `Cbor`  `construables.cborConstruable`
row  `application/protobuf`  `Protobuf`  `construables.protobufConstruable`
row  `text/html`  `Document[Html]`  `construables.htmlConstruable`
row  `text/css`  `Css`  `construables.cssConstruable`
row  `image/svg+xml`  `Svg`  `construables.svgConstruable`
row  `text/markdown`  `Markdown of Layout`  `construables.markdownConstruable`
row  `text/csv`  `Sheet`  `construables.csvConstruable`
row  `text/tab-separated-values`  `Sheet`  `construables.tsvConstruable`
row  `text/plain`  `Text`  `construables.plainTextConstruable`
row  `application/octet-stream`  `Data`  `construables.octetStreamConstruable`
row  `application/x-www-form-urlencoded`  `Query`  `construables.formConstruable`
row  `image/png`  `Raster in Png`  `construables.pngConstruable`
row  `image/jpeg`  `Raster in Jpeg`  `construables.jpegConstruable`
row  `image/gif`  `Raster in Gif`  `construables.gifConstruable`
row  `image/bmp`  `Raster in Bmp`  `construables.bmpConstruable`
row  `image/webp`  `Raster in Webp`  `construables.webpConstruable`
```

A structured-syntax suffix falls back to the type it names, so `application/problem+json` is
construed by the JSON mapping. Where an operation offers several media types, the first with a
mapping in scope is chosen, with `application/json` first.

A request body is encoded to the carrier of the media type, and written by that carrier's
`Postable`, which is likewise imported by name, as `postables.jsonPostable` or
`postables.xmlPostable`. A value which already *is* the carrier, such as a `Query` for
`application/x-www-form-urlencoded` or `Data` for `application/octet-stream`, is sent as it is.

### Records from a response

A response is read in one of four ways:

```tabular
column  Method
column  Reads the response as
row  `call[T]()`  a `T`, raising the declared errors
row  `attempt[T]()`  an `Attempt` over a `T` and the declared errors
row  `record()`  a record typed by the response schema
row  `tuple()`  a named tuple typed by the response schema, read eagerly
```

A JSON response can be read without a mirror class at all. `record()` gives a
[record](records.md) typed by the response schema, with one member for each property, at the
type the schema declares. Nested objects and references to component schemas are nested
records, and an array of objects is a `List` of them:

```scala fuel
typecheck
##
val found = petstore.pet(42L).get.record()
val name: Text = found.name
val photos: List[Text] = found.photoUrls
val sold = petstore.pet.findByStatus.get(status = t"sold").record().map(_.name)
```

`tuple()` reads the same schema as a named tuple, eagerly. Both need a JSON response whose
schema describes an object or an array of objects. Anything else is a compile error which
suggests `call[T]()`.

### The document model

The specification itself is also a value. `OpenApi` decodes an OpenAPI 3.x document from JSON or
YAML into typed parts: info, servers with their variables, paths, operations, parameters,
request bodies, responses, and component schemas, parameters, responses and request bodies.
This serves tooling which inspects APIs rather than calling them. A YAML document is translated
to JSON and read by the same decoder, so the two forms cannot drift.

A parameter, response or request body written as a `$ref` is kept as an `OpenApi.Ref`, and
resolved on demand by `apply()`, given the document. A schema reference resolves the same way,
one hop at a time, so cyclic schemas stay finite. A document of an unsupported version, or one
which will not parse, raises an `OpenApi.Error` which names the fault.

The model and the client are exercised against a corpus of real descriptions: the examples of
the OpenAPI Initiative, Swagger's Petstore, Redocly's Museum, Twilio, Kubernetes, Discord, Box
and Stripe. They are checked in under `lib/apoplexy/res/test/openapi` with their provenance.

### Limitations

Cookie parameters, `callbacks`, `links`, `security` requirements, `webhooks` and references into
other documents are read into the model where the model has a place for them, but the client
does not act on them. A `multipart/form-data` body is not yet construed.

### Related

- [HTTP client](http-client.md) carries the requests, and describes the backends.
- [OAuth](oauth.md) describes the `Credential` values and the token flows.
- [Records](records.md) describes the typed records which `record()` gives.
- [JSON](json.md), [XML](xml.md) and [YAML](yaml.md) describe the carriers of the common media
  types.
- [Errors](errors.md) describes `attempt`, `recover` and `mitigate`.
- [Media types](media-types.md) describes the `MediaType` values which name each body.
