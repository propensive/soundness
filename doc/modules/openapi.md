## OpenAPI

### About

An [OpenAPI](https://www.openapis.org/) document describes a REST API — its paths, parameters,
request and response schemas — and Soundness turns that description into a *typed client* as the
code compiles. Pointing `Api` at a specification yields a value whose navigation mirrors the API's
paths: each segment, each path parameter, each query or header parameter and each response type is
checked against the specification, so a call the API does not offer, or a parameter of the wrong
type, does not compile.

### On API specifications

An OpenAPI document is a machine-readable contract, and the usual way to honor it is code
generation: a build step emits a client, which is compiled, versioned and kept in sync by
tooling. The contract is enforced, but at the price of generated sources and a build pipeline —
and when the generator is skipped, calls are made against remembered URLs and hoped-for schemas.

A specification derived from the types that serve it cannot fall out of date, which is [correctness](../philosophy/correctness.md) by construction rather than by review.

Soundness reads the specification during compilation instead. There is no generated code to
maintain; the client *is* the specification, interpreted by the compiler, and a drift between
code and contract is a compile error in the code. Everything comes from the `soundness` package,
with a few deliberate choices imported by name:

```scala
import soundness.*
import strategies.throwUnsafely
import internetAccess.online
import construables.jsonConstruable
import postables.jsonPostable
```

### A typed client

`Api` reads a specification from the classpath — JSON or YAML, OpenAPI 3.0 or 3.1 — as the code
compiles, and the resulting value navigates by path:

<!-- doccheck: skip -->
```scala
val api = Api(cp"/apis/petstore.json")

api.pets           // the /pets path
api.pets(42)       // /pets/{petId}, the parameter typed by the spec
api.unicorns       // does not compile: no such path
```

A path parameter of the wrong type — text where the specification says integer — is likewise a
compile error, caught where the call is written. A parameter of `type: integer` with
`format: int64` is a `Long` (an `Int` is accepted too); a segment the specification writes with
characters Scala does not allow in an identifier is written in backticks, as in
`` api.`museum-hours` `` or `` api.users.`@me` ``.

The base URL comes from the specification's first server, with its `{variables}` at their
defaults. A specification whose server is relative (`/api/v3`), or which declares none, takes the
base from the caller, which a relative server URL then extends:

<!-- doccheck: skip -->
```scala
val api = Api(cp"/apis/petstore3.json", base = t"https://petstore3.swagger.io")
```

### Calling

An endpoint is invoked with its method, its query and header parameters as named arguments, and a
request body as a value; `call` executes the request and reads the response as the type asked
for — which must conform to the response schema the specification declares:

<!-- doccheck: skip -->
```scala
case class Pet(id: Long, name: Text, tag: Optional[Text] = Unset)

api.pets(42).get.call[Pet]()             // GET /pets/42, decoded per the schema
api.pets.get(limit = 10).call[List[Pet]]()
api.pets.post(newPet).call[Pet]()        // a typed request body
api.sessions(token).delete.call()        // 204, no body: Unit
api.pets(42).delete(api_key = t"…")      // a header parameter, sent as a header
```

Parameters declared on the path item apply to every operation on it, and one written as a
`$ref` into the document's components is followed, as are `$ref` responses and request bodies.
Where an operation declares several successful responses, `200` is preferred, then `201`, then
the lowest. Asking for a type the response schema does not support is a compile error; a
response outside the success range raises an `Api.Error` carrying the status.

A bare `call()` reads the response as the type its media type *construes* (below): a `Json` for
`application/json`, a `Raster in Png` for `image/png`, `Unit` where the operation declares no
body, and the raw `Http.Response` — with a compile-time warning — where nothing in scope
construes the media type.

### Media types and their carriers

The specification names each body's media type, and a `Construable` names the Soundness type
which carries that media type: `("application/json" is Construable to Json)`,
`("image/png" is Construable to (Raster in Png))`. The typeclass carries no operations — the
carrier's own reader and writer do the work — and every mapping is a contextual value in the
`construables` family, imported by name, so that nothing is construed without a deliberate
choice:

<!-- doccheck: skip -->
```scala
import construables.{jsonConstruable, pngConstruable, plainTextConstruable}

val ticket: Raster in Png = api.tickets(id).qr.get.call()
val label: Text = api.items(7).label.get.call()
```

A `Construable` is provided for JSON, XML (`application/xml` and `text/xml`), YAML, HTML, CSS,
SVG, Markdown, TEL, CSV, plain text, octet streams, form-encoded queries and the raster image
formats. A structured-syntax suffix falls back to the type it names, so `application/problem+json`
is construed by the JSON mapping. Where an operation offers several media types, the first with a
mapping in scope is chosen, `application/json` first.

A request body is encoded to the media type's carrier and written by that carrier's `Postable`,
which is likewise imported by name (`postables.jsonPostable`, `postables.xmlPostable`); a value
which already *is* the carrier — a `Query` for `application/x-www-form-urlencoded`, `Data` for
`application/octet-stream` — is sent as it is.

### Records from a response

A JSON response can be read without a mirror class at all: `record()` yields a
[record](records.md) typed by the response schema, with one member per property at the type the
schema declares, nested objects and references to component schemas as nested records, and an
array of objects as a `List` of them.

<!-- doccheck: skip -->
```scala
val pet = api.pet(42L).get.record()
pet.name                                // Text
pet.photoUrls                           // List[Text]
api.pet.findByStatus.get(status = t"sold").record().map(_.name)
```

`tuple()` reads the same schema as a named tuple, eagerly. Both need a JSON response whose schema
describes an object or an array of objects; anything else is a compile error suggesting
`call[T]()`.

### The document model

The specification itself is also a value: `OpenApi` decodes an OpenAPI 3.x document from JSON or
YAML into typed parts — info, servers (with their variables), paths, operations, parameters,
request bodies, responses and component schemas, parameters, responses and request bodies — for
tooling that inspects APIs rather than calling them. A YAML document is translated to JSON and
read by the same decoder, so the two forms cannot drift. A parameter, response or request body
written as a `$ref` is kept as an `OpenApi.Ref` and resolved on demand by `apply()`, given the
document; a schema reference resolves the same way, one hop at a time, so cyclic schemas stay
finite. A document of an unsupported version, or one that will not parse, raises an
`OpenApi.Error` naming the fault.

The model and the client are exercised against a corpus of real descriptions — the OpenAPI
Initiative's examples, Swagger's Petstore, Redocly's Museum, Twilio, Kubernetes, Discord, Box and
Stripe — checked in under `lib/apoplexy/res/test/openapi` with their provenance.

### Limitations

Cookie parameters, `callbacks`, `links`, `security` requirements, `webhooks` and references into
other documents are read into the model where the model has a place for them but are not acted
on by the client; a `multipart/form-data` body is not yet construed.
