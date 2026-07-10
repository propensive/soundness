## OpenAPI

### About

An [OpenAPI](https://www.openapis.org/) document describes a REST API — its paths, parameters,
request and response schemas — and Soundness turns that description into a *typed client* as the
code compiles. Pointing `Api` at a specification yields a value whose navigation mirrors the API's
paths: each segment, each path parameter, each query parameter and each response type is checked
against the specification, so a call the API does not offer, or a parameter of the wrong type,
does not compile.

### On API specifications

An OpenAPI document is a machine-readable contract, and the usual way to honour it is code
generation: a build step emits a client, which is compiled, versioned and kept in sync by
tooling. The contract is enforced, but at the price of generated sources and a build pipeline —
and when the generator is skipped, calls are made against remembered URLs and hoped-for schemas.

Soundness reads the specification during compilation instead. There is no generated code to
maintain; the client *is* the specification, interpreted by the compiler, and a drift between
code and contract is a compile error in the code. Everything comes from the `soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
import internetAccess.online
```

### A typed client

`Api` reads a specification from the classpath — JSON or YAML — and the resulting value navigates
by path:

```scala
val api = Api(cp"/apis/petstore.json")

api.pets           // the /pets path
api.pets(42)       // /pets/{petId}, the parameter typed by the spec
api.unicorns       // does not compile: no such path
```

A path parameter of the wrong type — text where the specification says integer — is likewise a
compile error, caught where the call is written.

### Calling

An endpoint is invoked with its method, query parameters as named arguments, and a request body as
a value; `call` executes the request and decodes the response as the type asked for — which must
conform to the response schema the specification declares:

```scala
case class Pet(id: Int, name: Text, tag: Optional[Text] = Unset)

api.pets(42).get.call[Pet]()             // GET /pets/42, decoded per the schema
api.pets.get(limit = 10).call[List[Pet]]()
api.pets.post(newPet).call[Pet]()        // a typed request body
api.sessions(token).delete.call[Unit]()  // 204, no body
```

Asking for a type the response schema does not support is a compile error; a response outside the
success range raises an `ApiError` carrying the status.

### Wire formats

The specification decides the wire format: an API declaring JSON content is an `Api over Json`,
one declaring XML an `Api over Xml`, and the request and response bodies encode and decode through
the corresponding [format support](json.md) — with the right `Accept` header sent, derived rather
than remembered.

### The document model

The specification itself is also a value: `OpenApi` decodes an OpenAPI 3.x document from JSON or
YAML into typed parts — info, servers, paths, operations, parameters, component schemas — for
tooling that inspects APIs rather than calling them. A document of an unsupported version, or one
that will not parse, raises an `OpenApiError` naming the fault.
