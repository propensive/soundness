# A path parameter of the specification's type

The specification gives each path parameter a type, and the client takes it at that type. A
pet is identified by an integer and a session by text, and neither can be asked for with the
other.

```scala fuel
compile
frame
  label A pet, by name
  errors show
  code
      val api = Api(cp"/openapi/local/petstore.json")

      val pet = api.pets(t"Crab")
frame
  label A pet, by number
  highlight 42  petId is an integer
  code
      val api = Api(cp"/openapi/local/petstore.json")

      val pet = api.pets(42)
frame
  label A session, by its token
  highlight t"tybalt"  token is a string
  code
      val api = Api(cp"/openapi/local/petstore.json")

      val pet = api.pets(42)
      val session = api.sessions(t"tybalt")
##
```

The example is compiled, and is not run.
[A typed client](../../soundness/modules/openapi.md#a-typed-client) has the rules for each
type, and [impossible states](../../philosophy/impossible-states.md) the principle.
