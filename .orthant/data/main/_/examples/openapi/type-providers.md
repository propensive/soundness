# A client which is the specification

`Api` reads an OpenAPI document as the code compiles, and the value it gives is navigated by
the paths the document declares. Nothing is generated, and a path which the document does not
have is not a member.

```scala fuel
compile
frame
  label The specification
  code
      val api = Api(cp"/openapi/local/petstore.json")
frame
  label A path
  highlight api.pets  the path /pets
  code
      val api = Api(cp"/openapi/local/petstore.json")

      val pets = api.pets
frame
  label A path with a parameter
  highlight api.pets(42).photos  the path /pets/{petId}/photos
  code
      val api = Api(cp"/openapi/local/petstore.json")

      val pets = api.pets
      val photos = api.pets(42).photos
frame
  label A path which the specification does not have
  errors show
  code
      val api = Api(cp"/openapi/local/petstore.json")

      val pets = api.pets
      val photos = api.pets(42).photos
      val unicorns = api.unicorns
##
```

The example is compiled, and is not run. [A typed client](../../soundness/modules/openapi.md#a-typed-client)
says where the base URL comes from.
