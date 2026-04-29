A JSON Schema is a JSON document which defines invariants of the structure of other documents, most notably
the set of names of properties that are to be expected in the top-level JSON object; the JSON type of each
one; and recursively, the same for every nested object and the elements of each array.

Here is an example, modified from [json-schema.org](https://json-schema.org/learn/miscellaneous-examples.html):
```json
{
  "$id": "https://example.com/person.schema.json",
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "title": "Person",
  "type": "object",
  "properties": {
    "name" {
      "type": "object",
      "properties": {
        "first": { "type": "string" },
        "last": { "type": "string" }
      }
    },
    "age": { "type": "integer" }
  }
}
```

In Scala, it might be natural to implement this data structure with case classes,
```scala
case class Person(name: Name, age: Int)
case class Name(first: String, last: String)
```
but for many cases, it may be sufficient to use this structural type,
```scala
type Person = {
  def name: { def first: String; def last: String }
  def age: Int
}
```
which does not require a corresponding class file to exist for the compiler to use it. (In fact, an instance
of the case class `Person` would conform to the structural type `Person` anyway.)

### Creating a schema object

We need to create an object, say, `PersonSchema`, which represents our schema. This _must_ be a singleton
object (and not a `val`), and should extend `JsonSchema`, which requires a single constructor parameter.

This constructor parameter may be any value that can be read as `Bytes`, so the following options would
all work, using appropriate [Turbulence](https://github.com/propensive/turbulence) `Readable` instances:
```scala
val schema: Text = t"""{...}"""
object PersonSchema extends JsonSchema(schema)
```
or, using [Ambience](https://github.com/propensive/ambience/) and [Galilei](https://github.com/propensive/galilei),
```scala
object PersonSchema extends JsonSchema(env.pwd / p"data" / p"schema.json")
```
or, using [Telekinesis](https://github.com/propensive/telekinesis/),
```scala
object PersonSchema extends JsonSchema(url"https://example.com/schemata/person.jsons")
```

This object must be compiled before any code which uses it to create records. It should be sufficient to put it into
a separate source file, but it can also be compiled in a separate build module.

This is because the compiler needs to instantiate it at the time it compiles the code which constructs
records. It will therefore also need to execute the code which provides the JSON schema data, so in the file-based
example above, the file `schema.json` must be in the right place relative to the PWD *at compiletime*.

Furthermore, any exceptions that may be thrown during construction must be neutralized.

### Constructing the record

Given a `JsonSchema` object instance corresponding to our particular schema, _Villainy_
makes it possible to construct a new record instance from an untyped `Json` value with a structural type
derived from the JSON schema above, *without* needing to define the type for that schema:
```scala
import jacinta.*
import villainy.*

val json: Json = Json.parse(t"""{ "name": { "first": "Douglas", "last": "Richards" }, "age": 22 }""")
val person = PersonSchema.record(json)
```

We can then safely access fields such as, `person.name.first` (a `String`) and `person.age` (an `Int`), but
attempts to access fields not defined in the schema, such as `person.lastName`, will be compile errors.


