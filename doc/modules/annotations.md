## Annotations

### About

Scala's
[static annotations](https://docs.scala-lang.org/scala3/reference/other-new-features/annotations.html)
attach metadata to types and fields. The language offers no ordinary way to read them back, and
doing so normally means writing a macro.

Here they are read through a [typeclass]. Summoning `Annotated` for a type gives the annotations
on it, on its fields, or on the variants of a sealed hierarchy. The answer is resolved at
[compiletime], with no reflection at [runtime], and the consumer writes no macro.

The `@name` annotation, which renames fields in every serialization format, is defined here and
read by this means.

### On annotations

An annotation is where some metadata belongs. A wire name sits on the field it renames, and a
marker on the case it distinguishes. The annotation is beside the thing it describes, and
travels with it.

Annotations are underused in Scala because they are hard to read. Runtime reflection is slow and
untyped, and access at compiletime needs expertise with quotes and splices.

The `Annotated` typeclass hides that machinery. A request says what is wanted: which type of
annotation, on which field, of which type. The compiler supplies the answer as a value. A
misspelled field name or a wrong type is a compile error, which is
[safety by construction](../philosophy/safety-by-construction.md).

Everything comes from the `soundness` package, and the second import lets the samples print
their results:

```scala
import soundness.*
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Reading annotations

An annotation is an ordinary case class which extends `StaticAnnotation`. The samples use three
of them, and some definitions which carry them:

```scala
final case class ident() extends StaticAnnotation
final case class primary() extends StaticAnnotation
final case class number(number: Int) extends StaticAnnotation

case class Person(name: Text, @ident email: Text)
case class Employee(person: Person, @ident @primary code: Long)

@number(10)
case class Company(name: Text)
```

The annotations of a field are summoned by naming the type and the field:

```scala fuel
run
##
Out.println(summon[Employee is Annotated on "code"]().inspect)
```

`by` narrows the query to one type of annotation:

```scala fuel
run
##
Out.println(summon[Employee is Annotated by ident on "code"]().inspect)
```

The plain form gives the annotations on the type itself:

```scala fuel
run
##
Out.println(summon[Company is Annotated]().inspect)
```

`fields` gives every annotated field at once. A field with no annotation of the requested type
is absent from the map:

```scala fuel
run
##
Out.println(summon[Employee is Annotated by ident].fields.inspect)
```

The answer is computed as the code compiles, and is a constant in the compiled program. A field
with no annotations has an empty set, and so does a name which is not a field at all:

```scala fuel
run
##
Out.println(summon[Employee is Annotated on "person"]().inspect)
Out.println(summon[Employee is Annotated on "salary"]().inspect)
```

### Finding the annotated field

Often the question runs the other way: *which* field carries the annotation? When exactly one
field does, the summoned instance knows its name:

```scala fuel
run
##
Out.println(summon[Employee is Annotated by primary].field)
```

Finding the field is usually the first step towards reading or writing it, so the instance also
offers a [lens](optics.md#lenses-as-values) onto it:

```scala
val person = Person(t"Jack", t"jack@example.com")
```

```scala fuel
run
##
val identity = summon[Person is Annotated by ident]

Out.println(identity.lens(person))
Out.println(identity.lens.update(person, t"jill@example.com").inspect)
```

This is how a serialization library finds "the field marked as the identifier" without being
told its name. If more than one field carries the annotation, `field` is not available, because
the type of the instance records whether the match was unique.

### Variants of a sealed type

The cases of an [enumeration] or a sealed trait are queried with `under`. The result maps the
name of each variant to its annotations:

```scala
sealed trait Colored

@number(3)
case class Hsv(hue: Double, saturation: Double, @ident value: Double) extends Colored

@primary()
case class Rgb(red: Int, green: Int, blue: Int) extends Colored
```

```scala fuel
run
##
Out.println(summon[Annotated under Colored].subtypes.inspect)
```

These are the forms of query, and what each gives:

```tabular
column  Query form
column  Member
column  Returns
row  `T is Annotated`  `apply()`  the annotations on the type `T`
row  `T is Annotated on "f"`  `apply()`  the annotations on the field `f`
row  `T is Annotated by A on "f"`  `apply()`  the annotations of type `A` on the field `f`
row  `T is Annotated by A`  `fields`  each field to its annotations of type `A`
row  `T is Annotated by A`, one field matching  `field`, `lens`  the field's name, and a lens onto it
row  `Annotated under T`  `subtypes`  each variant of `T` to its annotations
```

### Parameterized annotations

An annotation may take a type argument, and a query can filter by it:

```scala
final case class marker[topic](id: Int) extends StaticAnnotation

case class Marked(@marker[Person](1) @marker[Company](2) one: Int, @marker[Company](3) two: Int)
```

```scala fuel
run
##
Out.println(summon[Marked is Annotated by marker[Person]].fields.inspect)
Out.println(summon[Marked is Annotated by marker[Company]].fields.inspect)
```

The field `two` carries a marker, but none for `Person`, so the first query gives it an empty
set.

A bare `@marker(1)` is inferred as `marker[Any]`. A query for `marker[Any]` sees it, and a query
for `marker[Person]` does not.

### The `@name` annotation

The `@name` annotation is defined here, and honored by every serialization format. It renames a
field or a variant on the wire. Bare, it applies to all formats. With a type argument, it
applies to one:

```scala
case class Record(@name[Json](t"first_name") firstName: Text, @name(t"yob") year: Int)
```

The [derivation] of a format reads these through `relabelling`. It maps each renamed field to
its serialized name, and a rename for one format overrides the bare default:

```scala fuel
run
##
Out.println(relabelling[Record, Json].inspect)
Out.println(relabelling[Record, Xml].inspect)
```

That is why one annotation serves [JSON](json.md#renaming-fields), [XML](xml.md),
[YAML](yaml.md) and the rest alike.

Four functions read annotations for a derivation. In each, the map holds only the fields or
variants which carry the annotation in question:

```tabular
column  Function
column  Reads
column  Returns
row  `relabelling[T, F]`  `@name` on the fields of `T`  each renamed field to its name in `F`
row  `variantRelabelling[T, F]`  `@name` on the variants of `T`  each renamed variant to its name in `F`
row  `fieldAnnotations[T, A]`  annotations of type `A` on fields  each field to its set of `A`
row  `subtypeAnnotations[T, A]`  annotations of type `A` on variants  each variant to its set of `A`
```

The last two are the general forms behind the first two.

### Reaching fields by name

The fields of a case class can also be reached by name at runtime. The mapping is generated as
the code compiles, and not by reflection.

A `Dereferenceable` gives the names of the fields, the value of one field by name, and the whole
record as a map. The instance is parameterized by the type which the fields yield. A record
whose fields are all `Int` dereferences to `Int`. Any record dereferences to `Any`, which covers
every field.

```scala
case class Letters(alpha: Int, beta: Int, gamma: Int)
val letters = Letters(1, 2, 3)
val fields = summon[Letters is Dereferenceable to Int]
```

```scala fuel
run
##
Out.println(fields.names(letters).inspect)
Out.println(fields.select(letters, t"beta").inspect)
Out.println(fields.members(letters).inspect)
```

The names and members come from a map, and are not in the order of declaration.

`membersOfType` picks the values of one type out of any record. A generic renderer or a template
engine can use it to read the fields of a value without casting:

```scala fuel
run
##
Out.println(letters.membersOfType[Int].inspect)
```

Each accessor is a [lens](optics.md), so a field found by name can be written as well as read.
`update` replaces the named field, and `modify` applies a function to it. Each gives a new
record, or `Unset` where the name is not a constructor parameter:

```scala fuel
run
##
Out.println(fields.update(letters, t"beta", 20).inspect)
Out.println(fields.modify(letters, t"gamma")(_*10).inspect)
Out.println(fields.update(letters, t"epsilon", 5).inspect)
```

A field is writable only where its type accepts every value which the instance yields. The
fields of a record dereferenced `to Any` can be read, but not written, since an `Any` cannot be
put into a `Long`:

```scala
val anything = summon[Employee is Dereferenceable to Any]
```

```scala fuel
run
##
Out.println(anything.select(Employee(person, 7L), t"code").toString.tt)
Out.println(anything.lens(t"code").absent.inspect)
```

These are the members of a `Dereferenceable`, for a record of type `T` dereferenced `to R`:

```tabular
column  Member
column  Returns
row  `names(record)`  the field names
row  `select(record, name)`  the named field's value, as an `R`
row  `members(record)`  each field name to its value
row  `lens(name)`  a lens onto the named field, or `Unset`
row  `update(record, name, value)`  a new `T` with the field replaced, or `Unset`
row  `modify(record, name)(lambda)`  a new `T` with the function applied to the field, or `Unset`
```

The mapping is a compiled table of accessors. Reaching a field costs a method call, and a name
which the type does not have is discovered where the table is built, not by a reflective lookup
which fails at runtime.

### Related

- [JSON](json.md#renaming-fields), [XML](xml.md), [YAML](yaml.md) and the other formats honor
  `@name`.
- [Optics](optics.md) describes the lenses which the annotated field and the named fields are
  reached through.
- [Derivation](derivation.md) describes the generated typeclass instances which consult
  annotations.
- [Metaprogramming](metaprogramming.md) describes the macros which this typeclass makes
  unnecessary.
- [CBOR](cbor.md), [Protobuf](protobuf.md), [databases](database.md) and [MCP](mcp.md) read annotations on fields and methods.
