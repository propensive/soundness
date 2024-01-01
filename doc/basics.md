Cellulose provides facilities for constructing, parsing, accessing,
manipulating and serializing CoDL data.

### Constructing

The easiest way to construct a new CoDL literal value is with an interpolated
`codl""` string, for example,
```scala
val codl: Codl = codl"""
  item 1
    size  1.08m
    mass  40kg
"""
```

Like any CoDL document, any amount of margin may be used as long is it is
consistent.

The CoDL literal will be parsed at compiletime, and any mistakes will present
themselves as compile errors. But the document will only be checked against the
"free" schema.

### Parsing

Text can be parsed as untyped CoDL at runtime with the `Codl.parse` method.
This currently accepts a `Reader`, but later versions will accept a
`DataStream`, i.e. a `LazyList[IArray[Byte]]`, and will be able to parse
directly from any streamable source.

Given a CoDL schema, it's possible to parse, checking for conformance at the same time, with,
```scala
schema.parse(input)
```

Both methods will produce a `CodlDoc` instance, which will include the schema
that checked it.

### Model

The CoDL document model accommodates nodes which may contain comments, data (as
parameters or child nodes) and remarks (comments at the ends of lines).

### Accessing values

A common way to work with CoDL data is to convert it to a case class structure.
This requires a `Codec` instance. `Codec`s are defined for simple types like
primitives and `Text`, and will be derived for product compositions of these
types on demand.

A `CodlDoc` can be converted to another type with the `CodlDoc#as` method, for example:
```scala
schema.parse(input).as[Person]
```

The exceptions which should be handled from such a call will depend on the
definition of `Person`, but the exceptions will be statically known.

### Serialization

Likewise, any type which has a `Codec` instance can be serialized to CoDL, with the `codl` extension method, for example,
```scala
val personCodl: CodlDoc =
  Person(t"Timothy", 28, TeamRef(t"Zeta")).codl
```

This can then be serialized to `Text` with the `Codl#serialize` method:
```scala
val txt = personCodl.serialize
```


