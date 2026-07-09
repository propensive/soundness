## Generic Data

### About

A value sometimes has to leave the world where its class is known — crossing into another
classloader, another process, or a staging boundary — and arrive intact. The `Pojo` representation
carries any Scala value using only the types every JVM context shares: boxed primitives, strings,
and arrays of objects. A case class or enumeration converts to a `Pojo` and back with derived
codecs, so the value survives the crossing without its class travelling with it.

### On classloader-neutral data

A Scala object is welded to its class, and its class to the classloader that defined it. Hand the
object to code loaded elsewhere — an isolated plugin, a remotely staged computation — and the
receiving side cannot cast it back: the "same" class from another loader is a different class.
Full serialization solves this with heavy machinery; passing strings solves it by giving up
structure.

A `Pojo` keeps the structure and drops the class. A product becomes an array of its fields, a
variant a labelled pair, a primitive its boxed self — all types defined by the JDK itself, equally
meaningful on both sides of any boundary. This is the transport beneath [staged](staging.md)
remote execution. Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Encoding and decoding

A value encodes with `pojo`, and decodes back with `decode` — fallibly, since the receiving side
must state the type it expects, and the data may not match:

```scala
case class Person(name: Text, age: Int)
case class Group(persons: List[Person], size: Int)

val group = Group(List(Person(t"John", 30), Person(t"Jane", 25)), 2)

val transportable = group.pojo         // arrays, strings and boxes only
safely(transportable.decode[Group])    // the original Group, on the other side
```

Nested case classes, collections and enumerations all derive their conversions, so any data-shaped
value crosses without preparation.

### The representation

The encoding is deliberately transparent: a case class is an `Array[Object]` of its fields in
declaration order, an enumeration case is a pair of its name and its payload, and primitives are
their boxed forms:

```scala
Person(t"John", 30).pojo    // Array("John", 30)
(Color.Green: Color).pojo   // Array("Green", Array())
```

Because both sides derive the codec from the same type definition, the encoding needs no schema on
the wire — but it also means both sides must agree on that definition, and a mismatch surfaces as
a typed `PojoError` when decoding, not as silent corruption.
