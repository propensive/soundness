## Generic Data

### About

A value sometimes has to leave the world where its class is known, and arrive intact: in
another classloader, in another process, or across a staging boundary. The `Pojo`
representation carries any Scala value using only the types which every JVM context shares.
Those are boxed primitives, strings, and arrays of objects.

A case class or an enumeration converts to a `Pojo` and back with [derived][derivation] codecs,
so the value survives the crossing without its class traveling with it.

### On classloader-neutral data

A Scala object is welded to its class, and its class to the classloader which defined it. Hand
the object to code which was loaded elsewhere, such as an isolated plugin or a computation
staged on another machine, and the receiving side cannot cast it back. The "same" class from
another loader is a different class.

Full serialization solves this with heavy machinery. Passing strings solves it by giving up
structure.

A `Pojo` keeps the structure and drops the class. A product becomes an array of its fields, a
variant becomes a labeled pair, and a primitive becomes its boxed self. All of these are types
defined by the JDK, and mean the same on both sides of any boundary. This is the transport
beneath [staged](staging.md) remote execution, and working on a generic representation in this
way is what [composability](../philosophy/composability.md) between formats rests on.

Everything comes from the `soundness` package. The samples throw any error as an exception, and
the last import lets them print their results:

```scala
import soundness.*
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Encoding and decoding

`pojo` encodes a value. The result holds only arrays, strings and boxed primitives:

```scala
case class Person(name: Text, age: Int)
case class Group(persons: List[Person], size: Int)

val group = Group(List(Person(t"John", 30), Person(t"Jane", 25)), 2)

val transportable = group.pojo
```

`as` decodes it. The receiving side must state the type it expects, and the data may not match
that type, so decoding can fail, and needs a [strategy]:

```scala fuel
run
##
Out.println(transportable.as[Group].inspect)
```

Data which is not shaped like the expected type raises a `Pojo.Error`. A string, for
instance, is not a `Person`:

```scala fuel
run
errors show
##
t"John".pojo.as[Person]
```

Nested case classes, collections and enumerations all derive their conversions, so any value
which is shaped like data crosses without preparation.

### The representation

The encoding is deliberately transparent:

```tabular
column  Scala value
column  Pojo form
row  `Text` or `String`  a `String`
row  `Int`, `Long`, `Float`, `Double`, `Char`, `Boolean` or `Byte`  its boxed `java.lang` form
row  a collection: `List`, `Set`, `Sequence` or other `Iterable`  an `Array[Object]` of its elements
row  a case class  an `Array[Object]` of its fields in declaration order
row  an enumeration case  an `Array[Object]` pair of its name and its payload
```

So `Person(t"John", 30).pojo` is an array of the string `"John"` and the boxed integer `30`,
and the `Green` case of an enumeration is an array of the string `"Green"` and an empty array:

```scala
enum Light:
  case Red, Amber, Green

val person = Person(t"John", 30).pojo
val light = (Light.Green: Light).pojo
```

Both sides derive the codec from the same type definition, so the encoding needs no schema on
the wire. It does mean that both sides must agree on that definition.

`Pojo` is an [opaque type] over the JDK types it permits. A `Pojo` cannot be confused with an
arbitrary `Object`, and the only way to produce one is through an encoder.

### Where it is used

The obvious case is a boundary between classloaders. Code which is loaded in an isolated
loader, such as a plugin, a compiled fragment or a sandboxed extension, shares the JDK's
classes with its host, and nothing else. The JDK's classes are therefore exactly the vocabulary
the two can speak. [Classpath](classpath.md) describes classloaders.

The same reasoning applies across a boundary between processes, which is what
[staged](staging.md) remote execution uses. The arguments to a staged computation are encoded
on one side, sent, and decoded on the far side against the type which the staged code expects.

Nothing about the representation is specific to either use. Any boundary which can carry boxed
primitives, strings and arrays of objects can carry a Scala value.

### Related

- [Derivation](derivation.md) explains the engine which generates the codecs.
