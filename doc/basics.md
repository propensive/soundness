### Constructing a new UUID

A UUID can be constructed with the `Uuid()` factory method. This will create a
new, and by definition, universally unique, identifier. The `Uuid` instance is
composed of two 64-bit longs, `msb` (for "most significant bits") and `lsb`
("least significant bits"), implying (in theory) 128 bits of entropy.

#### Specific UUIDs

A particular UUID, for example `e6388c03-3dd2-4044-bb38-e58dbf8368fd`, may be
constructed using the `uuid""` interpolator, like so,
```scala
val uuid = uuid"e6388c03-3dd2-4044-bb38-e58dbf8368fd"
```
which will parse (at compiletime) the UUID hex digits and their format
ensuring, in particular, that all are present to represent 128 bits of data.

Additionally, a `Uuid` can be created at runtime with,
```scala
val uuid = Uuid(value)
```
which will parse the `Text` value, `value`, raising a `UuidError` if it is not
in the correct format.

#### Methods on `Uuid`s

Two convenience methods are provided on `Uuid`s:
- the unary `~` operator, which will construct a new `Uuid` by inverting its bits, and
- the binary `^` operator, which will combine two `Uuid`s by XORing their bits
