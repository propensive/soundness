### The `Optional` type

An optional value, which might be an instance of `ValueType`, or may be
_absent_, may be given the type `Optional[ValueType]`. If it is absent, then it
has the value, `Unset`, which is a singleton object. `Optional[ValueType]` is
an alias for the union type, `ValueType | Unset.type`.

Note that the declarations,
```scala
val value: Text = t"Hello world"
```
and,
```scala
val value: Optional[Text] = t"Hello world"
```
differ only in their types; the syntax of the expression is identical, and does
not need to be wrapped with another factory method, like `Some`.

