### Creating a new Mutex

A new mutex, an instance of type `Mutex[ValueType]` for some choice of
`ValueTyp`, can be constructed by supplying its initial value to the `Mutex`
factory method, like so:
```scala
val count: Mutex[Int] = Mutex(32)
```

This represents a mutex variable, which is set to `32`. Any thread which has a
reference to `count` may read or modify this variable, but only in delimited
blocks.

We can read the value only through a lambda applied to the `read` method of
`Mutex`, which provides a reference for accessing the value.

