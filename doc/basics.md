_Cardinality_ provides a representation of numbers which must lie within a certain (closed) range. A range type is written with the
infix `~` type operator, between two doubles, for example, `-1.0 ~ 1.0` represents a `Double` which is at least `-1.0` and at most
`1.0`.

Compiletime operations check `Double` literals for conformance to the claimed bounds, for example:
```scala
val x: 0.0 ~ 100.0 = 33.3 // good
val y: 0.0 ~ 1.0 = 2.0    // compile error
```

Standard arithmetic operations are also implemented on ranged `Double`s. Depending on whether the right-hand operand is a statically-unknown
`Double`, a `Double` singleton literal, or a ranged `Double`, the result will be typed as precisely as possible. For example, adding `10.0` to
an instance of `3.0 ~ 5.0` will produce a result of type, `13.0 ~ 15.0`. These operations use typelevel arithmetic to calculate the resultant
range of the calculation, and can be composed like other arithmetic functions, with the return type inferred. For example,
```scala
var x: 0.0 ~ 1.0 = 0.2
var y: -1.0 ~ 1.0 = 0.2
var z: 1e3 ~ 1e8 = 10000

val result = (x + y*3.0)*z
```
will infer the type of `result` to be `-2.0e8 ~ 3.0e8` (while its value will be `6000.0`.

### Forcing values

Unranged `Double`s are pervasive in Scala, so a `Double#force` extension method is provided which can be used (carefully) to convert a `Double`
to an expected ranged type.


