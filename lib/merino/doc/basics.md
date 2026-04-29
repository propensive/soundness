Merino will parse a `DataStream`, that is, a `LazyList[IArray[Byte]]`. This
makes it easy to parse any type which can be converted to a byte array or a
lazy stream.

The `JsonAst.parse` method will parse the input and return a `JsonAst`, which
is an opaque type alias for the union of the following types:
- `Long`, `Double` and `BigDecimal`, representing JSON number types,
- `String` for JSON strings,
- `Boolean` for JSON `true` and `false` values,
- `Null` for the `null` value,
- `IArray[Any]` representing a JSON array, and,
- `(IArray[String], IArray[Any])` representing a JSON object

Note that which type of `Long`, `Double` or `BigDecimal` the parser chooses to
represent a given number will be determined by whether the type can represent
the number, as specified in the JSON source, precisely.

The types `IArray[Any]` and `(IArray[String], IArray[Any])`, representing
arrays and objects, will only ever contain `JsonAst`-typed values, despite
having type parameters of `Any`. But type aliases cannot refer to themselves,
so `Any` is used instead.

Additionally, the type `(IArray[String], IArray[Any])` was chosen as an
alternative to `IArray[(String, Any)]` since the former requires `2n + 3`
objects to be constructed for each field in the JSON object, as opposed to
`3n + 1` in the latter case: for anything but an object of exactly one key, the
former requires fewer objects to be created. Additionally, the types may be
disambiguated reflectively by their erased types.

If parsing fails, a `JsonParseError` will be thrown, including the line and
column in which the error occurs, and an enumeration value (of type
`JsonParseError.Issue`) describing the error.




