# A short record, handled as the caller chooses

`unpack` raises one error, when the bytes run out before the record does. It does not decide
what follows. The code around it does, and each choice gives the result a different type.

```scala fuel
compile
alternative An absent value
  highlight safely  `Unset` for a short record
  code
      import byteOrders.bigEndianByteOrder

      case class Size(width: Short, height: Short)

      object Size:
        given structural: (Size is Structural) = Structural.derived

      def size(data: Data): Optional[Size] = safely(data.unpack[Size](0))
alternative A replacement
  highlight recover  a default in place of the error
  code
      import byteOrders.bigEndianByteOrder

      case class Size(width: Short, height: Short)

      object Size:
        given structural: (Size is Structural) = Structural.derived

      def size(data: Data): Size =
        recover:
          case error: Structural.Error => Size(0, 0)
        . protect(data.unpack[Size](0))
alternative The caller's decision
  highlight raises..Structural.Error  passed on, in the type
  code
      import byteOrders.bigEndianByteOrder

      case class Size(width: Short, height: Short)

      object Size:
        given structural: (Size is Structural) = Structural.derived

      def size(data: Data): Size raises Structural.Error = data.unpack[Size](0)
##
```

[Reading](../../soundness/modules/binary-data.md#reading) says what the error holds, and
[choosing a strategy](../../soundness/modules/errors.md#choosing-a-strategy) lists the rest.
