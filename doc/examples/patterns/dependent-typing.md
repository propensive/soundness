# A capture whose type is read from the pattern

The type of a bound variable follows from what it captures. One character is a `Char`, a
repeated one is a `List`, and an optional one is an `Optional`, so the way a capture is
written and the way it is used cannot drift apart.

```scala fuel
compile
frame
  label One digit
  highlight Char  one character
  code
      def scene(text: Text): Char = text match
        case r"scene $number\d" => number
        case _                  => '0'
frame
  label A quantifier the signature has not followed
  errors show
  code
      def scene(text: Text): Char = text match
        case r"scene $number\d+" => number
        case _                   => '0'
frame
  label One or more digits
  highlight List[Char]  + binds a list
  code
      def scene(text: Text): List[Char] = text match
        case r"scene $number\d+" => number
        case _                   => Nil
frame
  label A digit, or none
  highlight Optional[Char]  ? binds an optional value
  code
      def scene(text: Text): Optional[Char] = text match
        case r"scene $number\d?" => number
        case _                   => Unset
##
```

[Characters and quantifiers](../../soundness/modules/patterns.md#characters-and-quantifiers)
has the table of captures and their types.
