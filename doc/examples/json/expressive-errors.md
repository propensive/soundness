# A field which cannot be decoded

A decoder is derived field by field. When one field's type has no decoder, the compiler names
the field, the case class which holds it and the type at fault, and says what would do instead.

```scala fuel
compile
frame
  label A field of an ordinary class
  errors show
  code
      import strategies.throwUnsafely

      class Poison(val name: Text)
      case class Vial(label: Text, contents: Poison)

      def vial(json: Json): Vial = json.as[Vial]
frame
  label A field of a case class
  highlight case..Poison  now derivable
  code
      import strategies.throwUnsafely

      case class Poison(name: Text)
      case class Vial(label: Text, contents: Poison)

      def vial(json: Json): Vial = json.as[Vial]
##
```

[Case classes and enumerations](../../soundness/modules/json.md#case-classes-and-enumerations)
says which types a field may have, and
[expressive errors](../../philosophy/expressive-errors.md) why the message is worded so.
