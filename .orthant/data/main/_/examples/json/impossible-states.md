# A path the schema does not allow

`verify` checks a document against the shape of a case class, once. The document it gives back
carries that shape in its type, so a field is a member, and a member the case class does not
have is not there to be selected.

```scala fuel
compile
frame
  label A field of a verified document
  highlight verify[Player]  checked once, as it runs
  code
      import strategies.throwUnsafely

      case class Player(name: Text, age: Int)

      def name(json: Json): Text = json.verify[Player].name.as[Text]
frame
  label A field the case class does not have
  errors show
  code
      import strategies.throwUnsafely

      case class Player(name: Text, age: Int)

      def patron(json: Json): Text = json.verify[Player].patron.as[Text]
frame
  label An index into a string
  errors show
  code
      import strategies.throwUnsafely

      case class Player(name: Text, age: Int)

      def initial(json: Json): Text = json.verify[Player].name(0).as[Text]
##
```

[Validation](../../soundness/modules/json.md#validation) follows a verified path through
nested objects and arrays.
