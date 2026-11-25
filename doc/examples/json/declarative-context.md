# The layout of JSON is chosen by an import

Writing a value as JSON text is one expression, `in[Text over Json]`. Whether the text is
compact or indented is not an argument to it, and is decided by the formatting in scope.

```scala fuel
compile
alternative Compact
  highlight compactJsonFormatting  no whitespace
  code
      import formatting.compactJsonFormatting

      case class Player(name: Text, age: Int)

      val written: Text = Player(t"Feste", 30).in[Text over Json]
alternative Indented
  highlight indentedJsonFormatting  newlines and indentation
  code
      import formatting.indentedJsonFormatting

      case class Player(name: Text, age: Int)

      val written: Text = Player(t"Feste", 30).in[Text over Json]
##
```

[Formatting](../../soundness/modules/json.md#formatting) has the forms of output which deliver
a large document a block at a time.
