# Text which may not be a valid name

Text which arrives at runtime becomes a `Name` only by passing validation, and validation can
fail. A method which constructs a name from text says so in its type, or does not compile.

```scala fuel
compile
frame
  label A method which does not admit that validation may fail
  errors show
  code
      def anchor(text: Text): Name[DomId] = Name[DomId](text)
frame
  label The failure, in the return type
  highlight raises..Name.Error  the caller must handle it
  code
      def anchor(text: Text): Name[DomId] raises Name.Error = Name[DomId](text)
##
```

[Validating at runtime](../../soundness/modules/names.md#validating-at-runtime) shows the error
which is raised.
