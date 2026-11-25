# Member access is a permission

The compiler does not know the fields of a parsed document, so `fool.name` could only be
checked at runtime. It is not allowed until the code asks for it, and `dynamically[Json]`
grants it for one block and one format.

```scala fuel
compile
frame
  label A member of an unchecked document
  errors show
  code
      import strategies.throwUnsafely

      def name(fool: Json): Text = fool.name.as[Text]
frame
  label The same member, with permission
  highlight dynamically[Json]  permission for this block
  code
      import strategies.throwUnsafely

      def name(fool: Json): Text =
        dynamically[Json]:
          fool.name.as[Text]
##
```

[Fields as members](../../soundness/modules/json.md#fields-as-members) has the import which
grants the same permission to a whole scope.
