# Text which may not be an identifier

Text which arrives at runtime may not be a UUID, and `Uuid.parse` says so in its type. A
method which calls it, and neither handles a `Uuid.Error` nor declares one, does not compile.

```scala fuel
compile
frame
  label A failure nobody answers for
  errors show
  code
      def session(header: Text): Uuid = Uuid.parse(header)
frame
  label The failure, in the return type
  highlight raises..Uuid.Error  the caller must answer for it
  code
      def session(header: Text): Uuid raises Uuid.Error = Uuid.parse(header)
##
```

[Parsing](../../soundness/modules/uuid.md#parsing) shows the error as it is raised, and
[error handling](../../philosophy/error-handling.md) what `raises` means.
