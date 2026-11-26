# One parser, and the caller's choice of failure

`Uuid.parse` raises a `Uuid.Error` and does not say what becomes of it. The code around it
does, and the same call serves a caller who wants the error, one who wants a default, and one
who wants an absent value.

```scala fuel
compile
alternative Declared
  highlight raises..Uuid.Error  the caller must answer for it
  code
      def session(header: Text): Uuid raises Uuid.Error =
        Uuid.parse(header)
alternative Replaced
  highlight recover  the nil identifier stands in
  code
      def session(header: Text): Uuid =
        recover:
          case Uuid.Error(_) => uuid"00000000-0000-0000-0000-000000000000"
        . protect(Uuid.parse(header))
alternative Absent
  highlight safely  a failure becomes `Unset`
  code
      def session(header: Text): Optional[Uuid] =
        safely(Uuid.parse(header))
##
```

[Choosing a strategy](../../soundness/modules/errors.md#choosing-a-strategy) has the full set
of choices.
