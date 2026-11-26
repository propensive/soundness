# A field which may not be there

A field of type `Optional[Text]` may be missing from the JSON. An optional value is not a
`Text`, and cannot be used as one until the code has said what absence means.

```scala fuel
compile
frame
  label An optional value used as though it were present
  errors show
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      case class Player(name: Text, patron: Optional[Text])

      val feste = t"""{"name": "Feste"}""".read[Json].as[Player]
      val patron: Text = feste.patron
frame
  label An alternative, for when it is absent
  highlight .or(t"nobody")
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      case class Player(name: Text, patron: Optional[Text])

      val feste = t"""{"name": "Feste"}""".read[Json].as[Player]
      val patron: Text = feste.patron.or(t"nobody")
##
```

[Optional fields and defaults](../../soundness/modules/json.md#optional-fields-and-defaults)
has the policies for `null`, and [optionality](../../philosophy/optionality.md) the case
against `Option`.
