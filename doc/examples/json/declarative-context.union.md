# Three conventions for a tagged union

An enumeration is written as JSON with a tag which names its case. Where the tag goes is a
convention, and a given chooses it. The enumeration and the code which encodes it stay the same.

```scala fuel
compile
alternative A field beside the others
  highlight jsonByKindDiscriminable  a field called kind
  code
      import discriminables.jsonByKindDiscriminable

      enum Exit:
        case Pursued(by: Text)
        case Alone

      val exit: Json = (Exit.Pursued(t"a bear"): Exit).in[Json]
alternative A wrapper
  highlight Json.DiscriminantWrapper()  an object with one key
  code
      enum Exit:
        case Pursued(by: Text)
        case Alone

      given Exit is Discriminable in Json = Json.DiscriminantWrapper()

      val exit: Json = (Exit.Pursued(t"a bear"): Exit).in[Json]
alternative An envelope
  highlight Json.DiscriminantEnvelope..)  one field for the tag, one for the value
  code
      enum Exit:
        case Pursued(by: Text)
        case Alone

      given Exit is Discriminable in Json = Json.DiscriminantEnvelope(t"type", t"value")

      val exit: Json = (Exit.Pursued(t"a bear"): Exit).in[Json]
##
```

The three write `{"by":"a bear","kind":"Pursued"}`, `{"Pursued":{"by":"a bear"}}` and
`{"type":"Pursued","value":{"by":"a bear"}}`.
[Tagged unions](../../soundness/modules/json.md#tagged-unions) describes each convention.
