# No absence inside a presence

An `Option` of an `Option` has two ways of being empty, and code must decide what each means.
An `Optional` is a union with `Unset`, and a union of a type with itself is that type, so an
optional optional value is just an optional value.

```scala fuel
compile
frame
  label Two kinds of nothing
  highlight Some(None)  present, and yet absent
  code
      val reply: Option[Option[Text]] = Some(None)
frame
  label One kind of nothing
  highlight Unset  the only absence there is
  code
      val reply: Optional[Optional[Text]] = Unset
frame
  label The two types are one
  highlight Optional[Text]  the same type, with no conversion
    line 2
  code
      val reply: Optional[Optional[Text]] = Unset
      val same: Optional[Text] = reply
##
```

[On absence](../../soundness/modules/optional-values.md#on-absence) compares the three ways
of saying that there is no value.
