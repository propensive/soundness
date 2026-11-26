# The typefaces every PDF viewer has

The name of a typeface is its type: `Typeface["Times"]` is a `Typeface of "Times"`. A provision
is found by that type, and PDF's standard fonts have theirs already, so the compiler tells a
typeface which a viewer is sure to have from one it is not.

```scala fuel
compile
frame
  label One of the standard fourteen
  highlight Typeface["Times"]  provided in print, with no declaration
  code
      import strategies.throwUnsafely

      val body: Font in Print = Print.font(Typeface["Times"].italic)
frame
  label A typeface the standard does not define
  errors show
  code
      import strategies.throwUnsafely

      val body: Font in Print = Print.font(Typeface["Garamond"].italic)
##
```

[Fonts](../../soundness/modules/fonts.md#fonts) describes the pairing of a face with its
provision.
