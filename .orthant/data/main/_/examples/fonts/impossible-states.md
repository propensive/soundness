# A font the page does not carry

Naming a typeface does not put it on the reader's machine. A `Font` is a face paired with a
provision, which says how the medium obtains the typeface, so a page cannot be set in a font
which nothing provides.

```scala fuel
compile
frame
  label A typeface which nothing provides
  errors show
  code
      import strategies.throwUnsafely

      val heading: Font in Web = Web.font(Typeface["Inter"].bold)
frame
  label A provision for it
  highlight Web.local()  the reader is expected to have it installed
  code
      import strategies.throwUnsafely

      given inter: ((Typeface of "Inter") is Typesettable in Web) = Web.local()

      val heading: Font in Web = Web.font(Typeface["Inter"].bold)
##
```

[Providing a typeface](../../soundness/modules/fonts.md#providing-a-typeface) lists the kinds
of provision, and [impossible states](../../philosophy/impossible-states.md) the principle.
