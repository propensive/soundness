# A font in a medium

A provision is declared for a typeface in a medium, and its type says exactly that:
`(Typeface of "Inter") is Typesettable in Web`. A `Font in Web` is for a browser and a
`Font in Print` for a PDF, and each is made only from a provision in its own medium.

```scala fuel
compile
alternative In a browser
  highlight Font..Web  the only font which a stylesheet or an SVG accepts
  code
      import strategies.throwUnsafely

      given inter: ((Typeface of "Inter") is Typesettable in Web) = Web.local()

      val caption: Font in Web = Web.font(Typeface["Inter"].bold)
alternative In a PDF
  highlight Font..Print  made from a provision which `Print` has for Helvetica
  code
      import strategies.throwUnsafely

      val caption: Font in Print = Print.font(Typeface["Helvetica"].bold)
##
```

[Providing a typeface](../../soundness/modules/fonts.md#providing-a-typeface) has the table of
provisions and their media, and [infix types](../../philosophy/infix-types.md) the words
which join the types.
