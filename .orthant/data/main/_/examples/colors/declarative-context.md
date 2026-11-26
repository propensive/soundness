# The same paints, mixed in another mode

Yellow and cyan are added in equal parts. What the sum is depends on the mixing mode, and the
mode is chosen by an import: an average, a product as inks give, or the screen of two lights.

```scala fuel
compile
alternative Proportional
  highlight proportionalMixing  an average
  code
      import mixing.proportionalMixing

      val mixture = 1*WebColors.Yellow + 1*WebColors.Cyan
alternative Multiply
  highlight multiplyMixing  as inks mix
  code
      import mixing.multiplyMixing

      val mixture = 1*WebColors.Yellow + 1*WebColors.Cyan
alternative Screen
  highlight screenMixing  as lights mix
  code
      import mixing.screenMixing

      val mixture = 1*WebColors.Yellow + 1*WebColors.Cyan
##
```

[Mixing modes](../../soundness/modules/colors.md#mixing-modes) defines each mode, and
[declarative context](../../philosophy/declarative-context.md) explains why the mode is not
an argument.
