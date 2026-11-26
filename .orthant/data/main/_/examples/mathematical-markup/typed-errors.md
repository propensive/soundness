# Not every formula has a shorthand

Ergo covers a subset of MathML, so turning a tree back into Ergo can fail: running text, for
one, has no glyph. `Ergo.serialize` names the failure in its type, and does not write an
approximation.

```scala fuel
compile
frame
  label The error is neither handled nor declared
  errors show
  code
      def shorthand(formula: Math): Text = Ergo.serialize(formula)
frame
  label The error is declared
  highlight raises..Error  the caller must now decide
  code
      def shorthand(formula: Math): Text raises Ergo.Error = Ergo.serialize(formula)
frame
  label Or absence stands for it
  highlight safely  a tree with no shorthand gives no text
  code
      def shorthand(formula: Math): Optional[Text] = safely(Ergo.serialize(formula))
##
```

[Serializing back](../../soundness/modules/mathematical-markup.md#serializing-back) describes
the guarantee, and [error handling](../../philosophy/error-handling.md) the choices.
