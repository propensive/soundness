# The precision of a number is asked for

Writing a quantity out means choosing how many figures to give, and that is not a choice the
library makes in secret. `show` needs a `Decimalizer`, and a method which shows a quantity
either supplies one or asks its caller for it.

```scala fuel
compile
frame
  label A quantity shown, with no precision chosen
  errors show
  code
      def label(height: Quantity[Metres[1]]): Text = height.show
frame
  label The precision, asked of the caller
  highlight (using..Decimalizer)  named in the signature
  code
      def label(height: Quantity[Metres[1]])(using Decimalizer): Text = height.show
frame
  label And supplied, once
  highlight Decimalizer(3)  three significant figures
  code
      def label(height: Quantity[Metres[1]])(using Decimalizer): Text = height.show

      given decimalizer: Decimalizer = Decimalizer(3)

      val globe: Text = label(9.754*Metre)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(globe)
```

[Displaying](../../soundness/modules/quantities.md#displaying) describes how a quantity is
written, and [honest signatures](../../philosophy/honest-signatures.md) why the choice is a
parameter.
