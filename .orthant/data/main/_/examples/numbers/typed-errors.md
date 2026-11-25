# An overflow which cannot go unnoticed

Integer arithmetic takes its overflow policy from an import. Under `checkedOverflow`, the type
of `+` changes to one which can raise an `Arithmetic.Error`, and a sum which ignores that no
longer compiles.

```scala fuel
compile
frame
  label Wrapping, as the hardware does
  highlight uncheckedOverflow  the sum wraps to a negative number
  code
      import scala.language.experimental.genericNumberLiterals
      import arithmeticOptions.uncheckedOverflow

      val ducats: S32 = 2000000000
      val total = ducats + ducats
frame
  label Checked, and unhandled
  errors show
  code
      import scala.language.experimental.genericNumberLiterals
      import arithmeticOptions.checkedOverflow

      val ducats: S32 = 2000000000
      val total = ducats + ducats
frame
  label Checked, and handled
  highlight safely  an overflow becomes Unset
  code
      import scala.language.experimental.genericNumberLiterals
      import arithmeticOptions.checkedOverflow

      val ducats: S32 = 2000000000
      val total = safely(ducats + ducats)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(total.inspect)
```

The sum does not fit in 32 bits, so the result is absent, and not a negative number.
[Checked arithmetic](../../soundness/modules/numbers.md#checked-arithmetic) has the policies
for division too.
