# A quantity knows the name of what it measures

The units in a quantity's type are enough for the compiler to say what kind of thing it is.
However a force or an energy was arrived at, `dimension` gives its physical name, worked out
from the type.

```scala fuel
compile
frame
  label A length over a time
  highlight (Metre/Second).dimension  from the units alone
  code
      val first: Text = (Metre/Second).dimension
frame
  label A force along a length
  highlight (Newton*Metre).dimension  named units, reduced to base ones
  code
      val first: Text = (Metre/Second).dimension
      val second: Text = (Newton*Metre).dimension
frame
  label The same, in feet and kilograms
  highlight Foot*Foot*Kilo(Gram)/(Second*Second)  other units, one dimension
  code
      val first: Text = (Metre/Second).dimension
      val second: Text = (Newton*Metre).dimension
      val third: Text = (Foot*Foot*Kilo(Gram)/(Second*Second)).dimension
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(first)
Out.println(second)
Out.println(third)
```

[Derived units](../../soundness/modules/quantities.md#derived-units) has the table of names.
