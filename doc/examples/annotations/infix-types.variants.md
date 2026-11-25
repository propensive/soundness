# The annotations of every case

The cases of a sealed type are asked about with another word of the same vocabulary: the
annotations `under` a type. The answer maps the name of each variant to what it carries.

```scala fuel
compile
frame
  label Two annotated variants
  code
      final case class primary() extends StaticAnnotation
      final case class number(number: Int) extends StaticAnnotation

      sealed trait Colored

      @number(3)
      case class Hsv(hue: Double, saturation: Double, value: Double) extends Colored

      @primary()
      case class Rgb(red: Int, green: Int, blue: Int) extends Colored
frame
  label Their annotations, by name
  highlight Annotated..Colored  every variant of Colored
    line 11
  code
      final case class primary() extends StaticAnnotation
      final case class number(number: Int) extends StaticAnnotation

      sealed trait Colored

      @number(3)
      case class Hsv(hue: Double, saturation: Double, value: Double) extends Colored

      @primary()
      case class Rgb(red: Int, green: Int, blue: Int) extends Colored

      val variants = summon[Annotated under Colored].subtypes
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(variants.inspect)
```

[Variants of a sealed type](../../soundness/modules/annotations.md#variants-of-a-sealed-type)
sets this form beside the others.
