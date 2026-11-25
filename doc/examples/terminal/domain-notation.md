# Colors which nest

An escape code sets a color and cannot restore the one before it. In the `e"…"` interpolator a
style has an opening and a closing parenthesis instead, so a styled span inside another ends
where it says, and the outer style resumes.

```scala fuel
compile
frame
  label One span
  code
      import WebColors.{Gold, Indigo}

      val banner = e"$Gold(All that glisters is not gold)"
frame
  label A span within it
  highlight $Indigo(glisters)  gold resumes after the parenthesis
  code
      import WebColors.{Gold, Indigo}

      val banner = e"$Gold(All that $Indigo(glisters) is not gold)"
frame
  label A third level
  highlight $Bold(not)  bold, and still gold
  code
      import WebColors.{Gold, Indigo}

      val banner = e"$Gold(All that $Indigo(glisters) is $Bold(not) gold)"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xtermTrueColorTermcap

Out.println(banner)
```

[Styled text](../../soundness/modules/terminal.md#styled-text) explains the stack of styles
behind it.
