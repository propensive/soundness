# A face, said as a typographer would

A face is a typeface with a weight, a slant and the features to set it with. It is written as
the name of the typeface followed by what is wanted of it, and each word gives a new face.

```scala fuel
compile
frame
  label A typeface
  code
      val garamond = Typeface["Garamond"]
frame
  label A weight and a slant
  highlight bold.italic
  code
      val garamond = Typeface["Garamond"]
      val title = garamond.bold.italic
frame
  label Features turned on and off
  highlight enabling(Face.Feature.OldstyleNumerals)  an OpenType feature, by name
  code
      val garamond = Typeface["Garamond"]
      val title = garamond.bold.italic
      val dates = garamond.face.enabling(Face.Feature.OldstyleNumerals)
        . disabling(Face.Feature.Ligatures)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(title.inspect)
Out.println(dates.inspect)
```

[Typefaces and faces](../../soundness/modules/fonts.md#typefaces-and-faces) has the attributes
of a face and the table of features.
