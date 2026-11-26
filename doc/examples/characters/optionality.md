# A superscript which may not exist

Unicode has a superscript form for the digits and for a few letters, and for nothing else. The
form is an optional value, so code which asks for one must say what to do without it.

```scala fuel
compile
frame
  label A superscript, assumed to exist
  errors show
  code
      def raised(char: Char): Char =
        val form: Optional[Char] = char.superscript
        form
frame
  label The character itself, where there is none
  highlight form.or(char)  the fallback is the character
  code
      def raised(char: Char): Char =
        val form: Optional[Char] = char.superscript
        form.or(char)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(t"Henry IV, Part ${raised('2')}")
Out.println(t"Henry IV, Part ${raised('é')}")
```

[Unicode properties](../../soundness/modules/characters.md#unicode-properties) lists what else
a character can be asked.
