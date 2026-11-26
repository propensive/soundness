# One method, two languages

`highlight` is a method of the language. `Scala.highlight` and `Java.highlight` give the same
`SourceCode`, of tokens with accents, and each reads the source with the tokenizer of its own
compiler: `inline` is a modifier where Scala says it is, and a name everywhere else.

```scala fuel
compile
frame
  label Scala, where inline is a soft keyword
  code
      def accents(code: SourceCode): List[Text] =
        code(1).map { token => t"${token.text}: ${token.accent}" }

      val scala = Scala.highlight(t"inline def inline = 1")
frame
  label Java, where it is only a name
  highlight Java.highlight  the same method, on another language
  code
      def accents(code: SourceCode): List[Text] =
        code(1).map { token => t"${token.text}: ${token.accent}" }

      val scala = Scala.highlight(t"inline def inline = 1")
      val java = Java.highlight(t"int inline = 1;")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(accents(scala).join(t", "))
Out.println(accents(java).join(t", "))
```

[Highlighting](../../soundness/modules/syntax-highlighting.md#highlighting) lists the accents.
