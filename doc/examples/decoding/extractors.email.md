# Validation and conversion in one pattern

`As` works for any type which text can be read as, and not only for numbers. Matching against
`As[EmailAddress]` checks that the text is a well-formed address and binds it as one, and the
fallback is a literal which the compiler has already checked.

```scala fuel
compile
frame
  label Text which may be an address
  highlight As[EmailAddress](address)  binds an EmailAddress, not Text
  code
      def recipient(text: Text): EmailAddress = text match
        case As[EmailAddress](address) => address
        case _                         => email"nobody@example.com"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(recipient(t"hamlet@example.com").show)
Out.println(recipient(t"hamlet.example.com").show)
```

[Extracting in patterns](../../soundness/modules/decoding.md#extracting-in-patterns) shows the
same extractor inside larger patterns.
