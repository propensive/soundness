# Three answers to a byte which is not text

A decoder which meets bytes that are not valid in its encoding consults the sanitizer in
scope. The decoding is written once, and an import decides whether the bad bytes are dropped,
replaced or refused.

```scala fuel
compile
alternative Dropped
  highlight textSanitizers.skipSanitizer  the bad byte disappears
  code
      import textSanitizers.skipSanitizer

      val bytes = Data(80, 117, -62, 99, 107)
      val name = safely(charsets.utf8Charset.decoded(bytes))
alternative Replaced
  highlight textSanitizers.substituteSanitizer  a question mark stands in for it
  code
      import textSanitizers.substituteSanitizer

      val bytes = Data(80, 117, -62, 99, 107)
      val name = safely(charsets.utf8Charset.decoded(bytes))
alternative Refused
  highlight textSanitizers.strictSanitizer  the decoding fails, and gives Unset
  code
      import textSanitizers.strictSanitizer

      val bytes = Data(80, 117, -62, 99, 107)
      val name = safely(charsets.utf8Charset.decoded(bytes))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(name.inspect)
```

[Bad input](../../soundness/modules/characters.md#bad-input) has a fourth sanitizer, which
keeps the text and reports every fault.
