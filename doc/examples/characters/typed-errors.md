# Bad bytes are an error only where they are forbidden

What a decoder does with bytes which are not valid in its encoding is decided by the sanitizer
in scope. A lenient one cannot fail, so the decoder's type has no error in it. The strict one
can, and the same call then declares a `Charset.Error`, or handles it.

```scala fuel
compile
frame
  label Bad bytes are replaced, and nothing can fail
  highlight substituteSanitizer  cannot fail
  code
      import textSanitizers.substituteSanitizer

      def read(bytes: Data): Text =
        charsets.utf8Charset.decoded(bytes)
frame
  label Bad bytes are an error, which is declared
  highlight raises..Charset.Error  the strict sanitizer can
  code
      import textSanitizers.strictSanitizer

      def read(bytes: Data): Text raises Charset.Error =
        charsets.utf8Charset.decoded(bytes)
frame
  label Or handled, as an absent value
  highlight safely  a failure becomes Unset
  code
      import textSanitizers.strictSanitizer

      def read(bytes: Data): Optional[Text] =
        safely(charsets.utf8Charset.decoded(bytes))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(read(Data(80, 117, 99, 107)).inspect)
Out.println(read(Data(80, 117, 99, -62)).inspect)
```

The last byte of the second sequence begins a character which never ends.
[Bad input](../../soundness/modules/characters.md#bad-input) has the other sanitizers, and one
which reports every fault while keeping the text.
