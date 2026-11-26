# A digest written in the alphabet in scope

A digest is bytes, and has no text form of its own. `serialize` writes it in a base encoding,
and the alphabet of that encoding is an import.

```scala fuel
compile
alternative Lower-case hexadecimal
  highlight hexLowerCase  0-9 and a-f
  code
      import providers.javaBaseProvider, codepages.utf8Codepage
      import alphabets.hexLowerCase

      val fingerprint: Text = t"Hello world".digest[Sha2[256]].serialize[Hex]
alternative Upper-case hexadecimal
  highlight hexUpperCase  0-9 and A-F
  code
      import providers.javaBaseProvider, codepages.utf8Codepage
      import alphabets.hexUpperCase

      val fingerprint: Text = t"Hello world".digest[Sha2[256]].serialize[Hex]
alternative Base64
  highlight base64Standard  compact, with + and /
  code
      import providers.javaBaseProvider, codepages.utf8Codepage
      import alphabets.base64Standard

      val fingerprint: Text = t"Hello world".digest[Sha2[256]].serialize[Base64]
##
```

[Rendering a digest](../../soundness/modules/hashing.md#rendering-a-digest) shows the output,
and [declarative context](../../philosophy/declarative-context.md) why the alphabet is not an
argument.
