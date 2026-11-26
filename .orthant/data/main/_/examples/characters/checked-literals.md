# An encoding the compiler has looked up

An encoding named with `enc"…"` is looked up as the code compiles, so a name which is not an
encoding never reaches runtime. Asking for an encoder also checks that the encoding can
encode, which not every one can.

```scala fuel
compile
frame
  label A name which is no encoding
  errors show
  code
      val encoding = enc"UFT-8"
frame
  label An encoding
  highlight enc"UTF-8"  looked up by the compiler
  code
      val encoding = enc"UTF-8"
frame
  label Its encoder
  highlight encoder  exists because UTF-8 can encode
    line 2
    occurrence 2
  code
      val encoding = enc"UTF-8"
      val encoder = enc"UTF-8".encoder
frame
  label An encoding which only decodes
  errors show
  code
      val encoding = enc"ISO-2022-CN"
      val encoder = enc"ISO-2022-CN".encoder
##
```

[Encoding and decoding](../../soundness/modules/characters.md#encoding-and-decoding) lists the
encodings which need no lookup.
