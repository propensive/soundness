# DER, with the verbs of every other format

The structures beneath keys and certificates are ASN.1 values, carried as DER. Encoding one is
`in[Der]` and decoding it is `as[Asn1]`, the methods which write and read every format.

```scala fuel
compile
frame
  label A sequence of an integer and a string
  code
      import strategies.throwUnsafely

      val value = Asn1.Sequence(List(Asn1.Integer(BigInt(1)), Asn1.Utf8String(t"Verona")))
frame
  label Encoded
  highlight in[Der]  the one canonical encoding
  code
      import strategies.throwUnsafely

      val value = Asn1.Sequence(List(Asn1.Integer(BigInt(1)), Asn1.Utf8String(t"Verona")))
      val bytes = value.in[Der]
frame
  label And decoded
  highlight as[Asn1]  checked to be canonical
  code
      import strategies.throwUnsafely

      val value = Asn1.Sequence(List(Asn1.Integer(BigInt(1)), Asn1.Utf8String(t"Verona")))
      val bytes = value.in[Der]
      val again = bytes.as[Asn1]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println((again == value).inspect)
```

[ASN.1 and DER](../../soundness/modules/cryptography.md#asn1-and-der) lists the faults a
decoder reports.
