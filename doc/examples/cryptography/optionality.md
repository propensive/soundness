# A certificate which may name nobody

A certificate's subject is read back from its bytes, and a certificate need not have one. The
subject is an optional value, so its common name cannot be read as though it were there.

```scala fuel
compile
frame
  label A name, read as though the subject were present
  errors show
  code
      def owner(certificate: Certificate): Text = certificate.subject.commonName
frame
  label A name, if there is a subject
  highlight let(_.commonName)  absent if the subject is
  code
      def owner(certificate: Certificate): Optional[Text] =
        certificate.subject.let(_.commonName)
frame
  label And what to say when there is none
  highlight or(t"nobody")  the meaning of absence, here
  code
      def owner(certificate: Certificate): Text =
        certificate.subject.let(_.commonName).or(t"nobody")
##
```

[Certificates](../../soundness/modules/cryptography.md#certificates) reads the other fields,
and [optionality](../../philosophy/optionality.md) explains `let` and `or`.
