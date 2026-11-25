# One parser, as the code compiles and as it runs

A literal is checked by the compiler, and cannot fail later. Text which arrives at runtime is
parsed by the same code, and there the failure is an error named in the type.

```scala fuel
compile
alternative A literal, which cannot fail
  highlight media"text/plain"  checked by the compiler
  code
      val kind: MediaType = media"text/plain"
alternative Text from outside, which can
  highlight raises..MediaType.Error  the same parser, at runtime
  code
      def kind(header: Text): MediaType raises MediaType.Error =
        header.as[MediaType]
##
```

[Choosing between compiletime and runtime](../../soundness/modules/interpolation.md#choosing-between-compiletime-and-runtime)
describes the convention of offering both.
