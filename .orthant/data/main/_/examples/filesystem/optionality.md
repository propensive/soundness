# A blob which may not be in the store

A content-addressed store is asked for a blob by its digest, and may not have it. The answer
is an `Optional` value, which is not bytes until the absent case has been given a meaning.

```scala fuel
compile
frame
  label A blob, taken for granted
  errors show
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem, strategies.throwUnsafely

      def recall(store: Cas in Sha2[256] on (Path on Local), digest: Digest in Sha2[256])
      :   Text =
        store(digest).utf8
frame
  label Text for the absent case
  highlight or(t"missing")  what absence means here
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem, strategies.throwUnsafely

      def recall(store: Cas in Sha2[256] on (Path on Local), digest: Digest in Sha2[256])
      :   Text =
        store(digest).let(_.utf8).or(t"missing")
##
```

The example is compiled, and is not run, since it has no store to ask.
[Content-addressed storage](../../soundness/modules/filesystem.md#content-addressed-storage)
has the store's operations, and [optionality](../../philosophy/optionality.md) explains `let`
and `or`.
