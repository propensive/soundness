# A store which accepts only its own digests

A content-addressed store names each blob by the digest of its bytes, and the hash algorithm
is in the store's type. A digest made by another algorithm is a different type, and a SHA-256
store cannot be asked for it.

```scala fuel
compile
frame
  label A SHA-256 store, asked for a SHA-512 digest
  errors show
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem, strategies.throwUnsafely

      def fetch(store: Cas in Sha2[256] on (Path on Local), digest: Digest in Sha2[512])
      :   Optional[Data] =
        store(digest)
frame
  label Asked for one of its own
  highlight Sha2[256]  the store's own algorithm
    occurrence 2
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem, strategies.throwUnsafely

      def fetch(store: Cas in Sha2[256] on (Path on Local), digest: Digest in Sha2[256])
      :   Optional[Data] =
        store(digest)
##
```

[Content-addressed storage](../../soundness/modules/filesystem.md#content-addressed-storage)
has the store's operations, and [Hashing](../../soundness/modules/hashing.md) the digests.
