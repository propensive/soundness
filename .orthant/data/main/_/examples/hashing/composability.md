# The identity of a directory

`checksum` digests what is behind a value. For a path on the filesystem that is the content of
a file, or of every file beneath a directory with its relative path. A build's cache key is
one expression, and the hashing library has no code for files.

```scala fuel
compile
frame
  label A directory of sources
  code
      import filesystemBackends.javaBaseFilesystem, filesystemOptions.dereferenceSymlinks
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem
      import providers.javaBaseProvider, strategies.throwUnsafely

      def key(sources: Path on Local): Digest in Sha2[256] = sources.checksum[Sha2[256]]
frame
  label The same call, on a stream of bytes
  highlight checksum[Sha2[256]]  whatever is behind the value
    line 8
  code
      import filesystemBackends.javaBaseFilesystem, filesystemOptions.dereferenceSymlinks
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem
      import providers.javaBaseProvider, strategies.throwUnsafely

      def key(sources: Path on Local): Digest in Sha2[256] = sources.checksum[Sha2[256]]

      def key(download: Chain[Data]): Digest in Sha2[256] =
        download.checksum[Sha2[256]]
##
```

The example is compiled, and is not run, since it reads a directory.
[Hashing a stream](../../soundness/modules/hashing.md#hashing-a-stream) says what a
directory's checksum covers.
