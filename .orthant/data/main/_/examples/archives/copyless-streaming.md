# Entries read from a stream which is still arriving

A tar archive is parsed lazily, straight off a stream of bytes. Decompression is one more stage
of that stream, so a compressed archive of any size is read an entry at a time and is never
assembled.

```scala fuel
compile
frame
  label An archive of two files
  code
      import codepages.utf8Codepage, strategies.throwUnsafely

      val cast = Tar.Entry(? / "cast.txt", t"Viola, Feste".in[Data])
      val scene = Tar.Entry(? / "act1" / "scene1.txt", t"Illyria".in[Data])
      val tarball = Tarfile(List(cast, scene))
frame
  label Compressed as it is written
  highlight tarball.gzip  a stream, not an array
  code
      import codepages.utf8Codepage, strategies.throwUnsafely

      val cast = Tar.Entry(? / "cast.txt", t"Viola, Feste".in[Data])
      val scene = Tar.Entry(? / "act1" / "scene1.txt", t"Illyria".in[Data])
      val tarball = Tarfile(List(cast, scene))

      def wire = tarball.gzip
frame
  label Decompressed as it is read
  highlight decompress[Gzip]  one more stage
  code
      import codepages.utf8Codepage, strategies.throwUnsafely

      val cast = Tar.Entry(? / "cast.txt", t"Viola, Feste".in[Data])
      val scene = Tar.Entry(? / "act1" / "scene1.txt", t"Illyria".in[Data])
      val tarball = Tarfile(List(cast, scene))

      def wire = tarball.gzip
      def names = Tarfile.read(wire.decompress[Gzip]).map(_.entryName)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

names.each(Out.println(_))
```

[Tar](../../soundness/modules/archives.md#tar) explains what single-pass reading allows, and
[compression](../../soundness/modules/compression.md#compressing-a-stream) the stage it is
composed with.
