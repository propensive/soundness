## Archives

### About

The two archive formats of everyday computing — [ZIP](https://en.wikipedia.org/wiki/ZIP_(file_format))
and [tar](https://en.wikipedia.org/wiki/Tar_(computing)) — read and write as typed values. A
`Zipfile` or a `Tarfile` is a stream of entries, each with a validated archive-relative path
and streamed content, so an archive of any size is read entry by entry and written without
assembling it in memory: a ZIP entry inflates as it is read, and a tar entry parses straight off
the underlying source. Tar entries carry the Unix metadata the format exists to preserve — modes,
owners, modification times — as typed values.

### On archives

Archives are streams pretending to be filesystems, and APIs for them tend to expose one pretence or
the other badly: either a mutable random-access object that must be opened, mutated and remembered
to close, or a raw stream of headers and bytes. Entry names are plain strings, so nothing prevents
the classic mistakes — absolute paths, `..` traversal — and metadata is an afterthought of ints.

Soundness treats an archive as data. Entries are immutable values; their paths are typed
archive-relative [paths](paths.md) that cannot express an escape from the archive root; and
reading, writing and compressing are the same streaming operations used everywhere else.
Everything comes from the `soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
import charEncoders.utf8Encoder
```

### ZIP

A ZIP archive reads from a path or from bytes, its entries a stream; an entry is looked up by
its path, and its content reads as any type:

```scala
val zipfile = Zipfile.read(path)
zipfile.entries.to(List).map(_.ref.encode)   // the entry names

zipfile.entry(t"readme.txt".as[Path on Zip]).read[Text]
```

Writing takes entries — each a path and a content source — and either writes to a path or
serializes as a byte stream. Compression is a policy in scope, deflating by default and storing
where deflation would not help:

```scala
val entry = Zip.Entry(t"hello.txt".as[Path on Zip], t"Hello world".in[Data])
Zipfile.write(path)(List(entry))
```

An archive can also be *opened*, which makes its entries available for the duration of a scope
and closes the underlying source at the end of it, in the same way as a
[file](filesystem.md#reading-and-writing):

```scala
archive.open[Zip]():
  zip.entries.length
```

A JAR is a ZIP with a manifest, so opening one as `Jar` gives the same handle refined with the
main attributes parsed from `META-INF/MANIFEST.MF`, continuation lines rejoined as the
specification requires. An archive without a manifest simply has no attributes:

```scala
jarfile.open[Jar]():
  zip.manifest   // Map(t"Manifest-Version" -> t"1.0", t"Main-Class" -> …)
```

### Tar

A tar entry is one of the format's typed cases — a file, a directory, a link, a FIFO — with its
Unix metadata spelled out:

```scala
val script = Tar.Entry.File
  ( path  = t"bin/run".as[Relative on Tar],
    mode  = UnixMode(ownerExec = true, groupExec = true, otherExec = true),
    user  = UnixUser(1000, t"alice"),
    group = UnixGroup(1000, t"alice"),
    mtime = timestamp,
    data  = TarBody(scriptText.in[Data]) )
```

A `Tarfile` of entries streams as tar blocks, or as the compressed forms the format usually
travels in:

```scala
val tarball = Tarfile(List(script))
tarball.gzip      // a .tar.gz byte stream
tarball.zlib
tarball.deflate
```

Reading runs the other way. `Tarfile.read` parses entries lazily straight off a byte stream —
one consumed entry advances the source past it, so an archive is never materialized — and a
compressed archive is simply a decompressed stream read the same way:

```scala
Tarfile.read(source)
Tarfile.read(source.decompress[Gzip])
```

Reading an archive this way is single-pass and single-owner: consume the entries in order, on
one thread. An entry passed over remains readable, because its body memoizes when the iterator
advances, but the sequence itself is not replayable. Opening the archive as `Tar` scopes the
underlying source in the same way as a ZIP archive, and takes the compression as a flag:

```scala
archive.open[Tar](TarFlag.Gzip): tar ?=>
  tar.entries.map(_.entryName).to(List)
```

A whole directory tree archives with `Tarfile.from(directory)` and unpacks with `extractTo`,
connecting archives to the [filesystem](filesystem.md):

```scala
Tarfile.from(sourceDirectory).gzip
tarball.extractTo(destination)
```

Long names are handled in POSIX's pax form by default, or GNU's, chosen when the archive is built;
sparse files and pax extended headers round-trip faithfully.
