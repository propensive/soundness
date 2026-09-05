## Archives

### About

The two archive formats of everyday computing — [ZIP](https://en.wikipedia.org/wiki/ZIP_(file_format))
and [tar](https://en.wikipedia.org/wiki/Tar_(computing)) — read and write as typed values. A
`Zipfile` or a `Tarfile` is a sequence of entries, each with a validated archive-relative path
and streamed content, so an archive of any size is read entry by entry and written without
assembling it in memory: a ZIP entry inflates as it is read, and a tar entry parses straight off
the underlying source. Tar entries carry the Unix metadata the format exists to preserve — modes,
owners, modification times — as typed values.

### On archives

Archives are streams pretending to be filesystems, and APIs for them tend to expose one pretense or
the other badly: either a mutable random-access object that must be opened, mutated and remembered
to close, or a raw stream of headers and bytes. Entry names are plain strings, so nothing prevents
the classic mistakes — absolute paths, `..` traversal — and metadata is an afterthought of ints.

Reading an archive lazily, through a scope that owns the file, is the shape described under [delimited scopes](../philosophy/delimited-scopes.md).

Soundness treats an archive as data. Entries are immutable values; their paths are typed
archive-relative [paths](paths.md) that cannot express an escape from the archive root; and
reading, writing and compressing are the same streaming operations used everywhere else.
Everything comes from the `soundness` package. Writing an archive to disk logs an event and can
fail, so an error strategy and a logging choice are in scope, and the examples work in a
directory of their own:

```scala
import soundness.*

import charEncoders.utf8Encoder
import charDecoders.utf8Decoder
import filesystemOptions.createNonexistentParents
import logging.silentLogging
import pathInterfaces.pathOnLinux
import strategies.throwUnsafely
import temporaryDirectories.javaBaseTemporaryDirectory

val work = temporaryDirectory[Path on Linux] / "archives"
work.create[Directory]()
```

### ZIP

Writing a ZIP archive takes entries — each an archive-relative path and a content source — and
either writes to a path or serializes as a byte stream. Compression is a policy in scope,
deflating by default and storing where deflation would not help:

```scala
val zipPath = work / "hello.zip"
val entry = Zip.Entry(t"hello.txt".as[Path on Zip], t"Hello world".in[Data])
Zipfile.write(zipPath)(List(entry))
```

An entry's path is a `Path on Zip`: relative to the archive root, so a text that names an absolute
path or climbs above the root fails to parse rather than producing a malicious entry.

An archive reads from a path or from bytes; an entry is looked up by its path, and its content
reads as any type:

```scala
val zipfile = Zipfile.read(zipPath)
zipfile.entries.map(_.ref.encode)                  // List(t"hello.txt")
zipfile.entry(t"hello.txt".as[Path on Zip]).read[Text]   // t"Hello world"
```

`write` also takes an optional `prefix` of raw bytes to place before the archive proper. The ZIP
format locates its own directory from the end of the file rather than the start, so an archive
concatenated onto arbitrary leading bytes still reads correctly — which is how a self-extracting
or directly-executable archive is made, and how the [packaged](packaging.md) launchers carry their
own JAR. `Zipfile.rebase` corrects the one field of a very large (ZIP64) archive that a prefix
does invalidate.

An archive can also be *opened*, which makes its entries available for the duration of a scope
and closes the underlying source at the end of it, in the same way as a
[file](filesystem.md#reading-and-writing). Inside the scope, `zip` is the open handle:

```scala
zipPath.open[Zip]():
  zip.entries.size   // 1
```

A JAR is a ZIP with a manifest, so opening one as `Jar` gives the same handle refined with the
main attributes parsed from `META-INF/MANIFEST.MF`, continuation lines rejoined as the
specification requires. An archive without a manifest simply has no attributes:

```scala
val manifest = Zip.Entry(t"META-INF/MANIFEST.MF".as[Path on Zip], t"Manifest-Version: 1.0".in[Data])
val jarPath = work / "hello.jar"
Zipfile.write(jarPath)(List(manifest, entry))

jarPath.open[Jar]():
  zip.manifest   // Map(t"Manifest-Version" -> t"1.0")
```

Errors are `Zip.Error`s whose reason says what was wrong: `NotFound` for an entry that is not
there, `DuplicateEntry` when two entries share a path, `MissingEocd` when the bytes are not a ZIP
archive at all.

### Tar

A tar entry is one of the format's typed cases — a file, a directory, a hard or symbolic link, a
character or block device, a FIFO — with its Unix metadata spelled out. `Tar.Entry` builds the
common case, a file, defaulting the mode, owner and timestamp; the case classes take everything
explicitly:

```scala
val readme = Tar.Entry(t"README".as[Relative on Tar], t"Read me first".in[Data])

val script = Tar.Entry.File
  ( path  = t"bin/run".as[Relative on Tar],
    mode  = UnixMode(ownerExec = true, groupExec = true, otherExec = true),
    user  = UnixUser(1000, t"alice"),
    group = UnixGroup(1000, t"alice"),
    mtime = 0.bits.u32,
    data  = Tar.Body(t"#!/bin/sh\necho hello\n".in[Data]) )
```

A `Tarfile` of entries streams as tar blocks, or as the compressed forms the format usually
travels in:

```scala
val tarball = Tarfile(List(readme, script))
tarball.gzip      // a .tar.gz byte stream
tarball.zlib
tarball.deflate
```

Reading runs the other way. `Tarfile.read` parses entries lazily straight off a byte stream —
one consumed entry advances the source past it, so an archive is never materialized — and a
compressed archive is simply a decompressed stream read the same way:

```scala
Tarfile.read(tarball.source[Data]).map(_.entryName)             // List(t"README", t"bin/run")
Tarfile.read(tarball.gzip.decompress[Gzip]).map(_.entryName)
```

Reading an archive this way is single-pass and single-owner: consume the entries in order, on
one thread. An entry passed over remains readable, because its body memoizes when the sequence
advances, but the sequence itself is not replayable. Opening the archive as `Tar` scopes the
underlying source in the same way as a ZIP archive, and takes the compression as a flag; `tar` is
the open handle:

```scala
val tarPath = work / "hello.tar.gz"
tarPath.create[File](): handle ?=>
  handle.write(tarball.gzip)

tarPath.open[Tar](Tar.Flag.Gzip):
  tar.entries.map(_.entryName)
```

A whole directory tree archives with `Tarfile.from(directory)` and unpacks with `extractTo`,
connecting archives to the [filesystem](filesystem.md):

```scala
val unpacked = work / "unpacked"
tarball.extractTo(unpacked)
Tarfile.from(unpacked).entries.map(_.entryName)
```

Long names are handled in POSIX's pax form by default, or GNU's, chosen when the archive is built
(`Tarfile(entries, LongNameFormat.Gnu)`); sparse files and pax extended headers round-trip
faithfully. A malformed archive raises a `Tar.Error` naming the fault — a bad checksum, an
unparseable header field, a truncated body.
