## Archives

### About

The two archive formats of everyday computing,
[ZIP](https://en.wikipedia.org/wiki/ZIP_(file_format)) and
[tar](https://en.wikipedia.org/wiki/Tar_(computing)), are read and written as typed values. A
`Zipfile` or a `Tarfile` is a sequence of entries, each with a validated archive-relative path
and streamed content. An archive of any size is read entry by entry, and written without being
assembled in memory. A ZIP entry inflates as it is read, and a tar entry parses straight off the
underlying source. Tar entries carry the Unix metadata which the format exists to preserve,
which is modes, owners and modification times, as typed values.

### On archives

Archives are streams which pretend to be filesystems, and APIs for them tend to expose one
pretense or the other badly: either a mutable random-access object which must be opened,
mutated and remembered to close, or a raw stream of headers and bytes. Entry names are plain
strings, so nothing prevents the classic mistakes, which are absolute paths and `..` traversal,
and metadata is an afterthought of integers.

Here an archive is data. Entries are [immutable] values. Their paths are typed archive-relative
[paths](paths.md). Reading, writing
and compressing are the same streaming operations used everywhere else. Reading an archive
lazily, through a scope which owns the file, is the shape described under
[delimited scopes](../philosophy/delimited-scopes.md).

Everything comes from the `soundness` package. Reading an archive from disk logs an event, and
reading or writing one can fail, so an error [strategy] and a logging choice are in scope. The
last import lets the samples print their results, and the samples work in a directory of their
own:

```scala
import soundness.*
import filesystemOptions.overwritePreexisting

import codepages.utf8Codepage
import charsets.utf8Charset
import textSanitizers.skipSanitizer
import filesystemBackends.javaBaseFilesystem
import filesystemOptions.createNonexistentParents
import filesystemOptions.deleteRecursively
import logging.silentLogging
import pathInterfaces.pathOnLinux
import strategies.throwUnsafely
import temporaryDirectories.javaBaseTemporaryDirectory
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

val work = temporaryDirectory[Path on Linux] / "archives" / Uuid().show
work.create[Directory](CreateFlag.Parents)
```

Each format has its own archive and entry types, its own handle when opened, and its own error:

```tabular
column  Format
column  Archive
column  Entry
column  Open as
column  Handle
column  Open flags
column  Error
row  ZIP  `Zipfile`  `Zip.Entry`  `Zip`  `zipfile`  none  `Zip.Error`
row  JAR  `Zipfile`  `Zip.Entry`  `Jar`  `zipfile`, with `manifest`  none  `Zip.Error`
row  tar  `Tarfile`  `Tar.Entry`  `Tar`  `tar`  `Tar.Flag.Gzip`, `Zlib`, `Deflate`  `Tar.Error`
row  ar  `Arfile`  `Ar.Entry`  `Ar`  `ar`  none  `Ar.Error`
```

### ZIP

A ZIP archive is a `Zipfile` of entries. Each entry is an archive-relative path and a source of
content. The archive streams as bytes, so writing one to disk is writing any other value.
Compression is a policy in scope, which deflates by default, and stores where deflation would
not help:

```scala
val zipPath = work / "hello.zip"
val entry = Zip.Entry(t"hello.txt".as[Path on Zip], t"Hello world".in[Data])
zipPath.write(Zipfile(List(entry)))
```

The path of an entry is a `Path on Zip`, which is relative to the root of the archive.

An archive reads from a path or from bytes. An entry is looked up by its path, and its content
reads as any type:

```scala fuel
run
##
val hello = Zipfile.read(zipPath)

hello.entries.each(entry => Out.println(entry.ref.encode))
Out.println(hello.entry(t"hello.txt".as[Path on Zip]).read[Text])
```

An entry read from an archive carries the metadata of its header around its content as well.
Writing it back reproduces the original archive byte for byte, except for the ZIP64 records,
which are rebuilt:

```tabular
column  Field
column  Holds
row  `flags`  the general-purpose flags
row  `versionMadeBy`  the "version made by"
row  `localVersion`  the "version needed" of the local header
row  `centralVersion`  the "version needed" of the central directory
row  `internalAttributes`  the internal attributes
row  `externalAttributes`  the external attributes, where a Unix mode lives
row  `localExtra`  the local header's extra field
row  `centralExtra`  the central directory's extra field
```

Every one of those fields defaults to `Unset` on a new entry, which means the value the library
would choose itself. Bit 3 of `flags` marks an entry as *streamed*. Its CRC and sizes are
repeated in a data descriptor after the payload, which is how an archive is written to a sink
which cannot seek.

`Zipfile` also takes an optional `prefix` of raw bytes to place before the archive proper. The
ZIP format locates its own directory from the end of the file, not the start, so an archive
concatenated onto arbitrary leading bytes still reads correctly. That is how a self-extracting
or directly-executable archive is made, and how the [packaged](packaging.md) launchers carry
their own JAR. `Zipfile.rebase` corrects the one field of a very large (ZIP64) archive which a
prefix does invalidate.

An archive can also be *opened*, which makes its entries available for the duration of a scope,
and closes the underlying source at the end of it, as with a
[file](filesystem.md#reading-and-writing). Inside the scope, `zipfile` is the open handle:

```scala fuel
run
##
import dysasymptotics.linearSize

zipPath.open[Zip]():
  Out.println(zipfile.entries.size.show)
```

A JAR is a ZIP with a manifest. Opening one as `Jar` gives the same handle, refined with the main
attributes parsed from `META-INF/MANIFEST.MF`, with continuation lines rejoined as the
specification requires. An archive without a manifest simply has no attributes:

```scala fuel
run
##
val manifest = Jar.manifest(t"Manifest-Version" -> t"1.0")
val jarPath = work / "hello.jar"
jarPath.write(Zipfile(List(manifest, entry)))

jarPath.open[Jar]():
  Out.println(zipfile.manifest.inspect)
```

Errors are `Zip.Error`s whose reason says what was wrong: `NotFound` for an entry which is not
there, `DuplicateEntry` when two entries share a path, and `MissingEocd` when the bytes are not a
ZIP archive at all:

```scala fuel
run
errors show
##
hello.entry(t"missing.txt".as[Path on Zip])
```

### Tar

A tar entry is one of the typed cases of the format: a file, a directory, a hard or symbolic
link, a character or block device, or a FIFO, with its Unix metadata spelled out. `Tar.Entry`
builds the common case, a file, with a default mode and owner, stamped with the current time.
`Tar.Entry.at` takes the timestamp as any instant type. The case classes take everything
explicitly:

```scala
val readme = Tar.Entry(t"README".as[Relative on Tar], t"Read me first".in[Data])

val script = Tar.Entry.File
  ( path  = t"bin/run".as[Relative on Tar],
    mode  = UnixMode(ownerExec = true, groupExec = true, otherExec = true),
    user  = UnixUser(1000, t"alice"),
    group = UnixGroup(1000, t"alice"),
    mtime = 0.bits.u32,
    data  = Archive.Body(t"#!/bin/sh\necho hello\n".in[Data]) )
```

A `Tarfile` of entries streams as tar blocks, or as the compressed forms in which the format
usually travels:

```scala
val tarball = Tarfile(List(readme, script))
val gzipped = tarball.gzip
val zlibbed = tarball.zlib
val deflated = tarball.deflate
```

Reading runs the other way. `Tarfile.read` parses entries lazily, straight off a stream of
bytes. Consuming one entry advances the source past it, so an archive is never materialized. A
compressed archive is simply a decompressed stream, read the same way:

```scala fuel
run
##
Tarfile.read(tarball.source[Data]).each(entry => Out.println(entry.entryName))
Tarfile.read(tarball.gzip.decompress[Gzip]).each(entry => Out.println(entry.entryName))
```

Reading an archive this way is single-pass and single-owner. Consume the entries in order, on
one thread. An entry which is passed over remains readable, because its body is memoized when
the sequence advances, but the sequence itself cannot be replayed.

Opening the archive as `Tar` scopes the underlying source, as a ZIP archive is scoped, and takes
the compression as a flag. `tar` is the open handle:

```scala fuel
run
##
val tarPath = work / "hello.tar.gz"
tarPath.create[File](): handle ?=>
  handle.write(tarball.gzip.memoize)

tarPath.open[Tar](Tar.Flag.Gzip):
  tar.entries.each(entry => Out.println(entry.entryName))
```

A tarball unpacks onto the [filesystem](filesystem.md) with `extractTo`:

```scala fuel
run
##
val unpacked: Path on Linux = work / "unpacked"
tarball.extractTo(unpacked)

Out.println((unpacked / "README").open[File]()(file.read[Text]))
```

Long names are handled in POSIX's pax form by default, or in GNU's, chosen when the archive is
built, as `Tarfile(entries, LongNameFormat.Gnu)`. Sparse files and pax extended headers
round-trip faithfully. A malformed archive raises a `Tar.Error` which names the fault: a bad
checksum, a header field which cannot be parsed, or a truncated body.

### Archiving a directory

A directory tree archives in any of the formats with `archive`, with the format chosen as a
type:

```scala
import filesystemOptions.preserveSymlinks
```

```scala fuel
run
##
unpacked.archive[Tar]().entries.each(entry => Out.println(entry.entryName))
unpacked.archive[Zip]().entries.each(entry => Out.println(entry.ref.encode))
unpacked.archive[Ar]().entries.each(entry => Out.println(entry.name))
```

The result is the in-memory archive, a `Tarfile`, `Zipfile` or `Arfile`, to stream, compress or
write wherever it is wanted. Every descendant is visited in a fixed order, with the children of
each directory sorted, so the same tree always produces the same bytes. Tar records each entry
as its own kind, with the mode, owner and timestamp which the filesystem reports. The formats
differ in what they can store:

```tabular
column  Format
column  Result
column  Symbolic links
column  Directories
column  Flags
row  `Tar`  `Tarfile`  stored or followed, by the `filesystemOptions` symlink policy  stored as entries  `Archive.Flag`, `LongNameFormat`
row  `Zip`  `Zipfile`  followed  stored as entries  `Zip.Compression`
row  `Ar`  `Arfile`  followed  flattened, each file named by its slash-joined relative path  `Archive.Flag`
```

Flags tune the archiving. `Archive.Flag.Owner` and `Archive.Flag.Mtime` replace the owner and
timestamp of every entry. They are `tar --owner`, `--group` and `--mtime`, the knobs of a
reproducible build. A `LongNameFormat` chooses tar's convention for long names. A ZIP takes a
`Zip.Compression` instead:

```scala
val reproducible = unpacked.archive[Tar](LongNameFormat.Gnu, Archive.Flag.Owner(UnixUser(0), UnixGroup(0)))
val stored = unpacked.archive[Zip](Zip.Compression.Stored)
```

### Ar

`ar` is the container of every `*.deb` package and every `*.a` static library: an eight-byte
magic, then a flat run of members, each a sixty-byte header followed inline by its payload. It
carries no entry types, no checksum and no end-of-archive marker, so a member is just a name,
its Unix metadata and a body. `Ar.Entry` builds one. The `mode` is the raw octal field, such as
`100644`, since `ar` stores the file-type bits beside the permissions:

```scala
val control =
  Ar.Entry(t"control.tar.xz", Archive.Body(t"...".in[Data]), 0L, UnixUser(0), UnixGroup(0), 33188)
val deb = Arfile(List(control))
```

Reading mirrors `Tarfile.read`. Entries parse lazily off a stream of bytes, single-pass and in
order, with the body of each member streamed in bounded chunks, and memoized if the sequence
advances past it. Opening as `Ar` scopes the source as `Tar` does, with `ar` as the handle.
There are no flags, because a `*.deb` is plain `ar`, and only its members are compressed:

```scala fuel
run
##
Arfile.read(deb.source[Data]).each(entry => Out.println(entry.name))

val debPath = work / "hello.deb"
debPath.create[File](): handle ?=>
  handle.write(deb)

debPath.open[Ar]():
  ar.entries.each(entry => Out.println(entry.name))
```

A GNU archive stores names longer than fifteen characters in a `//` name-table member, and
refers to them by offset. The reader resolves those, and strips the SysV terminating `/`, into
`name`. `rawName` keeps the literal field, so a parsed archive serializes again byte for byte,
as `Arfile.from(stream).source[Data]`. The `role` of each entry says whether it is a `Regular`
member, the `NameTable`, or a `SymbolTable`, which is `/` or `/SYM64/`. A malformed archive
raises an `Ar.Error`: a missing magic, a corrupt header terminator, a non-numeric field, a
truncated stream, or a long-name reference which does not resolve.

### Related

- [Compression](compression.md) describes the gzip, zlib and deflate which a tarball travels
  in.
- [Filesystem](filesystem.md) describes the files, directories and handles which archives are
  read from, written to and extracted onto.
- [Paths](paths.md) describes the archive-relative paths of entries.
- [Packaging](packaging.md) builds the JAR files and launchers which are ZIP archives.
- [Containers](docker.md) builds image layers from tar archives.
- [Streams](streams.md) describes the lazy streams which entries are read from.
- [Versioning](versioning.md) describes the manifests which a JAR carries.
