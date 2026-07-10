## Archives

### About

The two archive formats of everyday computing — [ZIP](https://en.wikipedia.org/wiki/ZIP_(file_format))
and [tar](https://en.wikipedia.org/wiki/Tar_(computing)) — read and write as typed values. A
`Zipfile` or a `Tarfile` is a lazy sequence of entries, each with a validated archive-relative path
and streamed content, so an archive of any size is read entry by entry and written without
assembling it in memory. Tar entries carry the Unix metadata the format exists to preserve — modes,
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

A ZIP archive reads from a path or from bytes, its entries a lazy list; an entry is looked up by
its path, and its content reads as any type:

```scala
val zipfile = Zipfile.read(path)
zipfile.entries.map(_.ref.encode)   // the entry names

zipfile.entry(t"readme.txt".decode[Path on Zip]).read[Text]
```

Writing takes entries — each a path and a content source — and either writes to a path or
serializes as a byte stream. Compression is a policy in scope, deflating by default and storing
where deflation would not help:

```scala
val entry = Zip.Entry(t"hello.txt".decode[Path on Zip], t"Hello world".data)
Zipfile.write(path)(List(entry))
```

### Tar

A tar entry is one of the format's typed cases — a file, a directory, a link, a FIFO — with its
Unix metadata spelled out:

```scala
val script = Tar.Entry.File
  ( path  = t"bin/run".decode[Relative on Tar],
    mode  = UnixMode(ownerExec = true, groupExec = true, otherExec = true),
    user  = UnixUser(1000, t"alice"),
    group = UnixGroup(1000, t"alice"),
    mtime = timestamp,
    data  = Stream(scriptText.data) )
```

A `Tarfile` of entries streams as tar blocks, or as the compressed forms the format usually
travels in:

```scala
val tarball = Tarfile(Stream(script))
tarball.gzip    // a .tar.gz byte stream
```

Reading runs the other way — `Tarfile.read` for raw blocks, `Tarfile.fromGzip` for compressed —
and a whole directory tree archives with `Tarfile.from(directory)` and unpacks with
`extractTo`, connecting archives to the [filesystem](filesystem.md):

```scala
Tarfile.from(sourceDirectory).gzip
tarball.extractTo(destination)
```

Long names are handled in POSIX's pax form by default, or GNU's, chosen when the archive is built;
sparse files and pax extended headers round-trip faithfully.
