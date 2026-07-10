## Filesystem

### About

Soundness reads and writes files on disk through the typed [paths](paths.md) it already knows
how to describe. A `Path on Linux` gains the operations that touch the disk — opening it to read
or write, creating it as a file or a directory, listing its children, copying, moving and
deleting — each declaring the errors it may raise and logging what it does. How an operation
behaves in the awkward cases, such as whether a copy overwrites an existing file or a delete
recurses into a directory, is decided by policy values chosen in scope.

Beyond individual files, the standard directory locations of a system — the home directory, the
cache, `/usr/share` — are named values, and a directory can be watched for changes, yielding a
stream of the files created, modified and deleted beneath it.

### On the filesystem

The JVM's filesystem API is a minefield of untyped edges. A path is a string or an untyped
object; a missing file is a `null` here and an exception there; and the crucial policy questions —
does this copy replace what is already there, does this delete empty a whole tree — are buried in
flag arguments or fixed by the method chosen, invisible in the type. A program that gets one
wrong destroys data.

Soundness lifts these into the types. A path carries its platform, so its rules are known; an
operation declares `raises IoError` and the specific reason it can fail — a missing file, a
permission denied, a directory that is not empty; and each policy is an explicit contextual value,
so overwriting or recursing is a decision the code states rather than a default it inherits.
Everything comes from the `soundness` package, with the system capabilities and the policies the
operations need brought into scope:

```scala
import soundness.*
import strategies.throwUnsafely
import systems.javaSystem
import temporaryDirectories.systemTemporaryDirectory
import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.overwritePreexisting.enabled
```

### Files and directories

A path names a location; whether it is a file or a directory is a matter of what is created
there. A temporary directory and a fresh name give a path to work with:

```scala
val directory = temporaryDirectory[Path on Local]/Uuid().show
directory.create[Directory]()

val file = directory/t"notes.txt"
file.create[File]()
```

`create[File]()` and `create[Directory]()` bring the entry into being, drawing on the policy
givens in scope — here, creating any missing parent directories and overwriting anything already
present.

### Reading and writing

A file is read and written by opening it, which yields a handle that behaves as a byte source and
sink for the [stream](streams.md) operations. Writing sends a value to the handle; reading pulls
the handle in as a chosen type:

```scala
import charEncoders.utf8Encoder
import charDecoders.utf8Decoder

file.open(t"Hello, world".writeTo(_))
val text = file.open(_.read[Text])
```

### Copying, moving and deleting

A path copies, moves, symlinks or deletes with operations that name the destination or act in
place. Each consults the policy in scope for the awkward cases, and each may raise an `IoError`:

```scala
file.copyTo(directory/t"backup.txt")
file.moveTo(directory/t"renamed.txt")
file.delete()
```

`copyInto` and `moveInto` place a path *inside* a destination directory, keeping its name;
`delete` removes a single entry, while `wipe` removes a directory and everything beneath it,
which is why recursive deletion is a policy that must be enabled deliberately.

### Listing and inspecting

A directory's immediate children stream from `children`, and its whole subtree from
`descendants`. A path reports whether it exists, its size, and what kind of entry it is:

```scala
directory.children       // Stream[Path on Local]
file.exists()            // true
file.size()             // the size in bytes
```

### Standard directories

The conventional locations of a system are named values, resolved against the environment. The
home directory and the paths beneath it, and the system directories under the root, are reached by
navigating and applying:

```scala
Home()             // the user's home directory
Home.Cache()       // ~/.cache
Home.Local.Bin()   // ~/.local/bin
Base.Usr.Share()   // /usr/share
```

### Watching for changes

A directory is watched within a scope, which yields a stream of `WatchEvent`s — a file created,
modified or deleted — and stops watching when the scope ends:

```scala
directory.watch: watch =>
  watch.stream.each:
    case WatchEvent.NewFile(dir, file) => Out.println(t"created $file")
    case WatchEvent.Modify(dir, file)  => Out.println(t"modified $file")
    case WatchEvent.Delete(dir, file)  => Out.println(t"deleted $file")
    case _                             => ()
```

The default watcher uses the operating system's own file-change notifications; where those are
unavailable, `watchers.polling` checks at an interval instead.
