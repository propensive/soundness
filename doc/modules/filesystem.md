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
operation declares `raises Io.Error` and the specific reason it can fail — a missing file, a
permission denied, a directory that is not empty; and each policy is an explicit contextual value,
so overwriting or recursing is a decision the code states rather than a default it inherits.
Everything comes from the `soundness` package, with the system capabilities and the policies the
operations need brought into scope:

```scala
import soundness.*
import strategies.throwUnsafely
import systems.javaSystem
import temporaryDirectories.systemTemporaryDirectory
import filesystemOptions.overwritePreexisting.enabled
import filesystemOptions.deleteRecursively.enabled
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

`create[File]()` and `create[Directory]()` bring the entry into being, and refuse to destroy
anything by default: creating over something that exists, or under a parent that does not, is an
`Io.Error`. Where that is the intent, a flag says so at the call site rather than an import saying
it for the whole file:

```scala
target.create[File](CreateFlag.Replace)
target.create[Directory](CreateFlag.Parents)
```

`Fifo` and `Socket` are entries too, created the same way.

### Reading and writing

A file is read and written by *opening* it. `open` names the form to open the path as, and the
mode to open it in, and runs a block with a handle for its capability. The handle exists only
for the duration of the block, so the descriptor cannot outlive the scope that owns it — the
shape described under [delimited scopes](../philosophy/delimited-scopes.md), and enforced by
[capture checking](../philosophy/capture-checking.md):

```scala
import charEncoders.utf8Encoder
import charDecoders.utf8Decoder

file.open[File](Write, OpenFlag.Create): handle ?=>
  handle.write(LazyList(t"Hello, world".in[Data]))

val text = file.open[File]()(file.stream.read[Data]).utf8
```

Where a whole small file is wanted, and the ceremony of a scope buys nothing, `read` and `write`
act directly on the path:

```scala
path.write(t"Hello world")
path.read[Text]
```

The mode is not merely a runtime flag: it is carried in the handle's type as a set of *grants*.
Opening with the default `Read` mode yields a handle that grants only reading, and a write
through it does not compile; `Read & Write` grants both. A whole class of mistakes — writing
through a read-only handle, holding a descriptor past its scope — becomes a compile error
rather than a runtime failure.

For a path that names a directory, opening confines what may be reached through it. Paths
derived from the handle live on a *fresh plane* of their own, so `..` does not compile, and a
path obtained from one open directory cannot be written under another:

```scala
directory.open[Directory](Read & Write): dir ?=>
  (dir/"greeting.txt").overwrite(t"Hello directory")
  dir.base.entries.to(List).map(_.name)
```

### Exclusive access

`Exclusive` is a third grant, alongside `Read` and `Write`, and it means what it says: no other
scope in the program may hold an overlapping path open while it lasts. Overlap is by containment,
not by equality — a directory and something beneath it overlap; two siblings do not:

```scala
directory.open[Directory](Read & Exclusive): dir ?=>
  // nothing else in this program may open `directory` or anything under it
```

Two ordinary reads may coexist. An exclusive open conflicts with an overlapping open in either
direction — whether the exclusive scope is the outer or the inner one — and the conflict is an
`Io.Error` whose reason is `Busy`, raised at the point of the second open rather than discovered
as corruption later. The claim is released when the scope ends, however it ends.

For a *file* rather than a directory, `Exclusive` additionally takes an operating-system lock, so
exclusivity holds against other processes and not merely within this one.

### Creating as a scope

`create` is the counterpart of `open`: where opening acts on something that exists, creating
brings it into being and hands back a handle over it, for the same lexical scope. Content
written within the scope is committed when the block completes, and a scope that fails leaves
nothing behind:

```scala
target.create[File](): handle ?=>
  handle.write(LazyList(t"payload".in[Data]))

target.create[Directory](): dir ?=>
  (dir/"inner.txt").overwrite(t"hello")
```

A *scratch* directory is created and removed by its scope, whether the scope succeeds or fails:

```scala
base.open[Scratch](Read & Write): scratch ?=>
  (scratch/"file.txt").overwrite(t"data")
```

### The opening pattern, generally

`open` is not specific to files. A great many things share the same shape: a value says *where*
something is — a path, a buffer in memory, a URL — and getting at its contents is a distinct,
scoped act. Naming that pattern once means it reads the same everywhere it applies.

An `Openable` instance relates a target to a *form*, which is why the form is a type argument
rather than being implied by the target: the same path opens as a `File`, as a `Directory`, as a
`Zip` [archive](archives.md), as a [PDF](pdf.md) or as an [image](images.md), and each is a
different handle with a different repertoire. The form may be omitted where a target has only one
instance; where it has several, the ambiguity is reported with the alternatives listed.

Three things follow uniformly. The *mode* — `Read`, `Write`, `Exclusive` and their combinations —
is carried in the handle's type, so the grants requested are the operations permitted. Flags after
the mode belong to the instance, so they are specific to the kind of thing being opened, and
irrelevant flags do not typecheck. And the handle is a capability confined to the block, so
neither it nor anything derived from it can escape.

`create` is the same pattern for something that does not yet exist, and `session` for a target
whose access is a conversation with something running — a [browser](web-automation.md), a
[debuggee](debugging.md) — rather than a handle over stored bytes. In each case the scope is the
lifetime, and the end of the block is the end of the access.

### Memory-mapped access

A file opened as `Ram` is memory-mapped, and serves positional reads and writes without
streaming through it. The `Write` grant is required to write, as everywhere else:

```scala
file.open[Ram](): ram ?=>
  ram(2, 3)               // three bytes from offset 2

file.open[Ram](Read & Write): ram ?=>
  ram(3L) = t"XYZ".in[Data]
```

### Copying, moving and deleting

A path copies, moves, symlinks or deletes with operations that name the destination or act in
place. Each consults the policy in scope for the awkward cases, and each may raise an `Io.Error`:

Those policies are not defaults that can be left alone. Moving a file onto a path where something
already exists either destroys that thing or refuses to; neither answer is right in general, and
choosing one silently would make the wrong programs compile. So `moveTo` requires
`overwritePreexisting` to be either `enabled` or `disabled` in scope, and calling it with neither
is a compile error. The point is not only to be unpresumptuous but to be instructive: a reader who
had not realised the question needed answering is told that it does.

The choice also changes what must be handled. With `disabled` in scope a collision is a failure to
deal with; with `enabled` it cannot arise, and the obligation goes away with it. Being contextual
values, the policies can be imported for a file or narrowed to a single block.

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
file.existent()          // true
file.size()              // the size in bytes
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

Each object is named for the directory it stands for, capitalized, with any leading `.` dropped —
`.` being the separator between members — so `$HOME/.local/bin` is `Home.Local.Bin` and `/var/lib`
is `Base.Var.Lib`. Writing a standard location this way rather than as text means a
mistyped directory is a compile error.

The resolution honours the environment where the [specification](environment.md) says it should:
`Home.Config` is normally `$HOME/.config`, but resolves to `$XDG_CONFIG_HOME` where the user has
set it. Applying a layout constructs a value of whichever directory type is asked for, so the same
names serve a `Path on Linux` and any other representation.

### Watching for changes

A directory is watched by opening it as `Watch`, which yields a stream of `WatchEvent`s — a
file created, modified or deleted. The registration lasts exactly as long as the block:

```scala
directory.open[Watch](): watcher ?=>
  watcher.stream.each:
    case WatchEvent.NewFile(dir, file) => Out.println(t"created $file")
    case WatchEvent.Modify(dir, file)  => Out.println(t"modified $file")
    case WatchEvent.Delete(dir, file)  => Out.println(t"deleted $file")
    case _                             => ()
```

Several paths are watched together by opening a list of them, which yields one event stream
across all of them.

The default watcher uses the operating system's own file-change notifications; where those are
unavailable, `watchers.polling` checks at an interval instead.

### Choosing a backend

Nothing above names a platform API. The primitive operations a filesystem must offer — stat,
open, read, write, list, link, delete — are gathered into a `FilesystemBackend` for a plane,
and everything else is defined in terms of them. The `java.nio` implementation is
`filesystemBackends.virtualMachineFilesystem`, and a WASI implementation over `wasi:filesystem` is
supplied by `galilei.wasi`, so the same code reads and writes files on the JVM and inside a
WebAssembly component. An operation a backend cannot support raises an `Io.Error` whose reason
is `Unsupported`, rather than approximating it. Narrowing the platform's surface to a seam
this small is [decoupling](../philosophy/decoupling.md) applied within a module.
