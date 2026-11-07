## Filesystem

### About

Files on disk are read and written through the typed [paths](paths.md) that already describe
them. A `Path on Linux` gains the operations that touch the disk — opening it to read or write,
creating it as a file or a directory, listing its children, copying, moving and deleting — each
declaring the errors it may raise and logging what it does. How an operation behaves in the
awkward cases, such as whether a copy overwrites an existing file or a delete recurses into a
directory, is decided by policy values chosen in scope.

Beyond individual files, the standard directory locations of a system — the home directory, the
cache, `/usr/share` — are named values; a directory can be watched for changes, yielding a
stream of the files created, modified and deleted beneath it; and a file can be locked,
memory-mapped, tagged with extended attributes, or searched for along a path of directories.

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
import filesystemOptions.dereferenceSymlinks
import filesystemOptions.moveAtomically
import environments.javaBaseEnvironment
import stdios.javaLangSystemStdio
import termcapDefinitions.basicTermcap

import charsets.utf8Charset
import codepages.utf8Codepage
import filesystemBackends.javaBaseFilesystem
import filesystemOptions.createNonexistentParents
import filesystemOptions.deleteRecursively
import filesystemOptions.overwritePreexisting
import pathInterfaces.pathOnLinux
import strategies.throwUnsafely
import systems.javaBaseSystem
import temporaryDirectories.javaBaseTemporaryDirectory
import textSanitizers.strictSanitizer
```

### Files and directories

A path names a location; whether it is a file or a directory is a matter of what is created
there. A temporary directory and a fresh name give a path to work with:

```scala
val directory = temporaryDirectory[Path on Linux] / "filesystem" / Uuid().show
directory.create[Directory](CreateFlag.Parents)

val file = directory / "notes.txt"
file.create[File]()
```

`create[File]()` and `create[Directory]()` bring the entry into being, and refuse to destroy
anything by default: creating over something that exists, or under a parent that does not, is an
`Io.Error`. Where that is the intent, a flag says so at the call site rather than an import saying
it for the whole file:

```scala
file.create[File](CreateFlag.Replace)
(directory / "deep" / "nested").create[Directory](CreateFlag.Parents)
```

`Fifo` and `Sock` are entries too, created the same way.

### Reading and writing

A file is read and written by *opening* it. `open` names the form to open the path as, and the
mode to open it in, and runs a block with a handle for its capability. The handle exists only
for the duration of the block, so the descriptor cannot outlive the scope that owns it — the
shape described under [delimited scopes](../philosophy/delimited-scopes.md), and enforced by
[capture checking](../philosophy/capture-checking.md). Inside the block, `file` is the open
handle:

```scala
file.open[File](Write, OpenFlag.Create): handle ?=>
  handle.write(Chain(t"Hello, world".in[Data]))

file.open[File]()(file.read[Text])   // t"Hello, world"
```

Where a whole small file is wanted, and the ceremony of a scope buys nothing, `read` and `write`
act directly on the path:

```scala
file.write(t"Hello world")
file.read[Text]   // t"Hello world"
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
  (dir / "greeting.txt").overwrite(t"Hello directory")
  dir.base.entries.map(_.name)   // List(t"notes.txt", t"greeting.txt", …)
```

### Exclusive and shared access

`Exclusive` is a third grant, alongside `Read` and `Write`, and it means what it says: no other
scope in the program may hold an overlapping path open while it lasts. Overlap is by containment,
not by equality — a directory and something beneath it overlap; two siblings do not:

```scala
directory.open[Directory](Read & Exclusive): dir ?=>
  ()   // nothing else in this program may open `directory` or anything under it
```

Two ordinary reads may coexist. An exclusive open conflicts with an overlapping open in either
direction — whether the exclusive scope is the outer or the inner one — and the conflict is an
`Io.Error` whose reason is `Busy`, raised at the point of the second open rather than discovered
as corruption later. The claim is released when the scope ends, however it ends.

For a *file* rather than a directory, `Exclusive` additionally takes an operating-system lock, so
exclusivity holds against other processes and not merely within this one. `Shared` is the
reader's counterpart: any number of shared holders may coexist, and an exclusive open waits for
them, or fails at once if the lock is requested without blocking. A lock may also cover a byte
range rather than the whole file, for the record-oriented layouts databases use.

The four grants, side by side:

```tabular
column  Grant
column  Permits
column  Conflicts with
row  `Read`  reading, and is the default mode  an overlapping `Exclusive` open
row  `Write`  writing  an overlapping `Exclusive` open
row  `Exclusive`  no other overlapping open in the program, and for a file an OS lock  any overlapping open, as `Busy`
row  `Shared`  for a file, a shared OS lock, held alongside any number of others  an `Exclusive` open, which waits for it
```

### Creating as a scope

`create` is the counterpart of `open`: where opening acts on something that exists, creating
brings it into being and hands back a handle over it, for the same lexical scope. Content
written within the scope is committed when the block completes, and a scope that fails leaves
nothing behind:

```scala
val target = directory / "payload.bin"

target.create[File](): handle ?=>
  handle.write(Chain(t"payload".in[Data]))

(directory / "inner").create[Directory](): dir ?=>
  (dir / "inner.txt").overwrite(t"hello")
```

A *scratch* directory is created and removed by its scope, whether the scope succeeds or fails,
and a scratch *file* likewise — a throwaway index, a fixture — whose handle also knows its
`path`, for whatever must be told where the file is:

```scala
directory.open[Scratch](Read & Write): scratch ?=>
  (scratch / "file.txt").overwrite(t"data")
```

```scala
directory.open[Scratch.File](Read & Write): file ?=>
  file.write(Chain(t"data".in[Data]))
  file.path.read[Text]   // t"data", until the scope ends
```

### Creation masks

A process creates files under its *umask*, the permission bits the operating system withholds
from everything it creates, and ordinarily that is the end of the matter. A process serving
several callers at once — a [daemon](daemons.md) — has one umask but many invocations, each
with its own, so galilei lets the mask be a contextual value: a `Umask` in scope is applied to
every file and directory created while it applies, requested at creation so that the entry
never exists more permissive than asked. Where none is in scope, `Umask.process` — the
operating system's own — applies, exactly as before:

```scala
def privately(): Unit =
  given Umask = Umask(0x3f) // 077: readable and writable by the owner alone
  (directory / "secret.txt").create[File](): handle ?=>
    handle.write(Chain(t"payload".in[Data]))
```

A mask is parsed from the octal form `umask` prints, `Umask.parse(t"022")`, and renders back
the same way with `octal`. A daemon's service handle is a `Umask.Provider`, so within a `cli`
block the invocation's mask is already in scope with no further declaration.

### The opening pattern, generally

`open` is not specific to files. A great many things share the same shape: a value says *where*
something is — a path, a buffer in memory, a URL — and getting at its contents is a distinct,
scoped act. Naming that pattern once means it reads the same everywhere it applies.

An `Openable` instance relates a target to a *form*, which is why the form is a type argument
rather than being implied by the target: the same path opens as a `File`, as a `Directory`, as a
`Zip` [archive](archives.md), as a [PDF](pdf.md) or as an [image](images.md), and each is a
different handle with a different repertoire. The form may be omitted where a target has only one
instance; where it has several, the ambiguity is reported with the alternatives listed. Behind
the pattern are the typeclasses `Openable`, `Creatable` and `Sessional`, for `open`, `create` and
`session`, and the mode the first two take is a combination of `Grant`s.

The forms this page describes are:

```tabular
column  Form
column  The handle gives
row  `File`  the file's contents, to read or write as the grants allow
row  `Directory`  paths confined beneath the directory, on a fresh plane
row  `Scratch`  a directory removed when the scope ends
row  `Scratch.File`  a file removed when the scope ends, and its `path`
row  `Ram`  positional reads and writes over a memory-mapped file
row  `Watch`  a stream of the changes beneath a directory
```

Three things follow uniformly. The *mode* — `Read`, `Write`, `Exclusive`, `Shared` and their
combinations — is carried in the handle's type, so the grants requested are the operations
permitted. Flags after the mode belong to the instance, so they are specific to the kind of thing
being opened, and irrelevant flags do not typecheck. And the handle is a capability confined to
the block, so neither it nor anything derived from it can escape.

`create` is the same pattern for something that does not yet exist, and `session` for a target
whose access is a conversation with something running — a [browser](web-automation.md), a
[debuggee](debugging.md) — rather than a handle over stored bytes. In each case the scope is the
lifetime, and the end of the block is the end of the access.

### Positional access

A file opened as `Ram` is memory-mapped, and serves positional reads and writes without
streaming through it. The `Write` grant is required to write, as everywhere else:

```scala
file.open[Ram](): ram ?=>
  ram(2, 3)               // three bytes from offset 2

file.open[Ram](Read & Write): ram ?=>
  ram(3L) = t"XYZ".in[Data]
```

A `Slice` is a window onto part of a file — an offset and an extent — opened for reading or
writing like a file of its own, so a region of a large file streams in or out without the rest
being touched, and a write cannot stray outside its window.

### Copying, moving and deleting

A path copies, moves, symlinks or deletes with operations that name the destination or act in
place. Each consults the policy in scope for the awkward cases, and each may raise an `Io.Error`.

Those policies are not defaults that can be left alone. Moving a file onto a path where something
already exists either destroys that thing or refuses to; neither answer is right in general, and
choosing one silently would make the wrong programs compile. So `moveTo` requires either
`overwritePreexisting` or `failOnPreexisting` in scope, and calling it with neither is a compile
error. The point is not only to be unpresumptuous but to be instructive: a reader who had not
realized the question needed answering is told that it does.

The choice also changes what must be handled. With `failOnPreexisting` in scope a collision is a
failure to deal with; with `overwritePreexisting` it cannot arise, and the obligation goes away
with it. Being contextual values, the policies can be imported for a file or narrowed to a single
block. Each answers one question, and comes in a pair from `filesystemOptions`:

```tabular
column  Question
column  Yes
column  No
row  Replace what is already at the destination?  `overwritePreexisting`  `failOnPreexisting`
row  Empty a directory before deleting it?  `deleteRecursively`  `deleteOnlyEmpty`
row  Create missing parent directories?  `createNonexistentParents`  `requireParents`
row  Follow symbolic links?  `dereferenceSymlinks`  `preserveSymlinks`
row  Move atomically?  `moveAtomically`  `moveNonAtomically`
```

Replacing an existing entry deletes it first, so `overwritePreexisting` needs one of the deletion
policies in scope as well.

```scala
file.copyTo(directory / "backup.txt")
file.moveTo(directory / "renamed.txt")
(directory / "backup.txt").delete()
```

Each operation consults the policies that bear on it:

```tabular
column  Operation
column  Acts on
column  Policies consulted
row  `copyTo(path)`  the path given  replacing, parents, symbolic links
row  `copyInto(directory)`  a child of the directory, keeping the name  replacing, symbolic links
row  `moveTo(path)`  the path given  replacing, parents, symbolic links, atomicity
row  `moveInto(directory)`  a child of the directory, keeping the name  replacing, symbolic links, atomicity
row  `symlinkTo(path)`  a new link at the path given, pointing here  replacing, parents
row  `symlinkInto(directory)`  a new link in the directory, keeping the name  replacing
row  `delete()`, `wipe()`  the path itself  deletion
```

The `…Into` forms create any missing parents themselves. Deleting a directory with children is
an `Io.Error` whose reason is `DirectoryNotEmpty` unless `deleteRecursively` is in scope, which is
why recursive deletion is a policy that must be enabled deliberately.

### Content-addressed storage

A directory of blobs, each named by the digest of its own bytes, is a *content-addressed store*:
a blob is written once, verified as it is written, and found by anything that knows its digest.
`Cas` is that store, with the hash algorithm in its type and in the type of every digest it
accepts, so a SHA-256 store cannot be asked for a BLAKE3 digest:

```scala
import providers.javaBaseProvider

val store: Cas in Sha2[256] on (Path on Linux) = Cas[Sha2[256]](directory, t".blob")
val digest: Digest in Sha2[256] = store() = t"some bytes".in[Data]
store(digest)                       // the bytes, or Unset
store.has(digest)                   // true
```

A store has these operations:

```tabular
column  Expression
column  Does
row  `store() = data`  stores the data under its own digest, and returns it; nothing if the blob is already there
row  `store(digest) = data`  the same, after checking that the data has that digest, raising a `Cas.Error` if not
row  `store(digest)`  the bytes, or `Unset`
row  `store.has(digest)`  whether the blob is stored
row  `store.missing(digests)`  those of a list of digests not yet stored
row  `store.digests`  every digest stored
row  `store.prune(digests)`  deletes every blob not in the set, and returns how many it deleted
```

`prune` is how a cache is trimmed to what is still referenced:

```scala
store.missing(List(digest))         // Nil
store.prune(Set(digest))            // 0: nothing else to delete
```

A blob is written to a uniquely-named sibling and moved into place atomically, so a reader
never sees a partial blob. The store's root is a `Path` on any platform here, but its type is
the type of that root: a `Cas.Storage` instance for another kind of root makes a store over it
with the same interface. As a collection, a store is its digests: `store.size`,
`store.has(digest)`, `store.at(digest)` and traversal all work on it.

### Listing and inspecting

A directory's immediate children stream from `children`, and its whole subtree from
`descendants`. A path reports whether it exists, its size, and what kind of entry it is:

```scala
directory.children.map(_.name)        // the entries' names
(directory / "renamed.txt").existent()   // true
(directory / "renamed.txt").filesize()   // the size in bytes
```

A `glob` selects children by pattern, with `*` matching within a name and a whole-segment `**`
spanning directories:

```scala
directory.glob(Glob.parse(t"*.txt")).map(_.name)   // List(t"renamed.txt", …)
```

Two paths can name one entry through symbolic links, or through `/tmp` and `/private/tmp` on
macOS. `canonical` is the entry's path as the filesystem itself knows it, every link resolved,
which is what a registry keyed by path should store:

```scala
(directory / "renamed.txt").canonical()   // the same path, with any symlinks resolved
```

### Extended attributes and volumes

The filesystem a path sits on is a typed fact about it: `Ext4`, `Btrfs`, `Apfs`, `Ntfs` and
`Dos` are the *axes* a path can be matched against, and a filesystem that supports extended
attributes lets a path carry named metadata beyond its contents:

```scala
file match
  case Apfs(path) => path.attribute(t"origin", t"soundness".in[Data]) yet path.attributes()
  case Ext4(path) => path.attribute(t"origin", t"soundness".in[Data]) yet path.attributes()
  case _          => Nil
```

On btrfs, `subvolume` reports the subvolume a path belongs to, and `subvolumeRoot` whether it
is the top of one.

### Standard directories

The conventional locations of a system are named values, resolved against the environment. The
home directory and the paths beneath it, and the system directories under the root, are reached by
navigating and applying:

```scala
Home[Path on Linux]()             // the user's home directory
Home.Cache[Path on Linux]()       // ~/.cache
Home.Local.Bin[Path on Linux]()   // ~/.local/bin
Base.Usr.Share[Path on Linux]()   // /usr/share
```

Each object is named for the directory it stands for, capitalized, with any leading `.` dropped —
`.` being the separator between members — so `$HOME/.local/bin` is `Home.Local.Bin` and `/var/lib`
is `Base.Var.Lib`. Writing a standard location this way rather than as text means a
mistyped directory is a compile error. The layouts, and which of them `readOnly` marks as not
meant to be written to, are:

```tabular
column  Layout
column  Directory
column  Read-only
row  `Home`  `~`  no
row  `Home.Cache`  `~/.cache`  no
row  `Home.Config`  `~/.config`  no
row  `Home.Local`  `~/.local`  no
row  `Home.Local.Bin`  `~/.local/bin`  no
row  `Home.Local.Lib`  `~/.local/lib`  no
row  `Home.Local.Share`  `~/.local/share`  no
row  `Home.Local.State`  `~/.local/state`  no
row  `Base`  `/`  no
row  `Base.Boot`  `/boot`  yes
row  `Base.Dev`  `/dev`  no
row  `Base.Dev.Shm`  `/dev/shm`  no
row  `Base.Efi`  `/efi`  yes
row  `Base.Etc`  `/etc`  no
row  `Base.Home`  `/home`  no
row  `Base.Proc`  `/proc`  no
row  `Base.Proc(pid)`  `/proc/<pid>`  yes
row  `Base.Proc.Sys`  `/proc/sys`  yes
row  `Base.Root`  `/root`  no
row  `Base.Run`  `/run`  no
row  `Base.Run.Log`  `/run/log`  no
row  `Base.Run.User`  `/run/user`  no
row  `Base.Srv`  `/srv`  no
row  `Base.Sys`  `/sys`  yes
row  `Base.Tmp`  `/tmp`  no
row  `Base.Usr`  `/usr`  yes
row  `Base.Usr.Bin`  `/usr/bin`  yes
row  `Base.Usr.Include`  `/usr/include`  yes
row  `Base.Usr.Lib`  `/usr/lib`  yes
row  `Base.Usr.Share`  `/usr/share`  yes
row  `Base.Usr.Share.Doc`  `/usr/share/doc`  yes
row  `Base.Usr.Share.Factory`  `/usr/share/factory`  yes
row  `Base.Usr.Share.Factory.Etc`  `/usr/share/factory/etc`  yes
row  `Base.Usr.Share.Factory.Var`  `/usr/share/factory/var`  yes
row  `Base.Var`  `/var`  no
row  `Base.Var.Cache`  `/var/cache`  no
row  `Base.Var.Lib`  `/var/lib`  no
row  `Base.Var.Log`  `/var/log`  no
row  `Base.Var.Spool`  `/var/spool`  no
row  `Base.Var.Tmp`  `/var/tmp`  no
```

A layout is a fixed place: `Home.Config` is always `$HOME/.config`, and only `$HOME` is read from
the environment. The locations the [XDG specification](environment.md#standard-directories) lets
the user move, such as `$XDG_CONFIG_HOME`, are given by `Xdg` instead. Applying a layout
constructs a value of whichever directory type is asked for, so the same names serve a
`Path on Linux` and any other representation.

### Search paths

Many files are looked up not at one location but along a list of them — an icon in whichever of
the XDG data directories holds it, a configuration file in the first of several places. A
`Searchpaths.Stems` instance names the directories to try, in order, and a relative path on
the corresponding plane resolves with `search` to the first stem holding it, or `Unset`:

```scala
import galilei.search

given Searchpaths.Stems on Xdg.Data onto Linux = new Searchpaths.Stems:
  type Plane = Xdg.Data
  type Target = Linux
  val stems: List[Path on Linux] = List(directory, directory / "inner")

(% / "inner.txt").on[Xdg.Data].search()   // the copy under `inner`
```

`dataSearch` and `configSearch` supply the XDG data and configuration search paths from the
environment, so a program's resources are found wherever the user's system keeps them.

### Watching for changes

A directory is watched by opening it as `Watch`, which yields a stream of `Watch.Event`s — a
file created, modified or deleted, a directory created. The registration lasts exactly as long
as the block:

```scala
def observe(): Unit =
  directory.open[Watch](): watcher ?=>
    watcher.stream.each:
      case Watch.Event.NewFile(dir, file) => Out.println(t"created $file")
      case Watch.Event.Modify(dir, file)  => Out.println(t"modified $file")
      case Watch.Event.Delete(dir, file)  => Out.println(t"deleted $file")
      case _                              => ()
```

Several paths are watched together by opening a list of them, which yields one event stream
across all of them.

Editors save in bursts, and a rebuild wants one notification per burst rather than one per
event. `batches` yields a `Watch.Batch` each time no event has arrived for the given quiet
period; its `paths` lists every changed path once:

```scala
def rebuild(changed: List[Path on Linux]): Unit = ()

def observeBatches(): Unit =
  directory.open[Watch](): watcher ?=>
    watcher.batches(0.2*Second).each: batch =>
      rebuild(batch.paths[Path on Linux])
```

The default watcher uses the operating system's own file-change notifications; where those are
unavailable, `watchers.polling` checks at an interval instead.

### Choosing a backend

Nothing above names a platform API. The primitive operations a filesystem must offer — stat,
open, read, write, list, link, delete — are gathered into a `FilesystemBackend` for a plane,
and everything else is defined in terms of them. The `java.nio` implementation is
`filesystemBackends.javaBaseFilesystem`, and a WASI implementation over `wasi:filesystem` is
supplied for WebAssembly components, so the same code reads and writes files on the JVM and
inside a component. An operation a backend cannot support raises an `Io.Error` whose reason
is `Unsupported`, rather than approximating it. Narrowing the platform's surface to a seam
this small is [decoupling](../philosophy/decoupling.md) applied within a module.
