## Filesystem

### About

Files on a disk are read and written through the typed [paths](paths.md) which already
describe them. A `Path on Linux` has the operations which touch the disk: opening it to read
or write, creating it as a file or a directory, listing its children, copying, moving and
deleting. Each operation declares the errors it may raise.

How an operation behaves in the awkward cases is decided by policies, chosen in scope. They
say, for example, whether a copy overwrites an existing file, and whether a delete descends
into a directory.

Beyond individual files, the standard directories of a system, such as the home directory,
the cache and `/usr/share`, are named values. A directory can be watched for changes. A file
can be locked, mapped into memory, tagged with extended attributes, or searched for along a
list of directories.

### On the filesystem

The JVM's filesystem API is full of untyped edges. A path is a string, or an untyped object.
A missing file is a `null` here and an exception there. The crucial questions of policy are
buried in flag arguments, or fixed by the method which is chosen, and are invisible in the
type: does this copy replace what is already there, and does this delete empty a whole tree?
A program which gets one wrong destroys data.

Here these are lifted into the types:

- A path carries its platform, so its rules are known.
- An operation declares `raises Io.Error`, and the error has a specific reason: a missing
  file, a permission denied, a directory which is not empty.
- Each policy is an explicit [contextual value], so overwriting, or deleting recursively, is a
  decision which the code states, not a default it inherits.

Everything comes from the `soundness` package. The capabilities of the system, and the
policies which the operations need, are brought into scope:

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

```tabular
column  Imports
column  Decide
row  `filesystemBackends.javaBaseFilesystem`  which implementation touches the disk: here, `java.nio`
row  `pathInterfaces.pathOnLinux`  the type of path which the standard directories are given as
row  the five from `filesystemOptions`  the answers to the questions of policy, described under [Copying, moving and deleting](#copying-moving-and-deleting)
row  `charsets`, `codepages`, `textSanitizers`  how bytes become text, and text becomes bytes; [Characters](characters.md) describes them
row  `environments`, `systems`, `temporaryDirectories`  where the [environment](environment.md), system properties and temporary directory come from
row  `strategies.throwUnsafely`  that an error is thrown as an exception; [Errors](errors.md#choosing-a-strategy) has the other strategies
row  `stdios`, `termcapDefinitions`  where the samples print their results
```

The samples work in a directory of their own, beneath the system's temporary directory:

```scala
val directory = temporaryDirectory[Path on Linux] / "soundness-filesystem-tutorial"
val file = directory / "notes.txt"
```

### Files and directories

A path names a location. Whether it is a file or a directory is a matter of what is created
there. `create[Directory]` and `create[File]` bring an entry into being:

```scala fuel
typecheck
##
directory.create[Directory]()
file.create[File]()
```

By default, creating refuses to destroy anything. Creating over something which exists, or
under a parent which does not, is an `Io.Error`. Where either is the intent, a flag says so,
at the place where it happens, instead of an import saying it for the whole file:

```scala fuel
run
##
directory.create[Directory](CreateFlag.Replace, CreateFlag.Parents)
file.create[File](CreateFlag.Replace)
(directory / "deep" / "nested").create[Directory](CreateFlag.Parents)

Out.println(file.existent().inspect)
Out.println((directory / "deep" / "nested").existent().inspect)
```

`Fifo` and `Sock` are entries too, and are created in the same way.

### Reading and writing

Where a whole small file is wanted, `read` and `write` act directly on the path:

```scala fuel
run
##
file.write(t"Hello world")
Out.println(file.read[Text])
```

`read` takes the type to read the file as. `Text` is decoded with the charset in scope, and a
type with a decoder for a format reads in the same way, so `file.read[Json]` parses a
[JSON](json.md#parsing) file.

A larger file is read and written by *opening* it. `open` names the form to open the path as,
and the mode to open it in, and runs a block with a handle. Inside the block, `file` is the
open handle:

```scala fuel
run
##
file.open[File](Write, OpenFlag.Create): handle ?=>
  handle.write(Chain(t"Hello, world".in[Data]))

Out.println(file.open[File]()(file.read[Text]))
```

The handle exists only for the duration of the block, so the file's descriptor cannot outlive
the scope which owns it. This is the shape described under
[delimited scopes](../philosophy/delimited-scopes.md), and
[capture checking](../philosophy/capture-checking.md) enforces it.

The mode is not merely a flag at runtime. It is carried in the handle's type, as a set of
*grants*. Opening with the default mode, `Read`, gives a handle which grants only reading, and
`Read & Write` grants both. Writing through a handle which was opened only for reading does
not compile:

```scala fuel
compile
errors show
##
file.open[File](): handle ?=>
  handle.write(Chain(t"Hello, world".in[Data]))
```

A whole class of mistakes thereby becomes a compile error: writing through a read-only handle,
and holding a descriptor past its scope.

Opening a path which names a directory confines what may be reached through it. Paths which
are derived from the handle live on a *fresh plane* of their own. `..` does not compile on
such a path, and a path obtained from one open directory cannot be written under another:

```scala fuel
run
##
val names = directory.open[Directory](Read & Write): dir ?=>
  (dir / "greeting.txt").overwrite(t"Hello directory")
  dir.base.entries.map(_.name)

Out.println(names.to[List].has(t"greeting.txt").inspect)
```

### Exclusive and shared access

`Exclusive` is a third grant, beside `Read` and `Write`, and it means what it says. No other
scope in the program may hold an overlapping path open while it lasts. Overlap is by
containment, not by equality: a directory and something beneath it overlap, and two siblings
do not.

```scala fuel
typecheck
##
directory.open[Directory](Read & Exclusive): dir ?=>
  ()
```

Two ordinary reads may coexist. An exclusive open conflicts with an overlapping open in either
direction, whether the exclusive scope is the outer or the inner one. The conflict is an
`Io.Error` whose reason is `Busy`. It is raised at the second open, and not discovered later
as corruption. The claim is released when the scope ends, however it ends.

For a *file*, as opposed to a directory, `Exclusive` also takes a lock from the operating
system, so exclusivity holds against other processes, and not merely within this one. `Shared`
is the reader's counterpart. Any number of shared holders may coexist, and an exclusive open
waits for them, or fails at once if the lock is requested without blocking. A lock may also
cover a range of bytes instead of the whole file, for the record-oriented layouts which
databases use.

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

`create` is the counterpart of `open`. Where opening acts on something which exists, creating
brings it into being, and gives a handle over it for the same scope. Content written within
the scope is committed when the block completes, and a scope which fails leaves nothing
behind:

```scala fuel
typecheck
##
val target = directory / "payload.bin"

target.create[File](): handle ?=>
  handle.write(Chain(t"payload".in[Data]))

(directory / "inner").create[Directory](): dir ?=>
  (dir / "inner.txt").overwrite(t"hello")
```

A *scratch* directory is created and removed by its scope, whether the scope succeeds or
fails. A scratch *file* is likewise, for a throwaway index or a test fixture. Its handle also
knows its `path`, for whatever must be told where the file is:

```scala fuel
run
##
directory.open[Scratch](Read & Write): scratch ?=>
  (scratch / "file.txt").overwrite(t"data")

val content = directory.open[Scratch.File](Read & Write): file ?=>
  file.write(Chain(t"data".in[Data]))
  file.path.read[Text]

Out.println(content)
```

### Creation masks

A process creates files under its *umask*: the permission bits which the operating system
withholds from everything it creates. Ordinarily that is the end of the matter. But a process
which serves several callers at once, such as a [daemon](daemons.md), has one umask and many
invocations, each with its own.

The mask can therefore be a contextual value. A `Umask` in scope is applied to every file and
directory which is created while it applies. It is requested at creation, so the entry never
exists with wider permissions than were asked for. Where none is in scope, `Umask.process`
applies, which is the operating system's own, exactly as before:

```scala
def privately(): Unit =
  given umask: Umask = Umask(0x3f)

  (directory / "secret.txt").create[File](): handle ?=>
    handle.write(Chain(t"payload".in[Data]))
```

`0x3f` is the mask written `077` in octal, which leaves a file readable and writable by its
owner alone. A mask is parsed from the octal form which `umask` prints, with
`Umask.parse(t"022")`, and `octal` writes it back in the same form. A daemon's service handle
is a `Umask.Provider`, so within a `cli` block the invocation's mask is already in scope, with
no further declaration.

### The opening pattern, generally

`open` is not specific to files. A great many things share the same shape. A value says
*where* something is, whether a path, a buffer in memory or a URL, and getting at its contents
is a distinct, scoped act. Naming that pattern once means that it reads the same wherever it
applies.

An `Openable` instance relates a target to a *form*. That is why the form is a type argument,
and is not implied by the target. The same path opens as a `File`, as a `Directory`, as a
`Zip` [archive](archives.md), as a [PDF](pdf.md) or as an [image](images.md), and each is a
different handle with different operations. The form may be omitted where a target has only
one instance. Where it has several, the ambiguity is reported, with the alternatives listed.

Behind the pattern are the [typeclasses][typeclass] `Openable`, `Creatable` and `Sessional`,
for `open`, `create` and `session`. The mode which the first two take is a combination of
`Grant`s.

The forms which this page describes are:

```tabular
column  Form
column  The handle gives
row  `File`  the file's contents, to read or write as the grants allow
row  `Directory`  paths confined beneath the directory, on a fresh plane
row  `Scratch`  a directory removed when the scope ends
row  `Scratch.File`  a file removed when the scope ends, and its `path`
row  `Ram`  positional reads and writes over a memory-mapped file, of any size
row  `Watch`  a stream of the changes beneath a directory
```

Three things follow uniformly:

- The *mode*, which is `Read`, `Write`, `Exclusive`, `Shared` or a combination of them, is
  carried in the handle's type, so the grants which are requested are the operations which
  are permitted.
- Flags after the mode belong to the instance. They are specific to the kind of thing being
  opened, and an irrelevant flag does not typecheck.
- The handle is a [capability] confined to the block, so neither it, nor anything derived from
  it, can escape.

`create` is the same pattern for something which does not yet exist. `session` is the same
pattern for a target whose access is a conversation with something running, such as a
[browser](web-automation.md) or a [program being debugged](debugging.md), and not a handle
over stored bytes. In each case the scope is the lifetime, and the end of the block is the end
of the access.

### Positional access

A file which is opened as `Ram` is mapped into memory, and serves reads and writes at any
position, without streaming through it. The `Write` grant is required to write, as everywhere
else:

```scala fuel
run
##
file.write(t"Hello world")

val middle = file.open[Ram](): ram ?=>
  ram(2, 3)

file.open[Ram](Read & Write): ram ?=>
  ram(3L) = t"XYZ".in[Data]

Out.println(middle.utf8)
Out.println(file.read[Text])
```

`mapped` maps a whole file for reading, and lends a `Mapped` view of it for the length of a
block. Nothing is copied: integers are loaded from the mapped memory in place, little-endian
and at any offset, and a `slice` is a narrower view of the same memory. Several processes
which map one file share a single copy of it, in the operating system's page cache, which is
what makes a mapped file a way of sharing a large index between programs:

```scala fuel
run
##
file.write(t"0123456789ABCDEF")

val word = file.mapped(_.u32(0L))

val letters = file.mapped: view =>
  val tail = view.slice(10L, 6L)
  tail.copy(0L, 3).utf8

Out.println(word.show)
Out.println(letters)
```

The loads are these:

```tabular
column  Method
column  Gives
row  `u8`, `u16`  an unsigned byte or 16-bit integer, as an `Int`
row  `u32`  an unsigned 32-bit integer, as a `Long`
row  `i32`  the same 32 bits as a signed `Int`
row  `u64`  a 64-bit integer, as a `Long`
row  `slice`  a view of part of the memory, with no copy
row  `copy`  some of the bytes as `Data`, for a value which must outlive the view
row  `same`  whether the bytes at an offset are those of some `Data`, compared in place
```

The file is unmapped when the block ends. The view, and every slice of it, is a [capability]
confined to the block, so a load from memory which is no longer mapped cannot be written. A method which receives a view declares it as `Mapped^`. The same loads are
available on `Data`, with a big-endian `u16be`, `i32be`, `u32be` and `u64be` beside them, so a
parser reads a format the same way from either.

A mapped file should be replaced, by moving a new file onto its name, and not rewritten in
place: a view goes on seeing the file it mapped, unchanged, for as long as it is held. Mapping
is available on the JVM and on Scala Native; WASI has no memory mapping, and reports
`Unsupported`.

### Publishing versions

A mapped file is a way for one program to share a large value with others, without a copy in
each. The program which produces the value *owns* its name, and publishes one version after
another under it. The programs which use it map whichever version is current, and move on to
the next when they choose to. Nothing arbitrates between them, because two rules are kept:

- A published file never changes. Each version is written beside the name and moved over it
  in one step, so a reader which has mapped a version goes on seeing it, whole.
- A name has one owner. `own` holds an operating-system lock on `<name>.lock` for as long as
  its block runs, and a second owner, in this process or another, is refused with `Busy`.

`own` gives an `Owner`, whose `publish` writes a value in whatever form its `Encodable in Data`
instance gives, and returns the `Edition` it published:

```scala fuel
run
##
val edition = file.own[Text]: owner =>
  owner.publish(t"first edition")

Out.println(file.read[Text])
```

`published` gives a reader, a `Published`. Its `map` lends a `Mapped` view of the version it
holds, and its `refresh` moves it to the version which the name now leads to, and says whether
there was one:

```scala fuel
run
##
file.own[Text](_.publish(t"one"))

file.published[Text]: reader =>
  file.own[Text](_.publish(t"two"))
  val before = reader.map(_.u8(0L))
  val moved = reader.refresh()
  val after = reader.map(_.u8(0L))
  Out.println(t"$before, then $after")
```

A reader takes no lock, so it never delays the owner, and the owner never delays it. A view
which is in use when a `refresh` happens, on this thread or another, stays valid until its
block ends. Two editions are equal exactly when they are the same version of the file; they
have no order.

A reader keeps the version it has mapped in existence, even after a newer one has replaced it
under the name, so a long-lived reader should `refresh` when it learns that there is something
new, whether from a message or from [watching](#watching-for-changes) the name. The lock is
advisory, and is dependable between cooperating programs on one machine's own filesystems,
but not over a network filesystem.

A `Slice` is a window onto part of a file, with an offset and an extent. It is opened for
reading or writing like a file of its own. A region of a large file can therefore be streamed
in or out without the rest being touched, and a write cannot stray outside its window.

### Copying, moving and deleting

A path is copied, moved, linked or deleted with operations which name the destination, or act
in place:

```scala fuel
run
##
file.write(t"Hello world")
file.copyTo(directory / "backup.txt")
file.moveTo(directory / "renamed.txt")
(directory / "backup.txt").delete()

Out.println(file.existent().inspect)
Out.println((directory / "renamed.txt").existent().inspect)
Out.println((directory / "backup.txt").existent().inspect)
```

Each consults the policies in scope for the awkward cases, and each may raise an `Io.Error`.

Those policies are not defaults which can be left alone. Moving a file onto a path where
something already exists either destroys that thing, or refuses to. Neither answer is right in
general, and choosing one silently would make the wrong programs compile. So `moveTo` requires
either `overwritePreexisting` or `failOnPreexisting` in scope, and calling it with neither is
a compile error. That is instructive, as well as cautious: a reader who had not realized that
the question needed an answer is told that it does.

The choice also changes what must be handled. With `failOnPreexisting` in scope, a collision
is a failure to deal with. With `overwritePreexisting`, it cannot arise, and the obligation
goes away with it.

The policies are contextual values, so they can be imported for a whole file, or narrowed to
a single block. Each answers one question, and comes as one of a pair, from the package
`filesystemOptions`:

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

Replacing an existing entry deletes it first, so `overwritePreexisting` needs one of the
deletion policies in scope as well.

Each operation consults the policies which bear on it:

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

The `…Into` forms create any missing parents themselves. Deleting a directory which has
children is an `Io.Error`, whose reason is `DirectoryNotEmpty`, unless `deleteRecursively` is
in scope. Recursive deletion is thus a policy which must be enabled deliberately.

### Listing and inspecting

`children` gives a directory's immediate children, and `descendants` gives its whole subtree.
A path reports whether it exists, its size, and what kind of entry it is. A `glob` selects
children by a pattern, in which `*` matches within a name, and a whole segment `**` spans
directories:

```scala fuel
run
##
(directory / "renamed.txt").write(t"Hello world")

Out.println((directory / "renamed.txt").existent().inspect)
Out.println((directory / "renamed.txt").filesize().inspect)
Out.println(directory.children.map(_.name).to[List].has(t"renamed.txt").inspect)
Out.println(directory.glob(Glob.parse(t"*.txt")).map(_.name).to[List].has(t"renamed.txt").inspect)
```

Two paths can name one entry, through symbolic links, or through `/tmp` and `/private/tmp` on
macOS. `canonical` is the entry's path as the filesystem itself knows it, with every link
resolved. That is the path which a registry keyed by path should store:

```scala fuel
typecheck
##
val canonical = (directory / "renamed.txt").canonical()
```

[Patterns](patterns.md) describes globs.

### Content-addressed storage

A directory of blobs, each named by the digest of its own bytes, is a *content-addressed
store*. A blob is written once, verified as it is written, and found by anything which knows
its digest. `Cas` is that store. The [hash algorithm](hashing.md) is in its type, and in the
type of every digest it accepts, so a SHA-256 store cannot be asked for a BLAKE3 digest:

```scala
import providers.javaBaseProvider

val store: Cas in Sha2[256] on (Path on Linux) = Cas[Sha2[256]](directory, t".blob")
```

```scala fuel
run
##
val digest: Digest in Sha2[256] = store() = t"some bytes".in[Data]

Out.println(store(digest).let(_.utf8).or(t"missing"))
Out.println(store.has(digest).inspect)
Out.println(store.missing(List(digest)).inspect)
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

`prune` is how a cache is trimmed to what is still referenced.

A blob is written to a sibling with a unique name, and moved into place atomically, so a
reader never sees a partial blob. The store's root is a `Path` here, but its type is the type
of that root, and a `Cas.Storage` instance for another kind of root makes a store over it
with the same interface. As a [collection](collections.md), a store is its digests:
`store.size`, `store.has(digest)`, `store.at(digest)` and traversal all work on it.

### Extended attributes and volumes

The filesystem which a path sits on is a typed fact about it. `Ext4`, `Btrfs`, `Apfs`, `Ntfs`
and `Dos` are the *axes* which a path can be matched against. A filesystem which supports
extended attributes lets a path carry named metadata beyond its contents:

```scala fuel
typecheck
##
val attributes = file match
  case Apfs(path) => path.attribute(t"origin", t"soundness".in[Data]) yet path.attributes()
  case Ext4(path) => path.attribute(t"origin", t"soundness".in[Data]) yet path.attributes()
  case _          => Nil
```

On btrfs, `subvolume` reports the subvolume which a path belongs to, and `subvolumeRoot`
reports whether it is the top of one.

### Standard directories

The conventional locations of a system are named values, resolved against the
[environment](environment.md). The home directory and the paths beneath it, and the system
directories under the root, are reached by navigating and applying:

```scala fuel
typecheck
##
val home = Home[Path on Linux]()
val cache = Home.Cache[Path on Linux]()
val bin = Home.Local.Bin[Path on Linux]()
val share = Base.Usr.Share[Path on Linux]()
```

Each object is named for the directory it stands for, capitalized, with any leading `.`
dropped, since `.` is the separator between members. So `$HOME/.local/bin` is
`Home.Local.Bin`, and `/var/lib` is `Base.Var.Lib`. Writing a standard location in this way,
and not as text, means that a mistyped directory does not compile:

```scala fuel
compile
errors show
##
Home.Cashe[Path on Linux]()
```

These are the layouts, and whether `readOnly` marks each as not meant to be written to:

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

A layout is a fixed place. `Home.Config` is always `$HOME/.config`, and only `$HOME` is read
from the environment. The locations which the
[XDG specification](environment.md#standard-directories) lets the user move, such as
`$XDG_CONFIG_HOME`, are given by `Xdg` instead. Applying a layout constructs a value of
whichever directory type is asked for, so the same names serve a `Path on Linux` and any
other representation.

### Search paths

Many files are looked up not at one location, but along a list of them: an icon in whichever
of the XDG data directories holds it, or a configuration file in the first of several places.
A `Searchpaths.Stems` instance names the directories to try, in order. A relative path on the
corresponding plane is resolved with `search` to the first of them which holds it, or to
`Unset`:

```scala fuel
typecheck
##
import galilei.search

given stems: (Searchpaths.Stems on Xdg.Data onto Linux) = new Searchpaths.Stems:
  type Plane = Xdg.Data
  type Target = Linux
  val stems: List[Path on Linux] = List(directory, directory / "inner")

val found = (% / "inner.txt").on[Xdg.Data].search()
```

`dataSearch` and `configSearch` supply the XDG search paths for data and configuration, from
the environment, so a program's resources are found wherever the user's system keeps them.

### Watching for changes

A directory is watched by opening it as `Watch`, which gives a [stream](streams.md) of
`Watch.Event`s: a file created, modified or deleted, or a directory created. The registration
lasts exactly as long as the block:

```scala
def observe(): Unit =
  directory.open[Watch](): watcher ?=>
    watcher.stream.each:
      case Watch.Event.NewFile(dir, file) => Out.println(t"created $file")
      case Watch.Event.Modify(dir, file)  => Out.println(t"modified $file")
      case Watch.Event.Delete(dir, file)  => Out.println(t"deleted $file")
      case _                              => ()
```

Several paths are watched together by opening a list of them, which gives one stream of
events across all of them.

Editors save in bursts, and a rebuild wants one notification for each burst, not one for each
event. `batches` gives a `Watch.Batch` each time no event has arrived for the given quiet
period, and its `paths` lists every changed path once:

```scala
def rebuild(changed: List[Path on Linux]): Unit = ()

def observeBatches(): Unit =
  directory.open[Watch](): watcher ?=>
    watcher.batches(0.2*Second).each: batch =>
      rebuild(batch.paths[Path on Linux])
```

The default watcher uses the operating system's own notifications of changes to files. Where
those are unavailable, `watchers.polling` checks at an interval instead.

### Choosing a backend

Nothing above names a platform's API. The primitive operations which a filesystem must offer,
which are stat, open, read, write, list, link and delete, are gathered into a
`FilesystemBackend` for a plane, and everything else is defined in terms of them.

The `java.nio` implementation is `filesystemBackends.javaBaseFilesystem`. A WASI
implementation, over `wasi:filesystem`, is supplied for WebAssembly components, so the same
code reads and writes files on the JVM and inside a component. An operation which a backend
cannot support raises an `Io.Error` whose reason is `Unsupported`. It is never approximated.
Narrowing the platform's surface to a seam this small is
[decoupling](../philosophy/decoupling.md) applied within one library.

### Related

- [Paths](paths.md) describes the paths which these operations act on.
- [Streams](streams.md) reads and writes files a part at a time.
- [Archives](archives.md) opens a path as a ZIP or tar archive.
- [Compression](compression.md#compressing-a-stream) writes a compressed file.
- [Hashing](hashing.md#hashing-a-stream) computes the checksum of a file or a directory.
