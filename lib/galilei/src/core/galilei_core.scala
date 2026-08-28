                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package galilei

import java.io as ji
import java.nio.file as jnf

import anticipation.*
import contingency.*
import denominative.*
import kaleidoscope.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import symbolism.*
import turbulence.{Aggregable, Streamable}
import zephyrine.Credit
import vacuous.*

import Io.Error.{Operation, Reason}

final val C: Drive = Drive('C')
final val D: Drive = Drive('D')

extension (inline context: StringContext)
  transparent inline def p(): Path = ${galilei.internal.path('context)}

extension [target: Substantiable](value: target)
  def existent(): Boolean = target.existence(value)

// The contextual file handle within an `open` block, in the manner of facsimile's `pdf`.
// Transparent inline so the handle's precise (grant-refined, capturing) type is preserved: a
// non-inline accessor would widen the scoped capture set, losing the `Granting` refinement
// that gates read and write operations.
transparent inline def file(using handle: galilei.Handle^): handle.type = handle

// The contextual directory handle within an `open[Directory]` block.
transparent inline def dir(using handle: galilei.Directory.Handle^): handle.type = handle

package filesystemTraversal:
  given preOrderTraversal: TraversalOrder = TraversalOrder.PreOrder
  given postOrderTraversal: TraversalOrder = TraversalOrder.PostOrder

extension [plane: Filesystem](path: Path on plane)

  inline def children(using explorable: plane is Explorable): Chain[Path on plane] =
    explorable.children(path)

  // Write `content` to the file in its entirety as a single, direct operation: the whole file is
  // written at once, holding no handle and needing no scope — unlike streaming to a path, which
  // must be `open`ed and consumed within a scope.
  // A real `using` clause rather than the `raises` sugar: a context-function result would
  // hide the `streamable` parameter, which the separation checker rejects.
  def write[content](content: content)
    ( using streamable: (content is Streamable by Data over Credit)^ )
    ( using Tactic[Io.Error]^ )
  :   Unit =
    val bytes: Data = summon[Data is Aggregable by Data].accept(streamable.stream(content))
    protect(Operation.Write)(jnf.Files.write(nioPath, Array.unsafeJvm(bytes)))

  // Inline, so the thunk never crosses a checked context-function boundary (which would
  // hide the `block` parameter); the body is checked at each expansion site instead.
  private[galilei] inline def protect[result](operation: Operation)(inline block: result)
  :   result raises Io.Error =

    import Reason.*

    try block catch
      case break: boundary.Break[?]          => throw break
      case _: jnf.NoSuchFileException        => abort(Io.Error(path, operation, Nonexistent))
      case _: jnf.FileAlreadyExistsException => abort(Io.Error(path, operation, AlreadyExists))
      case _: jnf.DirectoryNotEmptyException => abort(Io.Error(path, operation, DirectoryNotEmpty))
      case _: jnf.AccessDeniedException      => abort(Io.Error(path, operation, PermissionDenied))
      case _: jnf.NotDirectoryException      => abort(Io.Error(path, operation, IsNotDirectory))
      case _: SecurityException              => abort(Io.Error(path, operation, PermissionDenied))
      case _: jnf.FileSystemLoopException    => abort(Io.Error(path, operation, Cycle))
      case _: jnf.FileSystemException        => abort(Io.Error(path, operation, IsDirectory))
      case other                             => abort(Io.Error(path, operation, Unsupported))

  // Internal only: the public `java.nio` interop lives in `galilei.jvm`, whose `javaPath` and
  // `javaFile` are the ones users reach for. This one exists because `core`'s own operations
  // call `jnf.Files` directly and cannot depend on `jvm`.
  private[galilei] def nioPath: jnf.Path = jnf.Path.of(Path.encodable.encode(path).s).nn


  // Scoped positional (random-access) reading (issue #1608): passes a `zephyrine.Expanse`
  // view of the file — `size` plus pread-style `read(offset, length)` — valid for the scope
  // of the call. Platform-neutral: every filesystem backend implements it, so seek-oriented
  // format readers need neither `java.nio` nor a whole-file memory mapping, and files beyond
  // 2 GiB are addressable.
  // A real `using` clause rather than the `raises` sugar: a context-function result would
  // hide the `lambda` parameter, which the separation checker rejects (as on `write` above).
  def expanse[result](lambda: zephyrine.Expanse => result)
    ( using backend: FilesystemBackend on plane )
    ( using Tactic[Io.Error]^ )
  :   result =

    backend.expanse(path)(lambda)


  def descendants(using DereferenceSymlinks, TraversalOrder, plane is Explorable)
  :   Chain[Path on plane] raises Io.Error =

    path.children.bind: child =>
      summon[TraversalOrder] match
        case TraversalOrder.PreOrder  => child #:: child.descendants
        case TraversalOrder.PostOrder => child.descendants #::: Chain(child)


  // Expands `pattern` against the directory tree beneath this path, walking segment-wise:
  // each ordinary segment filters the current directories' `children` by match, and a segment
  // which is exactly `**` matches any number of intermediate directories — so only the
  // directories on the pattern's spine are ever listed, rather than every descendant being
  // walked and filtered. The pattern is relative to this path, which should name a directory.
  // A `**` mixed into a segment (`a**b`) matches within a single name, exactly as `*` does:
  // only a whole-segment `**` spans directories.
  def glob(pattern: Glob)
    ( using explorable: plane is Explorable,
            symlinks:   DereferenceSymlinks,
            backend:    FilesystemBackend on plane )
  :   List[Path on plane] raises Io.Error =

    import Glob.Token

    def segments(tokens: scala.List[Token]): scala.List[scala.List[Token]] =
      val head = tokens.takeWhile(_ != Token.Exact('/'))
      val tail = tokens.dropWhile(_ != Token.Exact('/'))
      if tail.isEmpty then scala.List(head) else head :: segments(tail.tail)

    def directory(candidate: Path on plane): Boolean =
      safely(candidate.entry() == Directory).or(false)

    def recur(dirs: scala.List[Path on plane], todo: scala.List[scala.List[Token]])
    :   scala.List[Path on plane] =

      todo match
        case scala.Nil => dirs

        case scala.::(segment, rest) =>
          if segment == scala.List(Token.Globstar) then
            // `**` matches zero or more directories: the rest of the pattern is expanded both
            // here and, with the `**` retained, in every subdirectory.
            val deeper = dirs.flatMap: dir =>
              recur(dir.children.stdlib.toList.filter(directory), todo)

            recur(dirs, rest) ++ deeper
          else
            val matcher = Glob(segment*)

            val matched = dirs.flatMap: dir =>
              dir.children.stdlib.toList.filter { child => matcher.matches(child.name) }

            recur(if rest.isEmpty then matched else matched.filter(directory), rest)

    recur(scala.List(path), segments(pattern.tokens.toList)).distinct.to(List)


  // Named `filesize` (not `size`): a same-named export beside the collections' `size` at the
  // `soundness` umbrella makes extension resolution commit to the wrong candidate.
  def filesize()(using plane is Explorable, FilesystemBackend on plane): Bytes raises Io.Error =
    import filesystemOptions.dereferenceSymlinks.disabled
    given TraversalOrder = TraversalOrder.PreOrder

    descendants.stdlib.fuse(summon[FilesystemBackend on plane].stat(path, false).size.b):
      state + next.filesize()

  def delete()(using deleteRecursively: DeleteRecursively on plane)
    ( using backend: FilesystemBackend on plane )
  ( using Tactic[Io.Error], (Io.Event is Loggable)^ )
  :   Path on plane =

    // Created and consumed under the same ambient tactic; no aliased writer.
    scala.caps.unsafe.unsafeAssumeSeparate:
      deleteRecursively.conditionally(path)(backend.delete(path))
    Log.info(Io.Event.Delete(path.show))
    path


  def wipe()(using deleteRecursively: DeleteRecursively on plane)(using io: Tactic[Io.Error])
    ( using backend: FilesystemBackend on plane )
    ( using (Io.Event is Loggable)^ )
  :   Path on plane =

    // As above: same ambient tactic on both sides.
    scala.caps.unsafe.unsafeAssumeSeparate:
      deleteRecursively.conditionally(path)(backend.deleteIfExists(path))
    Log.info(Io.Event.Delete(path.show))
    path


  def volume()(using backend: FilesystemBackend on plane): Volume raises Io.Error =
    backend.volume(path)

  // The entry's identity on its storage device — its device number and inode number — which
  // together name a file uniquely: two paths with equal identities are hard links to one file.
  // `Unset`, rather than raising, where the platform does not record it: WASI's
  // `descriptor-stat` carries neither number, and the JVM's `unix` view is absent off POSIX.
  // Named for `entry()` rather than plainly `identity`, which would compete with
  // `proscenium`'s function of that name wherever the `soundness` umbrella is imported.
  def entryIdentity(using backend: FilesystemBackend on plane): Optional[Stat.Identity] =
    backend.identity(path, true)

  def hardLinkTo(destination: Path on plane)
    ( using overwritePreexisting: OverwritePreexisting on plane,
            createNonexistentParents: CreateNonexistentParents on plane,
            backend:                  FilesystemBackend on plane )
  ( using Tactic[Io.Error], (Io.Event is Loggable)^ )
  :   Path on plane =

    // Created and consumed under the same ambient tactic; no aliased writer.
    scala.caps.unsafe.unsafeAssumeSeparate:
      createNonexistentParents(destination):
        overwritePreexisting(destination):
          backend.hardLink(destination, path)

    Log.info(Io.Event.HardLink(path.show, destination.show))
    destination


  def entry()(using symlinks: DereferenceSymlinks)
    ( using backend: FilesystemBackend on plane )
  :   Entry raises Io.Error =

    backend.stat(path, symlinks.dereference).entry


  def copyTo(destination: Path on plane)
    ( using overwritePreexisting: OverwritePreexisting on plane,
            dereferenceSymlinks:      DereferenceSymlinks,
            createNonexistentParents: CreateNonexistentParents on plane )
    ( using FilesystemBackend on plane )
  ( using Tactic[Io.Error], (Io.Event is Loggable)^ )
  :   Path on plane =

    // Created and consumed under the same ambient tactic; no aliased writer.
    scala.caps.unsafe.unsafeAssumeSeparate:
      createNonexistentParents(destination):
        overwritePreexisting(destination):
          summon[FilesystemBackend on plane].copy(path, destination, dereferenceSymlinks.dereference)

    Log.info(Io.Event.Copy(path.show, destination.show))
    destination


  def copyInto(destination: Path on plane)
    ( using overwritePreexisting: OverwritePreexisting on plane,
            dereferenceSymlinks:  DereferenceSymlinks,
            substantiable:        (Path on plane) is Substantiable )
    ( using FilesystemBackend on plane )
  :   Path on plane raises Io.Error =

    given CreateNonexistentParents on plane =
      filesystemOptions.createNonexistentParents.enabled[plane]

    val file2: Path on plane = unsafely(destination.child(path.descent.head))
    copyTo(file2)


  def moveTo(destination: Path on plane)
    ( using overwritePreexisting: OverwritePreexisting on plane,
            moveAtomically:           MoveAtomically,
            dereferenceSymlinks:      DereferenceSymlinks,
            createNonexistentParents: CreateNonexistentParents on plane )
    ( using backend: FilesystemBackend on plane )
  ( using Tactic[Io.Error], (Io.Event is Loggable)^ )
  :   Path on plane =

    // Created and consumed under the same ambient tactic; no aliased writer.
    scala.caps.unsafe.unsafeAssumeSeparate:
      createNonexistentParents(destination):
        overwritePreexisting(destination):
          backend.move(path, destination, moveAtomically.atomic, dereferenceSymlinks.dereference)

    Log.info(Io.Event.Move(path.show, destination.show))
    destination


  def moveInto
    ( destination: Path on plane )
    ( using overwritePreexisting: OverwritePreexisting on plane,
            moveAtomically:       MoveAtomically,
            substantiable:        (Path on plane) is Substantiable,
            dereferenceSymlinks:  DereferenceSymlinks )
    ( using FilesystemBackend on plane )
  :   Path on plane raises Io.Error =

    import filesystemOptions.createNonexistentParents.enabled
    moveTo(unsafely(destination.child(path.descent.head)))


  def symlinkTo(destination: Path on plane)
    ( using overwritePreexisting: OverwritePreexisting on plane,
            createNonexistentParents: CreateNonexistentParents on plane,
            backend:                  FilesystemBackend on plane )
  ( using Tactic[Io.Error], (Io.Event is Loggable)^ )
  :   Path on plane =

    // Created and consumed under the same ambient tactic; no aliased writer.
    scala.caps.unsafe.unsafeAssumeSeparate:
      createNonexistentParents(destination):
        overwritePreexisting(destination):
          backend.symlink(destination, path)

    Log.info(Io.Event.Symlink(destination.show, path.show))
    destination


  def symlinkInto
    ( destination: Path on plane )
    ( using overwritePreexisting: OverwritePreexisting on plane,
            moveAtomically:       MoveAtomically,
            substantiable:        (Path on plane) is Substantiable,
            dereferenceSymlinks:  DereferenceSymlinks )
    ( using FilesystemBackend on plane )
  :   Path on plane raises Io.Error =

    import filesystemOptions.createNonexistentParents.enabled
    symlinkTo(unsafely(destination.child(path.descent.head)))


  def modified[instant: Instantiable across Instants from Long as instantiable]()
    ( using backend: FilesystemBackend on plane )
  :   instant raises Io.Error =

    instantiable.apply(backend.stat(path, true).modified)

  def accessed[instant: Instantiable across Instants from Long as instantiable]()
    ( using backend: FilesystemBackend on plane )
  :   instant raises Io.Error =

    instantiable.apply(backend.stat(path, true).accessed)

  def readable(using FilesystemBackend on plane): FilesystemAttribute.Readable[plane] =
    FilesystemAttribute.Readable(path)

  def writable(using FilesystemBackend on plane): FilesystemAttribute.Writable[plane] =
    FilesystemAttribute.Writable(path)

  def hidden()(using backend: FilesystemBackend on plane): Boolean raises Io.Error =
    backend.hidden(path)

  def touch()(using backend: FilesystemBackend on plane)
    ( using Tactic[Io.Error], (Io.Event is Loggable)^ )
  :   Unit =
    backend.touch(path)
    Log.fine(Io.Event.Touch(path.show))

extension (path: Path on Windows)
  def created[instant: Instantiable across Instants from Long as instantiable]()
    ( using backend: FilesystemBackend on Windows )
  :   instant raises Io.Error =

    instantiable.apply:
      backend.stat(path, true).created.or:
        abort(Io.Error(path, Operation.Metadata, Reason.Unsupported))

extension [plane <: Posix: Filesystem](path: Path on plane)
  def executable(using FilesystemBackend on plane): FilesystemAttribute.Executable[plane] =
    FilesystemAttribute.Executable(path)

  def hardLinks()(using dereferenceSymlinks: DereferenceSymlinks)
    ( using backend: FilesystemBackend on plane )
  :   Int raises Io.Error =

    backend.hardLinkCount(path, dereferenceSymlinks.dereference)

package filesystemOptions:
  object dereferenceSymlinks:
    given enabled: DereferenceSymlinks:
      def dereference = true

    given disabled: DereferenceSymlinks:
      def dereference = false

  object moveAtomically:
    given enabled: MoveAtomically:
      def atomic = true

    given disabled: MoveAtomically:
      def atomic = false

  object copyAttributes:
    given enabled: CopyAttributes:
      def attributes = true

    given disabled: CopyAttributes:
      def attributes = false

  object deleteRecursively:
    given enabled: [plane: Filesystem]
    =>  ( explorable: plane is Explorable, backend: FilesystemBackend on plane )
    =>  DeleteRecursively on plane:

      type Plane = plane

      def recur(path: Path on plane): Unit raises Io.Error =
        path.children.each(recur(_))
        backend.delete(path)

      def conditionally[result](path: Path on Plane)(operation: => result)
      :   (Tactic[Io.Error]^) ?->{operation} result =
        path.children.each(recur(_)) yet operation

    given disabled: [plane: {Filesystem, Explorable}] => DeleteRecursively on plane:

      type Plane = plane

      def conditionally[result](path: Path on Plane)(operation: => result)
      :   (Tactic[Io.Error]^) ?->{operation} result =
        if !path.children.nil
        then abort(Io.Error(path, Io.Error.Operation.Delete, Reason.DirectoryNotEmpty))
        else operation

  object overwritePreexisting:
    given enabled: [plane: Filesystem]
    =>  ( deleteRecursively: DeleteRecursively on plane )
    =>  OverwritePreexisting on plane:

      type Plane = plane

      def apply[result](path: Path on Plane)(operation: => result)
      :   (Tactic[Io.Error]^) ?->{operation} result =
        deleteRecursively.conditionally(path)(operation)

    // The backend raises `AlreadyExists` itself when the operation collides with an existing
    // entry, so nothing needs intercepting here.
    given disabled: [plane: Filesystem] => OverwritePreexisting on plane:

      type Plane = plane

      def apply[result](path: Path on Plane)(operation: => result)
      :   (Tactic[Io.Error]^) ?->{operation} result =
        operation

  object createNonexistentParents:
    given enabled: [plane: Filesystem]
    =>  ( backend: FilesystemBackend on plane )
    =>  CreateNonexistentParents on plane:

      def apply[result](path: Path on plane)(operation: => result)
      :   (Tactic[Io.Error]^) ?->{operation} result =
        def ensure(directory: Path on plane): Unit =
          if !backend.exists(directory, true) then
            safely(directory.parent).let(ensure(_))
            backend.createDirectory(directory)

        safely(path.parent).let(ensure(_))
        operation

    given disabled: [plane: Filesystem] => CreateNonexistentParents on plane:

      type Plane = plane

      def apply[result](path: Path on plane)(block: => result)
      :   (Tactic[Io.Error]^) ?->{block} result =
        block


// Extended attributes, gated by the storage-filesystem axis (issue #567): available only on a
// path whose filesystem is known — by extractor probing — to support them. The setter is the
// two-argument overload: `path.attribute(t"user.origin", data)`.
extension [plane: Filesystem, transport <: Attributed](path: Path on plane over transport)
  def attributes()(using backend: FilesystemBackend on plane): List[Text] raises Io.Error =
    backend.attributes(path)

  // Generic in its result — decoded through `Aggregable`, like turbulence's `read` — so the
  // fresh capabilities of a byte-array result bind at the call site; a concrete `Data` result
  // cannot cross the umbrella's synthesized export forwarder under capture checking.
  def attribute[content: Aggregable by Data](name: Text)
    ( using backend: FilesystemBackend on plane )
  :   Optional[content] raises Io.Error =

    backend.attribute(path, name).absolve match
      case Unset => Unset

      case data: Data =>
        summon[content is Aggregable by Data].accept(zephyrine.Stream(scala.Iterator(data)))

  def attribute(name: Text, value: Data)(using backend: FilesystemBackend on plane)
  :   Unit raises Io.Error =
    backend.attribute(path, name, value)

// Btrfs-specific metadata, gated by the storage-filesystem axis (issue #567). Btrfs gives every
// subvolume its own anonymous device number and numbers every subvolume root's inode 256, so both
// accessors here are answered from `stat` alone — no `ioctl` — at the cost of not being able to
// report a subvolume's ID or UUID, which only `BTRFS_IOC_GET_SUBVOL_INFO` knows.
extension [plane: Filesystem](path: Path on plane over Btrfs)

  // The subvolume the entry is stored in, found by ascending while the device number holds:
  // crossing out of a subvolume changes it, so the last ancestor to share the entry's device
  // number is the subvolume's root.
  def subvolume()(using FilesystemBackend on plane): Btrfs.Subvolume[plane] raises Io.Error =

    val device =
      path.entryIdentity.let(_.device).or:
        abort(Io.Error(path, Operation.Metadata, Reason.Unsupported))

    def ascend(current: Path on plane): Path on plane =
      current.parent.lay(current): parent =>
        if parent.entryIdentity.let(_.device) == device then ascend(parent) else current

    Btrfs.Subvolume(ascend(path).asInstanceOf[Path on plane over Btrfs])

  // Whether the entry is itself the root of a subvolume. Total rather than raising, like
  // `exists`: an entry whose inode cannot be read is not a subvolume root as far as any
  // operation gated on this can tell.
  def subvolumeRoot(using FilesystemBackend on plane): Boolean =
    path.entryIdentity.let(_.inode) == 256L

// Metadata gated by the storage-filesystem axis (issue #567): available only on a path whose
// filesystem is known — by extractor probing — to record it.
extension [plane: Filesystem, transport <: CreationTimed](path: Path on plane over transport)
  // Total, unlike `Stat.created`: the `CreationTimed` bound says the filesystem records
  // creation times, so an absent value is a backend failure rather than an expected case.
  def creation[instant: Instantiable across Instants from Long as instantiable]()
    ( using backend: FilesystemBackend on plane )
  :   instant raises Io.Error =

    instantiable.apply:
      backend.stat(path, true).created.or:
        abort(Io.Error(path, Operation.Metadata, Reason.Unsupported))
