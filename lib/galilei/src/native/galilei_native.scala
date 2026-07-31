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

import scala.caps

import java.io as ji
import java.nio.channels as jnc
import java.nio.file as jnf
import java.nio.file.attribute as jnfa

import anticipation.*
import contingency.*
import gossamer.*
import prepositional.*
import serpentine.*
import turbulence.*
import vacuous.*

import IoError.{Operation, Reason}

package filesystemBackends:
  given native: [plane: Filesystem] => FilesystemBackend on plane =
    new FilesystemBackend:
      type Plane = plane

      private def javaPath(path: Path on Plane): jnf.Path =
        jnf.Path.of(Path.encodable.encode(path).s).nn

      private def dereferenceOptions(dereference: Boolean): List[jnf.LinkOption] =
        if dereference then Nil else List(jnf.LinkOption.NOFOLLOW_LINKS)

      // Maps `java.nio`'s failure exceptions onto the common `Reason` vocabulary, as informatively
      // as their types (and, for `FileSystemException`, their `getReason` texts) permit.
      // Inline, as the core `Path.protect`: the thunk must not cross a checked boundary.
      private inline def protect[result](path: Path on Plane, operation: Operation)
        (inline block: result)
        ( using Tactic[IoError] )
      :   result =

        import Reason.*

        def filesystemReason(error: jnf.FileSystemException): Reason =
          val message = Optional(error.getReason).let(_.tt.lower).or(t"")

          if message.contains(t"not a directory") then IsNotDirectory
          else if message.contains(t"is a directory") then IsDirectory
          else if message.contains(t"cross-device") || message.contains(t"different disk")
          then NotSameVolume
          else if message.contains(t"too many links") then TooManyLinks
          else if message.contains(t"name too long") then NameTooLong
          else if message.contains(t"quota") then QuotaExceeded
          else if message.contains(t"no space") then StorageFull
          else if message.contains(t"read-only") then ReadOnly
          else if message.contains(t"busy") then Busy
          else if message.contains(t"operation not permitted") then PermissionDenied
          else Unsupported

        def fail(reason: Reason): Nothing = abort(IoError(path, operation, reason))

        try block catch
          case break: boundary.Break[?]               => throw break
          case _: jnf.NoSuchFileException             => fail(Nonexistent)
          case _: jnf.FileAlreadyExistsException      => fail(AlreadyExists)
          case _: jnf.DirectoryNotEmptyException      => fail(DirectoryNotEmpty)
          case _: jnf.AccessDeniedException           => fail(PermissionDenied)
          case _: jnf.NotDirectoryException           => fail(IsNotDirectory)
          // (Scala Native's javalib has no `AtomicMoveNotSupportedException`; an unsupported atomic
          // move surfaces as a generic `FileSystemException` below and maps to `Unsupported`.)
          case _: SecurityException                   => fail(PermissionDenied)
          case _: jnf.FileSystemLoopException         => fail(Cycle)
          case error: ji.InterruptedIOException       => fail(Interrupted)
          case error: jnf.FileSystemException         => fail(filesystemReason(error))
          case error: ji.IOException                  => fail(Physical)
          case other                                  => fail(Unsupported)

      def stat(path: Path on Plane, dereference: Boolean)(using Tactic[IoError]): Stat =
        protect(path, Operation.Metadata):
          // Individual-argument varargs calls, never an array splice: under the Scala Native
          // javalib the varargs formals are pure Scala arrays, which no array value can
          // satisfy under separation checking.
          val attributes =
            ( if dereference
              then jnf.Files.readAttributes(javaPath(path), classOf[jnfa.BasicFileAttributes])
              else jnf.Files.readAttributes
                     ( javaPath(path), classOf[jnfa.BasicFileAttributes],
                       jnf.LinkOption.NOFOLLOW_LINKS ) )
            . nn

          val entry: Entry =
            if attributes.isSymbolicLink then Symlink
            else if attributes.isRegularFile then File
            else if attributes.isDirectory then Directory
            else
              try
                ( if dereference then jnf.Files.getAttribute(javaPath(path), "unix:mode")
                  else jnf.Files.getAttribute
                         (javaPath(path), "unix:mode", jnf.LinkOption.NOFOLLOW_LINKS) )
                . nn.absolve match
                case mode: Int => (mode & 61440) match
                  case  4096 => Fifo
                  case  8192 => CharDevice
                  case 24576 => BlockDevice
                  case 49152 => Socket
                  case _     => File
              catch case _: Exception => File

          // `FileTime.toMillis` directly, rather than `.toInstant.toEpochMilli` (the JVM backend's
          // way): Scala Native's javalib has no `java.time.Instant.MIN`, which `FileTime.toInstant`
          // references, so the `Instant` round-trip fails to link.
          val created: Optional[Long] =
            val time = attributes.creationTime().nn.toMillis
            if time == 0L then Unset else time

          Stat
            ( entry,
              attributes.size(),
              attributes.lastModifiedTime().nn.toMillis,
              attributes.lastAccessTime().nn.toMillis,
              created )

      def exists(path: Path on Plane, dereference: Boolean): Boolean =
        if dereference then jnf.Files.exists(javaPath(path))
        else jnf.Files.exists(javaPath(path), jnf.LinkOption.NOFOLLOW_LINKS)

      def children(path: Path on Plane)(using Tactic[IoError]): Chain[Text] =
        protect(path, Operation.Read):
          if !jnf.Files.isDirectory(javaPath(path)) then Chain()
          else
            // `Files.list` holds the directory's file descriptor until the stream is
            // closed — exhausting its iterator does not release it, and a `Chain` would
            // defer even that — so the names are materialized strictly and the stream
            // closed before returning. Left unclosed, each directory listed leaks a
            // descriptor until its stream is garbage-collected, which a low-allocation
            // process may never do: a long traversal-heavy run then exhausts the process's
            // file-descriptor limit.
            val stream = jnf.Files.list(javaPath(path)).nn

            try
              Chain.from:
                stream.iterator().nn.asScala.map(_.getFileName.nn.toString.tt).toList
            finally stream.close()

      def createDirectory(path: Path on Plane)(using Tactic[IoError]): Unit =
        protect(path, Operation.Create)(jnf.Files.createDirectory(javaPath(path)))

      def createFile(path: Path on Plane)(using Tactic[IoError]): Unit =
        protect(path, Operation.Create)(jnf.Files.createFile(javaPath(path)))

      def createFifo(path: Path on Plane)(using Tactic[IoError]): Unit =
        protect(path, Operation.Create):
          val process =
            new ProcessBuilder("mkfifo", Path.encodable.encode(path).s).start().nn

          if process.waitFor() != 0
          then abort(IoError(path, Operation.Create, Reason.Unsupported))

      def delete(path: Path on Plane)(using Tactic[IoError]): Unit =
        protect(path, Operation.Delete)(jnf.Files.delete(javaPath(path)))

      def deleteIfExists(path: Path on Plane)(using Tactic[IoError]): Unit =
        protect(path, Operation.Delete)(jnf.Files.deleteIfExists(javaPath(path)))

      def symlink(link: Path on Plane, target: Path on Plane)(using Tactic[IoError]): Unit =
        protect(link, Operation.Create):
          jnf.Files.createSymbolicLink(javaPath(link), javaPath(target))

      def hardLink(link: Path on Plane, target: Path on Plane)(using Tactic[IoError]): Unit =
        protect(link, Operation.Create)(jnf.Files.createLink(javaPath(link), javaPath(target)))

      def copy(source: Path on Plane, destination: Path on Plane, dereference: Boolean)
        ( using Tactic[IoError] )
      :   Unit =

        protect(source, Operation.Copy):
          if dereference then jnf.Files.copy(javaPath(source), javaPath(destination))
          else
            jnf.Files.copy
              (javaPath(source), javaPath(destination), jnf.LinkOption.NOFOLLOW_LINKS)

      def move
        ( source:      Path on Plane,
          destination: Path on Plane,
          atomic:      Boolean,
          dereference: Boolean )
        ( using Tactic[IoError] )
      :   Unit =

        protect(source, Operation.Move):
          // Individual-argument varargs, per option combination (see `stat`).
          (dereference, atomic) match
            case (true, false) => jnf.Files.move(javaPath(source), javaPath(destination))

            case (true, true) =>
              jnf.Files.move
                (javaPath(source), javaPath(destination), jnf.StandardCopyOption.ATOMIC_MOVE)

            case (false, false) =>
              jnf.Files.move
                (javaPath(source), javaPath(destination), jnf.LinkOption.NOFOLLOW_LINKS)

            case (false, true) =>
              jnf.Files.move
                ( javaPath(source), javaPath(destination), jnf.LinkOption.NOFOLLOW_LINKS,
                  jnf.StandardCopyOption.ATOMIC_MOVE )

      def touch(path: Path on Plane)(using Tactic[IoError]): Unit =
        protect(path, Operation.Metadata):
          jnf.Files.setLastModifiedTime
            ( javaPath(path),
              jnfa.FileTime.fromMillis(java.lang.System.currentTimeMillis) )

      def hidden(path: Path on Plane)(using Tactic[IoError]): Boolean =
        protect(path, Operation.Metadata)(jnf.Files.isHidden(javaPath(path)))

      def volume(path: Path on Plane)(using Tactic[IoError]): Volume =
        protect(path, Operation.Metadata):
          val fileStore = jnf.Files.getFileStore(javaPath(path)).nn
          Volume(fileStore.name.nn.tt, fileStore.`type`.nn.tt)

      def hardLinkCount(path: Path on Plane, dereference: Boolean)(using Tactic[IoError]): Int =
        protect(path, Operation.Metadata):
          ( if dereference then jnf.Files.getAttribute(javaPath(path), "unix:nlink")
            else jnf.Files.getAttribute
                   (javaPath(path), "unix:nlink", jnf.LinkOption.NOFOLLOW_LINKS) )
          . nn.absolve match
            case count: Int => count

      def attribute(path: Path on Plane, attribute: FilesystemBackend.Attribute): Boolean =
        attribute match
          case FilesystemBackend.Attribute.Readable   => jnf.Files.isReadable(javaPath(path))
          case FilesystemBackend.Attribute.Writable   => jnf.Files.isWritable(javaPath(path))
          case FilesystemBackend.Attribute.Executable => jnf.Files.isExecutable(javaPath(path))

      def update(path: Path on Plane, attribute: FilesystemBackend.Attribute, value: Boolean)
        ( using Tactic[IoError] )
      :   Unit =

        protect(path, Operation.Metadata):
          val file = javaPath(path).toFile.nn

          val success = attribute match
            case FilesystemBackend.Attribute.Readable   => file.setReadable(value)
            case FilesystemBackend.Attribute.Writable   => file.setWritable(value)
            case FilesystemBackend.Attribute.Executable => file.setExecutable(value)

          if !success then abort(IoError(path, Operation.Metadata, Reason.PermissionDenied))

      def open[result](path: Path on Plane, flags: List[OpenFlag])(lambda: Handle => result)
        ( using Tactic[IoError] )
      :   result =

        val options: scala.collection.immutable.List[jnf.OpenOption] = flags.stdlib.map:
          case OpenFlag.Read      => jnf.StandardOpenOption.READ
          case OpenFlag.Write     => jnf.StandardOpenOption.WRITE
          case OpenFlag.Append    => jnf.StandardOpenOption.APPEND
          case OpenFlag.Create    => jnf.StandardOpenOption.CREATE
          case OpenFlag.Exclusive => jnf.StandardOpenOption.CREATE_NEW
          case OpenFlag.Truncate  => jnf.StandardOpenOption.TRUNCATE_EXISTING
          case OpenFlag.Sync      => jnf.StandardOpenOption.SYNC
          case OpenFlag.Dsync     => jnf.StandardOpenOption.DSYNC
          case OpenFlag.NoFollow  => jnf.LinkOption.NOFOLLOW_LINKS

        // `READ` and `APPEND` cannot be combined on a `FileChannel`.
        val appending = options.contains(jnf.StandardOpenOption.APPEND)

        val options2 =
          if appending && options.contains(jnf.StandardOpenOption.READ)
          then options.filter(_ != jnf.StandardOpenOption.READ)
          else options

        val channel =
          // The `Set` overload (with no trailing attributes): under the Scala Native javalib
          // the varargs formal is a pure Scala array, which no array value can satisfy.
          val optionSet = java.util.HashSet[jnf.OpenOption]()
          options2.foreach { option => optionSet.add(option); () }
          protect(path, Operation.Open)(jnc.FileChannel.open(javaPath(path), optionSet).nn)

        try
          // The `source`/`intake` closures capture the `FileChannel` capability, which native's
          // capture checker (unlike the JVM's, on identical code) reports as leaking out of the
          // closure. The channel is genuinely scoped — closed in the `finally` after `lambda`
          // returns — so the capture is asserted safe with `unsafeAssumePure`.
          lambda:
           // The channel is this handle's single owner (see the comment above).
           scala.caps.unsafe.unsafeAssumeSeparate:
            Handle
              ( () => unsafely(zephyrine.toProgression(Streamable.channel.stream(channel))),
                data => unsafely(Writable.channel.write(channel, zephyrine.Stream(data.stdlib.iterator))) )
              ( () => unsafely(caps.unsafe.unsafeAssumePure(Streamable.channel.stream(channel))),
                () => unsafely(caps.unsafe.unsafeAssumePure(Sink.channel.intake(channel))) )
        finally channel.close()
