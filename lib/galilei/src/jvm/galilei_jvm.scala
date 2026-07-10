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
┃    Soundness, version 0.63.0.                                                                    ┃
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
  given virtualMachine: [plane: Filesystem] => FilesystemBackend on plane =
    new FilesystemBackend:
      type Plane = plane

      private def javaPath(path: Path on Plane): jnf.Path =
        jnf.Path.of(Path.encodable.encode(path).s).nn

      private def dereferenceOptions(dereference: Boolean): List[jnf.LinkOption] =
        if dereference then Nil else List(jnf.LinkOption.NOFOLLOW_LINKS)

      // Maps `java.nio`'s failure exceptions onto the common `Reason` vocabulary, as informatively
      // as their types (and, for `FileSystemException`, their `getReason` texts) permit.
      private def protect[result](path: Path on Plane, operation: Operation)(block: => result)
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
          case _: jnf.AtomicMoveNotSupportedException => fail(Unsupported)
          case _: SecurityException                   => fail(PermissionDenied)
          case _: jnf.FileSystemLoopException         => fail(Cycle)
          case error: ji.InterruptedIOException       => fail(Interrupted)
          case error: jnf.FileSystemException         => fail(filesystemReason(error))
          case error: ji.IOException                  => fail(Physical)
          case other                                  => fail(Unsupported)

      def stat(path: Path on Plane, dereference: Boolean)(using Tactic[IoError]): Stat =
        protect(path, Operation.Metadata):
          val options = dereferenceOptions(dereference)

          val attributes =
            jnf.Files.readAttributes(javaPath(path), classOf[jnfa.BasicFileAttributes], options*)
            . nn

          val entry: Entry =
            if attributes.isSymbolicLink then Symlink
            else if attributes.isRegularFile then File
            else if attributes.isDirectory then Directory
            else
              try jnf.Files.getAttribute(javaPath(path), "unix:mode", options*).nn.absolve match
                case mode: Int => (mode & 61440) match
                  case  4096 => Fifo
                  case  8192 => CharDevice
                  case 24576 => BlockDevice
                  case 49152 => Socket
                  case _     => File
              catch case _: Exception => File

          val created: Optional[Long] =
            val time = attributes.creationTime().nn.toInstant.nn.toEpochMilli
            if time == 0L then Unset else time

          Stat
            ( entry,
              attributes.size(),
              attributes.lastModifiedTime().nn.toInstant.nn.toEpochMilli,
              attributes.lastAccessTime().nn.toInstant.nn.toEpochMilli,
              created )

      def exists(path: Path on Plane, dereference: Boolean): Boolean =
        jnf.Files.exists(javaPath(path), dereferenceOptions(dereference)*)

      def children(path: Path on Plane)(using Tactic[IoError]): LazyList[Text] =
        protect(path, Operation.Read):
          if !jnf.Files.isDirectory(javaPath(path)) then LazyList()
          else
            jnf.Files.list(javaPath(path)).nn.iterator().nn.asScala
            . map(_.getFileName.nn.toString.tt)
            . to(LazyList)

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
          jnf.Files.copy(javaPath(source), javaPath(destination), dereferenceOptions(dereference)*)

      def move
        ( source:      Path on Plane,
          destination: Path on Plane,
          atomic:      Boolean,
          dereference: Boolean )
        ( using Tactic[IoError] )
      :   Unit =

        protect(source, Operation.Move):
          val atomically = if atomic then List(jnf.StandardCopyOption.ATOMIC_MOVE) else Nil
          val options: List[jnf.CopyOption] = dereferenceOptions(dereference) ++ atomically

          jnf.Files.move(javaPath(source), javaPath(destination), options*)

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
          jnf.Files.getAttribute(javaPath(path), "unix:nlink", dereferenceOptions(dereference)*)
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

        val options: List[jnf.OpenOption] = flags.map:
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
          protect(path, Operation.Open)(jnc.FileChannel.open(javaPath(path), options2*).nn)

        try
          lambda:
            Handle
              ( () => unsafely(Streamable.channel.stream(channel).stream[Data]),
                data => unsafely(Writable.channel.write(channel, data)) )
              ( () => unsafely(Source.channel.stream(channel)),
                () => unsafely(Sink.channel.intake(channel)) )
        finally channel.close()
