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

import scala.annotation.nowarn

import anticipation.*
import contingency.*
import gossamer.*
import hellenism.*
import hypotenuse.*
import prepositional.*
import rudiments.*
import serpentine.*
import vacuous.*
import soundness.{invoke, dispose}
import xenophile.*

import IoError.{Operation, Reason}

// The WIT definitions the navigation below is typechecked against, and which the `invoke`
// materializer consults (at its downstream expansion site) for module ids, resource methods and
// parameter types.
type WasiFilesystemApi = Interface in Wit at "/galilei/filesystem.wit"
given wasiFilesystemApi: WasiFilesystemApi = Interface[Wit](cp"/galilei/filesystem.wit")

package filesystemBackends:
  // A `FilesystemBackend` over `wasi:filesystem`. WASI filesystems are capability-based: the host
  // grants access as *preopened directory descriptors* (`wasmtime run --dir …`), and every
  // operation resolves its path against the preopen with the longest matching prefix — a path
  // outside every preopen raises `PermissionDenied`, which is the capability model speaking.
  // `inline`, so the `invoke`s expand at the downstream summoning site: the Wasm Component
  // imports only materialize in code compiled for a Wasm target. Summoning it requires
  // `wasiFilesystemApi` (and this module's WIT resource) to be visible at that site.
  //
  // WASI error codes are recovered from the `WitError` raised by `invoke`'s decoder and mapped
  // onto `IoError.Reason` — so a quota failure reports `QuotaExceeded`, not a generic error.
  //
  // The per-site duplication the compiler warns about is the point: the instance must
  // materialize at the downstream summoning site, and a WASI-linked application summons it once.
  @nowarn("msg=New anonymous class definition will be duplicated at each inline site")
  inline given wasi: [plane: Filesystem] => FilesystemBackend on plane =
    new FilesystemBackend:
      type Plane = plane

      private def reason(error: WitError): Reason = error.name.s match
        case "no-entry"                                    => Reason.Nonexistent
        case "exist"                                       => Reason.AlreadyExists
        case "not-directory"                               => Reason.IsNotDirectory
        case "is-directory"                                => Reason.IsDirectory
        case "not-empty"                                   => Reason.DirectoryNotEmpty
        case "access" | "not-permitted"                    => Reason.PermissionDenied
        case "loop"                                        => Reason.Cycle
        case "cross-device"                                => Reason.NotSameVolume
        case "busy" | "would-block" | "text-file-busy"     => Reason.Busy
        case "read-only"                                   => Reason.ReadOnly
        case "too-many-links"                              => Reason.TooManyLinks
        case "name-too-long"                               => Reason.NameTooLong
        case "quota"                                       => Reason.QuotaExceeded
        case "insufficient-space"                          => Reason.StorageFull
        case "illegal-byte-sequence"                       => Reason.InvalidData
        case "interrupted"                                 => Reason.Interrupted
        case "io"                                          => Reason.Physical
        case _                                             => Reason.Unsupported

      // The block's by-name thunk carries a capture set: it closes over the backend instance
      // and the `Tactic`, which a plain (pure) `=> result` rejects when the enclosing
      // `inline given` expands at a downstream summoning site under capture checking.
      private def protect[result](path: Path on Plane, operation: Operation)
        ( block: ->{caps.any} result )
        ( using Tactic[IoError] )
      :   result =

        try block catch case error: WitError => abort(IoError(path, operation, reason(error)))

      // The preopened directory granted by the host that covers `path` — by longest prefix —
      // paired with the path's remainder, relative to it (WASI paths are preopen-relative and
      // never begin with `/`).
      private def resolve(path: Path on Plane, operation: Operation)(using Tactic[IoError])
      :   (WitHandle of "descriptor", Text) =

        val target: Text = Path.encodable.encode(path)

        val preopens =
          Foreign["preopens", Wit].`get-directories`
          . invoke[List[(WitHandle of "descriptor", Text)]]

        def covers(preopen: Text): Boolean =
          target == preopen || preopen == t"/" || target.starts(t"$preopen/")

        val covering = preopens.filter: entry =>
          covers(entry(1))

        if covering.isEmpty then
          preopens.each(_(0).dispose())
          abort(IoError(path, operation, Reason.PermissionDenied))
        else
          val (descriptor, prefix) = covering.maxBy(_(1).length)

          preopens.each: entry =>
            if entry(0) != descriptor then entry(0).dispose()

          val remainder =
            if target == prefix then t"."
            else if prefix == t"/" then target.keep(target.length - 1, Rtl)
            else target.keep(target.length - prefix.length - 1, Rtl)

          (descriptor, remainder)

      private def follow(dereference: Boolean): U32 = U32(if dereference then 1.bits else 0.bits)

      // The `descriptor-stat` record's fields, as its ABI-equivalent tuple: type, link count,
      // size, and the access/modification/status-change timestamps.
      private type StatFields =
        (U8, U64, U64, Optional[(U64, U32)], Optional[(U64, U32)], Optional[(U64, U32)])

      private def statOf
        ( descriptor: WitHandle of "descriptor", relative: Text, dereference: Boolean )
      :   Stat =

        val directory: Foreign of "descriptor" from Wit = descriptor

        val fields = directory.`stat-at`(follow(dereference), relative).invoke[StatFields]

        // `descriptor-type`'s cases, by declaration order: unknown, block-device,
        // character-device, directory, fifo, symbolic-link, regular-file, socket.
        val entry: Entry = fields(0).byte.toInt match
          case 1 => BlockDevice
          case 2 => CharDevice
          case 3 => Directory
          case 4 => Fifo
          case 5 => Symlink
          case 7 => Socket
          case _ => File

        def millis(time: Optional[(U64, U32)]): Optional[Long] =
          time.let: time =>
            time(0).bits.s64.long*1000L + time(1).bits.s32.int/1000000L

        Stat
          ( entry,
            fields(2).bits.s64.long,
            millis(fields(4)).or(0L),
            millis(fields(3)).or(0L),
            Unset )

      def stat(path: Path on Plane, dereference: Boolean)(using Tactic[IoError]): Stat =
        protect(path, Operation.Metadata):
          val (descriptor, relative) = resolve(path, Operation.Metadata)
          try statOf(descriptor, relative, dereference) finally descriptor.dispose()

      def exists(path: Path on Plane, dereference: Boolean): Boolean =
        import strategies.throwUnsafely

        try
          stat(path, dereference) yet true
        catch case error: Exception => false

      def children(path: Path on Plane)(using Tactic[IoError]): LazyList[Text] =
        protect(path, Operation.Read):
          val (descriptor, relative) = resolve(path, Operation.Read)
          val directory: Foreign of "descriptor" from Wit = descriptor

          try
            val listing =
              directory.`open-at`(follow(true), relative, U32(2.bits), U32(1.bits))
              . invoke[WitHandle of "descriptor"]

            val target: Foreign of "descriptor" from Wit = listing

            try
              val streamHandle =
                target.`read-directory`.invoke[WitHandle of "directory-entry-stream"]

              val stream: Foreign of "directory-entry-stream" from Wit = streamHandle

              var names: List[Text] = Nil
              var done: Boolean = false

              while !done do
                val entry = stream.`read-directory-entry`.invoke[Optional[(U8, Text)]]
                if entry.absent then done = true else names = entry.vouch(1) :: names

              streamHandle.dispose()
              names.reverse.to(LazyList)
            finally listing.dispose()
          finally descriptor.dispose()

      private def act(path: Path on Plane, operation: Operation)
        ( block: (Foreign of "descriptor" from Wit, Text) => Unit )
        ( using Tactic[IoError] )
      :   Unit =

        protect(path, operation):
          val (descriptor, relative) = resolve(path, operation)
          val directory: Foreign of "descriptor" from Wit = descriptor

          try block(directory, relative) finally descriptor.dispose()

      def createDirectory(path: Path on Plane)(using Tactic[IoError]): Unit =
        act(path, Operation.Create): (directory, relative) =>
          directory.`create-directory-at`(relative).invoke[Unit]

      def createFile(path: Path on Plane)(using Tactic[IoError]): Unit =
        act(path, Operation.Create): (directory, relative) =>
          // open-flags: create (1) | exclusive (4); descriptor-flags: write (2)
          val created =
            directory.`open-at`(follow(true), relative, U32(5.bits), U32(2.bits))
            . invoke[WitHandle of "descriptor"]

          created.dispose()

      def createFifo(path: Path on Plane)(using Tactic[IoError]): Unit =
        abort(IoError(path, Operation.Create, Reason.Unsupported))

      def delete(path: Path on Plane)(using Tactic[IoError]): Unit =
        act(path, Operation.Delete): (directory, relative) =>
          val directoryEntry =
            try statOf(resolve(path, Operation.Delete)(0), relative, false).entry == Directory
            catch case error: WitError => false

          if directoryEntry then directory.`remove-directory-at`(relative).invoke[Unit]
          else directory.`unlink-file-at`(relative).invoke[Unit]

      def deleteIfExists(path: Path on Plane)(using Tactic[IoError]): Unit =
        if exists(path, false) then delete(path)

      def symlink(link: Path on Plane, target: Path on Plane)(using Tactic[IoError]): Unit =
        act(link, Operation.Create): (directory, relative) =>
          directory.`symlink-at`(Path.encodable.encode(target), relative).invoke[Unit]

      def hardLink(link: Path on Plane, target: Path on Plane)(using Tactic[IoError]): Unit =
        protect(link, Operation.Create):
          val (source, sourceRelative) = resolve(target, Operation.Create)
          val directory: Foreign of "descriptor" from Wit = source

          try
            val (destination, linkRelative) = resolve(link, Operation.Create)

            try
              directory.`link-at`(follow(false), sourceRelative, destination, linkRelative)
              . invoke[Unit]
            finally destination.dispose()
          finally source.dispose()

      def copy(source: Path on Plane, destination: Path on Plane, dereference: Boolean)
        ( using Tactic[IoError] )
      :   Unit =

        val content = open(source, List(OpenFlag.Read))(_.reader())
        val flags = List(OpenFlag.Write, OpenFlag.Create, OpenFlag.Truncate)

        open(destination, flags): handle =>
          handle.writer(content)

      def move
        ( source:      Path on Plane,
          destination: Path on Plane,
          atomic:      Boolean,
          dereference: Boolean )
        ( using Tactic[IoError] )
      :   Unit =

        protect(source, Operation.Move):
          val (from, fromRelative) = resolve(source, Operation.Move)
          val directory: Foreign of "descriptor" from Wit = from

          try
            val (to, toRelative) = resolve(destination, Operation.Move)

            try directory.`rename-at`(fromRelative, to, toRelative).invoke[Unit]
            finally to.dispose()
          finally from.dispose()

      def touch(path: Path on Plane)(using Tactic[IoError]): Unit =
        act(path, Operation.Metadata): (directory, relative) =>
          directory.`set-times-at`
            ( follow(true),
              relative,
              WitCase["new-timestamp"](t"now"),
              WitCase["new-timestamp"](t"now") )

          . invoke[Unit]

      def hidden(path: Path on Plane)(using Tactic[IoError]): Boolean =
        path.descent.prim.let(_.starts(t".")).or(false)

      def volume(path: Path on Plane)(using Tactic[IoError]): Volume =
        abort(IoError(path, Operation.Metadata, Reason.Unsupported))

      def hardLinkCount(path: Path on Plane, dereference: Boolean)(using Tactic[IoError]): Int =
        stat2(path, dereference)

      private def stat2(path: Path on Plane, dereference: Boolean)(using Tactic[IoError]): Int =
        protect(path, Operation.Metadata):
          val (descriptor, relative) = resolve(path, Operation.Metadata)
          val directory: Foreign of "descriptor" from Wit = descriptor

          try
            val fields = directory.`stat-at`(follow(dereference), relative).invoke[StatFields]
            fields(1).bits.s64.long.toInt
          finally descriptor.dispose()

      // WASI has no per-path permission query or `chmod`: readability and writability are
      // capability-scoped (a path is accessible exactly if a preopen covers it), so existence is
      // the best answer available; updates are honestly unsupported.
      def attribute(path: Path on Plane, attribute: FilesystemBackend.Attribute): Boolean =
        attribute match
          case FilesystemBackend.Attribute.Executable => false
          case _                                      => exists(path, true)

      def update(path: Path on Plane, attribute: FilesystemBackend.Attribute, value: Boolean)
        ( using Tactic[IoError] )
      :   Unit =

        abort(IoError(path, Operation.Metadata, Reason.Unsupported))

      def open[result](path: Path on Plane, flags: List[OpenFlag])(lambda: Handle => result)
        ( using Tactic[IoError] )
      :   result =

        protect(path, Operation.Open):
          val (descriptor, relative) = resolve(path, Operation.Open)
          val directory: Foreign of "descriptor" from Wit = descriptor

          // open-flags: create (1) | exclusive (4) | truncate (8); descriptor-flags: read (1) |
          // write (2).
          val create = if flags.contains(OpenFlag.Create) then 1 else 0
          val exclusive = if flags.contains(OpenFlag.Exclusive) then 4 else 0
          val truncate = if flags.contains(OpenFlag.Truncate) then 8 else 0
          val openFlags = create | exclusive | truncate

          val writing = flags.contains(OpenFlag.Write) || flags.contains(OpenFlag.Append)

          val descriptorFlags =
            (if flags.contains(OpenFlag.Read) then 1 else 0) | (if writing then 2 else 0)

          val pathFlags = follow(!flags.contains(OpenFlag.NoFollow))

          try
            val opened =
              directory
              . `open-at`(pathFlags, relative, U32(openFlags.bits), U32(descriptorFlags.bits))
              . invoke[WitHandle of "descriptor"]

            val target: Foreign of "descriptor" from Wit = opened

            def read(): LazyList[Data] =
              val streamHandle =
                target.`read-via-stream`(U64(0L.bits)).invoke[WitHandle of "input-stream"]

              val stream: Foreign of "input-stream" from Wit = streamHandle
              var chunks: List[Data] = Nil

              try
                while true do
                  chunks = stream.`blocking-read`(U64(65536L.bits)).invoke[Data] :: chunks
              catch case error: WitError => ()

              streamHandle.dispose()
              chunks.reverse.to(LazyList)

            def write(data: LazyList[Data]): Unit =
              val streamHandle =
                if flags.contains(OpenFlag.Append)
                then target.`append-via-stream`.invoke[WitHandle of "output-stream"]
                else target.`write-via-stream`(U64(0L.bits)).invoke[WitHandle of "output-stream"]

              val stream: Foreign of "output-stream" from Wit = streamHandle

              data.each: chunk =>
                stream.`blocking-write-and-flush`(chunk).invoke[Unit]

              streamHandle.dispose()

            // The `source`/`intake` endpoints are spelled out rather than defaulted: the
            // defaults' thunks carry root capabilities in the default-getter methods' result
            // types, which are not visible from the downstream site this `inline given`
            // expands at under capture checking.
            val handle =
              Handle(() => read(), write(_))
                ( () => zephyrine.Stream(read().iterator),
                  () => turbulence.Sink.buffered((), (_, stream) => write(stream)) )

            try lambda(handle) finally opened.dispose()
          finally descriptor.dispose()
