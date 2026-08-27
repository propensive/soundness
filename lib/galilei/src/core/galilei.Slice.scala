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

import anticipation.*
import aperture.*
import contingency.*
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import vacuous.*

import Io.Error.{Operation, Reason}

// A byte range of a file, as a subject for `open` (issue #566): opening a `Slice` with an
// `Exclusive` or `Shared` mode takes an OS advisory lock over exactly that range —
// `FileChannel.lock(position, size, shared)` — and enrols the range in the access register,
// where it conflicts only with overlapping ranges (a whole-file open overlaps every range).
// The handle is a `Slice.Window`: a positional view windowed to the slice, whose `size` is
// the window's, and whose reads and writes are relative to the window's start and confined
// to it (issue #1878) — reads clamp, and writes store as much as fits and return the count,
// in the manner of `pwrite`. The `read` and `write` extensions are gated by the mode's
// grants, as `Ram`'s are. Opened without a locking mode, a `Slice` is simply a windowed
// view, which even lockless backends (WASI) support.
//
//     Slice(path, 0L, 1024L).open[File](Read & Write & Exclusive): window ?=>
//       window.write(0L, data)  // stored at the file's start, under a range lock
//       window.read(0L, 16)
case class Slice[plane](path: Path on plane, offset: Long, extent: Long)

object Slice:
  // The handle for an open slice. `readFrom` and `writeTo` are the operational methods, which
  // backends implement already windowed; the public, grant-gated names are the `read` and
  // `write` extensions below, following `Ram`'s pattern.
  trait Window:
    def size: Long
    def readFrom(offset: Long, length: Int): Data
    def writeTo(offset: Long, data: Data): Int

  // Transparent inline, as `Ram`'s gated accessors are: a non-inline extension's pure
  // receiver type cannot accept the capability-captured handle.
  extension (window: Window & Granting[Grant.Read])
    transparent inline def read(offset: Long, length: Int): Data = window.readFrom(offset, length)

  extension (window: Window & Granting[Grant.Write])
    // Stores `data` at `offset` within the window, returning how many bytes were written:
    // clamped, `pwrite`-style, to the window's extent, so a caller storing near the boundary
    // can observe the truncation rather than lose data silently.
    transparent inline def write(offset: Long, data: Data): Int = window.writeTo(offset, data)

  class SliceOpenable[filesystem: Filesystem, slice <: Slice[filesystem]]
    ( using backend: FilesystemBackend on filesystem, ioError: Tactic[Io.Error] )
  extends Openable:

    type Self = slice
    type Form = File
    type Operand = OpenFlag
    type Result = Window

    def open[grants <: Grant, result]
      ( value: slice, mode: Mode granting grants, flags: List[OpenFlag] )
      ( block: ((Window & Granting[grants])^) ?=> result )
    :   result =

      val lockFlags =
        if mode.atoms.has(Exclusive) then List(OpenFlag.Lock)
        else if mode.atoms.has(Shared) then List(OpenFlag.LockShared)
        else List()

      val modeFlags =
        (if mode.atoms.has(Read) then List(OpenFlag.Read).stdlib else Nil.stdlib) ++
          (if mode.atoms.has(Write) then List(OpenFlag.Write).stdlib else Nil.stdlib)

      val locking = !lockFlags.stdlib.isEmpty
      val range: (Long, Long) = (value.offset, value.extent)

      // As in `FileOpenable`: the register works on real paths.
      val real: Text =
        if !locking then t"" else
          try value.path.nioPath.toRealPath().nn.toString.tt
          catch case _: Exception =>
            value.path.nioPath.toAbsolutePath.nn.normalize.nn.toString.tt

      val awaiting = flags.stdlib.contains(OpenFlag.Await)

      if locking then
        if awaiting then AccessRegister.acquireAwait(real, mode.atoms, range)
        else if !AccessRegister.acquire(real, mode.atoms, range)
        then abort(Io.Error(value.path, Operation.Open, Reason.Busy))

      try
        backend.slice(value.path, value.offset, value.extent,
            (modeFlags ++ lockFlags.stdlib ++ flags.stdlib).to(List)): window =>
          // Mixed in rather than cast: `Window & Granting` is a trait intersection, whose
          // erased cast is to `Granting`, which the backend's window does not implement.
          val granted = new Window with Granting[grants]:
            def size: Long = window.size
            def readFrom(offset: Long, length: Int): Data = window.readFrom(offset, length)
            def writeTo(offset: Long, data: Data): Int = window.writeTo(offset, data)

          block(using granted)
      finally if locking then AccessRegister.release(real, mode.atoms, range)

  // Capture-annotated by the tactic, as `Platform`'s `File` openable is.
  given openable: [filesystem: Filesystem, slice <: Slice[filesystem]]
  =>  ( backend: FilesystemBackend on filesystem,
        tactic:  Tactic[Io.Error] )
  =>  ( SliceOpenable[filesystem, slice]^{tactic} ) =
    SliceOpenable[filesystem, slice]
