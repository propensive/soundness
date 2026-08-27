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
// The handle is a positional view (`zephyrine.Expanse`) windowed to the slice: its `size` is
// the window's, and reads are relative to the window's start and clamped to it. Opened
// without a locking mode, a `Slice` is simply a windowed view, which even lockless backends
// (WASI) support.
//
//     Slice(path, 0L, 1024L).open[File](Read & Exclusive): view ?=>
//       view.read(0L, 16)  // the file's first sixteen bytes, under a range lock
case class Slice[plane](path: Path on plane, offset: Long, extent: Long)

object Slice:
  class SliceOpenable[filesystem: Filesystem, slice <: Slice[filesystem]]
    ( using backend: FilesystemBackend on filesystem, ioError: Tactic[Io.Error] )
  extends Openable:

    type Self = slice
    type Form = File
    type Operand = OpenFlag
    type Result = zephyrine.Expanse

    def open[grants <: Grant, result]
      ( value: slice, mode: Mode granting grants, flags: List[OpenFlag] )
      ( block: ((zephyrine.Expanse & Granting[grants])^) ?=> result )
    :   result =

      val lockFlags =
        if mode.atoms.has(Exclusive) then List(OpenFlag.Lock)
        else if mode.atoms.has(Shared) then List(OpenFlag.LockShared)
        else List()

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
            (lockFlags.stdlib ++ flags.stdlib).to(List)): view =>
          // Mixed in rather than cast: `Expanse & Granting` is a trait intersection, whose
          // erased cast is to `Granting`, which the backend's view does not implement.
          val granted = new zephyrine.Expanse with Granting[grants]:
            def size: Long = view.size
            def read(offset: Long, length: Int): Data = view.read(offset, length)

          block(using granted)
      finally if locking then AccessRegister.release(real, mode.atoms, range)

  // Capture-annotated by the tactic, as `Platform`'s `File` openable is.
  given openable: [filesystem: Filesystem, slice <: Slice[filesystem]]
  =>  ( backend: FilesystemBackend on filesystem,
        tactic:  Tactic[Io.Error] )
  =>  ( SliceOpenable[filesystem, slice]^{tactic} ) =
    SliceOpenable[filesystem, slice]
