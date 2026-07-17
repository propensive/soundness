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

import java.nio as jn
import java.nio.channels as jnc
import java.nio.file as jnf

import anticipation.*
import aperture.*
import contingency.*
import prepositional.*
import rudiments.*
import serpentine.*
import vacuous.*
import zephyrine.*

import IoError.{Operation, Reason}

// The form for random access to a file's bytes through memory mapping:
// `path.open[Ram](Read & Write)`. The handle serves positional reads with `ram(offset, length)`
// and, when the `Write` grant was selected, positional writes with `ram(offset) = data`; both
// go through a `MappedByteBuffer`, so the OS pages data in and out on demand. An `Exclusive`
// mode additionally acquires an OS file lock for the duration of the scope, so exclusivity
// holds against other *processes*, not just other scopes in this one. (galilei's jvm module
// is not yet capture-checked, so confinement is enforced only for callers compiled with
// capture checking; the annotations sharpen when the module joins the rollout.)
trait Ram

object Ram:
  class RamHandle private[galilei] (buffer: jn.MappedByteBuffer, val size: Long)
  extends caps.ExclusiveCapability:

    // Public: called from the grant-gated transparent-inline operations below, where a
    // `private` member's inline-accessor bridge would fail capture checking.
    def readFrom(offset: Long, length: Int): Data =
      val array = new Array[Byte](length)
      buffer.get(offset.toInt, array)
      array.immutable(using Unsafe)

    def writeTo(offset: Long, data: Data): Unit =
      buffer.put(offset.toInt, data.mutable(using Unsafe), 0, data.length)

    // A shared positional view, for consumers written against zephyrine's `Expanse`.
    def expanse: Expanse = new Expanse:
      def size: Long = RamHandle.this.size
      def read(offset: Long, length: Int): Data = readFrom(offset, length)

  extension (handle: RamHandle & Granting[Grant.Read])
    transparent inline def apply(offset: Long, length: Int): Data = handle.readFrom(offset, length)

  extension (handle: RamHandle & Granting[Grant.Write])
    transparent inline def update(offset: Long, data: Data): Unit = handle.writeTo(offset, data)

  // A named class rather than an anonymous given instance, for the reasons documented on
  // `FileOpenable`. An `Exclusive` mode implies a writable channel, since OS exclusive locks
  // require one.
  class RamOpenable[filesystem <: Platform: Filesystem, path <: Path on filesystem]
    ( using ioError: Tactic[IoError] )
  extends Openable:

    type Self = path
    type Form = Ram
    type Operand = Nothing
    type Result = RamHandle

    def open[grants <: Grant, result]
      ( value: path, mode: Mode granting grants, flags: List[Nothing] )
      ( block: (RamHandle & Granting[grants]) ?=> result )
    :   result =

      val writable = mode.atoms.contains(Write) || mode.atoms.contains(Exclusive)

      val options: Seq[jnf.StandardOpenOption] =
        if writable then Seq(jnf.StandardOpenOption.READ, jnf.StandardOpenOption.WRITE)
        else Seq(jnf.StandardOpenOption.READ)

      value.protect(Operation.Open):
        val channel = jnc.FileChannel.open(value.javaPath, options*).nn

        try
          val size = channel.size

          // `MappedByteBuffer` addresses with `Int`, so mapping beyond 2GiB needs segmented
          // mapping, which can follow if it proves necessary.
          if size > Int.MaxValue then abort(IoError(value, Operation.Open, Reason.Unsupported))

          val lock =
            if mode.atoms.contains(Exclusive) then
              try Option(channel.tryLock()) catch
                case _: jnc.OverlappingFileLockException => None
            else Some(null)

          if lock.isEmpty then abort(IoError(value, Operation.Open, Reason.Busy))

          try
            val mapMode =
              if mode.atoms.contains(Write) then jnc.FileChannel.MapMode.READ_WRITE
              else jnc.FileChannel.MapMode.READ_ONLY

            val buffer = channel.map(mapMode, 0, size).nn

            try block(using new RamHandle(buffer, size) with Granting[grants] {})
            finally if mode.atoms.contains(Write) then buffer.force()
          finally lock.foreach { held => if held != null then held.release() }
        finally channel.close()

  given openable: [filesystem <: Platform: Filesystem, path <: Path on filesystem]
  =>  Tactic[IoError]
  =>  RamOpenable[filesystem, path] =
    RamOpenable[filesystem, path]
