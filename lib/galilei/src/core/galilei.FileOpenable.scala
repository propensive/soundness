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
import gossamer.*
import contingency.*
import prepositional.*
import serpentine.*
import rudiments.*
import symbolism.*

import Io.Error.{Operation, Reason}

// The `Openable` instance for opening a file's content: `path.open[File](Read & Write)`. A
// named class rather than an anonymous given instance: instantiating an anonymous subclass
// freshens `Handle`'s (capability) field types in the inferred `Result` member, which then
// fails to conform to the declared `to Handle` refinement.
class FileOpenable[filesystem: Filesystem, path <: Path on filesystem]
  ( using backend: FilesystemBackend on filesystem, ioError: Tactic[Io.Error] )
extends Openable:

  type Self = path
  type Form = File
  type Operand = OpenFlag
  type Result = Handle

  def open[grants <: Grant, result]
    ( value: path, mode: Mode granting grants, flags: List[OpenFlag] )
    ( block: ((Handle & Granting[grants])^) ?=> result )
  :   result =

    // The mode's atoms translate to OS open flags. `aperture.Exclusive` is deliberately not
    // translated to `OpenFlag.Exclusive`: POSIX `O_EXCL` governs exclusive *creation*, not
    // exclusive access. Instead it enrols the open in the access register — so file opens
    // participate in the same intra-JVM arbitration as directory scopes — and asks the
    // backend for an OS advisory lock (`OpenFlag.Lock`) to cover the cross-process case
    // (issue #566).
    val modeFlags: List[OpenFlag] =
      (if mode.atoms.has(Read) then List(OpenFlag.Read) else Nil) +
        (if mode.atoms.has(Write) then List(OpenFlag.Write) else Nil) +
        (if mode.atoms.has(Exclusive) then List(OpenFlag.Lock)
         else if mode.atoms.has(Shared) then List(OpenFlag.LockShared)
         else Nil)

    val locking = mode.atoms.has(Exclusive) || mode.atoms.has(Shared)

    // The register works on real paths, so two routes to one file register as the same file
    // and overlap correctly with enclosing directory scopes; a file which is about to be
    // created cannot be resolved, so it falls back to its normalized absolute form.
    val real: Text =
      if !locking then t"" else
        try value.nioPath.toRealPath().nn.toString.tt
        catch case _: Exception => value.nioPath.toAbsolutePath.nn.normalize.nn.toString.tt

    val awaiting = flags.has(OpenFlag.Await)

    if locking then
      if awaiting then AccessRegister.acquireAwait(real, mode.atoms)
      else if !AccessRegister.acquire(real, mode.atoms)
      then abort(Io.Error(value, Operation.Open, Reason.Busy))

    try
      backend.open(value, modeFlags + flags): handle =>
        // `Granting` is a phantom marker, so the cast only refines the static type with the
        // grants that `modeFlags` has just made true operationally.
        block(using handle.asInstanceOf[Handle & Granting[grants]])
    finally if locking then AccessRegister.release(real, mode.atoms)
