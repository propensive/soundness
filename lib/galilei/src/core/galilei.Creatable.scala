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

import anticipation.*
import aperture.*
import contingency.*
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import vacuous.*

import CreateFlag.*
import IoError.{Operation, Reason}

// The `Creatable` instances for filesystem entries, replacing the earlier standalone
// `Creatable` typeclass. `path.create[Directory]()` and `path.create[File]()` instantiate an
// empty entry; the block forms author the newborn entry — a fresh-plane `DirectoryHandle`
// for a directory, a write `Handle` (staged through a temporary sibling and moved atomically)
// for a file — with the guarantee that an exception escaping the scope leaves nothing
// behind. FIFOs are instantiation-only: opening one just to have a handle would block until
// a peer appears.
object Creation:
  // Walk up from the parent, creating missing ancestors — the behavior the retired
  // `createNonexistentParents.enabled` given supplied implicitly, now selected by the
  // `Parents` flag.
  private[galilei] def ensure[filesystem: Filesystem]
    ( path: Path on filesystem, flags: List[CreateFlag] )
    ( using backend: FilesystemBackend on filesystem, tactic: Tactic[IoError] )
  :   Unit =

    if flags.has(Parents) then
      def ancestors(current: Path on filesystem): List[Path on filesystem] =
        safely(current.parent).let { parent => (parent :: ancestors(parent)): List[Path on filesystem] }
        . or(Nil)

      ancestors(path).stdlib.reverse.each: ancestor =>
        if !backend.exists(ancestor, true) then backend.createDirectory(ancestor)

  private[galilei] def replace[filesystem: Filesystem]
    ( path: Path on filesystem, flags: List[CreateFlag] )
    ( using backend: FilesystemBackend on filesystem, tactic: Tactic[IoError] )
  :   Unit =

    if flags.has(Replace) && backend.exists(path, false) then wipe(path)

  // Public: shared with `Scratch`-style cleanup and called from creation rollback.
  def wipe[filesystem: Filesystem](path: Path on filesystem)
    ( using backend: FilesystemBackend on filesystem, tactic: Tactic[IoError] )
  :   Unit =

    if backend.stat(path, false).entry == Directory
    then backend.children(path).each { name => wipe(path.child(name)(using Unsafe)) }
    backend.delete(path)

  class DirectoryCreatable[filesystem <: Platform: Filesystem, path <: Path on filesystem]
    ( using backend: FilesystemBackend on filesystem,
            ioError: Tactic[IoError],
            loggable: (IoEvent is Loggable)^ )
  extends Creatable:

    type Self = path
    type Form = Directory
    type Operand = CreateFlag
    type Grants = Grant.Read & Grant.Write
    type Result = DirectoryHandle { type Under = filesystem }

    override def make(value: path, flags: List[CreateFlag]): Unit =
      Log.info(IoEvent.Create((value: Path on filesystem).show))
      ensure(value, flags)
      replace(value, flags)
      backend.createDirectory(value)

    def create[result]
      ( value: path, flags: List[CreateFlag] )
      ( block: (((DirectoryHandle { type Under = filesystem })
                  & Granting[Grant.Read & Grant.Write])^) ?=> result )
    :   result =

      make(value, flags)

      // The newborn directory has a caller-chosen, visible name, so unlike a `Scratch`
      // scope it participates in the access register like any open directory.
      val real: Text = value.javaPath.toRealPath().nn.toString.tt
      val atoms: Set[Mode] = Set(aperture.Read, aperture.Write)

      if !AccessRegister.acquire(real, atoms) then
        wipe(value)
        abort(IoError(value, Operation.Create, Reason.Busy))

      try
        val handle =
          new DirectoryHandle with Granting[Grant.Read & Grant.Write]:
            type Under = filesystem
            val stem: Path on filesystem = value
            val atoms: Set[Mode] = Set(aperture.Read, aperture.Write)

        try block(using handle)
        catch case throwable: Throwable =>
          safely(wipe(value))
          throw throwable
      finally AccessRegister.release(real, atoms)

  class FileCreatable[filesystem <: Platform: Filesystem, path <: Path on filesystem]
    ( using backend: FilesystemBackend on filesystem,
            ioError: Tactic[IoError],
            loggable: (IoEvent is Loggable)^ )
  extends Creatable:

    type Self = path
    type Form = File
    type Operand = CreateFlag
    type Grants = Grant.Read & Grant.Write
    type Result = Handle

    override def make(value: path, flags: List[CreateFlag]): Unit =
      Log.info(IoEvent.Create((value: Path on filesystem).show))
      ensure(value, flags)
      replace(value, flags)
      backend.createFile(value)

    // The content is authored into a temporary sibling, moved onto the target only when the
    // scope completes: creation is the one moment atomic replacement is free, since nothing
    // can yet hold the path.
    def create[result]
      ( value: path, flags: List[CreateFlag] )
      ( block: ((Handle & Granting[Grant.Read & Grant.Write])^) ?=> result )
    :   result =

      Log.info(IoEvent.Create((value: Path on filesystem).show))
      ensure(value, flags)

      if !flags.has(Replace) && backend.exists(value, false)
      then abort(IoError(value, Operation.Create, Reason.AlreadyExists))

      // `peer` needs a statically-known `Topic`, which an abstract `path` lacks, so the
      // temporary sibling is built through the parent instead.
      val temporary: Path on filesystem =
        safely(value.parent).let(_.child(t".${value.name}.part")(using Unsafe))
          .or(abort(IoError(value, Operation.Create, Reason.Unsupported)))

      try
        val outcome =
          backend.open(temporary, List(OpenFlag.Write, OpenFlag.Create)): handle =>
            block(using handle.asInstanceOf[Handle & Granting[Grant.Read & Grant.Write]])

        backend.move(temporary, value, true, false)
        outcome
      catch case throwable: Throwable =>
        safely(backend.deleteIfExists(temporary))
        throw throwable

  class FifoCreatable[filesystem <: Platform: Filesystem, path <: Path on filesystem]
    ( using backend: FilesystemBackend on filesystem,
            ioError: Tactic[IoError],
            loggable: (IoEvent is Loggable)^ )
  extends Creatable:

    type Self = path
    type Form = Fifo
    type Operand = CreateFlag
    type Grants = Grant.Read & Grant.Write
    type Result = Fifo

    override def make(value: path, flags: List[CreateFlag]): Unit =
      Log.info(IoEvent.Create((value: Path on filesystem).show))
      ensure(value, flags)
      replace(value, flags)
      backend.createFifo(value)

    // Authoring a FIFO within its creation scope is refused: opening either end blocks
    // until a peer appears, so a scoped handle could never be provided safely.
    def create[result]
      ( value: path, flags: List[CreateFlag] )
      ( block: ((Fifo & Granting[Grant.Read & Grant.Write])^) ?=> result )
    :   result =

      abort(IoError(value, Operation.Create, Reason.Unsupported))
