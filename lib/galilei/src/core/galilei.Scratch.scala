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
import inimitable.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import vacuous.*

import IoError.{Operation, Reason}

// The form for transient working directories: `parent.open[Scratch](Read & Write)` creates a
// fresh, uniquely-named directory beneath `parent`, provides a `DirectoryHandle` over it for
// the scope, and deletes it — and everything created within it — when the scope ends, however
// it ends. The lifetime of the directory *is* the scope, and the fresh-plane machinery
// guarantees that no path into it survives beyond it, so the deletion is always sound.
trait Scratch

object Scratch:
  class ScratchOpenable[filesystem <: Platform: Filesystem, path <: Path on filesystem]
    ( using backend: FilesystemBackend on filesystem, ioError: Tactic[IoError] )
  extends Openable:

    type Self = path
    type Form = Scratch
    type Operand = Nothing
    type Result = DirectoryHandle { type Under = filesystem }

    def open[grants <: Grant, result]
      ( value: path, mode: Mode granting grants, flags: List[Nothing] )
      ( block: (((DirectoryHandle { type Under = filesystem }) & Granting[grants])^) ?=> result )
    :   result =

      if backend.stat(value, true).entry != Directory
      then abort(IoError(value, Operation.Open, Reason.IsNotDirectory))

      // A fresh name under `value`: no other scope can denote it, so unlike opening an
      // existing directory, a scratch scope needs no access-register arbitration.
      val name: Text = Uuid().show
      val child: Path on filesystem = value.child(name)(using Unsafe)
      backend.createDirectory(child)

      def wipe(path: Path on filesystem): Unit =
        if backend.stat(path, false).entry == Directory
        then backend.children(path).each { name => wipe(path.child(name)(using Unsafe)) }
        backend.delete(path)

      try
        val handle =
          new DirectoryHandle with Granting[grants]:
            type Under = filesystem
            val stem: Path on filesystem = child
            val atoms: Set[Mode] = mode.atoms

        block(using handle)
      finally wipe(child)

  given openable: [filesystem <: Platform: Filesystem, path <: Path on filesystem]
  =>  ( FilesystemBackend on filesystem,
        Tactic[IoError] )
  =>  ( ScratchOpenable[filesystem, path]^ ) =
    ScratchOpenable[filesystem, path]
