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

import contingency.*
import prepositional.*
import serpentine.*
import turbulence.*

object Openable:
  // A named class rather than an anonymous given instance: instantiating an anonymous
  // subclass freshens `Handle`'s (capability) field types in the inferred `Result` member,
  // which then fails to conform to the declared `to Handle` refinement.
  class FileOpenable[filesystem: Filesystem, path <: Path on filesystem]
    ( using read:        ReadAccess,
            write:       WriteAccess,
            dereference: DereferenceSymlinks,
            create:      CreateNonexistent on filesystem,
            backend:     FilesystemBackend on filesystem,
            ioError:     Tactic[IoError] )
  extends Openable:

    type Self = path
    type Operand = OpenFlag
    type Result = Handle^

    def open[result](path: path, lambda: Result => result, extraOptions: List[OpenFlag]): result =
      val dereferenceFlags = if dereference.dereference then Nil else List(OpenFlag.NoFollow)

      val flags =
        read.flags() ++ write.flags() ++ dereferenceFlags ++ create.flags() ++ extraOptions

      backend.open(path, flags)(lambda)

  given openable: [filesystem: Filesystem, path <: Path on filesystem]
  =>  ( ReadAccess,
        WriteAccess,
        DereferenceSymlinks,
        CreateNonexistent on filesystem,
        FilesystemBackend on filesystem,
        Tactic[IoError] )
  =>  ( FileOpenable[filesystem, path]^ ) =
    FileOpenable[filesystem, path]


  // Named capturing evidence and an honest result: `FileOpenable` instances retain their
  // filesystem and tactic evidence, which a pure context bound cannot accept.
  given eof: [file]
  =>  (openable: (file is Openable by OpenFlag)^)
  =>  (((Eof[file] is Openable by OpenFlag) { type Result = openable.Result })
        ^{openable, caps.any}) =

    new Openable:
      type Self = Eof[file]
      type Operand = OpenFlag
      type Result = openable.Result

      def open[result](eof: Eof[file], lambda: openable.Result => result, options: List[OpenFlag])
      :   result =

        openable.open(eof.file, lambda, OpenFlag.Append :: options)

trait Openable extends Typeclass, Operable, Resultant:
  def open[result](value: Self, lambda: Result => result, options: List[Operand]): result
