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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import java.nio.channels as jnc
import java.nio.file as jnf

import anticipation.*
import beneficence.*
import contingency.*
import prepositional.*
import serpentine.*
import turbulence.*

object Openable:
  class FileOpenable[filesystem: Filesystem, path <: Path on filesystem]
    ( using read:        ReadAccess,
            write:       WriteAccess,
            dereference: DereferenceSymlinks,
            create:      CreateNonexistent on filesystem,
            streamError: Tactic[StreamError],
            ioError:     Tactic[IoError] )
  extends Openable:

    type Self = path
    type Operand = jnf.OpenOption
    type Result = Handle
    type Transport = jnc.FileChannel

    def initialize(path: path, extraOptions: List[jnf.OpenOption]): jnc.FileChannel =
      val options =
        read.options() ++ write.options() ++ dereference.options() ++ create.options() ++
          extraOptions

      import jnf.StandardOpenOption as jnfsoo

      val options2 =
        if options.contains(jnfsoo.READ) && options.contains(jnfsoo.APPEND)
        then options.filter(_ != jnfsoo.READ)
        else options

      path.protect(IoError.Operation.Open)
        ( jnc.FileChannel.open(jnf.Path.of(path.encode.s), options2*).nn )

    def handle(channel: jnc.FileChannel): Handle^ =
      Handle
        ( () => Streamable.channel.stream(channel).stream[Data],
          Writable.channel.write(channel, _) )

    def close(channel: jnc.FileChannel): Unit = channel.close()


  given openable: [filesystem: Filesystem, path <: Path on filesystem]
  =>  ( ReadAccess,
        WriteAccess,
        DereferenceSymlinks,
        CreateNonexistent on filesystem,
        Tactic[StreamError],
        Tactic[IoError] )
  =>  ( FileOpenable[filesystem, path]^ ) =
    FileOpenable[filesystem, path]


  given eof: [file: Openable by jnf.OpenOption]
  =>  ( Openable
        { type Self = Eof[file]
          type Operand = file.Operand
          type Result = file.Result
          type Transport = file.Transport }^ ) =

    new Openable:
      type Self = Eof[file]
      type Operand = file.Operand
      type Result = file.Result
      type Transport = file.Transport

      def initialize(eof: Eof[file], options: List[Operand]): Transport =
        file.initialize(eof.file, jnf.StandardOpenOption.APPEND :: options)

      def handle(transport: Transport): Result^ = file.handle(transport)
      def close(transport: Transport): Unit = file.close(transport)

trait Openable extends Findable, Operable, Resultant:
  type Self
  protected type Transport

  def initialize(value: Self, options: List[Operand]): Transport
  def handle(transport: Transport): Result^

  def open[result](value: Self, lambda: Result^ => result, options: List[Operand]): result =
    val transport = initialize(value, options)
    try lambda(handle(transport)) finally close(transport)

  def close(transport: Transport): Unit
