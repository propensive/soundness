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
┃    Soundness, version 0.43.0.                                                                    ┃
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
import contingency.*
import prepositional.*
import proscenium.*
import serpentine.*
import spectacular.*
import turbulence.*

import java.nio.channels as jnc
import java.nio.file as jnf

object Openable:
  given openable: [system: System, path <: Path on system]
        => (read:        ReadAccess,
            write:       WriteAccess,
            dereference: DereferenceSymlinks,
            create:      CreateNonexistent on system,
            streamError: Tactic[StreamError],
            ioError:     Tactic[IoError])
        =>  path is Openable by jnf.OpenOption to Handle = new Openable:

    type Self = path
    type Operand = jnf.OpenOption
    type Result = Handle
    type Transport = jnc.FileChannel

    def init(path: path, extraOptions: List[jnf.OpenOption]): jnc.FileChannel =
      val options =
        read.options() ++ write.options() ++ dereference.options() ++ create.options()
        ++ extraOptions

      import jnf.StandardOpenOption as jnfsoo

      val options2 =
        if options.contains(jnfsoo.READ) && options.contains(jnfsoo.APPEND)
        then options.filter(_ != jnfsoo.READ)
        else options

      path.protect(IoError.Operation.Open)
       (jnc.FileChannel.open(jnf.Path.of(path.encode.s), options2*).nn)

    def handle(channel: jnc.FileChannel): Handle =
      Handle
       (() => Readable.channel.stream(channel).stream[Bytes],
        Writable.channel.write(channel, _))


    def close(channel: jnc.FileChannel): Unit = channel.close()

  given eof: [file: Openable by jnf.OpenOption] => Eof[file] is Openable:
    type Self = Eof[file]
    type Operand = file.Operand
    type Result = file.Result
    type Transport = file.Transport

    def init(eof: Eof[file], options: List[Operand]): Transport =
      file.init(eof.file, jnf.StandardOpenOption.APPEND :: options)

    def handle(transport: Transport): Result = file.handle(transport)
    def close(transport: Transport): Unit = file.close(transport)

trait Openable extends Typeclass, Operable, Resultant:
  protected type Transport

  def init(value: Self, options: List[Operand]): Transport
  def handle(transport: Transport): Result

  def open[result](value: Self, lambda: Result => result, options: List[Operand]): result =
    val transport = init(value, options)
    try lambda(handle(transport)) finally close(transport)

  def close(transport: Transport): Unit
