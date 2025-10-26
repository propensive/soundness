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
┃    Soundness, version 0.45.0.                                                                    ┃
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
package zeppelin

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import nomenclature.*
import prepositional.*
import proscenium.*
import rudiments.*
import serpentine.*
import turbulence.*
import vacuous.*
import zephyrine.*

import java.util.zip as juz

object ZipStream:
  def apply[readable: Readable by Bytes](source: readable): ZipStream logs Text =
    new ZipStream(() => source.stream[Bytes], _ => true)

class ZipStream(stream: () => Stream[Bytes], filter: (Path on Zip) => Boolean):

  def keep(predicate: (Path on Zip) => Boolean): ZipStream =
    new ZipStream(stream, { (ref: Path on Zip) => filter(ref) && predicate(ref) })

  def extract(ref: Path on Zip): ZipEntry raises ZipError logs Text =
    safely(keep(_.descent == ref.descent).map(identity(_)).headOption.get).or:
      abort(ZipError())

  def each(lambda: ZipEntry => Unit): Unit raises ZipError = map[Unit](lambda).strict

  def map[element](lambda: ZipEntry => element): Stream[element] raises ZipError =
    val conduit = Conduit(stream())
    if !conduit.search(0x50, 0x4b, 0x03, 0x04) then Stream() else

      conduit.truncate()
      val zipIn = juz.ZipInputStream(conduit.remainder.inputStream)

      def recur(): Stream[ZipEntry] = zipIn.getNextEntry() match
        case null                         => Stream()
        case entry if entry.isDirectory() => recur()

        case entry =>
          import errorDiagnostics.empty
          val ref: Path on Zip =
            mitigate:
              case PathError(reason, path) => ZipError()
              case NameError(_, _, _)      => ZipError()

            . within:
                entry.getName().nn.tt.decode[Path on Zip]

          if !filter(ref) then recur() else
            def read(): Stream[Bytes] =
              if zipIn.available == 0 then Stream() else
                val size = entry.getSize.toInt.min(4096).puncture(-1).or(4096)
                val array: Array[Byte] = new Array[Byte](size)
                val count = zipIn.read(array, 0, array.length)

                if count == 0 then Stream() else
                  (if count < size then array.take(count) else array).immutable(using Unsafe)
                  #:: read()

            ZipEntry(ref, read()) #:: recur()

      recur().map(lambda)
