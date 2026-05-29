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
package zeppelin

import java.util.zip as juz

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*
import turbulence.*
import vacuous.*
import zephyrine.*

object ZipStream:
  def apply[streamable: Streamable by Data](source: streamable): ZipStream logs Text =
    new ZipStream(() => source.stream[Data], _ => true)

class ZipStream(stream: () => Stream[Data], filter: (Path on Zip) => Boolean):
  def keep(predicate: (Path on Zip) => Boolean): ZipStream =
    new ZipStream(stream, { (ref: Path on Zip) => filter(ref) && predicate(ref) })

  def extract(ref: Path on Zip): Zip.Entry raises ZipError logs Text =
    safely(keep(_.descent == ref.descent).map(identity(_)).headOption.get).or:
      abort(ZipError(ZipError.Reason.NotFound(ref)))

  def each(lambda: Zip.Entry => Unit): Unit raises ZipError = map[Unit](lambda).strict

  def map[element](lambda: Zip.Entry => element): Stream[element] raises ZipError =
    val cursor = Cursor[Data](stream().filter(_.nonEmpty).iterator)

    def findMagic(): Boolean =
      var done = false
      var found = false

      while !done && !cursor.finished do
        if !cursor.seek(0x50.toByte) then done = true
        else
          val matched = cursor.lookahead:
            cursor.next() && cursor.lay(false)(_ == 0x4b.toByte)
            && cursor.next() && cursor.lay(false)(_ == 0x03.toByte)
            && cursor.next() && cursor.lay(false)(_ == 0x04.toByte)

          if matched then
            found = true
            done = true
          else
            cursor.next()

      found

    if !findMagic() then Stream() else
      val zipIn = juz.ZipInputStream(cursor.remainder.inputStream)

      def recur(): Stream[Zip.Entry] = zipIn.getNextEntry() match
        case null                         => Stream()
        case entry if entry.isDirectory() => recur()

        case entry =>
          import errorDiagnostics.empty

          val ref: Path on Zip =
            val name = entry.getName().nn.tt

            whereas:
              case PathError(_, _)    => ZipError(ZipError.Reason.InvalidName(name))
              case NameError(_, _, _) => ZipError(ZipError.Reason.InvalidName(name))

            . mitigate(name.decode[Path on Zip])

          if !filter(ref) then recur() else
            def read(): Stream[Data] =
              if zipIn.available == 0 then Stream() else
                val size = entry.getSize.toInt.min(4096).puncture(-1).or(4096)
                val array: Array[Byte] = new Array[Byte](size)
                val count = zipIn.read(array, 0, array.length)

                if count == 0 then Stream() else
                  (if count < size then array.take(count) else array).immutable(using Unsafe)
                  #:: read()

            Zip.Entry(ref, read()) #:: recur()

      recur().map(lambda)
