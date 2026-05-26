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
package stratiform

import scala.language.unsafeNulls

import anticipation.*
import gossamer.*
import vacuous.*

// Loads upstream TEL test corpus from classpath resources. The corpus is
// laid out under /stratiform/tel/{pos,neg}/ with paired <stem>.tel input
// files and <stem>.check fixture files. The pos.index / neg.index files
// list one stem per line; tests iterate the index to enumerate cases.

object CorpusLoader:

  case class Case(stem: Text, source: Data, check: Text)

  def positive: List[Case] = load(t"pos")
  def negative: List[Case] = load(t"neg")

  private def load(category: Text): List[Case] =
    readIndex(category).filterNot(_.s.startsWith("_")).map: stem =>
      val source = readResource(t"/stratiform/tel/$category/$stem.tel")
      val check = readResourceText(t"/stratiform/tel/$category/$stem.check")
      Case(stem, IArray.from(source), check)

  // Extract the expected E-code from a negative case's stem name. Filenames
  // follow the upstream convention `e<n>-<description>.tel`; cases without
  // an E-code prefix (`_contact-schema.tel`, `contact-document-missing-name`)
  // return Unset and are skipped by the error-code assertion.
  def expectedCode(stem: Text): Optional[Int] =
    val s = stem.s
    if s.startsWith("e") && s.length > 1 && Character.isDigit(s.charAt(1)) then
      val end =
        var idx = 1
        while idx < s.length && Character.isDigit(s.charAt(idx)) do idx += 1
        idx

      try s.substring(1, end).toInt: Optional[Int] catch case _: NumberFormatException => Unset
    else Unset

  private def readIndex(category: Text): List[Text] =
    val text = readResourceText(t"/stratiform/tel/$category.index")
    text.s.split('\n').toList.map(_.trim).filter(_.nonEmpty).map(Text(_))

  private def readResource(path: Text): Array[Byte] =
    val stream = getClass.getResourceAsStream(path.s)
    if stream == null then sys.error(s"missing resource: $path")
    try stream.readAllBytes() finally stream.close()

  private def readResourceText(path: Text): Text = Text(String(readResource(path), "UTF-8"))
