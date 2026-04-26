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
package cellulose

import java.io as ji

import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import spectacular.*
import vacuous.*

object Bcodl:
  private def write(out: ji.Writer, number: Int): Unit = out.write((number + 32).toChar)

  private def write(out: ji.Writer, text: Text): Unit =
    write(out, text.length)
    out.write(text.s)

  def write(out: ji.Writer, doc: CodlDoc): Unit =
    out.write("\u00b1\u00c0\u00d1")
    write(out, doc.schema, doc.children)

  private def write(out: ji.Writer, schema: CodlSchema, nodes: IArray[CodlNode]): Unit =
    val dataNodes = nodes.map(_.data).collect:
      case data: Atom => data

    write(out, dataNodes.length)

    dataNodes.each:
      case Atom(key, children, _, _) =>
        schema match
          case Field(_) =>
            write(out, key)

          case _ =>
            val index: Int = schema.keyMap.get(key).fold(0)(_ + 1)
            write(out, index)
            write(out, schema.entry(index - 1).schema, children)

  def read(schema: CodlSchema, reader: ji.Reader)(using binary: Tactic[BcodlError]): CodlDoc =
    if reader.read() != '\u00b1' || reader.read() != '\u00c0' || reader.read() != '\u00d1'
    then abort(BcodlError(t"header 0xb1c0d1", 0))

    def recur(schema: CodlSchema): List[CodlNode] =
      List.range(0, readNumber(reader)).map: _ =>
        schema match
          case Field(_) =>
            val key = readText(reader)
            CodlNode(Atom(key, IArray(), Layout.empty, CodlSchema.Free))

          case schema =>
            val subschema = readNumber(reader) match
              case 0     => (Unset, CodlSchema.Free)
              case index => schema.entry(index - 1).tuple

            val key = subschema(0).option.getOrElse(abort(BcodlError(t"unexpected key", 0)))

            val children = IArray.from(recur(subschema(1)))

            CodlNode(Atom(key, children, Layout.empty, subschema(1)))

    CodlDoc(IArray.from(recur(schema)), schema, 0)

  private def readNumber(in: ji.Reader): Int = in.read - 32

  private def readText(in: ji.Reader, length: Int = -1): Text =
    val buffer = new Array[Char](if length == -1 then readNumber(in) else length)
    in.read(buffer)
    String(buffer).show
