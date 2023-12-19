/*
    Cellulose, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package cellulose

import rudiments.*
import vacuous.*
import spectacular.*
import gossamer.*
import perforate.*
import anticipation.*

import java.io as ji

//import language.experimental.captureChecking

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
      case data: Data => data

    write(out, dataNodes.length)
    
    dataNodes.foreach:
      case Data(key, children, _, _) =>
        schema match
          case Field(_, _) =>
            write(out, key)
          
          case _ =>
            val idx: Int = schema.keyMap.get(key).fold(0)(_ + 1)
            write(out, idx)
            write(out, schema.entry(idx - 1).schema, children)
      
  def read(schema: CodlSchema, reader: ji.Reader)(using binary: Raises[BcodlError]): CodlDoc =
    if reader.read() != '\u00b1' || reader.read() != '\u00c0' || reader.read() != '\u00d1'
    then abort(BcodlError(t"header 0xb1c0d1", 0))
    
    def recur(schema: CodlSchema): List[CodlNode] =
      List.range(0, readNumber(reader)).map: _ =>
        schema match
          case Field(_, _) =>
            val key = readText(reader)
            CodlNode(Data(key, IArray(), Layout.empty, CodlSchema.Free))
          
          case schema =>
            val subschema = readNumber(reader) match
              case 0  => (Unset, CodlSchema.Free)
              case idx => schema.entry(idx - 1).tuple

            val key = subschema(0).option.getOrElse(abort(BcodlError(t"unexpected key", 0)))

            val children = IArray.from(recur(subschema(1)))
            
            CodlNode(Data(key, children, Layout.empty, subschema(1)))
    
    CodlDoc(IArray.from(recur(schema)), schema, 0)

  private def readNumber(in: ji.Reader): Int = in.read - 32

  private def readText(in: ji.Reader, length: Int = -1): Text =
    val buf = new Array[Char](if length == -1 then readNumber(in) else length)
    in.read(buf)
    String(buf).show
  
