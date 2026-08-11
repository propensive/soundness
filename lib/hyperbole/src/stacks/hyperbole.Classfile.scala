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
package hyperbole

import java.io as ji

import scala.collection.immutable as sci

import anticipation.*
import rudiments.*
import vacuous.*

private[hyperbole] object Classfile:
  // Reads the two things a stack-trace resolver wants from a classfile: the JSR-45
  // `SourceDebugExtension` attribute, whose body is the text of an SMAP, and the classes the
  // constant pool references, which are the candidates for where inlined code was defined. Like
  // TASTy, a classfile is only ever a source of extra detail, so anything unexpected about it—a
  // bad magic number, a constant-pool tag this reader does not know, a truncated attribute—means
  // `Unset`, never failure.
  def apply(data: Data): Optional[Classfile] =
    try
      val bytes = new scala.Array[Byte](data.length)
      System.arraycopy(Array.unsafeJvm(data), 0, bytes, 0, data.length)
      val in = ji.DataInputStream(ji.ByteArrayInputStream(bytes))

      if in.readInt() != 0xcafebabe then Unset else
        in.skipBytes(4) // minor and major version

        // Only the UTF-8 entries and the class references are kept; everything else in the
        // constant pool is skipped by its tag's fixed size.
        val count = in.readUnsignedShort()
        val utf8 = new scala.Array[String | Null](count)
        var classRefs: sci.List[Int] = sci.List()
        var index = 1
        var bad = false

        while index < count && !bad do
          in.readUnsignedByte() match
            case 1 =>
              utf8(index) = in.readUTF()

            case 7 =>
              classRefs = in.readUnsignedShort() :: classRefs

            case 8 | 16 | 19 | 20 =>
              in.skipBytes(2)

            case 15 =>
              in.skipBytes(3)

            case 3 | 4 | 9 | 10 | 11 | 12 | 17 | 18 =>
              in.skipBytes(4)

            case 5 | 6 =>
              in.skipBytes(8) yet { index += 1 } // longs and doubles take two slots

            case _ =>
              bad = true

          index += 1

        if bad then Unset else
          in.skipBytes(6) // access flags, this class, super class
          in.skipBytes(2*in.readUnsignedShort()) // interfaces

          def skipAttributes(): Unit =
            val attributes = in.readUnsignedShort()

            for _ <- 0 until attributes do
              in.skipBytes(2)
              in.skipBytes(in.readInt())

          def skipMembers(): Unit =
            val members = in.readUnsignedShort()

            for _ <- 0 until members do
              in.skipBytes(6) // access flags, name, descriptor
              skipAttributes()

          skipMembers() // fields
          skipMembers() // methods

          // The class-level attributes, where a `SourceDebugExtension` lives. Its body is the
          // SMAP text in modified UTF-8, which for the ASCII an SMAP contains is plain UTF-8.
          val attributes = in.readUnsignedShort()
          var smap: Optional[Text] = Unset
          var attribute = 0

          while attribute < attributes && smap.absent do
            val name = utf8(in.readUnsignedShort())
            val length = in.readInt()

            if name == "SourceDebugExtension" then
              val body = new scala.Array[Byte](length)
              in.readFully(body)
              smap = String(body, "UTF-8").tt
            else
              in.skipBytes(length)

            attribute += 1

          // Internal class names, in dotted form; array types reference no source definition.
          val classes = classRefs.reverse.flatMap: ref =>
            utf8(ref) match
              case null                         => sci.List()
              case name if name.startsWith("[") => sci.List()
              case name                         => sci.List(name.replace('/', '.').nn.tt)

          Classfile(smap, List.of(classes))

    catch case error: Exception => Unset

// What a classfile records that helps decipher a stack trace: its SMAP, when the compiler wrote
// one, and the classes it references.
private[hyperbole] case class Classfile(smap: Optional[Text], classes: List[Text])
