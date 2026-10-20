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
package xylophone

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import denominative.dysasymptotics.linearSize

// The codec modes over XML, and the generated printer: a suite of its own, since the direct
// parsing suite's object is at the class-file size limit.
object CodecModeTests extends Suite("xml-codecs", m"Xylophone codec mode tests"):
  def run(): Unit =
    given XmlSchema = XmlSchema.Freeform

    suite(m"Codec modes"):
      val team = PTeam(t"Crew", List(PWorker(t"Zoë <&> \"q\"", 3), PWorker(t"Bob", 40)))
      val tree = PTree(t"root", List(PTree(t"a", Nil), PTree(t"b", List(PTree(t"c", Nil)))))
      val ping = XPing(1, List(XPong(2, List(XPing(3, Nil))), XPong(4, Nil)))
      val opt = XOpt(t"a", Unset, PWorker(t"b", 2))
      val holder = XHolder(t"h", XNick(Unset), XEmpty())
      val mapped = XMapped(t"m", Map(t"x" -> 1, t"y" -> 2))
      val mixed = XMixed(true, 2.5, 1L << 40, t"")
      val playlist = PPlaylist(t"mix", List(t"one", t"two"))

      test(m"`codecs.direct` writes each record as `show` renders it"):
        import codecs.direct

        ( team.in[Text over Xml], tree.in[Text over Xml], ping.in[Text over Xml],
          opt.in[Text over Xml], holder.in[Text over Xml], mapped.in[Text over Xml],
          mixed.in[Text over Xml], playlist.in[Text over Xml] )
      . assert(_ == (team.in[Xml].show, tree.in[Xml].show, ping.in[Xml].show, opt.in[Xml].show,
          holder.in[Xml].show, mapped.in[Xml].show, mixed.in[Xml].show, playlist.in[Xml].show))

      test(m"`codecs.direct` reads each record back"):
        import codecs.direct

        ( team.in[Text over Xml].read[PTeam in Xml], ping.in[Text over Xml].read[XPing in Xml],
          opt.in[Text over Xml].read[XOpt in Xml] )
      . assert(_ == (team, ping, opt))

      test(m"`codecs.direct` writes a record with no instance of its own directly"):
        import codecs.direct

        given workerSeam: (PWorker is Encodable in Xml) =
          _ => panic(m"a generated printer encoded a nested record through its Encodable")

        PTeam(t"Crew", List(PWorker(t"Bob", 40))).in[Text over Xml]
      . assert(_ == t"<PTeam><name>Crew</name><members><name>Bob</name><age>40</age></members></PTeam>")

      test(m"`codecs.direct` leaves a generic record on the derived path"):
        import codecs.direct
        PBoxed(7).in[Text over Xml]
      . assert(_ == PBoxed(7).in[Xml].show)

      test(m"`codecs.derived` encodes even where a direct printer is in scope"):
        import codecs.derived

        given (PWorker is Xml.Printable) =
          Xml.Printable.fromEncodable[PWorker](_ => panic(m"printed directly"))

        PWorker(t"Ada", 36).in[Text over Xml]
      . assert(_ == t"<PWorker><name>Ada</name><age>36</age></PWorker>")

      test(m"`codecs.strict` writes and reads a record directly"):
        import codecs.strict

        given workerSeam: (PWorker is Encodable in Xml) =
          _ => panic(m"a generated printer encoded a nested record through its Encodable")

        team.in[Text over Xml].read[PTeam in Xml]
      . assert(_ == team)

      test(m"`codecs.strict` rejects a type with no direct codec"):
        val errors = demilitarize:
          import codecs.strict
          PBoxed(7).in[Text over Xml]

        errors.map(_.message.contains("has no direct codec"))
      . assert(_ == List(true))

      test(m"`derives Xml.Direct` writes and reads without an import"):
        XDeclared(5, t"five").in[Text over Xml].read[XDeclared in Xml]
      . assert(_ == XDeclared(5, t"five"))

      test(m"a printed record is written in blocks as it is printed"):
        given Buffering:
          def capacity(substrate: Substrate): Int = 8
          def depth: Int = 2

        given (PTree is Xml.Printable) = Inlinable.printable[PTree]
        val blocks = scala.collection.mutable.ListBuffer[Text]()
        Xml.Printable.emit(tree, blocks += _)
        (blocks.length > 1, blocks.toList.join)
      . assert(_ == (true, tree.in[Xml].show))


