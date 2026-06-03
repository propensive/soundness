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
package xylophone

import soundness.*

import strategies.throwUnsafely

// The `Encodable in Xml` derivation is the exact mirror of the decoder, so the
// fixtures (`DPerson`/`DContact`/`DShape`) are shared with `DecoderTests`. A
// product encodes to an element named after its type; each field becomes a
// child element named after the field; a sum encodes its variant under an
// element named after the variant. `@attribute` is intentionally not honoured
// (the decoder cannot read attributes back), so an `@attribute` field still
// encodes as a child element — locked in below.

object EncoderTests extends Suite(m"Xylophone case-class encoder tests"):
  def run(): Unit =
    given XmlSchema = XmlSchema.Freeform

    suite(m"Simple product"):
      test(m"Encode a flat case class"):
        DPerson(t"Alice", 30, t"a@b.c").xml
      . assert(_ == x"<DPerson><name>Alice</name><age>30</age><email>a@b.c</email></DPerson>")

      test(m"Flat case class round-trips"):
        DPerson(t"Alice", 30, t"a@b.c").xml.as[DPerson]
      . assert(_ == DPerson(t"Alice", 30, t"a@b.c"))

    suite(m"Nested product"):
      test(m"Encode a nested case class"):
        DContact(DPerson(t"Carol", 40, t"c@x"), t"Acme").xml
      . assert: xml =>
          xml == x"""<DContact><person><name>Carol</name><age>40</age><email>c@x</email></person><company>Acme</company></DContact>"""

      test(m"Nested case class round-trips"):
        DContact(DPerson(t"Carol", 40, t"c@x"), t"Acme").xml.as[DContact]
      . assert(_ == DContact(DPerson(t"Carol", 40, t"c@x"), t"Acme"))

    suite(m"Sum type by element label"):
      test(m"Encode the Circle variant"):
        (DShape.Circle(5): DShape).xml
      . assert(_ == x"<Circle><radius>5</radius></Circle>")

      test(m"Encode the Square variant"):
        (DShape.Square(4): DShape).xml
      . assert(_ == x"<Square><side>4</side></Square>")

      test(m"Circle variant round-trips"):
        (DShape.Circle(5): DShape).xml.as[DShape]
      . assert(_ == DShape.Circle(5))

      test(m"Square variant round-trips"):
        (DShape.Square(4): DShape).xml.as[DShape]
      . assert(_ == DShape.Square(4))

    suite(m"@attribute is not honoured (locked-in behaviour)"):
      test(m"An @attribute field still encodes as a child element"):
        Book(t"Dune", t"0441013597").xml
      . assert(_ == x"<Book><title>Dune</title><isbn>0441013597</isbn></Book>")
