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
// element named after the variant. A field marked `@attribute` (e.g. `Book`'s
// `isbn`, defined in `xylophone_test.scala`) is written to / read from the
// element's attributes, so it round-trips.

// A stand-in for some other serialization format, to check that a `@name`
// scoped to it is ignored by XML.
sealed trait OtherFormat

// `title` is renamed for XML only; `author` for all formats (a bare `@name`,
// which infers `@name[Any]`); `note`'s rename is scoped to another format, so
// XML must ignore it and use the field name.
case class Labelled
   (@name[Xml](t"Title")          title:  Text,
    @name(t"writer")              author: Text,
    @name[OtherFormat](t"n")      note:   Text,
                                  pages:  Int)
derives CanEqual

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

    suite(m"@attribute fields"):
      test(m"An @attribute field encodes as an attribute"):
        Book(t"Dune", t"0441013597").xml
      . assert(_ == x"""<Book isbn="0441013597"><title>Dune</title></Book>""")

      test(m"An @attribute field round-trips"):
        Book(t"Dune", t"0441013597").xml.as[Book]
      . assert(_ == Book(t"Dune", t"0441013597"))

    suite(m"@name fields"):
      test(m"@name[Xml] and bare @name rename elements; other-format @name ignored"):
        Labelled(t"Dune", t"Herbert", t"sci-fi", 412).xml
      . assert: xml =>
          xml == x"""<Labelled><Title>Dune</Title><writer>Herbert</writer><note>sci-fi</note><pages>412</pages></Labelled>"""

      test(m"@name fields round-trip"):
        Labelled(t"Dune", t"Herbert", t"sci-fi", 412).xml.as[Labelled]
      . assert(_ == Labelled(t"Dune", t"Herbert", t"sci-fi", 412))
