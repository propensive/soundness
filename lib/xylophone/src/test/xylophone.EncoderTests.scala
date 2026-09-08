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
   (@name[Xml]("Title")          title:  Text,
    @name("writer")              author: Text,
    @name[OtherFormat]("n")      note:   Text,
                                  pages:  Int)
derives CanEqual

// `Stop` is renamed for XML only; `Go` for all formats (a bare `@name`); `Wait`
// is unannotated.
enum Light derives CanEqual:
  @name[Xml]("red") case Stop(seconds: Int)
  @name("green")    case Go(seconds: Int)
                     case Wait(seconds: Int)

object EncoderTests extends Suite(m"Xylophone case-class encoder tests"):
  def run(): Unit =
    given XmlSchema = XmlSchema.Freeform

    suite(m"Simple product"):
      test(m"Encode a flat case class"):
        DPerson("Alice", 30, "a@b.c").in[Xml]
      . assert(_ == x"<DPerson><name>Alice</name><age>30</age><email>a@b.c</email></DPerson>")

      test(m"Flat case class round-trips"):
        DPerson("Alice", 30, "a@b.c").in[Xml].as[DPerson]
      . assert(_ == DPerson("Alice", 30, "a@b.c"))

    suite(m"Nested product"):
      test(m"Encode a nested case class"):
        DContact(DPerson("Carol", 40, "c@x"), "Acme").in[Xml]
      . assert: xml =>
          xml == x"""<DContact><person><name>Carol</name><age>40</age><email>c@x</email></person><company>Acme</company></DContact>"""

      test(m"Nested case class round-trips"):
        DContact(DPerson("Carol", 40, "c@x"), "Acme").in[Xml].as[DContact]
      . assert(_ == DContact(DPerson("Carol", 40, "c@x"), "Acme"))

    suite(m"Sum type by element label"):
      test(m"Encode the Circle variant"):
        (DShape.Circle(5): DShape).in[Xml]
      . assert(_ == x"<Circle><radius>5</radius></Circle>")

      test(m"Encode the Square variant"):
        (DShape.Square(4): DShape).in[Xml]
      . assert(_ == x"<Square><side>4</side></Square>")

      test(m"Circle variant round-trips"):
        (DShape.Circle(5): DShape).in[Xml].as[DShape]
      . assert(_ == DShape.Circle(5))

      test(m"Square variant round-trips"):
        (DShape.Square(4): DShape).in[Xml].as[DShape]
      . assert(_ == DShape.Square(4))

    suite(m"@attribute fields"):
      test(m"An @attribute field encodes as an attribute"):
        Book("Dune", "0441013597").in[Xml]
      . assert(_ == x"""<Book isbn="0441013597"><title>Dune</title></Book>""")

      test(m"An @attribute field round-trips"):
        Book("Dune", "0441013597").in[Xml].as[Book]
      . assert(_ == Book("Dune", "0441013597"))

    suite(m"@name fields"):
      test(m"@name[Xml] and bare @name rename elements; other-format @name ignored"):
        Labelled("Dune", "Herbert", "sci-fi", 412).in[Xml]
      . assert: xml =>
          xml == x"""<Labelled><Title>Dune</Title><writer>Herbert</writer><note>sci-fi</note><pages>412</pages></Labelled>"""

      test(m"@name fields round-trip"):
        Labelled("Dune", "Herbert", "sci-fi", 412).in[Xml].as[Labelled]
      . assert(_ == Labelled("Dune", "Herbert", "sci-fi", 412))

    suite(m"@name variants"):
      test(m"@name[Xml] renames a variant's element"):
        (Light.Stop(30): Light).in[Xml]
      . assert(_ == x"<red><seconds>30</seconds></red>")

      test(m"bare @name renames a variant's element"):
        (Light.Go(45): Light).in[Xml]
      . assert(_ == x"<green><seconds>45</seconds></green>")

      test(m"an unannotated variant keeps its name"):
        (Light.Wait(5): Light).in[Xml]
      . assert(_ == x"<Wait><seconds>5</seconds></Wait>")

      test(m"@name variants round-trip"):
        List(Light.Stop(30), Light.Go(45), Light.Wait(5)).map(_.in[Xml].as[Light])
      . assert(_ == List(Light.Stop(30), Light.Go(45), Light.Wait(5)))
