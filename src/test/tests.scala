/*
    Xylophone, version 0.1.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package xylophone

import probably.*
import eucalyptus.*
import rudiments.*
import gossamer.*
import printers.compact

import scala.util.{Try, Success, Failure}

import unsafeExceptions.canThrowAny

given Log(Everything |-> Stdout)

case class Person(name: Txt, age: Int)
case class Firm(name: Txt, ceo: Person)

case class Book(title: Txt, @xmlAttribute isbn: Txt)
case class Bibliography(author: Txt, book: Book)

enum Color:
  case Rgb(red: Int, green: Int, blue: Int)
  case Cmyk(cyan: Int, magenta: Int, yellow: Int, key: Int)

case class Pixel(x: Int, y: Int, color: Color)

object Tests extends Suite(str"Xylophone tests"):

  def run(using Runner): Unit =
    test(str"extract integer") {
      Xml.parse(str"""<message>1</message>""").as[Int]
    }.assert(_ == 1)
    
    test(str"extract string") {
      Xml.parse(str"""<message>Hello world</message>""").as[Txt]
    }.check(_ == str"Hello world")

    test(str"extract string from fragment") {
      val xml = Xml.parse(str"""<message><info>Hello world</info></message>""")
      xml.info.as[Txt]
    }.check(_ == str"Hello world")
    
    test(str"extract string from node") {
      val xml = Xml.parse(str"""<message><info>Hello world</info></message>""")
      xml.info().as[Txt]
    }.check(_ == str"Hello world")

    test(str"serialize simple case class") {
      val person = Person(str"Jack", 50)
      person.xml.string
    }.check(_ == str"<Person><name>Jack</name><age>50</age></Person>")
    
    test(str"serialize nested case class") {
      val person = Person(str"Jack", 50)
      val company = Firm(str"Acme Inc", person)
      company.xml.string
    }.check(_ == str"<Firm><name>Acme Inc</name><ceo><name>Jack</name><age>50</age></ceo></Firm>")

    test(str"access second element") {
      val xml = Xml.parse(str"""<events><eventId>1</eventId><eventId>2</eventId></events>""")
      xml.eventId(1).as[Int]
    }.assert(_ == 2)
    
    test(str"extract to simple case class") {
      val string = str"<jack><name>Jack</name><age>50</age></jack>"
      val xml = Xml.parse(string)
      xml.as[Person]
    }.assert(_ == Person(str"Jack", 50))
    
    test(str"extract to nested case class") {
      val string = str"<Firm><name>Acme Inc</name><ceo><name>Jack</name><age>50</age></ceo></Firm>"
      val xml = Xml.parse(string)
      xml.as[Firm]
    }.assert(_ == Firm(str"Acme Inc", Person(str"Jack", 50)))

    test(str"serialize with attribute") {
      val book = Book(str"Lord of the Flies", str"9780399529207")
      book.xml.string
    }.check(_ == str"<Book isbn=\"9780399529207\"><title>Lord of the Flies</title></Book>")
    
    test(str"serialize nested type with attribute") {
      val bibliography = Bibliography(str"William Golding", Book(str"Lord of the Flies", str"9780399529207"))
      bibliography.xml.string
    }.check(_ == str"<Bibliography><author>William Golding</author><book isbn=\"9780399529207\"><title>Lord of the Flies</title></book></Bibliography>")

    test(str"serialize coproduct") {
      val color: Color = Color.Rgb(5, 10, 15)
      color.xml.string
    }.check(_ == str"""<Color type="Rgb"><red>5</red><green>10</green><blue>15</blue></Color>""")
    
    test(str"serialize nested coproduct") {
      val pixel: Pixel = Pixel(100, 200, Color.Cmyk(1, 2, 3, 4))
      pixel.xml.string
    }.check(_ == str"""<Pixel><x>100</x><y>200</y><color type="Cmyk"><cyan>1</cyan><magenta>2</magenta><yellow>3</yellow><key>4</key></color></Pixel>""")

    test(str"read coproduct") {
      val string = str"""<Color type="Cmyk"><cyan>1</cyan><magenta>2</magenta><yellow>3</yellow><key>4</key></Color>"""
      val xml = Xml.parse(string)
      xml.as[Color]
    }.assert(_ == Color.Cmyk(1, 2, 3, 4))
    
    test(str"read nested coproduct") {
      val string = str"""<Pixel><x>100</x><y>200</y><color type="Cmyk"><cyan>1</cyan><magenta>2</magenta><yellow>3</yellow><key>4</key></color></Pixel>"""
      val xml = Xml.parse(string)
      xml.as[Pixel]
    }.assert(_ == Pixel(100, 200, Color.Cmyk(1, 2, 3, 4)))

    test(str"read attribute value from fragment") {
      val string = str"""<node><content key="value"/></node>"""
      val xml = Xml.parse(string)
      xml.content.attribute(str"key").as[Txt]
    }.check(_ == str"value")
    
    test(str"read attribute value from node") {
      val string = str"""<node><content key="value"/></node>"""
      val xml = Xml.parse(string)
      xml.content().attribute(str"key").as[Txt]
    }.check(_ == str"value")

    test(str"read with namespace") {
      val string = str"""<?xml version="1.0"?>
                        |<root>
                        |<h:table xmlns:h="http://www.w3.org/TR/html4/">
                        |  <h:tr>
                        |    <h:td>Apples</h:td>
                        |    <h:td>Bananas</h:td>
                        |  </h:tr>
                        |</h:table>
                        |</root>""".s.stripMargin

      val xml = Xml.parse(Txt(string))
      xml.table.tr.td().as[Txt]
    }.check(_ == str"Apples")

    test(str"serialize list of strings") {
      val xs = List(str"one", str"two", str"three")
      Xml.print(xs.xml)
    }.check(_ == str"<List><Text>one</Text><Text>two</Text><Text>three</Text></List>")
    
    test(str"serialize list of complex objects") {
      val book1 = Book(str"Lord of the Flies", str"9780399529207")
      val book2 = Book(str"Brave New World", str"9781907704345")
      val books = List(book1, book2)
      Xml.print(books.xml)
    }.check(_ == str"""<List><Book isbn="9780399529207"><title>Lord of the Flies</title></Book><Book isbn="9781907704345"><title>Brave New World</title></Book></List>""")

    test(str"serialize empty node") {
      Xml.print(List[Txt]().xml)
    }.check(_ == str"<List/>")

    test(str"serialize case object") {
      case object Foo
      Xml.print(Foo.xml)
    }.check(_ == str"<Foo/>")

  //   test("parse error: unclosed tag") {
  //     Try(Xml.parse("""<foo key="value"><bar></foo>"""))
  //   }.assert(_ == Failure(XmlParseError(0, 24)))

  //   test("parse error: unclosed string") {
  //     Try(Xml.parse("""<foo key="value><bar/></foo>"""))
  //   }.assert(_ == Failure(XmlParseError(0, 16)))

  //   test("read error: not an integer") {
  //     val xml = Xml.parse("""<foo>not an integer</foo>""")
  //     Try(xml.as[Int])
  //   }.assert(Failure(XmlReadError()) == _)

  //   test("access error; proactively resolving head nodes") {
  //     val xml = Xml.parse("""<root><company><staff><ceo><name>Xyz</name></ceo></staff></company></root>""")
  //     Try(xml.company().staff().cto().name().as[Txt])
  //   }.assert(_ == Failure(XmlAccessError(0, List("company", 0, "staff", 0, "cto"))))
    
  //   test("access error; taking all children") {
  //     val xml = Xml.parse("""<root><company><staff><ceo><name>Xyz</name></ceo></staff></company></root>""")
  //     Try(xml.company.staff.cto.name().as[Txt])
  //   }.assert(_ == Failure(XmlAccessError(0, List("company", "staff", "cto", "name"))))
    
  //   test("access non-zero node") {
  //     val xml = Xml.parse("""<root><company><staff><ceo><name>Xyz</name></ceo></staff></company></root>""")
  //     Try(xml.company(1).staff().cto.name().as[Txt])
  //   }.assert(_ == Failure(XmlAccessError(1, List("company"))))
