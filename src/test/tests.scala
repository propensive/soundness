/*
    Xylophone, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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
import rudiments.*
import anticipation.*
import gossamer.*
import spectacular.*
import contingency.*
import printers.compact

import unsafeExceptions.canThrowAny
import errorHandlers.throwUnsafely

case class Worker(name: Text, age: Int)
case class Firm(name: Text, ceo: Worker)

case class Book(title: Text, @xmlAttribute isbn: Text)
case class Bibliography(author: Text, book: Book)

enum ColorVal:
  case Rgb(red: Int, green: Int, blue: Int)
  case Cmyk(cyan: Int, magenta: Int, yellow: Int, key: Int)

case class Pixel(x: Int, y: Int, color: ColorVal)

object Tests extends Suite(t"Xylophone tests"):
  def run(): Unit =
    test(t"extract integer"):
      Xml.parse(t"""<message>1</message>""").as[Int]
    .assert(_ == 1)
    
    test(t"extract string"):
      Xml.parse(t"""<message>Hello world</message>""").as[Text]
    .assert(_ == t"Hello world")

    test(t"extract string from fragment"):
      val xml = Xml.parse(t"""<message><info>Hello world</info></message>""")
      xml.info.as[Text]
    .assert(_ == t"Hello world")
    
    test(t"extract string from node"):
      val xml = Xml.parse(t"""<message><info>Hello world</info></message>""")
      xml.info().as[Text]
    .assert(_ == t"Hello world")

    test(t"serialize simple case class"):
      val person = Worker(t"Jack", 50)
      person.xml.string
    .assert(_ == t"<Worker><name>Jack</name><age>50</age></Worker>")
    
    test(t"serialize nested case class"):
      val person = Worker(t"Jack", 50)
      val company = Firm(t"Acme Inc", person)
      company.xml.string
    .assert(_ == t"<Firm><name>Acme Inc</name><ceo><name>Jack</name><age>50</age></ceo></Firm>")

    test(t"access second element"):
      val xml = Xml.parse(t"""<events><eventId>1</eventId><eventId>2</eventId></events>""")
      xml.eventId(1).as[Int]
    .assert(_ == 2)
    
    test(t"extract to simple case class"):
      val string = t"<jack><name>Jack</name><age>50</age></jack>"
      val xml = Xml.parse(string)
      xml.as[Worker]
    .assert(_ == Worker(t"Jack", 50))
    
    test(t"extract to nested case class"):
      val string = t"<Firm><name>Acme Inc</name><ceo><name>Jack</name><age>50</age></ceo></Firm>"
      val xml = Xml.parse(string)
      xml.as[Firm]
    .assert(_ == Firm(t"Acme Inc", Worker(t"Jack", 50)))

    test(t"serialize with attribute"):
      val book = Book(t"Lord of the Flies", t"9780399529207")
      book.xml.string
    .assert(_ == t"<Book isbn=\"9780399529207\"><title>Lord of the Flies</title></Book>")
    
    test(t"serialize nested type with attribute"):
      val bibliography = Bibliography(t"William Golding", Book(t"Lord of the Flies", t"9780399529207"))
      bibliography.xml.string
    .assert(_ == t"<Bibliography><author>William Golding</author><book isbn=\"9780399529207\"><title>Lord of the Flies</title></book></Bibliography>")

    test(t"serialize coproduct"):
      val color: ColorVal = ColorVal.Rgb(5, 10, 15)
      color.xml.string
    .assert(_ == t"""<ColorVal type="Rgb"><red>5</red><green>10</green><blue>15</blue></ColorVal>""")
    
    test(t"serialize nested coproduct"):
      val pixel: Pixel = Pixel(100, 200, ColorVal.Cmyk(1, 2, 3, 4))
      pixel.xml.string
    .assert(_ == t"""<Pixel><x>100</x><y>200</y><color type="Cmyk"><cyan>1</cyan><magenta>2</magenta><yellow>3</yellow><key>4</key></color></Pixel>""")

    test(t"read coproduct"):
      val string = t"""<ColorVal type="Cmyk"><cyan>1</cyan><magenta>2</magenta><yellow>3</yellow><key>4</key></ColorVal>"""
      val xml = Xml.parse(string)
      xml.as[ColorVal]
    .assert(_ == ColorVal.Cmyk(1, 2, 3, 4))
    
    test(t"read nested coproduct"):
      val string = t"""<Pixel><x>100</x><y>200</y><color type="Cmyk"><cyan>1</cyan><magenta>2</magenta><yellow>3</yellow><key>4</key></color></Pixel>"""
      val xml = Xml.parse(string)
      xml.as[Pixel]
    .assert(_ == Pixel(100, 200, ColorVal.Cmyk(1, 2, 3, 4)))

    test(t"read attribute value from fragment"):
      val string = t"""<node><content key="value"/></node>"""
      val xml = Xml.parse(string)
      xml.content.attribute(t"key").as[Text]
    .assert(_ == t"value")
    
    test(t"read attribute value from node"):
      val string = t"""<node><content key="value"/></node>"""
      val xml = Xml.parse(string)
      xml.content().attribute(t"key").as[Text]
    .assert(_ == t"value")

    test(t"read with namespace"):
      val string = t"""<?xml version="1.0"?>
                        |<root>
                        |<h:table xmlns:h="http://www.w3.org/TR/html4/">
                        |  <h:tr>
                        |    <h:td>Apples</h:td>
                        |    <h:td>Bananas</h:td>
                        |  </h:tr>
                        |</h:table>
                        |</root>""".s.stripMargin

      val xml = Xml.parse(Text(string))
      xml.table.tr.td().as[Text]
    .assert(_ == t"Apples")

    test(t"serialize list of strings"):
      val xs = List(t"one", t"two", t"three")
      Xml.print(xs.xml)
    .assert(_ == t"<Seq><Text>one</Text><Text>two</Text><Text>three</Text></Seq>")
    
    test(t"serialize list of complex objects"):
      val book1 = Book(t"Lord of the Flies", t"9780399529207")
      val book2 = Book(t"Brave New World", t"9781907704345")
      val books = List(book1, book2)
      Xml.print(books.xml)
    .assert(_ == t"""<Seq><Book isbn="9780399529207"><title>Lord of the Flies</title></Book><Book isbn="9781907704345"><title>Brave New World</title></Book></Seq>""")

    test(t"serialize empty node"):
      Xml.print(List[Text]().xml)
    .assert(_ == t"<Seq/>")

    test(t"serialize case object"):
      case object Foo
      Xml.print(Foo.xml)
    .assert(_ == t"<Foo/>")

    test(t"parse error: unclosed tag"):
      capture(Xml.parse(t"""<foo key="value"><bar></foo>"""))
    .assert(_ == XmlParseError(0, 24))

    test(t"parse error: unclosed string"):
      capture(Xml.parse(t"""<foo key="value><bar/></foo>"""))
    .assert(_ == XmlParseError(0, 16))

    test(t"read error: not an integer"):
      val xml = Xml.parse(t"""<foo>not an integer</foo>""")
      capture(xml.as[Int])
    .assert(NumberError(t"not an integer", Int) == _)

    test(t"access error; proactively resolving head nodes"):
      val xml = Xml.parse(t"""<root><company><staff><ceo><name>Xyz</name></ceo></staff></company></root>""")
      capture(xml.company().staff().cto().name().as[Text])
    .assert(_ == XmlAccessError(0, List(t"company", 0, t"staff", 0, t"cto")))
    
    test(t"access error; taking all children"):
      val xml = Xml.parse(t"""<root><company><staff><ceo><name>Xyz</name></ceo></staff></company></root>""")
      capture(xml.company.staff.cto.name().as[Text])
    .assert(_ == XmlAccessError(0, List(t"company", t"staff", t"cto", t"name")))
    
    test(t"access non-zero node"):
      val xml = Xml.parse(t"""<root><company><staff><ceo><name>Xyz</name></ceo></staff></company></root>""")
      capture(xml.company(1).staff().cto.name().as[Text])
    .assert(_ == XmlAccessError(1, List(t"company")))
    
    // test(t"simple literal content is as expected"): 
    //   x"""<root attribute=""/>""".show
    // .assert(_ == t"""<root attribute=""/>""")

    // test(t"literal content is as expected"): 
    //   x"<root><company><staff><ceo><name>Xyz</name></ceo></staff></company></root>"
    // .assert(_ == t"""<root><company><staff><ceo><name>Xyz</name></ceo></staff></company></root>""")
