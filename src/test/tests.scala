package xylophone

import probably.*
import printers.compact

object Tests extends Suite("Xylophone tests"):

  case class Person(name: String, age: Int)
  case class Firm(name: String, ceo: Person)

  case class Book(title: String, @xmlAttribute isbn: String)
  case class Bibliography(author: String, book: Book)

  enum Color:
    case Rgb(red: Int, green: Int, blue: Int)
    case Cmyk(cyan: Int, magenta: Int, yellow: Int, key: Int)

  case class Pixel(x: Int, y: Int, color: Color)

  def run(using Runner): Unit =
    test("extract integer") {
      Xml.parse("""<message>1</message>""").as[Int]
    }.assert(_ == 1)
    
    test("extract string") {
      Xml.parse("""<message>Hello world</message>""").as[String]
    }.assert(_ == "Hello world")

    test("extract string from fragment") {
      val xml = Xml.parse("""<message><info>Hello world</info></message>""")
      xml.info.as[String]
    }.assert(_ == "Hello world")
    
    test("extract string from node") {
      val xml = Xml.parse("""<message><info>Hello world</info></message>""")
      xml.info().as[String]
    }.assert(_ == "Hello world")

    test("serialize simple case class") {
      val person = Person("Jack", 50)
      person.xml.string
    }.assert(_ == "<Person><name>Jack</name><age>50</age></Person>")
    
    test("serialize nested case class") {
      val person = Person("Jack", 50)
      val company = Firm("Acme Inc", person)
      company.xml.string
    }.assert(_ == "<Firm><name>Acme Inc</name><ceo><name>Jack</name><age>50</age></ceo></Firm>")

    test("access second element") {
      val xml = Xml.parse("""<events><eventId>1</eventId><eventId>2</eventId></events>""")
      xml.eventId(1).as[Int]
    }.assert(_ == 2)
    
    test("extract to simple case class") {
      val string = "<jack><name>Jack</name><age>50</age></jack>"
      val xml = Xml.parse(string)
      xml.as[Person]
    }.assert(_ == Person("Jack", 50))
    
    test("extract to nested case class") {
      val string = "<Firm><name>Acme Inc</name><ceo><name>Jack</name><age>50</age></ceo></Firm>"
      val xml = Xml.parse(string)
      xml.as[Firm]
    }.assert(_ == Firm("Acme Inc", Person("Jack", 50)))

    test("serialize with attribute") {
      val book = Book("Lord of the Flies", "9780399529207")
      book.xml.string
    }.assert(_ == "<Book isbn=\"9780399529207\"><title>Lord of the Flies</title></Book>")
    
    test("serialize nested type with attribute") {
      val bibliography = Bibliography("William Golding", Book("Lord of the Flies", "9780399529207"))
      bibliography.xml.string
    }.assert(_ == "<Bibliography><author>William Golding</author><book isbn=\"9780399529207\"><title>Lord of the Flies</title></book></Bibliography>")

    test("serialize coproduct") {
      val color: Color = Color.Rgb(5, 10, 15)
      color.xml.string
    }.assert(_ == """<Color type="Rgb"><red>5</red><green>10</green><blue>15</blue></Color>""")
    
    test("serialize nested coproduct") {
      val pixel: Pixel = Pixel(100, 200, Color.Cmyk(1, 2, 3, 4))
      pixel.xml.string
    }.assert(_ == """<Pixel><x>100</x><y>200</y><color type="Cmyk"><cyan>1</cyan><magenta>2</magenta><yellow>3</yellow><key>4</key></color></Pixel>""")

    test("read coproduct") {
      val string = """<Color type="Cmyk"><cyan>1</cyan><magenta>2</magenta><yellow>3</yellow><key>4</key></Color>"""
      val xml = Xml.parse(string)
      xml.as[Color]
    }.assert(_ == Color.Cmyk(1, 2, 3, 4))
    
    test("read nested coproduct") {
      val string = """<Pixel><x>100</x><y>200</y><color type="Cmyk"><cyan>1</cyan><magenta>2</magenta><yellow>3</yellow><key>4</key></color></Pixel>"""
      val xml = Xml.parse(string)
      xml.as[Pixel]
    }.assert(_ == Pixel(100, 200, Color.Cmyk(1, 2, 3, 4)))

    test("read attribute value from fragment") {
      val string = """<node><content key="value"/></node>"""
      val xml = Xml.parse(string)
      xml.content.attribute("key").as[String]
    }.assert(_ == "value")
    
    test("read attribute value from node") {
      val string = """<node><content key="value"/></node>"""
      val xml = Xml.parse(string)
      xml.content().attribute("key").as[String]
    }.assert(_ == "value")