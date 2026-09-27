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
import dynamicAccess.dynamicXml

@xmlns("urn:shop")
case class Order(id: Int, item: Item, note: Text)

case class Item(sku: Text, quantity: Int)

@xmlns("urn:shop")
case class Ticket(id: Int, @unqualified reference: Text)

@xmlns("urn:shop", qualified = false)
case class Receipt(id: Int, total: Int)

case class Plain(id: Int)

object NamespaceTests extends Suite(m"Xylophone namespace tests"):
  given svg: ("svg" is Namespace of "http://www.w3.org/2000/svg") = Namespace()

  // The document element of a parse result
  def root(xml: Xml): Element = xml match
    case element: Element           => element
    case Fragment(element: Element) => element
    case _                          => Element(t"none", Attributes.empty, Array())

  def child(xml: Xml): Element = root(xml) match
    case Element(_, _, Array(child: Element)) => child
    case _                                    => Element(t"none", Attributes.empty, Array())

  def run(): Unit =
    given XmlSchema = XmlSchema.Freeform

    suite(m"Resolution"):
      test(m"An element's namespace resolves through the default declaration"):
        root(t"""<a xmlns="urn:x"/>""".read[Xml]).namespace
      . assert(_ == t"urn:x")

      test(m"A prefixed element's qualified name resolves"):
        root(t"""<p:a xmlns:p="urn:p"/>""".read[Xml]).qualified
      . assert(_ == Xml.Name(t"urn:p", t"a"))

      test(m"The local name has no prefix"):
        root(t"""<p:a xmlns:p="urn:p"/>""".read[Xml]).localName
      . assert(_ == t"a")

      test(m"The prefix is available"):
        root(t"""<p:a xmlns:p="urn:p"/>""".read[Xml]).prefix
      . assert(_ == t"p")

      test(m"An unprefixed element without a default namespace has none"):
        root(t"<a/>".read[Xml]).namespace
      . assert(_ == Unset)

      test(m"A child inherits the default namespace"):
        child(t"""<a xmlns="urn:x"><b/></a>""".read[Xml]).namespace
      . assert(_ == t"urn:x")

      test(m"A namespaced attribute is found by resolved name"):
        root(t"""<a xmlns:p="urn:p" p:b="c"/>""".read[Xml]).attribute(Xml.Name(t"urn:p", t"b"))
      . assert(_ == t"c")

      test(m"An unprefixed attribute is in no namespace"):
        root(t"""<a xmlns="urn:x" b="c"/>""".read[Xml]).attribute(Xml.Name(t"urn:x", t"b"))
      . assert(_ == Unset)

      test(m"An unprefixed attribute is found by its local name"):
        root(t"""<a xmlns="urn:x" b="c"/>""".read[Xml]).attribute(Xml.Name(t"b"))
      . assert(_ == t"c")

      test(m"A hand-built element resolves its own declarations"):
        Element(t"p:a", Attributes(t"xmlns:p" -> t"urn:p"), Array()).namespace
      . assert(_ == t"urn:p")

      test(m"A qualified name shows in Clark notation"):
        Xml.Name(t"urn:p", t"a").show
      . assert(_ == t"{urn:p}a")

    suite(m"Selection"):
      val doc =
        t"""<r xmlns:a="urn:a"><a:x>1</a:x><b:x xmlns:b="urn:b">2</b:x><x>3</x></r>""".read[Xml]

      test(m"A bare name selects by raw label"):
        doc.x().as[Int]
      . assert(_ == 3)

      test(m"A prefix declared in the document selects by resolved name"):
        doc.`a:x`().as[Int]
      . assert(_ == 1)

      test(m"A prefix bound by a Scope given selects by namespace, whatever the document's prefix"):
        given Xml.Scope = Xml.Scope(t"q" -> t"urn:b")
        doc.`q:x`().as[Int]
      . assert(_ == 2)

      test(m"elements selects by resolved name"):
        doc.elements(Xml.Name(t"urn:a", t"x")).as[Int]
      . assert(_ == 1)

      test(m"element selects the first by resolved name"):
        doc.element(Xml.Name(t"urn:b", t"x")).as[Int]
      . assert(_ == 2)

      test(m"A detached child keeps its default namespace"):
        child(t"""<r xmlns="urn:d"><s><t/></s></r>""".read[Xml].s()).namespace
      . assert(_ == t"urn:d")

    suite(m"Serialization"):
      test(m"A parsed document is written back unchanged"):
        val text = t"""<r xmlns:a="urn:a"><a:x>1</a:x></r>"""
        text.read[Xml].show
      . assert(_ == t"""<r xmlns:a="urn:a"><a:x>1</a:x></r>""")

      test(m"A scoped element built in code declares its namespace"):
        Element(t"p:a", Attributes.empty, Array(), Xml.Scope(t"p" -> t"urn:p")).show
      . assert(_ == t"""<p:a xmlns:p="urn:p"/>""")

      test(m"A child does not redeclare what its parent declared"):
        val scope = Xml.Scope(t"p" -> t"urn:p")
        val inner = Element(t"p:b", Attributes.empty, Array(), scope)
        Element(t"p:a", Attributes.empty, Array(inner), scope).show
      . assert(_ == t"""<p:a xmlns:p="urn:p"><p:b/></p:a>""")

      test(m"A default namespace in scope is declared"):
        Element(t"a", Attributes.empty, Array(), Xml.Scope(t"" -> t"urn:d")).show
      . assert(_ == t"""<a xmlns="urn:d"/>""")

      test(m"An element with no scope is written as it is"):
        Element(t"p:a", Attributes.empty, Array()).show
      . assert(_ == t"<p:a/>")

    suite(m"Literals"):
      test(m"A literal's prefix is bound by a Namespace given"):
        x"<svg:rect/>".show
      . assert(_ == t"""<svg:rect xmlns:svg="http://www.w3.org/2000/svg"/>""")

      test(m"A literal element resolves its namespace"):
        root(x"<svg:rect/>").namespace
      . assert(_ == t"http://www.w3.org/2000/svg")

      test(m"A literal declaring its prefix needs no given"):
        root(x"""<q:a xmlns:q="urn:q"/>""").namespace
      . assert(_ == t"urn:q")

      test(m"A literal's child inherits the bound prefix"):
        child(x"<svg:g><svg:rect/></svg:g>").namespace
      . assert(_ == t"http://www.w3.org/2000/svg")

      test(m"An unbound prefix in a literal does not compile"):
        demilitarize:
          x"<zz:a/>"
      . assert(_.nonEmpty)

      test(m"Lenient namespacing lets an unbound prefix through"):
        import namespaceOptions.lenientNamespaces
        root(x"<zz:a/>").label
      . assert(_ == t"zz:a")

    suite(m"Derivation"):
      test(m"An annotated type encodes in its namespace"):
        Order(1, Item(t"a", 2), t"n").in[Xml].show
      . assert(_ == t"""<Order xmlns="urn:shop"><id>1</id><item><sku>a</sku><quantity>2</quantity></item><note>n</note></Order>""")

      test(m"An annotated type decodes from its own encoding"):
        Order(1, Item(t"a", 2), t"n").in[Xml].show.read[Xml].as[Order]
      . assert(_ == Order(1, Item(t"a", 2), t"n"))

      test(m"An annotated type decodes from a prefixed document"):
        t"""<s:Order xmlns:s="urn:shop"><s:id>1</s:id><s:item><s:sku>a</s:sku><s:quantity>2</s:quantity></s:item><s:note>n</s:note></s:Order>"""
        . read[Xml].as[Order]
      . assert(_ == Order(1, Item(t"a", 2), t"n"))

      test(m"An annotated type ignores a same-named element in another namespace"):
        capture[Xml.Error]:
          t"""<Order xmlns="urn:shop"><id xmlns="urn:other">1</id><item><sku>a</sku><quantity>2</quantity></item><note>n</note></Order>"""
          . read[Xml].as[Order]
      . assert(_.reason == Xml.Error.Reason.Untextual(t"Int"))

      test(m"An unqualified field undeclares the namespace"):
        Ticket(1, t"r").in[Xml].show
      . assert(_ == t"""<Ticket xmlns="urn:shop"><id>1</id><reference xmlns="">r</reference></Ticket>""")

      test(m"An unqualified field decodes by raw label"):
        t"""<s:Ticket xmlns:s="urn:shop"><s:id>1</s:id><reference>r</reference></s:Ticket>"""
        . read[Xml].as[Ticket]
      . assert(_ == Ticket(1, t"r"))

      test(m"An unqualified type keeps its fields out of the namespace"):
        Receipt(1, 2).in[Xml].show
      . assert(_ == t"""<Receipt xmlns="urn:shop"><id xmlns="">1</id><total xmlns="">2</total></Receipt>""")

      test(m"A Namespaced given supplies a namespace for an unannotated type"):
        given Plain is Xml.Namespaced = Xml.Namespaced(t"urn:plain")
        Plain(3).in[Xml].show
      . assert(_ == t"""<Plain xmlns="urn:plain"><id>3</id></Plain>""")

      test(m"A prefixed variant label selects the variant"):
        t"""<s:Item xmlns:s="urn:shop"><sku>a</sku><quantity>2</quantity></s:Item>""".read[Xml]
        . as[Item]
      . assert(_ == Item(t"a", 2))

    suite(m"XPath"):
      given k: ("k" is Namespace of "urn:a") = Namespace()

      val doc =
        t"""<r xmlns:a="urn:a"><a:x>1</a:x><b:x xmlns:b="urn:a">2</b:x><x>3</x></r>""".read[Xml]

      test(m"A prefixed name test matches by namespace when the prefix is bound"):
        doc.select(xp"//k:x").nodes.length
      . assert(_ == 2)

      test(m"An unbound prefixed name test matches the raw label"):
        doc.select(xp"//b:x").nodes.length
      . assert(_ == 1)

      test(m"namespace-uri() reports the resolved namespace"):
        doc.evaluate(xp"namespace-uri(/r/*[1])").text
      . assert(_ == t"urn:a")

      test(m"namespace-uri() is empty for an element in no namespace"):
        doc.evaluate(xp"namespace-uri(/r/x)").text
      . assert(_ == t"")

      test(m"in rebinds a path's prefixes"):
        doc.select(xp"//z:x".in(Xml.Scope(t"z" -> t"urn:a"))).nodes.length
      . assert(_ == 2)

      test(m"A prefixed attribute test matches by namespace"):
        val attributed = t"""<r xmlns:a="urn:a" a:id="1" id="2"/>""".read[Xml]
        attributed.selectText(xp"/r/@k:id")
      . assert(_ == t"1")
