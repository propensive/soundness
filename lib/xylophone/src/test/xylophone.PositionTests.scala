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
┃    Soundness, version 0.63.0.                                                                    ┃
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

object PositionTests extends Suite(m"Xylophone position-index tests"):

  private def line(p: Optional[Xml.Position]): Int = p.vouch.line.n1
  private def col(p: Optional[Xml.Position]): Int = p.vouch.column.n1
  private def len(p: Optional[Xml.Position]): Int = p.vouch.length.vouch

  def run(): Unit =
    given XmlSchema = XmlSchema.Freeform

    suite(m"Single-element parseTracked"):
      test(m"Root element line"):
        val tracked = Xml.parseTracked(t"<root/>")
        line(tracked.locate(XPath().element(t"root", 1)))
      . assert(_ == 1)

      test(m"Root element column"):
        val tracked = Xml.parseTracked(t"<root/>")
        col(tracked.locate(XPath().element(t"root", 1)))
      . assert(_ == 1)

      test(m"Root element source length spans the open / close"):
        val tracked = Xml.parseTracked(t"<root>hello</root>")
        len(tracked.locate(XPath().element(t"root", 1)))
      . assert(_ == 18)

    suite(m"Nested elements"):
      test(m"Single nested child line"):
        val tracked = Xml.parseTracked(t"<root><child/></root>")
        line(tracked.locate(XPath().element(t"root", 1).element(t"child", 1)))
      . assert(_ == 1)

      test(m"Single nested child column"):
        val tracked = Xml.parseTracked(t"<root><child/></root>")
        col(tracked.locate(XPath().element(t"root", 1).element(t"child", 1)))
      . assert(_ == 7)

      test(m"Second child with same name uses [2]"):
        val tracked = Xml.parseTracked(t"<root><x/><x/></root>")
        col(tracked.locate(XPath().element(t"root", 1).element(t"x", 2)))
      . assert(_ == 11)

      test(m"Missing child returns Unset"):
        val tracked = Xml.parseTracked(t"<root><a/></root>")
        tracked.locate(XPath().element(t"root", 1).element(t"missing", 1))
      . assert(_ == Unset)

    suite(m"Leading XML declaration"):
      test(m"Declaration does not prevent tracked parsing"):
        val tracked = Xml.parseTracked(t"""<?xml version="1.0"?><root/>""")
        tracked.locate(XPath().element(t"root", 1)) != Unset
      . assert(_ == true)

      test(m"Root column accounts for the declaration prefix"):
        val tracked = Xml.parseTracked(t"""<?xml version="1.0"?><root/>""")
        col(tracked.locate(XPath().element(t"root", 1)))
      . assert(_ == 22)

      test(m"Declaration on its own line, root on the next"):
        val tracked = Xml.parseTracked(t"<?xml version=\"1.0\"?>\n<root>hello</root>")
        line(tracked.locate(XPath().element(t"root", 1)))
      . assert(_ == 2)

    suite(m"Attributes"):
      test(m"Single attribute column"):
        val tracked = Xml.parseTracked(t"""<root name="x"/>""")
        col(tracked.locate(XPath().element(t"root", 1).attribute(t"name")))
      . assert(_ == 7)

      test(m"Attribute length spans name='value'"):
        val tracked = Xml.parseTracked(t"""<root name="alice"/>""")
        len(tracked.locate(XPath().element(t"root", 1).attribute(t"name")))
      . assert(_ == 12)

      test(m"Second attribute column"):
        val tracked = Xml.parseTracked(t"""<root a="1" b="2"/>""")
        col(tracked.locate(XPath().element(t"root", 1).attribute(t"b")))
      . assert(_ == 13)

      test(m"Missing attribute returns Unset"):
        val tracked = Xml.parseTracked(t"""<root a="1"/>""")
        tracked.locate(XPath().element(t"root", 1).attribute(t"missing"))
      . assert(_ == Unset)

    suite(m"Multi-line input"):
      test(m"Second-line child is on line 2"):
        val source = t"<root>\n  <child/>\n</root>"
        val tracked = Xml.parseTracked(source)
        line(tracked.locate(XPath().element(t"root", 1).element(t"child", 1)))
      . assert(_ == 2)

      test(m"Second-line child column reflects indent"):
        val source = t"<root>\n  <child/>\n</root>"
        val tracked = Xml.parseTracked(source)
        col(tracked.locate(XPath().element(t"root", 1).element(t"child", 1)))
      . assert(_ == 3)

    suite(m"Mixed content"):
      test(m"Text between elements doesn't break child indexing"):
        val tracked = Xml.parseTracked(t"<root>hi<a/>bye<b/>!</root>")
        col(tracked.locate(XPath().element(t"root", 1).element(t"b", 1)))
      . assert(_ == 16)

      test(m"Comment between elements doesn't break child indexing"):
        val tracked = Xml.parseTracked(t"<root><a/><!-- x --><b/></root>")
        col(tracked.locate(XPath().element(t"root", 1).element(t"b", 1)))
      . assert(_ == 21)
