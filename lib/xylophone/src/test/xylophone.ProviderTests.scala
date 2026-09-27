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

import charsets.utf8Charset
import classloaders.threadContextClassloader
import errorDiagnostics.stackTracesDiagnostics
import strategies.throwUnsafely
import textSanitizers.skipSanitizer

object ProviderTests extends Suite(m"Xylophone type provider tests"):
  given XmlSchema = XmlSchema.Freeform

  import Xml.Provider.Error.Reason.*

  def run(): Unit =
    suite(m"The purchase order"):
      val order = cp"/xsd/po.xml".read[Xml]
      val record = PurchaseOrder.record(order)

      test(m"A nested element reads as text"):
        record.shipTo.name
      . assert(_ == t"Alice Smith")

      test(m"A decimal reads as a Double"):
        record.billTo.zip
      . assert(_ == 95819.0)

      test(m"An optional element that is present"):
        record.comment
      . assert(_ == t"Hurry, my lawn is going wild!")

      test(m"A repeated element reads as a list"):
        record.items.item.map(_.productName)
      . assert(_ == List(t"Lawnmower", t"Baby Monitor"))

      test(m"A restricted positive integer reads as a Long"):
        record.items.item.map(_.quantity)
      . assert(_ == List(1L, 1L))

      test(m"A required attribute reads as text"):
        record.items.item.map(_.partNum)
      . assert(_ == List(t"872-AA", t"926-AA"))

      test(m"An optional element that is absent"):
        record.items.item.map(_.shipDate)
      . assert(_ == List(Unset, t"1999-05-21"))

      test(m"An optional attribute reads as text without a date interface"):
        record.orderDate
      . assert(_ == t"1999-10-20")

      test(m"A fixed attribute is optional"):
        record.shipTo.country
      . assert(_ == t"US")

      test(m"A pattern facet is checked"):
        val bad = t"""<purchaseOrder><shipTo><name>a</name><street>b</street><city>c</city><state>d</state><zip>1</zip></shipTo><billTo><name>a</name><street>b</street><city>c</city><state>d</state><zip>1</zip></billTo><items><item partNum="nope"><productName>p</productName><quantity>1</quantity><USPrice>1</USPrice></item></items></purchaseOrder>"""
        capture[Xml.Provider.Error](PurchaseOrder.record(bad.read[Xml]).items.item.map(_.partNum)).reason
      . assert(_ == PatternMismatch(t"nope", t"\\d{3}-[A-Z]{2}"))

      test(m"An exclusive maximum is checked"):
        val bad = t"""<purchaseOrder><shipTo><name>a</name><street>b</street><city>c</city><state>d</state><zip>1</zip></shipTo><billTo><name>a</name><street>b</street><city>c</city><state>d</state><zip>1</zip></billTo><items><item partNum="123-AB"><productName>p</productName><quantity>100</quantity><USPrice>1</USPrice></item></items></purchaseOrder>"""
        capture[Xml.Provider.Error](PurchaseOrder.record(bad.read[Xml]).items.item.map(_.quantity)).reason
      . assert(_ == OutOfRange(t"100", t"1", t"100"))

      test(m"A missing required element is reported"):
        val bad = t"""<purchaseOrder><shipTo><name>a</name></shipTo></purchaseOrder>"""
        capture[Xml.Provider.Error](PurchaseOrder.record(bad.read[Xml]).shipTo.city).reason
      . assert(_ == Absent(t"city"))

      test(m"The tuple form reads eagerly"):
        PurchaseOrder.tuple(order).shipTo.state
      . assert(_ == t"CA")

    suite(m"A namespaced document"):
      val order = cp"/xsd/shiporder.xml".read[Xml]
      val record = ShipOrder.record(order)

      test(m"Qualified elements are matched by namespace, whatever the prefix"):
        record.shipto.city
      . assert(_ == t"4000 Stavanger")

      test(m"An attribute with a pattern reads as text"):
        record.orderid
      . assert(_ == t"889923")

      test(m"An enumerated attribute with a default is optional"):
        record.priority
      . assert(_ == t"high")

      test(m"Repeated qualified elements read as a list"):
        record.item.map(_.title)
      . assert(_ == List(t"Empire Burlesque", t"Hide your heart"))

      test(m"An optional child in a repeated element"):
        record.item.map(_.note)
      . assert(_ == List(t"Special Edition", Unset))

      test(m"An element in the wrong namespace is not matched"):
        val other = t"""<sh:shiporder xmlns:sh="urn:example:shipping" orderid="889923"><orderperson>Nobody</orderperson></sh:shiporder>"""
        capture[Xml.Provider.Error](ShipOrder.record(other.read[Xml]).orderperson).reason
      . assert(_ == Absent(t"orderperson"))

      test(m"An enumeration is checked"):
        val bad = t"""<shiporder xmlns="urn:example:shipping" orderid="889923" priority="urgent"/>"""
        capture[Xml.Provider.Error](ShipOrder.record(bad.read[Xml]).priority).reason
      . assert(_ == NotPermitted(t"urgent", List(t"low", t"normal", t"high")))

    suite(m"Published schemas"):
      test(m"A Maven POM reads its coordinates"):
        val pom = t"""<project xmlns="http://maven.apache.org/POM/4.0.0"><modelVersion>4.0.0</modelVersion><groupId>com.example</groupId><artifactId>demo</artifactId><version>1.0</version><dependencies><dependency><groupId>org.x</groupId><artifactId>y</artifactId><version>2</version></dependency></dependencies></project>"""
        val record = MavenProject.record(pom.read[Xml])
        (record.artifactId, record.dependencies.let(_.dependency.map(_.artifactId)))
      . assert(_ == (t"demo", List(t"y")))

      test(m"A GPX waypoint's latitude is a bounded decimal"):
        val gpx = t"""<gpx xmlns="http://www.topografix.com/GPX/1/1" version="1.1" creator="test"><wpt lat="51.5" lon="-0.1"><name>London</name></wpt></gpx>"""
        Gpx.record(gpx.read[Xml]).wpt.map(_.lat)
      . assert(_ == List(51.5))

      test(m"A GPX latitude outside its range is rejected"):
        val gpx = t"""<gpx xmlns="http://www.topografix.com/GPX/1/1" version="1.1" creator="test"><wpt lat="95" lon="0"/></gpx>"""
        capture[Xml.Provider.Error](Gpx.record(gpx.read[Xml]).wpt.map(_.lat)).reason
      . assert(_ == OutOfRange(t"95", t"-90.0", t"90.0"))

      test(m"A nuspec package reads its metadata"):
        val nuspec = t"""<package xmlns="http://schemas.microsoft.com/packaging/2013/05/nuspec.xsd"><metadata><id>Demo</id><version>1.2.3</version><authors>Someone</authors><description>A demo</description></metadata></package>"""
        Nuspec.record(nuspec.read[Xml]).metadata.id
      . assert(_ == t"Demo")

      test(m"A SOAP envelope's body is raw XML"):
        val envelope = t"""<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"><soap:Body><ping/></soap:Body></soap:Envelope>"""
        val record = SoapEnvelope.record(envelope.read[Xml])
        (record.Header, record.Body.show)
      . assert(_ == (Unset, t"""<soap:Body xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"><ping/></soap:Body>"""))

      test(m"Spring beans read their ids"):
        val beans = t"""<beans xmlns="http://www.springframework.org/schema/beans"><bean id="a" class="A"/><bean id="b" class="B"><property name="x"><bean class="X"/></property></bean></beans>"""
        SpringBeans.record(beans.read[Xml]).bean.map(_.id)
      . assert(_ == List(t"a", t"b"))

      test(m"An XLIFF file reads its version"):
        val xliff = t"""<xliff xmlns="urn:oasis:names:tc:xliff:document:2.0" version="2.0" srcLang="en"><file id="f1"><unit id="u1"><segment><source>Hello</source></segment></unit></file></xliff>"""
        Xliff.record(xliff.read[Xml]).version
      . assert(_ == t"2.0")

    suite(m"Features"):
      test(m"Each alternative of a choice is optional"):
        val record = Choices.record(t"<shape><square>2.0</square></shape>".read[Xml])
        (record.circle, record.square)
      . assert(_ == (Unset, 2.0))

      test(m"An attribute clashing with a child element is reached with @"):
        val record = Clashes.record(t"""<entry id="x"><id>1</id><text>t</text></entry>""".read[Xml])
        (record.id, record.`@id`, record.text)
      . assert(_ == (1, t"x", t"t"))

      test(m"Simple content with attributes has a text field"):
        val record = Prices.record(t"""<price currency="USD" text="note">1.5</price>""".read[Xml])
        (record.`#text`, record.currency, record.text)
      . assert(_ == (1.5, t"USD", t"note"))

      test(m"A bounded integer within range"):
        Facets.record(t"<person><age>42</age></person>".read[Xml]).age
      . assert(_ == 42)

      test(m"A bounded integer out of range is rejected"):
        capture[Xml.Provider.Error](Facets.record(t"<person><age>200</age></person>".read[Xml]).age).reason
      . assert(_ == OutOfRange(t"200", t"0", t"150"))

      test(m"A length facet is checked"):
        capture[Xml.Provider.Error](Facets.record(t"<person><code>abcd</code></person>".read[Xml]).code).reason
      . assert(_ == LengthOutOfRange(t"abcd", t"3", t"3"))

      test(m"A pattern facet on a token is checked"):
        capture[Xml.Provider.Error](Facets.record(t"<person><tag>x</tag></person>".read[Xml]).tag).reason
      . assert(_ == PatternMismatch(t"x", t"[a-z]+-[0-9]+"))

      test(m"A pattern facet on a token accepts a match"):
        Facets.record(t"<person><tag>ab-12</tag></person>".read[Xml]).tag
      . assert(_ == t"ab-12")

      test(m"A named enumeration type is checked"):
        capture[Xml.Provider.Error](Facets.record(t"<person><mode>medium</mode></person>".read[Xml]).mode).reason
      . assert(_ == NotPermitted(t"medium", List(t"fast", t"slow")))

      test(m"Digit facets are checked"):
        capture[Xml.Provider.Error](Facets.record(t"<person><amount>1.234</amount></person>".read[Xml]).amount).reason
      . assert(_ == TooManyDigits(t"1.234", t"5", t"2"))

      test(m"Digit facets accept a valid decimal"):
        Facets.record(t"<person><amount>123.45</amount></person>".read[Xml]).amount
      . assert(_ == 123.45)

      test(m"An unsigned byte has implied bounds"):
        capture[Xml.Provider.Error](Facets.record(t"<person><count>300</count></person>".read[Xml]).count).reason
      . assert(_ == OutOfRange(t"300", t"0", t"255"))

      test(m"NMTOKENS read as a list of tokens"):
        Lists.record(t"<record><tags> a  b c </tags></record>".read[Xml]).tags
      . assert(_ == List(t"a", t"b", t"c"))

      test(m"A repeated list type reads as a list of lists"):
        Lists.record(t"<record><numbers>1 2</numbers><numbers>3</numbers></record>".read[Xml]).numbers
      . assert(_ == List(List(t"1", t"2"), List(t"3")))

      test(m"A nillable element with xsi:nil is absent"):
        val document = t"""<record xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"><middle xsi:nil="true"/></record>"""
        Lists.record(document.read[Xml]).middle
      . assert(_ == Unset)

      test(m"A nillable element with content is present"):
        Lists.record(t"<record><middle>m</middle></record>".read[Xml]).middle
      . assert(_ == t"m")

      test(m"A recursive type reads its recursion as raw XML"):
        val record = Trees.record(t"<tree><label>root</label><child><label>leaf</label></child></tree>".read[Xml])
        (record.label, record.child.map(_.show))
      . assert(_ == (t"root", List(t"<child><label>leaf</label></child>")))

      test(m"A schema in the default XSD namespace is read"):
        val record = DefaultPrefixed.record(t"<root><value>v</value><flag>true</flag></root>".read[Xml])
        (record.value, record.flag)
      . assert(_ == (t"v", true))

      test(m"Unqualified local elements are matched in no namespace"):
        val document = t"""<t:root xmlns:t="urn:t" kind="k"><value>7</value></t:root>"""
        val record = Unqualified.record(document.read[Xml])
        (record.value, record.kind)
      . assert(_ == (7, t"k"))

      test(m"An extension inherits its base's fields, group and attribute group"):
        val document = t"""<thing id="i" extra="true"><name>n</name><a>1</a><b>2</b></thing>"""
        val record = Extensions.record(document.read[Xml])
        (record.id, record.name, record.a, record.b, record.extra)
      . assert(_ == (t"i", t"n", 1, 2, true))

      test(m"A chosen root among several global elements"):
        Chosen.record(t"<two><y>9</y></two>".read[Xml]).y
      . assert(_ == 9)

