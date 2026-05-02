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

import scala.quoted.*

import ambience.*, environments.java, systems.java
import anticipation.*
import contingency.*, strategies.throwUnsafely
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContext
import probably.*
import proscenium.*
import quantitative.*
import sedentary.*
import symbolism.*
import temporaryDirectories.system
import turbulence.*
import vacuous.*

object Benchmarks extends Suite(m"Xylophone benchmarks"):
  sealed trait Information extends Dimension
  sealed trait Bytes[Power <: Nat] extends Units[Power, Information]
  val Byte: MetricUnit[Bytes[1]] = MetricUnit(1.0)

  given byteDesignation: Designation[Bytes[1]] = () => t"B"
  given decimalizer:     Decimalizer            = Decimalizer(2)
  given device:          BenchmarkDevice        = LocalhostDevice
  given schema:          XmlSchema              = XmlSchema.Freeform

  // Auto-scale byte sizes (B → kB → MB → GB → TB) and byte rates so the
  // table prints "1.3 MB·s¯¹" instead of "1.3×10⁶ B·s¯¹".
  given prefixes: Prefixes = Prefixes(List(Kilo, Mega, Giga, Tera))

  def parseXylophone(text: Text): Document[Xml] = unsafely(text.load[Xml])
  def parseXylophoneDirect(text: Text): Xml = unsafely(Xml.parseDirect(text, headers0 = true))

  def parseScalaXml(text: String): scala.xml.Elem = scala.xml.XML.loadString(text)

  def run(): Unit =
    val bench = Bench()

    val size1 = xmlText1.getBytes("UTF-8").nn.length*Byte
    val size2 = xmlText2.getBytes("UTF-8").nn.length*Byte
    val size3 = xmlText3.getBytes("UTF-8").nn.length*Byte
    val size4 = xmlText4.getBytes("UTF-8").nn.length*Byte
    val size5 = xmlText5.getBytes("UTF-8").nn.length*Byte

    suite(m"Parse example 1 (RSS feed)"):
      bench(m"Parse file with Xylophone")
        (target = 1*Second, operationSize = size1, baseline = Baseline(compare = Min)):
        '{ xylophone.Benchmarks.parseXylophone(xylophone.Benchmarks.xml1) }

      bench(m"Parse file with Xylophone Direct")(target = 1*Second, operationSize = size1):
        '{ xylophone.Benchmarks.parseXylophoneDirect(xylophone.Benchmarks.xml1) }

      bench(m"Parse file with scala-xml")(target = 1*Second, operationSize = size1):
        '{ xylophone.Benchmarks.parseScalaXml(xylophone.Benchmarks.xmlText1) }

    suite(m"Parse example 2 (SOAP envelope)"):
      bench(m"Parse file with Xylophone")
        (target = 1*Second, operationSize = size2, baseline = Baseline(compare = Min)):
        '{ xylophone.Benchmarks.parseXylophone(xylophone.Benchmarks.xml2) }

      bench(m"Parse file with Xylophone Direct")(target = 1*Second, operationSize = size2):
        '{ xylophone.Benchmarks.parseXylophoneDirect(xylophone.Benchmarks.xml2) }

      bench(m"Parse file with scala-xml")(target = 1*Second, operationSize = size2):
        '{ xylophone.Benchmarks.parseScalaXml(xylophone.Benchmarks.xmlText2) }

    suite(m"Parse example 3 (Atom feed)"):
      bench(m"Parse file with Xylophone")
        (target = 1*Second, operationSize = size3, baseline = Baseline(compare = Min)):
        '{ xylophone.Benchmarks.parseXylophone(xylophone.Benchmarks.xml3) }

      bench(m"Parse file with Xylophone Direct")(target = 1*Second, operationSize = size3):
        '{ xylophone.Benchmarks.parseXylophoneDirect(xylophone.Benchmarks.xml3) }

      bench(m"Parse file with scala-xml")(target = 1*Second, operationSize = size3):
        '{ xylophone.Benchmarks.parseScalaXml(xylophone.Benchmarks.xmlText3) }

    suite(m"Parse example 4 (100 book records)"):
      bench(m"Parse file with Xylophone")
        (target = 1*Second, operationSize = size4, baseline = Baseline(compare = Min)):
        '{ xylophone.Benchmarks.parseXylophone(xylophone.Benchmarks.xml4) }

      bench(m"Parse file with Xylophone Direct")(target = 1*Second, operationSize = size4):
        '{ xylophone.Benchmarks.parseXylophoneDirect(xylophone.Benchmarks.xml4) }

      bench(m"Parse file with scala-xml")(target = 1*Second, operationSize = size4):
        '{ xylophone.Benchmarks.parseScalaXml(xylophone.Benchmarks.xmlText4) }

    suite(m"Parse example 5 (500 log entries)"):
      bench(m"Parse file with Xylophone")
        (target = 1*Second, operationSize = size5, baseline = Baseline(compare = Min)):
        '{ xylophone.Benchmarks.parseXylophone(xylophone.Benchmarks.xml5) }

      bench(m"Parse file with Xylophone Direct")(target = 1*Second, operationSize = size5):
        '{ xylophone.Benchmarks.parseXylophoneDirect(xylophone.Benchmarks.xml5) }

      bench(m"Parse file with scala-xml")(target = 1*Second, operationSize = size5):
        '{ xylophone.Benchmarks.parseScalaXml(xylophone.Benchmarks.xmlText5) }

  lazy val xmlText1: String = xmlExample1.s
  lazy val xmlText2: String = xmlExample2.s
  lazy val xmlText3: String = xmlExample3.s
  lazy val xml1: Text = xmlExample1
  lazy val xml2: Text = xmlExample2
  lazy val xml3: Text = xmlExample3

  // Example 4: a catalogue of 100 book records — the typical "list of
  // records" pattern with a small fixed set of child elements per record.
  lazy val xmlText4: String =
    val sb = new _root_.java.lang.StringBuilder
    sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
    sb.append("<catalog>")
    var i = 0
    while i < 100 do
      sb.append("<book id=\"b")
      sb.append(i)
      sb.append("\"><title>Book Number ")
      sb.append(i)
      sb.append("</title><author>Author ")
      sb.append(i % 20)
      sb.append("</author><year>")
      sb.append(1950 + (i % 75))
      sb.append("</year><price currency=\"USD\">")
      sb.append(10 + (i % 90))
      sb.append(".95</price><stock>")
      sb.append(i*3)
      sb.append("</stock></book>")
      i += 1
    sb.append("</catalog>")
    sb.toString

  lazy val xml4: Text = xmlText4.tt

  // Example 5: a longer log document with 500 entries; 6 elements per entry,
  // all repeating across entries. Stresses the per-element tokenization path
  // more heavily than Example 4.
  lazy val xmlText5: String =
    val sb = new _root_.java.lang.StringBuilder
    sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
    sb.append("<logs>")
    val levels = Array("info", "debug", "warn", "error")
    val services = Array("auth", "api", "db", "cache", "worker")
    var i = 0
    while i < 500 do
      val ts = 1700000000L + i
      val level = levels(i & 3)
      val service = services(i % 5)
      val userId = 1000 + (i % 50)
      sb.append("<entry><timestamp>")
      sb.append(ts)
      sb.append("</timestamp><level>")
      sb.append(level)
      sb.append("</level><service>")
      sb.append(service)
      sb.append("</service><requestId>req-")
      sb.append(i)
      sb.append("</requestId><userId>")
      sb.append(userId)
      sb.append("</userId><message>event ")
      sb.append(i)
      sb.append(" processed</message></entry>")
      i += 1
    sb.append("</logs>")
    sb.toString

  lazy val xml5: Text = xmlText5.tt

  // Example 1: a small, self-contained RSS 2.0 feed with two items.
  val xmlExample1: Text = t"""<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0">
  <channel>
    <title>Example Channel</title>
    <link>http://example.com/</link>
    <description>A short example feed for parser benchmarking.</description>
    <language>en-gb</language>
    <pubDate>Mon, 12 Sep 2024 09:00:00 GMT</pubDate>
    <item>
      <title>First post</title>
      <link>http://example.com/posts/1</link>
      <description>The first post on this example feed.</description>
      <pubDate>Mon, 12 Sep 2024 09:00:00 GMT</pubDate>
      <guid isPermaLink="true">http://example.com/posts/1</guid>
    </item>
    <item>
      <title>Second post</title>
      <link>http://example.com/posts/2</link>
      <description>The second post on this example feed.</description>
      <pubDate>Tue, 13 Sep 2024 14:30:00 GMT</pubDate>
      <guid isPermaLink="true">http://example.com/posts/2</guid>
    </item>
  </channel>
</rss>
"""

  // Example 2: a SOAP 1.1 request envelope with a moderately nested body.
  val xmlExample2: Text = t"""<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"
               xmlns:ord="http://example.com/orders">
  <soap:Header>
    <ord:Auth>
      <ord:User>alice</ord:User>
      <ord:Token>a1b2c3d4e5f60718293a4b5c6d7e8f90</ord:Token>
    </ord:Auth>
  </soap:Header>
  <soap:Body>
    <ord:PlaceOrder>
      <ord:Customer id="c-4582">
        <ord:Name>Alice Example</ord:Name>
        <ord:Email>alice@example.com</ord:Email>
        <ord:Address>
          <ord:Street>10 Downing Street</ord:Street>
          <ord:City>London</ord:City>
          <ord:Postcode>SW1A 2AA</ord:Postcode>
          <ord:Country>UK</ord:Country>
        </ord:Address>
      </ord:Customer>
      <ord:Items>
        <ord:Item sku="W-100" quantity="2">
          <ord:Description>Standard widget</ord:Description>
          <ord:UnitPrice currency="GBP">12.50</ord:UnitPrice>
        </ord:Item>
        <ord:Item sku="W-200" quantity="1">
          <ord:Description>Deluxe sprocket</ord:Description>
          <ord:UnitPrice currency="GBP">42.00</ord:UnitPrice>
        </ord:Item>
        <ord:Item sku="A-007" quantity="3">
          <ord:Description>Assorted novelties</ord:Description>
          <ord:UnitPrice currency="GBP">5.25</ord:UnitPrice>
        </ord:Item>
      </ord:Items>
      <ord:Shipping method="standard"/>
      <ord:Notes>Please leave with the porter if no answer.</ord:Notes>
    </ord:PlaceOrder>
  </soap:Body>
</soap:Envelope>
"""

  // Example 3: an Atom 1.0 feed with several entries — exercises text-heavy
  // content and a richer mix of attributes and elements.
  val xmlExample3: Text = t"""<?xml version="1.0" encoding="UTF-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">
  <title>Example Atom Feed</title>
  <link href="http://example.org/" rel="alternate" type="text/html"/>
  <link href="http://example.org/feed" rel="self"/>
  <id>urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6</id>
  <updated>2024-09-12T09:00:00Z</updated>
  <author>
    <name>Jane Doe</name>
    <email>jane@example.org</email>
    <uri>http://example.org/~jane</uri>
  </author>
  <generator uri="http://example.org/feed-tool" version="1.4.2">FeedTool</generator>
  <rights>Copyright (c) 2024 Example Industries</rights>
  <subtitle>An example Atom feed used for parser benchmarking.</subtitle>
  <entry>
    <title>On the curious case of XML namespaces</title>
    <link href="http://example.org/posts/xml-namespaces" rel="alternate"/>
    <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
    <updated>2024-09-12T09:00:00Z</updated>
    <published>2024-09-11T18:30:00Z</published>
    <category term="parsing" label="Parsing"/>
    <category term="xml" label="XML"/>
    <summary>A brief look at namespace handling in XML parsers.</summary>
    <content type="html">
      &lt;p&gt;XML namespaces are a thorny corner of the spec. They are
      &lt;em&gt;essential&lt;/em&gt; in some contexts (notably SOAP) and
      essentially ignored in others (most data interchange).&lt;/p&gt;
      &lt;p&gt;This post sketches the rules and gives some examples.&lt;/p&gt;
    </content>
  </entry>
  <entry>
    <title>Streaming vs tree parsers</title>
    <link href="http://example.org/posts/streaming-vs-tree" rel="alternate"/>
    <id>urn:uuid:5a8b2c1d-3e4f-5061-7a2b-3c4d5e6f7081</id>
    <updated>2024-09-13T10:15:00Z</updated>
    <published>2024-09-13T10:15:00Z</published>
    <category term="parsing"/>
    <category term="performance"/>
    <summary>When to use a SAX-style parser and when to build a tree.</summary>
    <content type="text">
      Tree parsers are easier to use but materialise the entire document
      in memory; streaming parsers are harder to use but scale to inputs
      that don't fit in memory.
    </content>
  </entry>
  <entry>
    <title>A field guide to XML entities</title>
    <link href="http://example.org/posts/xml-entities" rel="alternate"/>
    <id>urn:uuid:9f8e7d6c-5b4a-3210-9876-543210fedcba</id>
    <updated>2024-09-15T08:00:00Z</updated>
    <published>2024-09-14T22:45:00Z</published>
    <category term="parsing"/>
    <category term="xml"/>
    <summary>The five built-in entities and the perils of the rest.</summary>
    <content type="text">
      Every XML parser handles &amp;amp;, &amp;lt;, &amp;gt;, &amp;apos; and
      &amp;quot;. Beyond that, it gets complicated.
    </content>
  </entry>
  <entry>
    <title>CDATA sections in practice</title>
    <link href="http://example.org/posts/cdata"/>
    <id>urn:uuid:11112222-3333-4444-5555-666677778888</id>
    <updated>2024-09-16T12:00:00Z</updated>
    <published>2024-09-16T12:00:00Z</published>
    <category term="xml"/>
    <summary>When CDATA helps and when it just confuses everyone.</summary>
    <content type="text">A short, opinionated overview.</content>
  </entry>
</feed>
"""
