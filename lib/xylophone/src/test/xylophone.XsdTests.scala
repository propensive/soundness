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

import charDecoders.utf8Decoder
import classloaders.threadContextClassloader
import errorDiagnostics.stackTracesDiagnostics
import strategies.throwUnsafely
import textSanitizers.skipSanitizer

object XsdTests extends Suite(m"Xylophone XML Schema tests"):
  given XmlSchema = XmlSchema.Freeform

  def schema(resource: Resource): Xsd = Xsd.parse(resource.read[Text])

  def run(): Unit =
    suite(m"Real-world schemas parse"):
      test(m"The purchase order schema has its global elements and types"):
        val xsd = schema(cp"/xsd/po.xsd")
        (Map.keys(xsd.elements).stdlib, Map.keys(xsd.complexTypes).stdlib, Map.keys(xsd.simpleTypes).stdlib)
      . assert(_ == (Set(t"purchaseOrder", t"comment"), Set(t"PurchaseOrderType", t"USAddress", t"Items"), Set(t"SKU")))

      test(m"The shipping order schema resolves its default-namespace type names"):
        Map.keys(schema(cp"/xsd/shiporder.xsd").complexTypes).stdlib
      . assert(_ == Set(t"shiptotype", t"itemtype", t"shipordertype"))

      test(m"The purchase order schema has no target namespace"):
        schema(cp"/xsd/po.xsd").targetNamespace
      . assert(_ == Unset)

      test(m"The Maven POM schema declares its target namespace"):
        schema(cp"/xsd/maven-4.0.0.xsd").targetNamespace
      . assert(_ == t"http://maven.apache.org/POM/4.0.0")

      test(m"The Maven POM schema has many complex types"):
        Map.keys(schema(cp"/xsd/maven-4.0.0.xsd").complexTypes).stdlib.size
      . assert(_ > 30)

      test(m"GPX declares a restricted latitude type"):
        schema(cp"/xsd/gpx.xsd").simpleTypes.at(t"latitudeType").let(_.definition)
      . assert:
          case Xsd.SimpleType.Restriction(Xsd.TypeRef.Named(base), facets) =>
            base == Xml.Name(Xsd.namespace, t"decimal") && facets.stdlib.length == 2
          case _ => false

      test(m"The xml namespace schema declares global attributes and an attribute group"):
        val xsd = schema(cp"/xsd/xml.xsd")
        (Map.keys(xsd.attributes).stdlib, Map.keys(xsd.attributeGroups).stdlib)
      . assert(_ == (Set(t"lang", t"space", t"base", t"id"), Set(t"specialAttrs")))

      test(m"The SOAP envelope schema declares its four elements"):
        Map.keys(schema(cp"/xsd/soap-envelope.xsd").elements).stdlib
      . assert(_ == Set(t"Envelope", t"Header", t"Body", t"Fault"))

      test(m"The nuspec schema is qualified"):
        schema(cp"/xsd/nuspec.xsd").elementFormDefault
      . assert(_ == Xsd.Form.Qualified)

      test(m"The Spring beans schema declares groups"):
        Map.keys(schema(cp"/xsd/spring-beans.xsd").groups).stdlib.contains(t"beanElements")
      . assert(_ == true)

      test(m"The XLIFF schema records its imports"):
        schema(cp"/xsd/xliff-core-2.0.xsd").imports.stdlib.map(_.namespace)
      . assert(_.contains(Optional(t"http://www.w3.org/XML/1998/namespace")))

      test(m"Documentation is kept"):
        schema(cp"/xsd/po.xsd").documentation.stdlib.headOption.map(_.trim.s.take(21))
      . assert(_ == Some("Purchase order schema"))

    suite(m"Errors"):
      test(m"A document that is not a schema is rejected"):
        capture[Xsd.Error](Xsd.parse(t"<root/>".read[Xml])).reason
      . assert(_ == Xsd.Error.Reason.NotSchema(Xml.Name(t"root")))

      test(m"An unknown child of the schema is rejected"):
        capture[Xsd.Error]:
          Xsd.parse(t"""<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"><xs:oops/></xs:schema>""".read[Xml])
        . reason
      . assert(_ == Xsd.Error.Reason.Unexpected(Xml.Name(Xsd.namespace, t"oops"), t"schema"))

      test(m"A bad occurrence count is rejected"):
        capture[Xsd.Error]:
          Xsd.parse:
            t"""<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"><xs:element name="a"><xs:complexType><xs:sequence><xs:element name="b" type="xs:string" maxOccurs="lots"/></xs:sequence></xs:complexType></xs:element></xs:schema>"""
            . read[Xml]
        . reason
      . assert(_ == Xsd.Error.Reason.BadOccurs(t"lots"))

      test(m"A type with an unbound prefix is rejected"):
        import namespaceOptions.lenientNamespaces
        capture[Xsd.Error]:
          Xsd.parse:
            t"""<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"><xs:element name="a" type="zz:thing"/></xs:schema>"""
            . read[Xml]
        . reason
      . assert(_ == Xsd.Error.Reason.UnknownPrefix(t"zz"))

      test(m"A duplicate global element is rejected"):
        capture[Xsd.Error]:
          Xsd.parse:
            t"""<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"><xs:element name="a" type="xs:string"/><xs:element name="a" type="xs:int"/></xs:schema>"""
            . read[Xml]
        . reason
      . assert(_ == Xsd.Error.Reason.Duplicate(t"element", t"a"))
