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

// Small schemas exercising one feature each, written as text and read through the provider's
// `Text` conversion

object Choices extends Xml.Provider(t"""
  <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
    <xs:element name="shape">
      <xs:complexType>
        <xs:choice>
          <xs:element name="circle" type="xs:double"/>
          <xs:element name="square" type="xs:double"/>
        </xs:choice>
      </xs:complexType>
    </xs:element>
  </xs:schema>""")

object Clashes extends Xml.Provider(t"""
  <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
    <xs:element name="entry">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="id" type="xs:int"/>
          <xs:element name="text" type="xs:string"/>
        </xs:sequence>
        <xs:attribute name="id" type="xs:string" use="required"/>
      </xs:complexType>
    </xs:element>
  </xs:schema>""")

object Prices extends Xml.Provider(t"""
  <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
    <xs:element name="price">
      <xs:complexType>
        <xs:simpleContent>
          <xs:extension base="xs:decimal">
            <xs:attribute name="currency" type="xs:string" use="required"/>
            <xs:attribute name="text" type="xs:string"/>
          </xs:extension>
        </xs:simpleContent>
      </xs:complexType>
    </xs:element>
  </xs:schema>""")

object Facets extends Xml.Provider(t"""
  <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
    <xs:simpleType name="Mode">
      <xs:restriction base="xs:string">
        <xs:enumeration value="fast"/>
        <xs:enumeration value="slow"/>
      </xs:restriction>
    </xs:simpleType>
    <xs:element name="person">
      <xs:complexType>
        <xs:all>
          <xs:element name="age">
            <xs:simpleType>
              <xs:restriction base="xs:int">
                <xs:minInclusive value="0"/>
                <xs:maxInclusive value="150"/>
              </xs:restriction>
            </xs:simpleType>
          </xs:element>
          <xs:element name="code">
            <xs:simpleType>
              <xs:restriction base="xs:string">
                <xs:length value="3"/>
              </xs:restriction>
            </xs:simpleType>
          </xs:element>
          <xs:element name="tag">
            <xs:simpleType>
              <xs:restriction base="xs:token">
                <xs:pattern value="[a-z]+-[0-9]+"/>
              </xs:restriction>
            </xs:simpleType>
          </xs:element>
          <xs:element name="mode" type="Mode"/>
          <xs:element name="amount">
            <xs:simpleType>
              <xs:restriction base="xs:decimal">
                <xs:totalDigits value="5"/>
                <xs:fractionDigits value="2"/>
              </xs:restriction>
            </xs:simpleType>
          </xs:element>
          <xs:element name="count" type="xs:unsignedByte"/>
        </xs:all>
      </xs:complexType>
    </xs:element>
  </xs:schema>""")

object Lists extends Xml.Provider(t"""
  <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
    <xs:simpleType name="Numbers">
      <xs:list itemType="xs:int"/>
    </xs:simpleType>
    <xs:element name="record">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="tags" type="xs:NMTOKENS"/>
          <xs:element name="numbers" type="Numbers" maxOccurs="unbounded"/>
          <xs:element name="middle" type="xs:string" nillable="true"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>
  </xs:schema>""")

object Trees extends Xml.Provider(t"""
  <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
    <xs:complexType name="Node">
      <xs:sequence>
        <xs:element name="label" type="xs:string"/>
        <xs:element name="child" type="Node" minOccurs="0" maxOccurs="unbounded"/>
      </xs:sequence>
    </xs:complexType>
    <xs:element name="tree" type="Node"/>
  </xs:schema>""")

object DefaultPrefixed extends Xml.Provider(t"""
  <schema xmlns="http://www.w3.org/2001/XMLSchema">
    <element name="root">
      <complexType>
        <sequence>
          <element name="value" type="string"/>
          <element name="flag" type="boolean"/>
        </sequence>
      </complexType>
    </element>
  </schema>""")

object Unqualified extends Xml.Provider(t"""
  <xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema" targetNamespace="urn:t">
    <xsd:element name="root">
      <xsd:complexType>
        <xsd:sequence>
          <xsd:element name="value" type="xsd:int"/>
        </xsd:sequence>
        <xsd:attribute name="kind" type="xsd:string"/>
      </xsd:complexType>
    </xsd:element>
  </xsd:schema>""")

object Extensions extends Xml.Provider(t"""
  <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
    <xs:attributeGroup name="Identified">
      <xs:attribute name="id" type="xs:string" use="required"/>
    </xs:attributeGroup>
    <xs:group name="Named">
      <xs:sequence>
        <xs:element name="name" type="xs:string"/>
      </xs:sequence>
    </xs:group>
    <xs:complexType name="Base">
      <xs:sequence>
        <xs:group ref="Named"/>
        <xs:element name="a" type="xs:int"/>
      </xs:sequence>
      <xs:attributeGroup ref="Identified"/>
    </xs:complexType>
    <xs:complexType name="Derived">
      <xs:complexContent>
        <xs:extension base="Base">
          <xs:sequence>
            <xs:element name="b" type="xs:int"/>
          </xs:sequence>
          <xs:attribute name="extra" type="xs:boolean"/>
        </xs:extension>
      </xs:complexContent>
    </xs:complexType>
    <xs:element name="thing" type="Derived"/>
  </xs:schema>""")

object Ambiguous extends Xml.Provider(t"""
  <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
    <xs:element name="one" type="xs:string"/>
    <xs:element name="two" type="xs:string"/>
  </xs:schema>""")

object Chosen extends Xml.Provider(t"""
  <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
    <xs:element name="one">
      <xs:complexType><xs:sequence><xs:element name="x" type="xs:int"/></xs:sequence></xs:complexType>
    </xs:element>
    <xs:element name="two">
      <xs:complexType><xs:sequence><xs:element name="y" type="xs:int"/></xs:sequence></xs:complexType>
    </xs:element>
  </xs:schema>""", root = t"two")
