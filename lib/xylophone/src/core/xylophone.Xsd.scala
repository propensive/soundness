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

import scala.collection.immutable.VectorMap
import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*
import zephyrine.*

object Xsd:
  val namespace: Text = t"http://www.w3.org/2001/XMLSchema"
  val instance: Text = t"http://www.w3.org/2001/XMLSchema-instance"

  enum Form:
    case Qualified, Unqualified

  enum Use:
    case Optional, Required, Prohibited

  // `max` is `Unset` for `unbounded`
  case class Occurs(min: Int = 1, max: Optional[Int] = 1):
    def optional: Boolean = min == 0
    def many: Boolean = max != 1

  object Builtin:
    private lazy val byName: scm.HashMap[String, Builtin] =
      val map = scm.HashMap[String, Builtin]()
      values.foreach: builtin => map(builtin.local.s) = builtin
      map

    def apply(local: Text): Optional[Builtin] = byName.get(local.s) match
      case Some(builtin) => builtin
      case None          => Unset

  // The builtin datatypes of XML Schema Part 2, by their local names in the XSD namespace
  enum Builtin(val local: Text):
    case String extends Builtin(t"string")
    case Boolean extends Builtin(t"boolean")
    case Decimal extends Builtin(t"decimal")
    case Float extends Builtin(t"float")
    case Double extends Builtin(t"double")
    case Duration extends Builtin(t"duration")
    case DateTime extends Builtin(t"dateTime")
    case Time extends Builtin(t"time")
    case Date extends Builtin(t"date")
    case GYearMonth extends Builtin(t"gYearMonth")
    case GYear extends Builtin(t"gYear")
    case GMonthDay extends Builtin(t"gMonthDay")
    case GDay extends Builtin(t"gDay")
    case GMonth extends Builtin(t"gMonth")
    case HexBinary extends Builtin(t"hexBinary")
    case Base64Binary extends Builtin(t"base64Binary")
    case AnyUri extends Builtin(t"anyURI")
    case QName extends Builtin(t"QName")
    case Notation extends Builtin(t"NOTATION")
    case NormalizedString extends Builtin(t"normalizedString")
    case Token extends Builtin(t"token")
    case Language extends Builtin(t"language")
    case NmToken extends Builtin(t"NMTOKEN")
    case NmTokens extends Builtin(t"NMTOKENS")
    case Name extends Builtin(t"Name")
    case NcName extends Builtin(t"NCName")
    case Id extends Builtin(t"ID")
    case IdRef extends Builtin(t"IDREF")
    case IdRefs extends Builtin(t"IDREFS")
    case Entity extends Builtin(t"ENTITY")
    case Entities extends Builtin(t"ENTITIES")
    case Integer extends Builtin(t"integer")
    case NonPositiveInteger extends Builtin(t"nonPositiveInteger")
    case NegativeInteger extends Builtin(t"negativeInteger")
    case Long extends Builtin(t"long")
    case Int extends Builtin(t"int")
    case Short extends Builtin(t"short")
    case Byte extends Builtin(t"byte")
    case NonNegativeInteger extends Builtin(t"nonNegativeInteger")
    case UnsignedLong extends Builtin(t"unsignedLong")
    case UnsignedInt extends Builtin(t"unsignedInt")
    case UnsignedShort extends Builtin(t"unsignedShort")
    case UnsignedByte extends Builtin(t"unsignedByte")
    case PositiveInteger extends Builtin(t"positiveInteger")
    case AnyType extends Builtin(t"anyType")
    case AnySimpleType extends Builtin(t"anySimpleType")

  // A `type="…"` attribute, or a definition written inline
  enum TypeRef:
    case Named(name: Xml.Name)
    case Inline(definition: Type)

  sealed trait Type

  enum SimpleType extends Type:
    case Builtin(builtin: Xsd.Builtin)
    case Restriction(base: TypeRef, facets: List[Facet])
    case Sequence(item: TypeRef)   // `xs:list`
    case Union(members: List[TypeRef])

  case class SimpleDecl(name: Optional[Text], definition: SimpleType, documentation: List[Text])

  enum Facet:
    case Enumeration(values: List[Text])
    case Pattern(regex: Text)
    case MinInclusive(value: Text)
    case MaxInclusive(value: Text)
    case MinExclusive(value: Text)
    case MaxExclusive(value: Text)
    case MinLength(length: Int)
    case MaxLength(length: Int)
    case Length(length: Int)
    case TotalDigits(digits: Int)
    case FractionDigits(digits: Int)
    case WhiteSpace(mode: Text)

  enum Particle:
    case Sequence(items: List[Particle], occurs: Occurs)
    case Choice(items: List[Particle], occurs: Occurs)
    case All(items: List[Particle], occurs: Occurs)
    case Element(declaration: ElementDecl, occurs: Occurs)
    case Group(ref: Xml.Name, occurs: Occurs)
    case Any(occurs: Occurs)

    def occurs: Occurs

  case class ElementDecl
    ( name:              Optional[Text],
      ref:               Optional[Xml.Name],
      typeRef:           Optional[TypeRef],
      nillable:          Boolean,
      default:           Optional[Text],
      fixed:             Optional[Text],
      form:              Optional[Form],
      substitutionGroup: Optional[Xml.Name],
      abstractDecl:      Boolean,
      documentation:     List[Text] )

  case class AttributeDecl
    ( name:    Optional[Text],
      ref:     Optional[Xml.Name],
      typeRef: Optional[TypeRef],
      use:     Use,
      default: Optional[Text],
      fixed:   Optional[Text],
      form:    Optional[Form] )

  enum AttributeUse:
    case Attribute(declaration: AttributeDecl)
    case Group(ref: Xml.Name)
    case Any

  enum Content:
    case Empty
    case Particles(particle: Particle)
    case SimpleExtension(base: TypeRef, attributes: List[AttributeUse])
    case SimpleRestriction(base: TypeRef, facets: List[Facet], attributes: List[AttributeUse])

    case ComplexExtension
      ( base: TypeRef, additions: Optional[Particle], attributes: List[AttributeUse] )

    case ComplexRestriction
      ( base: TypeRef, particle: Optional[Particle], attributes: List[AttributeUse] )

  case class ComplexType
    ( name:          Optional[Text],
      mixed:         Boolean,
      content:       Content,
      attributes:    List[AttributeUse],
      anyAttribute:  Boolean,
      documentation: List[Text] )
  extends Type

  case class Import(namespace: Optional[Text], location: Optional[Text])

  object Error:
    enum Reason(val number: Int) extends Clarification:
      case NotSchema(found: Xml.Name)                        extends Reason(1)
      case Unexpected(element: Xml.Name, within: Text)       extends Reason(2)
      case MissingAttribute(element: Text, attribute: Text)  extends Reason(3)
      case BadOccurs(value: Text)                            extends Reason(4)
      case UnknownPrefix(prefix: Text)                       extends Reason(5)
      case BadFacet(facet: Text, value: Text)                extends Reason(6)
      case Duplicate(kind: Text, name: Text)                 extends Reason(7)

    given communicable: Reason is Communicable =
      case Reason.NotSchema(found) =>
        m"the document element is ${found.show}, not an XML Schema"

      case Reason.Unexpected(element, within) =>
        m"${element.show} may not appear in $within"

      case Reason.MissingAttribute(element, attribute) =>
        m"the $element has no $attribute attribute"

      case Reason.BadOccurs(value) =>
        m"$value is not a valid number of occurrences"

      case Reason.UnknownPrefix(prefix) =>
        m"the prefix $prefix is not bound to a namespace"

      case Reason.BadFacet(facet, value) =>
        m"$value is not a valid value for the $facet facet"

      case Reason.Duplicate(kind, name) =>
        m"the $kind $name is declared more than once"

  case class Error(reason: Error.Reason)(using Diagnostics)
  extends fulminate.Error(992, reason.number)(m"the XML Schema could not be read because $reason")

  def parse(xml: Xml)(using Tactic[Error]): Xsd = Reader(root(xml)).schema()

  // The schema in a document's text, which may begin with an XML declaration; read with the
  // freeform vocabulary and strict namespaces, as a schema document binds every prefix it uses
  def parse(text: Text)(using Tactic[Error], Tactic[Parse.Error]): Xsd =
    val xml =
      Xml.XmlParser.fromText(text)(using XmlSchema.Freeform, Xml.Scope.xml, Xml.Namespacing.Strict)
      . parseXml(headers0 = true)

    parse(xml)

  private def root(xml: Xml): Xml.Element = xml match
    case element: Xml.Element               => element
    case Xml.Fragment(element: Xml.Element) => element

    case Xml.Fragment(nodes*) =>
      nodes.collectFirst { case element: Xml.Element => element }
      . getOrElse(Xml.Element(t"", Attributes.empty, Array()))

    case _ =>
      Xml.Element(t"", Attributes.empty, Array())

  // A single pass over the schema document, dispatching every element by its resolved name,
  // so `xs:`, `xsd:` and a default XSD namespace are all read alike
  private class Reader(document: Xml.Element)(using Tactic[Error]):
    private def fail(reason: Error.Reason): Nothing = abort(Error(reason))

    private def name(local: String): Xml.Name = Xml.Name(namespace, local.tt)

    private def children(element: Xml.Element): List[Xml.Element] =
      val buffer = scm.ArrayBuffer[Xml.Element]()

      element.children.extent.each: index =>
        element.children(index) match
          case child: Xml.Element => buffer += child
          case _                  => ()

      List.from(buffer)

    private def text(element: Xml.Element): Text =
      val builder = StringBuilder()

      element.children.extent.each: index =>
        element.children(index) match
          case Xml.Text(text)  => builder.append(text.s)
          case Xml.Cdata(text) => builder.append(text.s)
          case _               => ()

      builder.toString.tt

    private def attribute(element: Xml.Element, key: String): Optional[Text] =
      element.attributes.fetch(key.tt)

    private def required(element: Xml.Element, key: String): Text =
      attribute(element, key).or(fail(Error.Reason.MissingAttribute(element.localName, key.tt)))

    private def flag(element: Xml.Element, key: String): Boolean =
      attribute(element, key).lay(false): value => value.s == "true" || value.s == "1"

    // A QName-valued attribute, resolved through the element's bindings; an unprefixed name
    // takes the default namespace, as XML Schema reads it
    private def qname(element: Xml.Element, value: Text): Xml.Name =
      val (prefix, local) = Xml.Name.split(value)

      prefix.lay(Xml.Name(element.resolve(Unset), local)): prefix =>
        element.resolve(prefix).lay(fail(Error.Reason.UnknownPrefix(prefix))): uri =>
          Xml.Name(uri, local)

    private def form(element: Xml.Element, key: String): Optional[Form] =
      attribute(element, key).let: value =>
        if value.s == "qualified" then Form.Qualified else Form.Unqualified

    private def occurs(element: Xml.Element): Occurs =
      def count(value: Text): Int =
        try Integer.parseInt(value.s)
        catch case _: NumberFormatException => fail(Error.Reason.BadOccurs(value))

      val min = attribute(element, "minOccurs").lay(1)(count(_))

      val max: Optional[Int] = attribute(element, "maxOccurs") match
        case Unset       => 1
        case value: Text => if value.s == "unbounded" then Unset else count(value)

      Occurs(min, max)

    private def integer(element: Xml.Element, facet: String): Int =
      val value = required(element, "value")

      try Integer.parseInt(value.s)
      catch case _: NumberFormatException => fail(Error.Reason.BadFacet(facet.tt, value))

    private def documentation(element: Xml.Element): List[Text] =
      children(element).bind: child =>
        if child.qualified == name("annotation")
        then children(child).filter(_.qualified == name("documentation")).map(text(_))
        else Nil

    private def unexpected(child: Xml.Element, within: String): Nothing =
      fail(Error.Reason.Unexpected(child.qualified, within.tt))

    private def isAnnotation(child: Xml.Element): Boolean = child.qualified == name("annotation")

    def schema(): Xsd =
      if document.qualified != name("schema") then fail(Error.Reason.NotSchema(document.qualified))

      val elements = scm.LinkedHashMap[Text, ElementDecl]()
      val complexTypes = scm.LinkedHashMap[Text, ComplexType]()
      val simpleTypes = scm.LinkedHashMap[Text, SimpleDecl]()
      val attributes = scm.LinkedHashMap[Text, AttributeDecl]()
      val groups = scm.LinkedHashMap[Text, Particle]()
      val attributeGroups = scm.LinkedHashMap[Text, List[AttributeUse]]()
      val imports = scm.ArrayBuffer[Import]()
      val includes = scm.ArrayBuffer[Text]()

      def declare[value](map: scm.LinkedHashMap[Text, value], kind: String, key: Text, value: value)
      :   Unit =

        if map.contains(key) then fail(Error.Reason.Duplicate(kind.tt, key))
        map(key) = value

      children(document).each: child =>
        child.qualified.local.s match
          case _ if !isXsd(child) => unexpected(child, "schema")
          case "annotation"       => ()
          case "notation"         => ()

          case "element" =>
            val declaration = elementDecl(child)
            declare(elements, "element", required(child, "name"), declaration)

          case "complexType" =>
            declare(complexTypes, "complexType", required(child, "name"), complexType(child))

          case "simpleType" =>
            declare(simpleTypes, "simpleType", required(child, "name"), simpleDecl(child))

          case "attribute" =>
            declare(attributes, "attribute", required(child, "name"), attributeDecl(child))

          case "group" =>
            declare(groups, "group", required(child, "name"), groupParticle(child))

          case "attributeGroup" =>
            val name = required(child, "name")
            declare(attributeGroups, "attributeGroup", name, attributeUses(child))

          case "import" =>
            imports += Import(attribute(child, "namespace"), attribute(child, "schemaLocation"))

          case "include" =>
            includes += required(child, "schemaLocation")

          case "redefine" =>
            includes += required(child, "schemaLocation")

          case _ =>
            unexpected(child, "schema")

      Xsd
        ( attribute(document, "targetNamespace"),
          form(document, "elementFormDefault").or(Form.Unqualified),
          form(document, "attributeFormDefault").or(Form.Unqualified),
          frozen(elements),
          frozen(complexTypes),
          frozen(simpleTypes),
          frozen(attributes),
          frozen(groups),
          frozen(attributeGroups),
          List.from(imports),
          List.from(includes),
          documentation(document) )

    private def frozen[value](map: scm.LinkedHashMap[Text, value]): Map[Text, value] =
      val builder = VectorMap.newBuilder[Text, value]
      map.foreach: (key, value) => builder += ((key, value))
      Map.from(builder.result())

    private def isXsd(element: Xml.Element): Boolean = element.namespace == namespace

    private def elementDecl(element: Xml.Element): ElementDecl =
      var inline: Optional[TypeRef] = Unset

      children(element).each: child =>
        child.qualified.local.s match
          case _ if !isXsd(child)          => unexpected(child, "element")
          case "annotation"                => ()
          case "complexType"               => inline = TypeRef.Inline(complexType(child))
          case "simpleType"                => inline = TypeRef.Inline(simpleDecl(child).definition)
          case "unique" | "key" | "keyref" => ()
          case _                           => unexpected(child, "element")

      val named: Optional[TypeRef] = attribute(element, "type").let: value =>
        TypeRef.Named(qname(element, value))

      ElementDecl
        ( attribute(element, "name"),
          attribute(element, "ref").let(qname(element, _)),
          named.or(inline),
          flag(element, "nillable"),
          attribute(element, "default"),
          attribute(element, "fixed"),
          form(element, "form"),
          attribute(element, "substitutionGroup").let(qname(element, _)),
          flag(element, "abstract"),
          documentation(element) )

    private def attributeDecl(element: Xml.Element): AttributeDecl =
      var inline: Optional[TypeRef] = Unset

      children(element).each: child =>
        child.qualified.local.s match
          case _ if !isXsd(child) => unexpected(child, "attribute")
          case "annotation"       => ()
          case "simpleType"       => inline = TypeRef.Inline(simpleDecl(child).definition)
          case _                  => unexpected(child, "attribute")

      val named: Optional[TypeRef] = attribute(element, "type").let: value =>
        TypeRef.Named(qname(element, value))

      val use = attribute(element, "use").lay(Use.Optional): value =>
        value.s match
          case "required"   => Use.Required
          case "prohibited" => Use.Prohibited
          case _            => Use.Optional

      AttributeDecl
        ( attribute(element, "name"),
          attribute(element, "ref").let(qname(element, _)),
          named.or(inline),
          use,
          attribute(element, "default"),
          attribute(element, "fixed"),
          form(element, "form") )

    // The attribute uses among an element's children: attributes, attribute group references
    // and `anyAttribute`
    private def attributeUses(element: Xml.Element): List[AttributeUse] =
      children(element).bind: child =>
        child.qualified.local.s match
          case "attribute" if isXsd(child)    => List(AttributeUse.Attribute(attributeDecl(child)))
          case "anyAttribute" if isXsd(child) => List(AttributeUse.Any)

          case "attributeGroup" if isXsd(child) =>
            attribute(child, "ref").lay(Nil): ref => List(AttributeUse.Group(qname(child, ref)))

          case _ =>
            Nil

    private def isAttributeUse(child: Xml.Element): Boolean =
      isXsd(child) && (child.qualified.local.s match
        case "attribute" | "attributeGroup" | "anyAttribute" => true
        case _                                               => false)

    private def compositor(child: Xml.Element): Optional[Particle] = child.qualified.local.s match
      case "sequence" if isXsd(child) => Particle.Sequence(particles(child), occurs(child))
      case "choice" if isXsd(child)   => Particle.Choice(particles(child), occurs(child))
      case "all" if isXsd(child)      => Particle.All(particles(child), occurs(child))
      case "group" if isXsd(child)    => groupParticle(child)
      case "element" if isXsd(child)  => Particle.Element(elementDecl(child), occurs(child))
      case "any" if isXsd(child)      => Particle.Any(occurs(child))
      case _                          => Unset

    private def particles(element: Xml.Element): List[Particle] =
      children(element).bind: child =>
        if isAnnotation(child) then Nil
        else compositor(child).lay(unexpected(child, element.localName.s))(List(_))

    // A `group`: a reference, or a named definition whose single compositor is the particle
    private def groupParticle(element: Xml.Element): Particle =
      attribute(element, "ref") match
        case ref: Text => Particle.Group(qname(element, ref), occurs(element))
        case _         => particles(element).prim.or(Particle.Sequence(Nil, occurs(element)))

    private def complexType(element: Xml.Element): ComplexType =
      var content: Content = Content.Empty
      var anyAttribute = false

      children(element).each: child =>
        child.qualified.local.s match
          case _ if !isXsd(child)      => unexpected(child, "complexType")
          case "annotation"            => ()
          case "attribute"             => ()
          case "attributeGroup"        => ()
          case "anyAttribute"          => anyAttribute = true
          case "simpleContent"         => content = derivedContent(child, simple = true)
          case "complexContent"        => content = derivedContent(child, simple = false)

          case _ =>
            compositor(child) match
              case particle: Particle => content = Content.Particles(particle)
              case _                  => unexpected(child, "complexType")

      ComplexType
        ( attribute(element, "name"),
          flag(element, "mixed"),
          content,
          attributeUses(element),
          anyAttribute,
          documentation(element) )

    // The `extension` or `restriction` inside `simpleContent` or `complexContent`
    private def derivedContent(element: Xml.Element, simple: Boolean): Content =
      val derivation = children(element).filter(!isAnnotation(_)).prim.or:
        unexpected(element, element.localName.s)

      val base = TypeRef.Named(qname(derivation, required(derivation, "base")))
      val attributes = attributeUses(derivation)

      val particle: Optional[Particle] =
        val content = children(derivation).filter: child =>
          !isAnnotation(child) && !isAttributeUse(child)

        content.prim.let: child =>
          compositor(child).or(if simple then Unset else unexpected(child, derivation.localName.s))

      derivation.qualified.local.s match
        case "extension" if isXsd(derivation) =>
          if simple then Content.SimpleExtension(base, attributes)
          else Content.ComplexExtension(base, particle, attributes)

        case "restriction" if isXsd(derivation) =>
          if simple then Content.SimpleRestriction(base, facets(derivation), attributes)
          else Content.ComplexRestriction(base, particle, attributes)

        case _ =>
          unexpected(derivation, element.localName.s)

    private def facets(element: Xml.Element): List[Facet] =
      val enumeration = scm.ArrayBuffer[Text]()
      val others = scm.ArrayBuffer[Facet]()

      // The children of a restriction which are not facets: its annotation, an inline base type,
      // and — in a simple-content restriction — its attributes
      def ignored(local: String): Boolean =
        local == "annotation" || local == "simpleType" || local == "attribute" ||
          local == "attributeGroup" || local == "anyAttribute"

      children(element).each: child =>
        child.qualified.local.s match
          case _ if !isXsd(child)      => unexpected(child, "restriction")
          case local if ignored(local) => ()
          case "enumeration"           => enumeration += required(child, "value")
          case "pattern"               => others += Facet.Pattern(required(child, "value"))
          case "minInclusive"          => others += Facet.MinInclusive(required(child, "value"))
          case "maxInclusive"          => others += Facet.MaxInclusive(required(child, "value"))
          case "minExclusive"          => others += Facet.MinExclusive(required(child, "value"))
          case "maxExclusive"          => others += Facet.MaxExclusive(required(child, "value"))
          case "minLength"             => others += Facet.MinLength(integer(child, "minLength"))
          case "maxLength"             => others += Facet.MaxLength(integer(child, "maxLength"))
          case "length"                => others += Facet.Length(integer(child, "length"))
          case "totalDigits"           => others += Facet.TotalDigits(integer(child, "totalDigits"))
          case "whiteSpace"            => others += Facet.WhiteSpace(required(child, "value"))

          case "fractionDigits" =>
            others += Facet.FractionDigits(integer(child, "fractionDigits"))

          case _ =>
            unexpected(child, "restriction")

      val all =
        if enumeration.isEmpty then others
        else others.prepended(Facet.Enumeration(List.from(enumeration)))

      List.from(all)

    private def simpleDecl(element: Xml.Element): SimpleDecl =
      val definition = children(element).filter(!isAnnotation(_)).prim match
        case child: Xml.Element => simpleType(child)
        case _                  => unexpected(element, "simpleType")

      SimpleDecl(attribute(element, "name"), definition, documentation(element))

    private def simpleType(child: Xml.Element): SimpleType = child.qualified.local.s match
      case "restriction" if isXsd(child) =>
        val base: TypeRef = attribute(child, "base").lay(inlineType(child, "restriction")): value =>
          TypeRef.Named(qname(child, value))

        SimpleType.Restriction(base, facets(child))

      case "list" if isXsd(child) =>
        val item: TypeRef = attribute(child, "itemType").lay(inlineType(child, "list")): value =>
          TypeRef.Named(qname(child, value))

        SimpleType.Sequence(item)

      case "union" if isXsd(child) =>
        val named: List[TypeRef] = attribute(child, "memberTypes").lay(Nil): value =>
          value.cut(t" ").filter(_.s.nonEmpty).map: member => TypeRef.Named(qname(child, member))

        val inline: List[TypeRef] = children(child).bind: member =>
          if member.qualified == name("simpleType")
          then List(TypeRef.Inline(simpleDecl(member).definition))
          else Nil

        SimpleType.Union(List.from(named.stdlib ++ inline.stdlib))

      case _ =>
        unexpected(child, "simpleType")

    // The inline `simpleType` a `restriction` or `list` without a `base`/`itemType` carries
    private def inlineType(element: Xml.Element, within: String): TypeRef =
      children(element).filter(_.qualified == name("simpleType")).prim match
        case child: Xml.Element => TypeRef.Inline(simpleDecl(child).definition)
        case _                  => unexpected(element, within)

// An XML Schema (XSD 1.0) as an immutable model of its declarations, read from the schema
// document with `Xsd.parse`. References — a `type="tns:Order"`, a `ref="tns:item"`, a `base` — are
// kept by name and resolved by lookup, so a recursive schema is representable and the parse is a
// single pass; `import`s and `include`s are recorded, not fetched. The model is what an
// `Xml.Provider` reads its fields from; it does not validate documents.
case class Xsd
  ( targetNamespace:      Optional[Text],
    elementFormDefault:   Xsd.Form,
    attributeFormDefault: Xsd.Form,
    elements:             Map[Text, Xsd.ElementDecl],
    complexTypes:         Map[Text, Xsd.ComplexType],
    simpleTypes:          Map[Text, Xsd.SimpleDecl],
    attributes:           Map[Text, Xsd.AttributeDecl],
    groups:               Map[Text, Xsd.Particle],
    attributeGroups:      Map[Text, List[Xsd.AttributeUse]],
    imports:              List[Xsd.Import],
    includes:             List[Text],
    documentation:        List[Text] ):

  // Whether a name refers into this schema: its namespace is the target namespace (both may be
  // absent)
  def local(name: Xml.Name): Boolean = name.namespace == targetNamespace

  def element(name: Text): Optional[Xsd.ElementDecl] = elements.at(name)

  // The type a reference names: a complex or simple type declared here, or a builtin
  def resolve(ref: Xml.Name): Optional[Xsd.Type] =
    if local(ref) then complexTypes.at(ref.local).or(simpleTypes.at(ref.local).let(_.definition))
    else builtin(ref).let(Xsd.SimpleType.Builtin(_))

  def builtin(ref: Xml.Name): Optional[Xsd.Builtin] =
    if ref.namespace == Xsd.namespace then Xsd.Builtin(ref.local) else Unset
