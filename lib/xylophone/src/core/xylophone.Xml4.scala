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

import scala.collection.mutable as scm
import scala.language.experimental.into

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import polyvinyl.*
import prepositional.*
import rudiments.*
import turbulence.read
import vacuous.*

import strategies.throwUnsafely

// The lowest layer of `Xml`'s companion, holding the type provider over an XML Schema. It
// contributes no givens for `Xml` itself.
trait Xml4:
  // A polyvinyl type provider driven by an XML Schema: `object PurchaseOrder extends
  // Xml.Provider(cp"/xsd/po.xsd")` reads the schema at compiletime, and `PurchaseOrder.record(xml)`
  // is a record with one typed member per child element and attribute of the schema's root
  // element — `record.shipTo.name`, `record.items.item.map(_.quantity)` — checked by the
  // compiler against the schema.
  //
  // Child elements and attributes become fields by local name; an attribute that shares a
  // name with a child element is reached as `` `@name` ``, and the text of an element with
  // simple content and attributes as `text` (`` `#text` `` if an attribute is called `text`).
  // `minOccurs="0"` and `nillable` fields are `Optional`; `maxOccurs` above one gives a `List`;
  // each alternative of a `choice` is `Optional`. Builtin types read as `Text`, `Boolean`,
  // `Int`, `Long`, `Double`, and — through anticipation's interfaces — as instants, dates,
  // durations and URLs; `xs:list` types read as `List[Text]`; `anyType`, `xs:any`, recursive
  // and unresolved types read as raw `Xml`. Facets (`enumeration`, `pattern`, bounds, lengths,
  // digits) are checked as a field is read, raising `Xml.Provider.Error`.
  //
  // The provider reads instance documents by resolved name: a child is matched in the schema's
  // target namespace when the schema qualifies it (`elementFormDefault`, `form`, or a `ref` to
  // a global element), and in no namespace otherwise, whatever prefixes the document uses.
  // The lower-priority readers of the temporal and URI builtins: as `Text`, for when no
  // interface (`instantInterfaces.…`, `urlInterfaces.soundnessUrl`) is in scope to instantiate
  // them
  trait Provider2:
    given dateTimeText: ("dateTime" is Intensional in Xml.Provider from Xml to Text) =
      Intensional(_.as[Text])

    given dateText: ("date" is Intensional in Xml.Provider from Xml to Text) =
      Intensional(_.as[Text])

    given timeText: ("time" is Intensional in Xml.Provider from Xml to Text) =
      Intensional(_.as[Text])

    given durationText: ("duration" is Intensional in Xml.Provider from Xml to Text) =
      Intensional(_.as[Text])

    given anyUriText: ("anyURI" is Intensional in Xml.Provider from Xml to Text) =
      Intensional(_.as[Text])

  object Provider extends Provider2:
    // The text content of a leaf: an element's text nodes concatenated, or an attribute's value
    // as a `Xml.Text`; `Unset` for the absent sentinel
    private def text(xml: Xml): Optional[Text] = xml match
      case _ if xml eq Xml.Absent       => Unset
      case Xml.Text(text)               => text
      case Xml.Fragment(node: Xml.Node) => text(node)

      case element: Xml.Element =>
        val builder = StringBuilder()

        element.children.extent.each: index =>
          element.children(index) match
            case Xml.Text(text)  => builder.append(text.s)
            case Xml.Cdata(text) => builder.append(text.s)
            case _               => ()

        builder.toString.tt

      case _ =>
        Unset

    private def content(xml: Xml): Text = text(xml).or(t"")

    given string: ("string" is Intensional in Xml.Provider from Xml to Text) =
      Intensional(_.as[Text])

    given boolean: ("boolean" is Intensional in Xml.Provider from Xml to Boolean) =
      Intensional(_.as[Boolean])

    given int: ("int" is Intensional in Xml.Provider from Xml to Int) = Intensional(_.as[Int])
    given long: ("long" is Intensional in Xml.Provider from Xml to Long) = Intensional(_.as[Long])

    given short: ("short" is Intensional in Xml.Provider from Xml to Short) =
      Intensional(_.as[Short])

    given byte: ("byte" is Intensional in Xml.Provider from Xml to Byte) = Intensional(_.as[Byte])

    given double: ("double" is Intensional in Xml.Provider from Xml to Double) =
      Intensional(_.as[Double])

    given float: ("float" is Intensional in Xml.Provider from Xml to Float) =
      Intensional(_.as[Float])

    // `xs:decimal` reads as a `Double` for now
    given decimal: ("decimal" is Intensional in Xml.Provider from Xml to Double) =
      Intensional(_.as[Double])

    given xml: ("xml" is Intensional in Xml.Provider from Xml to Xml) = Intensional(identity(_))

    // An `xs:list` type: whitespace-separated tokens
    given tokens: ("tokens" is Intensional in Xml.Provider from Xml to List[Text]) =
      Intensional: xml => splitTokens(content(xml))

    private def splitTokens(text: Text): List[Text] =
      val words = text.s.trim.nn.split("\\s+").nn
      val parts = scala.collection.immutable.ArraySeq.unsafeWrapArray(words)
      List.from(parts.toList.filter(_.nn.nonEmpty).map(_.nn.tt))

    given dateTime: [instant: Instantiable across Instants from Text]
    =>  ( "dateTime" is Intensional in Xml.Provider from Xml to instant ) =
      Intensional(_.as[Text].instantiate)

    given date: [date: Instantiable across Dates from Text]
    =>  ( "date" is Intensional in Xml.Provider from Xml to date ) =
      Intensional(_.as[Text].instantiate)

    given time: [time: Instantiable across Times from Text]
    =>  ( "time" is Intensional in Xml.Provider from Xml to time ) =
      Intensional(_.as[Text].instantiate)

    given duration: [duration: Instantiable across Durations from Text]
    =>  ( "duration" is Intensional in Xml.Provider from Xml to duration ) =
      Intensional(_.as[Text].instantiate)

    given anyUri: [url: Instantiable across Urls from Text]
    =>  ( "anyURI" is Intensional in Xml.Provider from Xml to url ) =
      Intensional(_.as[Text].instantiate)

    private def fail(reason: Xml.Provider.Error.Reason)(using Tactic[Xml.Provider.Error]): Nothing =
      abort(Xml.Provider.Error(reason))

    // The facets of a restricted member travel as `key=value` parameters: `min`, `max`, `xmin`
    // and `xmax` (exclusive) bounds, `minLength`, `maxLength`, `length`, `pattern`, `enum`
    // (alternatives separated by `|`), `totalDigits` and `fractionDigits`.
    private class Facets(params: List[Text]):
      private val pairs: scm.HashMap[String, scm.ArrayBuffer[String]] = scm.HashMap()

      params.each: param =>
        val equals = param.s.indexOf('=')

        if equals > 0 then
          val key = param.s.substring(0, equals).nn
          val value = param.s.substring(equals + 1).nn
          pairs.getOrElseUpdate(key, scm.ArrayBuffer()) += value

      def apply(key: String): Optional[Text] = pairs.get(key) match
        case Some(values) if values.nonEmpty => values.head.tt
        case _                               => Unset

      def all(key: String): List[Text] = pairs.get(key) match
        case Some(values) => List.from(values.map(_.tt))
        case None         => Nil

      // The checks every restricted type shares: length, pattern and enumeration on the text
      def textual(value: Text)(using Tactic[Xml.Provider.Error]): Unit =
        val length = value.s.length

        def badLength(minimum: Optional[Text], maximum: Optional[Text]): Nothing =
          fail(Xml.Provider.Error.Reason.LengthOutOfRange(value, minimum, maximum))

        apply("minLength").let: minimum =>
          if length < Integer.parseInt(minimum.s) then badLength(minimum, apply("maxLength"))

        apply("maxLength").let: maximum =>
          if length > Integer.parseInt(maximum.s) then badLength(apply("minLength"), maximum)

        apply("length").let: exact =>
          if length != Integer.parseInt(exact.s) then badLength(exact, exact)

        all("pattern").each: pattern =>
          if !java.util.regex.Pattern.matches(pattern.s, value.s)
          then fail(Xml.Provider.Error.Reason.PatternMismatch(value, pattern))

        apply("enum").let: enumeration =>
          val alternatives = enumeration.s.split("\\|", -1).nn
          val parts = scala.collection.immutable.ArraySeq.unsafeWrapArray(alternatives)
          val permitted = List.from(parts.toList.map(_.nn.tt))

          if !permitted.has(value)
          then fail(Xml.Provider.Error.Reason.NotPermitted(value, permitted))

      def bounds[number](value: Text, number: number, parse: Text => number)
        ( using Tactic[Xml.Provider.Error], scala.math.Ordering[number] )
      :   Unit =

        import scala.math.Ordering.Implicits.infixOrderingOps

        def outside: Nothing =
          val minimum = apply("min").or(apply("xmin"))
          val maximum = apply("max").or(apply("xmax"))
          fail(Xml.Provider.Error.Reason.OutOfRange(value, minimum, maximum))

        apply("min").let: minimum => if number < parse(minimum) then outside
        apply("max").let: maximum => if number > parse(maximum) then outside
        apply("xmin").let: minimum => if number <= parse(minimum) then outside
        apply("xmax").let: maximum => if number >= parse(maximum) then outside

      def digits(value: Text)(using Tactic[Xml.Provider.Error]): Unit =
        val text = value.s.trim.nn.stripPrefix("-").stripPrefix("+")
        val point = text.indexOf('.')
        val integral = if point < 0 then text else text.substring(0, point).nn
        val fraction = if point < 0 then "" else text.substring(point + 1).nn
        val total = integral.count(_.isDigit) + fraction.count(_.isDigit)

        def tooMany: Nothing =
          val total = apply("totalDigits")
          val fraction = apply("fractionDigits")
          fail(Xml.Provider.Error.Reason.TooManyDigits(value, total, fraction))

        apply("totalDigits").let: limit => if total > Integer.parseInt(limit.s) then tooMany

        apply("fractionDigits").let: limit =>
          if fraction.count(_.isDigit) > Integer.parseInt(limit.s) then tooMany

    // The fallible readers are named classes with class-typed givens: a given typed with a
    // refinement over `any` would make every provider object a capability.
    class RestrictedText extends Intensional.Fallible:
      type Self = "string!"
      type Origin = Xml
      type Form = Xml.Provider
      type Result = Text
      type Error = Xml.Provider.Error

      def transform(xml: Xml, params: List[Text])(using Tactic[Xml.Provider.Error]): Text =
        val value = xml.as[Text]
        Facets(params).textual(value)
        value

    class RestrictedInt extends Intensional.Fallible:
      type Self = "int!"
      type Origin = Xml
      type Form = Xml.Provider
      type Result = Int
      type Error = Xml.Provider.Error

      def transform(xml: Xml, params: List[Text])(using Tactic[Xml.Provider.Error]): Int =
        val text = xml.as[Text]
        val value = xml.as[Int]
        val facets = Facets(params)
        facets.textual(text)
        facets.bounds(text, value, _.s.trim.nn.toInt)
        facets.digits(text)
        value

    class RestrictedLong extends Intensional.Fallible:
      type Self = "long!"
      type Origin = Xml
      type Form = Xml.Provider
      type Result = Long
      type Error = Xml.Provider.Error

      def transform(xml: Xml, params: List[Text])(using Tactic[Xml.Provider.Error]): Long =
        val text = xml.as[Text]
        val value = xml.as[Long]
        val facets = Facets(params)
        facets.textual(text)
        facets.bounds(text, value, _.s.trim.nn.toLong)
        facets.digits(text)
        value

    class RestrictedDouble extends Intensional.Fallible:
      type Self = "double!"
      type Origin = Xml
      type Form = Xml.Provider
      type Result = Double
      type Error = Xml.Provider.Error

      def transform(xml: Xml, params: List[Text])(using Tactic[Xml.Provider.Error]): Double =
        val text = xml.as[Text]
        val value = xml.as[Double]
        val facets = Facets(params)
        facets.textual(text)
        facets.bounds(text, value, _.s.trim.nn.toDouble)
        facets.digits(text)
        value

    given restrictedText: RestrictedText = RestrictedText()
    given restrictedInt: RestrictedInt = RestrictedInt()
    given restrictedLong: RestrictedLong = RestrictedLong()
    given restrictedDouble: RestrictedDouble = RestrictedDouble()

    // The fields of the schema's root element, with the namespaces its child elements and
    // attributes are expected in, keyed by parent and child local name
    case class Layout
      ( fields:     List[(Text, Member)],
        elements:   Map[(Text, Text), Optional[Text]],
        attributes: Map[(Text, Text), Optional[Text]] )

    def layout(xsd: Xsd, root: Optional[Text]): Layout = Walk(xsd).layout(root)

    def fieldsOf(xsd: Xsd, root: Optional[Text]): List[(Text, Member)] = layout(xsd, root).fields

    // The walk from an XML Schema to polyvinyl members. Names are followed by lookup; a type or
    // global element met again on the way down is recursive, and reads as raw `Xml`.
    private class Walk(xsd: Xsd):
      import Xsd.{AttributeUse, Builtin, Content, Facet, Particle, SimpleType, TypeRef}

      private val elementNames = scm.HashMap[(Text, Text), Optional[Text]]()
      private val attributeNames = scm.HashMap[(Text, Text), Optional[Text]]()

      private type Bag = scm.LinkedHashMap[Text, Member]

      // The names met on the way down, on the stdlib set for its `+`
      private type Seen = scala.collection.immutable.Set[Text]

      def layout(root: Optional[Text]): Layout =
        val names = List.from(Map.keys(xsd.elements).stdlib)

        // The root is the sole global element, or the sole one with complex content — a
        // schema often also declares the simple elements its types refer to
        val rootName: Text = root.or:
          val complex = names.filter: name => xsd.element(name).lay(false)(hasComplexContent(_))

          if names.stdlib.length == 1 then names.stdlib.head
          else if complex.stdlib.length == 1 then complex.stdlib.head
          else if names.nil then panic(m"the XML Schema declares no global element")
          else
            panic:
              m"""
                the XML Schema declares the global elements ${names.join(t", ")}; pass `root =` to
                choose one
              """

        val declaration = xsd.element(rootName).or:
          panic(m"the XML Schema declares no global element $rootName")

        val fields = member(declaration, rootName, scala.collection.immutable.Set(rootName)) match
          case Member.Record(fields, _) => fields

          case _ =>
            panic(m"the root element $rootName does not have complex content")

        Layout
          ( fields,
            Map.from(scala.collection.immutable.Map.from(elementNames)),
            Map.from(scala.collection.immutable.Map.from(attributeNames)) )

      private def hasComplexContent(declaration: Xsd.ElementDecl): Boolean =
        declaration.typeRef match
          case TypeRef.Inline(_: Xsd.ComplexType) => true

          case TypeRef.Named(name) =>
            xsd.resolve(name).let(_.isInstanceOf[Xsd.ComplexType]).or(false)

          case _                                  => false

      private def expectedNamespace(qualified: Boolean): Optional[Text] =
        if qualified then xsd.targetNamespace else Unset

      // The member of an element declaration, whose type is named, inline, or absent
      private def member(declaration: Xsd.ElementDecl, local: Text, seen: Seen): Member =
        declaration.ref match
          case ref: Xml.Name =>
            if !xsd.local(ref) || seen.contains(ref.local) then Member.Value(t"xml")
            else xsd.element(ref.local).lay(Member.Value(t"xml")): global =>
              member(global, ref.local, seen + ref.local)

          case _ =>
            declaration.typeRef match
              case TypeRef.Named(name)        => named(name, local, seen)
              case TypeRef.Inline(definition) => typed(definition, local, seen)
              case _                          => Member.Value(t"xml")

      private def named(name: Xml.Name, local: Text, seen: Seen): Member =
        xsd.builtin(name).lay(complexNamed(name, local, seen))(builtin(_, Nil))

      private def complexNamed(name: Xml.Name, local: Text, seen: Seen): Member =
        val key = t"type:${name.local}"

        if !xsd.local(name) || seen.contains(key) then Member.Value(t"xml")
        else xsd.resolve(name).lay(Member.Value(t"xml"))(typed(_, local, seen + key))

      private def typed(definition: Xsd.Type, local: Text, seen: Seen): Member =
        definition match
          case simple: SimpleType         => simpleMember(simple, Nil, seen)
          case complex: Xsd.ComplexType   => complexMember(complex, local, seen)

      // A simple type as a value member: the builtin at the base of the restriction chain,
      // with the facets accumulated along it
      private def simpleMember(simple: SimpleType, facets: List[Facet], seen: Seen): Member =
        simple match
          case SimpleType.Builtin(base) => builtin(base, facets)

          case SimpleType.Restriction(base, own) =>
            val combined = List.from(own.stdlib ++ facets.stdlib)

            base match
              case TypeRef.Inline(simple: SimpleType) => simpleMember(simple, combined, seen)
              case TypeRef.Inline(_)                  => Member.Value(t"xml")

              case TypeRef.Named(name) =>
                xsd.builtin(name).lay(restrictedNamed(name, combined, seen))(builtin(_, combined))

          case SimpleType.Sequence(_) => Member.Value(t"tokens")
          case SimpleType.Union(_)    => Member.Value(t"string")

      private def restrictedNamed(name: Xml.Name, facets: List[Facet], seen: Seen): Member =
        val key = t"type:${name.local}"

        if !xsd.local(name) || seen.contains(key) then Member.Value(t"xml")
        else xsd.resolve(name) match
          case simple: SimpleType => simpleMember(simple, facets, seen + key)
          case _                  => Member.Value(t"xml")

      // The label and implied bounds of a builtin, then the facets as parameters
      private def builtin(base: Builtin, facets: List[Facet]): Member =
        val labelled: (Text, List[Text]) = base match
          case Builtin.Boolean            => (t"boolean", Nil)
          case Builtin.Int                => (t"int", Nil)
          case Builtin.Short              => (t"short", Nil)
          case Builtin.Byte               => (t"byte", Nil)
          case Builtin.UnsignedByte       => (t"int!", List(t"min=0", t"max=255"))
          case Builtin.UnsignedShort      => (t"int!", List(t"min=0", t"max=65535"))
          case Builtin.Integer            => (t"long", Nil)
          case Builtin.Long               => (t"long", Nil)
          case Builtin.UnsignedInt        => (t"long!", List(t"min=0", t"max=4294967295"))
          case Builtin.UnsignedLong       => (t"long!", List(t"min=0"))
          case Builtin.NonNegativeInteger => (t"long!", List(t"min=0"))
          case Builtin.PositiveInteger    => (t"long!", List(t"min=1"))
          case Builtin.NonPositiveInteger => (t"long!", List(t"max=0"))
          case Builtin.NegativeInteger    => (t"long!", List(t"max=-1"))
          case Builtin.Decimal            => (t"decimal", Nil)
          case Builtin.Double             => (t"double", Nil)
          case Builtin.Float              => (t"float", Nil)
          case Builtin.DateTime           => (t"dateTime", Nil)
          case Builtin.Date               => (t"date", Nil)
          case Builtin.Time               => (t"time", Nil)
          case Builtin.Duration           => (t"duration", Nil)
          case Builtin.AnyUri             => (t"anyURI", Nil)
          case Builtin.NmTokens           => (t"tokens", Nil)
          case Builtin.IdRefs             => (t"tokens", Nil)
          case Builtin.Entities           => (t"tokens", Nil)
          case Builtin.AnyType            => (t"xml", Nil)
          case Builtin.AnySimpleType      => (t"string", Nil)
          case _                          => (t"string", Nil)

        val (label, implied) = labelled
        val params = List.from(implied.stdlib ++ facetParams(facets).stdlib)

        if params.nil then Member.Value(label)
        else
          val restricted = label.s match
            case "string" | "string!"                       => t"string!"
            case "int" | "short" | "byte" | "int!"          => t"int!"
            case "long" | "long!"                           => t"long!"
            case "decimal" | "double" | "float" | "double!" => t"double!"
            case _                                          => label

          if restricted == label && !label.s.endsWith("!") then Member.Value(label)
          else Member.Value(restricted, params)

      private def facetParams(facets: List[Facet]): List[Text] = facets.map:
        case Facet.Enumeration(values)    => t"enum=${values.join(t"|")}"
        case Facet.Pattern(regex)         => t"pattern=$regex"
        case Facet.MinInclusive(value)    => t"min=$value"
        case Facet.MaxInclusive(value)    => t"max=$value"
        case Facet.MinExclusive(value)    => t"xmin=$value"
        case Facet.MaxExclusive(value)    => t"xmax=$value"
        case Facet.MinLength(length)      => t"minLength=$length"
        case Facet.MaxLength(length)      => t"maxLength=$length"
        case Facet.Length(length)         => t"length=$length"
        case Facet.TotalDigits(digits)    => t"totalDigits=$digits"
        case Facet.FractionDigits(digits) => t"fractionDigits=$digits"
        case Facet.WhiteSpace(mode)       => t"whiteSpace=$mode"

      // A complex type as a record: its attributes, then its simple-content text, then the
      // fields of its particle, with an extension's base first. A type with simple content and
      // no attributes collapses to its text's value member.
      private def complexMember(complex: Xsd.ComplexType, local: Text, seen: Seen): Member =
        val attributes: Bag = scm.LinkedHashMap()
        val fields: Bag = scm.LinkedHashMap()
        var textMember: Optional[Member] = Unset

        def uses(attributeUses: List[AttributeUse]): Unit = attributeUses.each:
          case AttributeUse.Attribute(declaration) => attributeField(declaration, local, attributes)
          case AttributeUse.Any                    => ()

          case AttributeUse.Group(ref) =>
            if xsd.local(ref) then xsd.attributeGroups.at(ref.local).let(uses(_))

        def simpleBase(base: TypeRef, facets: List[Facet]): Unit =
          textMember = base match
            case TypeRef.Named(name) =>
              xsd.builtin(name).lay(restrictedNamed(name, facets, seen))(builtin(_, facets))

            case TypeRef.Inline(simple: SimpleType) => simpleMember(simple, facets, seen)
            case TypeRef.Inline(_)                  => Member.Value(t"xml")

        // The attributes and particle fields of a complex base type, for an extension
        def complexBase(base: TypeRef): Unit = base match
          case TypeRef.Named(name) if xsd.local(name) && !seen.contains(t"type:${name.local}") =>
            xsd.resolve(name) match
              case parent: Xsd.ComplexType =>
                complexMember(parent, local, seen + t"type:${name.local}") match
                  case Member.Record(parentFields, _) =>
                    parentFields.each: (name, member) =>
                      if name.s.startsWith("@")
                      then merge(attributes, name.s.substring(1).nn.tt, member)
                      else if name == t"text" || name == t"#text" then textMember = member
                      else merge(fields, name, member)

                  case value =>
                    textMember = value

              case simple: SimpleType => textMember = simpleMember(simple, Nil, seen)
              case _                  => ()

          case TypeRef.Inline(parent: Xsd.ComplexType) => complexBase(TypeRef.Named(Xml.Name(t"?")))
          case _                                       => ()

        complex.content match
          case Content.Empty => ()

          case Content.Particles(particle) =>
            particleFields(particle, local, seen, fields, false, false)

          case Content.SimpleExtension(base, attributeUses) =>
            simpleBase(base, Nil)
            uses(attributeUses)

          case Content.SimpleRestriction(base, facets, attributeUses) =>
            simpleBase(base, facets)
            uses(attributeUses)

          case Content.ComplexExtension(base, additions, attributeUses) =>
            complexBase(base)
            additions.let(particleFields(_, local, seen, fields, false, false))
            uses(attributeUses)

          case Content.ComplexRestriction(base, particle, attributeUses) =>
            particle.lay(complexBase(base))(particleFields(_, local, seen, fields, false, false))
            uses(attributeUses)

        uses(complex.attributes)

        textMember match
          case member: Member if attributes.isEmpty && fields.isEmpty => member

          // A type with nothing the schema names — `xs:any` alone, or nothing — reads as raw XML
          case _ if attributes.isEmpty && fields.isEmpty => Member.Value(t"xml")

          case _ =>
            val record: Bag = scm.LinkedHashMap()

            attributes.foreach: (name, member) =>
              record(if fields.contains(name) then t"@$name" else name) = member

            textMember.let: member =>
              record(if attributes.contains(t"text") then t"#text" else t"text") = member

            fields.foreach: (name, member) => if !record.contains(name) then record(name) = member

            Member.Record(List.from(record))

      private def attributeField(declaration: Xsd.AttributeDecl, parent: Text, bag: Bag): Unit =
        val resolution: (Optional[Text], Xsd.AttributeDecl, Boolean) =
          declaration.ref match
            case ref: Xml.Name =>
              if xsd.local(ref) then (ref.local, xsd.attributes.at(ref.local).or(declaration), true)
              else (Unset, declaration, true)

            case _ =>
              val qualified = declaration.form.or(xsd.attributeFormDefault) == Xsd.Form.Qualified
              (declaration.name, declaration, qualified)

        val (name, resolved, qualified) = resolution

        name.let: name =>
          if declaration.use != Xsd.Use.Prohibited then
            val member = resolved.typeRef match
              case TypeRef.Named(typeName) =>
                val fresh: Seen = scala.collection.immutable.Set()
                xsd.builtin(typeName).lay(restrictedNamed(typeName, Nil, fresh))(builtin(_, Nil))

              case TypeRef.Inline(simple: SimpleType) =>
                simpleMember(simple, Nil, scala.collection.immutable.Set())

              case _ =>
                Member.Value(t"string")

            val required = declaration.use == Xsd.Use.Required && declaration.default.absent
            attributeNames((parent, name)) = expectedNamespace(qualified)
            merge(bag, name, if required then member else member.optional)

      // The wider of two multiplicities, for a name declared twice within one type
      private def merge(bag: Bag, name: Text, member: Member): Unit =
        bag.get(name) match
          case Some(existing) =>
            val wider = (existing.multiplicity, member.multiplicity) match
              case (Multiplicity.Many, _) | (_, Multiplicity.Many)         => Multiplicity.Many
              case (Multiplicity.Optional, _) | (_, Multiplicity.Optional) => Multiplicity.Optional
              case (Multiplicity.Keyed, _) | (_, Multiplicity.Keyed)       => Multiplicity.Keyed
              case _                                                       => Multiplicity.One

            bag(name) = existing.of(wider)

          case None =>
            bag(name) = member

      private def particleFields
        ( particle: Particle, parent: Text, seen: Seen, bag: Bag, optional: Boolean, many: Boolean )
      :   Unit =

        val occurs = particle.occurs
        val optional1 = optional || occurs.optional
        val many1 = many || occurs.many

        particle match
          case Particle.Sequence(items, _) =>
            items.each(particleFields(_, parent, seen, bag, optional1, many1))

          case Particle.All(items, _) =>
            items.each(particleFields(_, parent, seen, bag, optional1, many1))

          case Particle.Choice(items, _) =>
            items.each(particleFields(_, parent, seen, bag, true, many1))

          case Particle.Group(ref, _) =>
            val key = t"group:${ref.local}"

            if xsd.local(ref) && !seen.contains(key) then
              xsd.groups.at(ref.local).let: group =>
                particleFields(group, parent, seen + key, bag, optional1, many1)

          case Particle.Any(_) =>
            ()

          case Particle.Element(declaration, _) =>
            val name: Optional[Text] = declaration.name.or(declaration.ref.let(_.local))

            name.let: name =>
              val qualified = declaration.ref.present ||
                declaration.form.or(xsd.elementFormDefault) == Xsd.Form.Qualified

              elementNames((parent, name)) = expectedNamespace(qualified)
              val base = member(declaration, name, seen)

              val multiplicity =
                if many1 then Multiplicity.Many
                else if optional1 || declaration.nillable then Multiplicity.Optional
                else Multiplicity.One

              merge(bag, name, base.of(multiplicity))

    // The schema a provider is built from: an `Xsd`, an `Xml` document of one, or — through
    // the conversions in the companion, applied at the `into` parameter — anything readable as
    // XML, such as a classpath resource (`cp"/xsd/po.xsd"`) or the schema's text.
    // A source is read as text and parsed with the freeform vocabulary, so no `XmlSchema` need
    // be in scope where a provider is declared
    trait Schema2:
      given readable: [source] => (readable: (source is turbulence.Readable to Text)^)
      =>  ( Conversion[source, Schema]^{readable} ) =
        source => Schema.parse(source.read[Text](using readable))

    object Schema extends Schema2:
      private[Provider] def parse(text: Text): Schema = Schema(Xsd.parse(text))

      given xsd: Conversion[Xsd, Schema] = Schema(_)
      given xml: Conversion[Xml, Schema] = xml => Schema(Xsd.parse(xml))
      given text: Conversion[Text, Schema] = parse(_)

    class Schema(val xsd: Xsd)

    object Error:
      enum Reason(val number: Int) extends Clarification:
        case OutOfRange(value: Text, minimum: Optional[Text], maximum: Optional[Text])
        extends Reason(1)

        case LengthOutOfRange(value: Text, minimum: Optional[Text], maximum: Optional[Text])
        extends Reason(2)

        case PatternMismatch(value: Text, pattern: Text)             extends Reason(3)
        case NotPermitted(value: Text, permitted: List[Text])        extends Reason(4)

        case TooManyDigits(value: Text, total: Optional[Text], fraction: Optional[Text])
        extends Reason(5)

        case Absent(name: Text)                                      extends Reason(6)

      given communicable: Reason is Communicable =
        case Reason.OutOfRange(value, minimum, maximum) =>
          m"the value $value is outside the range ${minimum.or(t"…")} to ${maximum.or(t"…")}"

        case Reason.LengthOutOfRange(value, minimum, maximum) =>
          m"""
            the length of $value is outside the range ${minimum.or(t"…")} to ${maximum.or(t"…")}
          """

        case Reason.PatternMismatch(value, pattern) =>
          m"the value $value does not match the pattern $pattern"

        case Reason.NotPermitted(value, permitted) =>
          m"the value $value is not one of ${permitted.join(t", ")}"

        case Reason.TooManyDigits(value, total, fraction) =>
          m"""
            the value $value has more digits than the ${total.or(t"…")} total or
            ${fraction.or(t"…")} fraction digits permitted
          """

        case Reason.Absent(name) =>
          m"the element or attribute $name is required"

    case class Error(reason: Xml.Provider.Error.Reason)(using Diagnostics)
    extends fulminate.Error(993, reason.number)
      ( m"the XML did not conform to its schema because $reason" )

  abstract class Provider(schema0: into[Xml.Provider.Schema], root: Optional[Text] = Unset)
  extends Specification:
    type Origin = Xml
    type Form = Xml.Provider

    val xsd: Xsd = schema0.xsd

    private lazy val layout: Xml.Provider.Layout = Xml.Provider.layout(xsd, root)

    def fields: List[(Text, Member)] = layout.fields

    // The element a value stands for: the document element of a parse result, or the element
    private def elementOf(xml: Xml): Optional[Xml.Element] = xml match
      case element: Xml.Element               => element
      case Xml.Fragment(element: Xml.Element) => element

      case Xml.Fragment(nodes*) =>
        nodes.collectFirst { case element: Xml.Element => element }.optional

      case _ =>
        Unset

    private def nil(element: Xml.Element): Boolean =
      element.attribute(Xml.Name(Xsd.instance, t"nil")).lay(false): value =>
        value.s == "true" || value.s == "1"

    // The children of `parent` the field selects: by resolved name in the namespace the schema
    // expects of that child, or by local name alone where the schema says nothing about the
    // parent (a root reached through a reference the walk did not follow)
    private def children(parent: Xml.Element, name: Text): List[Xml.Element] =
      // `Option`, since an expected namespace may itself be absent
      val expected: Option[Xml.Name] =
        layout.elements.stdlib.get((parent.localName, name)).map(Xml.Name(_, name))

      val buffer = scm.ArrayBuffer[Xml.Element]()

      parent.children.extent.each: index =>
        parent.children(index) match
          case child: Xml.Element =>
            val matches = expected match
              case Some(expected) => child.qualified == expected
              case None           => child.localName == name

            if matches then buffer += child

          case _ =>
            ()

      List.from(buffer)

    private def attributeOf(parent: Xml.Element, name: Text): Optional[Text] =
      layout.attributes.stdlib.get((parent.localName, name)) match
        case Some(namespace) => parent.attribute(Xml.Name(namespace, name))
        case None            => parent.attributes.fetch(name)

    private def ownText(element: Xml.Element): Xml =
      val builder = StringBuilder()

      element.children.extent.each: index =>
        element.children(index) match
          case Xml.Text(text)  => builder.append(text.s)
          case Xml.Cdata(text) => builder.append(text.s)
          case _               => ()

      Xml.Text(builder.toString.tt)

    def access(name: Text, xml: Xml): Xml = elementOf(xml).lay(Xml.Absent): parent =>
      if name.s.startsWith("#") then ownText(parent)
      else if name.s.startsWith("@") then
        attributeOf(parent, name.s.substring(1).nn.tt).lay(Xml.Absent)(Xml.Text(_))
      else
        children(parent, name).prim match
          case child: Xml.Element => if nil(child) then Xml.Absent else child

          case _ =>
            val fallback = if name == t"text" then ownText(parent) else Xml.Absent
            attributeOf(parent, name).lay(fallback)(Xml.Text(_))

    def absent(xml: Xml): Boolean = xml eq Xml.Absent

    override def required(name: Text, xml: Xml): Xml =
      if absent(xml) then abort(Xml.Provider.Error(Xml.Provider.Error.Reason.Absent(name))) else xml

    def repeated(name: Text, xml: Xml): List[Xml] = elementOf(xml).lay(Nil): parent =>
      children(parent, name).filter(!nil(_)).map(identity[Xml](_))
