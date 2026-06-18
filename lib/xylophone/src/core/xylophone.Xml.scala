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

import language.dynamics

import java.lang as jl
import java.util as ju

import scala.collection.mutable as scm
import scala.compiletime.*
import scala.quoted.*

import adversaria.*
import anticipation.*
import contextual.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import parasite.*
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*
import typonym.*
import vacuous.*
import wisteria.*
import zephyrine.*

object Xml extends Tag.Container
  ( label = "xml", admissible = Set("head", "body") ), Format:
  // Controls how an `Xml` tree is serialized. `indent` is the whitespace unit emitted per nesting
  // level when indenting element-only content; `Unset` (the default) keeps everything on a single
  // line. `trailingNewline` appends a final newline. Bundled as `formatting.compactFormatting`
  // (the default) and `formatting.indentedFormatting`.
  object Formatting:
    def apply(indent: Optional[Text], trailingNewline: Boolean): Formatting =
      Basic(indent, trailingNewline)

    private case class Basic(indent: Optional[Text], trailingNewline: Boolean) extends Formatting

    given default: Formatting = apply(Unset, trailingNewline = false)

  trait Formatting extends zephyrine.Formatting:
    def indent: Optional[Text]
    def trailingNewline: Boolean

  type Topic = "xml"
  type Transport = "head" | "body"

  sealed trait Integral
  sealed trait Decimal
  sealed trait Id

  // Default sum-type discriminator for XML: the element's label is the
  // variant tag. A sealed trait with case classes `Book` and `Magazine` is
  // therefore decoded from `<book .../>` and `<magazine .../>` respectively
  // without any explicit discriminator field — the XML-idiomatic choice.
  given xmlDiscriminable: [value] => value is Discriminable in Xml = new Discriminable:
    type Form = Xml
    type Self = value

    def discriminate(xml: Xml): Optional[Text] = xml match
      case Element(label, _, _)           => label
      case Fragment(Element(label, _, _)) => label
      case _                              => Unset

    def rewrite(kind: Text, xml: Xml): Xml = xml match
      case Element(_, attrs, children)           => Element(kind, attrs, children)
      case Fragment(Element(_, attrs, children)) => Element(kind, attrs, children)
      case other                                 => other

    def variant(xml: Xml): Xml = xml

  // Identity-checked sentinel used by `DecodableDerivation.conjunction` to
  // signal "this field was absent from the XML". The textual decoder and
  // the primitive `Decodable in Xml` instances detect it by reference
  // equality and `raise` an `XmlError` while continuing with a zero / empty
  // sentinel, so multi-error accrual can register every missing field in
  // one decode rather than aborting at the first.
  private[xylophone] val Absent: Xml = new Fragment()

  // Extract the textual content of a leaf-shaped Xml node. Returns `Unset`
  // for non-text shapes (other Elements, Comments, the `Absent` sentinel,
  // etc.). Used by the primitive decoders so they can decide whether to
  // parse the content or `raise` for a missing / wrong-shape input.
  private def textOf(xml: Xml): Optional[Text] = xml match
    case _ if xml eq Absent                    => Unset
    case TextNode(text)                        => text
    case Element(_, _, IArray(TextNode(text))) => text
    case Element(_, _, IArray())               => t""
    case Fragment(node: Node)                  => textOf(node)
    case _                                     => Unset

  // Explicit `Decodable in Xml` for the common primitive scalars. These
  // *raise + continue* (record an `XmlError` on the ambient `Foci` and
  // return a zero / false sentinel) rather than `abort`ing, so a case
  // class with multiple bad fields accrues one error per field instead of
  // bailing at the first.
  //
  // Specific givens here shadow the generic `decodable` summonFrom below
  // for these types — that branch's `summon[Decodable in Text]` for
  // `Int` would otherwise `abort` with `NumberError` and break accrual.
  given int: Tactic[XmlError] => Int is Decodable in Xml = xml =>
    textOf(xml).let: text =>
      try Integer.parseInt(text.s).nn
      catch case _: NumberFormatException => raise(XmlError()) yet 0

    . or:
        raise(XmlError()) yet 0

  given long: Tactic[XmlError] => Long is Decodable in Xml = xml =>
    textOf(xml).let: text =>
      try jl.Long.parseLong(text.s).nn
      catch case _: NumberFormatException => raise(XmlError()) yet 0L

    . or:
        raise(XmlError()) yet 0L

  given short: Tactic[XmlError] => Short is Decodable in Xml = xml =>
    textOf(xml).let: text =>
      try jl.Short.parseShort(text.s).nn
      catch case _: NumberFormatException => raise(XmlError()) yet 0.toShort

    . or:
        raise(XmlError()) yet 0.toShort

  given byte: Tactic[XmlError] => Byte is Decodable in Xml = xml =>
    textOf(xml).let: text =>
      try jl.Byte.parseByte(text.s).nn
      catch case _: NumberFormatException => raise(XmlError()) yet 0.toByte

    . or:
        raise(XmlError()) yet 0.toByte

  given double: Tactic[XmlError] => Double is Decodable in Xml = xml =>
    textOf(xml).let: text =>
      try jl.Double.parseDouble(text.s).nn
      catch case _: NumberFormatException => raise(XmlError()) yet 0.0

    . or:
        raise(XmlError()) yet 0.0

  given float: Tactic[XmlError] => Float is Decodable in Xml = xml =>
    textOf(xml).let: text =>
      try jl.Float.parseFloat(text.s).nn
      catch case _: NumberFormatException => raise(XmlError()) yet 0.0f

    . or:
        raise(XmlError()) yet 0.0f

  given boolean: Tactic[XmlError] => Boolean is Decodable in Xml = xml =>
    textOf(xml).let: text =>
      text.s match
        case "true"  => true
        case "false" => false
        case _       => raise(XmlError()) yet false

    . or:
        raise(XmlError()) yet false

  // Single entry-point for resolving `Decodable in Xml`. Prefers a textual
  // decoder when one exists (so any `Decodable in Text` value works as a
  // field type); otherwise falls back to Wisteria-derived case-class /
  // sealed-trait decoding via `DecodableDerivation`.
  //
  // The textual branch raises `XmlError` (and continues with `""`) on the
  // `Absent` sentinel and on wrong-shape input. The inner
  // `Decodable in Text` is then called with that sentinel `""`; primitive
  // numerics have explicit `Decodable in Xml` givens above that pre-empt
  // this branch entirely, so the surviving callers here are types whose
  // textual decoders accept the empty string (e.g., `Text` itself,
  // user-defined identifiers).
  inline given decodable: [value] => value is Decodable in Xml = summonFrom:
    case given (`value` is Decodable in Text) =>
      xml =>
        provide[Tactic[XmlError]]:
          val text: Text =
            if xml eq Absent then raise(XmlError()) yet t""
            else textOf(xml).or(raise(XmlError()) yet t"")

          summon[`value` is Decodable in Text].decoded(text)

    case given Reflection[`value`] =>
      DecodableDerivation.derived

  // Single entry-point for resolving `Encodable in Xml`, the exact mirror of
  // `decodable` above. Prefers a textual encoder when one exists (so any
  // `Encodable in Text` value — `Text`, `Int`, `Long`, user identifiers, … —
  // becomes a leaf `TextNode`); otherwise falls back to Wisteria-derived
  // case-class / sealed-trait encoding via `EncodableDerivation`.
  inline given encodable: [value] => value is Encodable in Xml = summonFrom:
    case given (`value` is Encodable in Text) =>
      value => TextNode(value.encode)

    case given Reflection[`value`] =>
      EncodableDerivation.derived

  // Wisteria-based derivation for case classes (conjunction) and sealed
  // traits / enums (disjunction). Each field is decoded from the first
  // child element whose label matches the field name; sum-type variants
  // are picked from the element's own label via the `Discriminable`
  // default.
  //
  // `wisteria.label[Text]` is used in place of the bare `label` identifier
  // because `Tag.Container`'s `label = "xml"` constructor argument shadows
  // Wisteria's `label aka "label"` parameter inside this object.
  object DecodableDerivation extends Derivable[Decodable in Xml]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Decodable in Xml =

      xml =>
        provide[Foci[Xml.Focus]]:
          provide[Tactic[XmlError]]:
            xml match
              case e: Element           => buildWith[derivation](e)
              case Fragment(e: Element) => buildWith[derivation](e)

              case _ =>
                // Wrong-shape input (including the `Absent` sentinel an
                // outer conjunction passes in for a missing nested case-
                // class field). If the user supplied `Default[derivation]`
                // we register one error at the current focus and continue
                // with the sentinel — a missing nested case class lands
                // as a single error rather than expanding per sub-field.
                // Without a `Default`, we fall back to running `build`
                // against an empty children map so each sub-field accrues
                // its own missing-field error.
                summonFrom:
                  case derivationDefault: Default[`derivation`] =>
                    raise(XmlError()) yet derivationDefault()

                  case _ =>
                    raise(XmlError())
                    buildWith[derivation](Element(t"", Attributes.empty, IArray.empty))

    private inline def buildWith[derivation <: Product: ProductReflection]
      ( element: Element )
      ( using Foci[Xml.Focus], Tactic[XmlError] )
    :   derivation =

      // Fields marked `@attribute` are read from the element's attributes
      // rather than its child elements, mirroring the encoder.
      val attributeFields: Map[Text, Set[attribute]] = fieldAnnotations[derivation, attribute]

      // `@name[Xml]` / bare `@name` renames: field name -> element/attribute
      // name on the wire. Read back the same way they are written.
      val renames: Map[Text, Text] = relabelling[derivation, Xml]

      val children: scm.HashMap[String, Element] = scm.HashMap.empty
      var i = 0

      while i < element.children.length do
        element.children(i) match
          case child: Element =>
            val childLabel = child.label.s

            if !children.contains(childLabel) then
              children.update(childLabel, child)

          case _ => ()

        i += 1

      build[derivation]: [field] =>
        context =>
          val fieldLabel: Text = wisteria.label[Text]
          val wireName: Text = renames.at(fieldLabel).or(fieldLabel)
          focus({
            // Each outer `focus` runs *after* the inner one, so we
            // extend `prior` at the root side (`/outer/inner`), not the
            // leaf side that `XPath#element` would use.
            val base = prior.let(_.path).or(XPath())
            Xml.Focus(base.prepend(wireName, 1))
          }):
            if attributeFields.contains(fieldLabel) then
              // `@attribute` field: decode from the matching attribute as a
              // `TextNode`; a missing attribute falls back to the declared
              // default, else the `Absent` sentinel (raise + continue).
              element.attributes.at(wireName).lay(default.or(context.decoded(Absent))): text =>
                context.decoded(TextNode(text))
            else
              children.get(wireName.s) match
                case Some(child) => context.decoded(child)
                // Missing field: fall back to the case-class declared
                // default (Wisteria's `default`); if absent, hand the
                // `Absent` sentinel to the field's decoder. Primitives
                // detect it and `raise + continue`; nested conjunctions
                // detect it and may further short-circuit via a
                // user-supplied `Default[Nested]`.
                case None        => default.or(context.decoded(Absent))

    // Sealed-trait disjunction picks a variant by element label. We screen
    // the discriminator against `variantLabels` *before* `delegate`-ing so
    // an unrecognised label doesn't punch through as `VariantError` — it
    // gets the same `Default[derivation]`-or-abort treatment as a missing
    // discriminator. When the user supplies `Default[derivation]` we
    // register one error at the current focus and continue with the
    // sentinel; without one we abort.
    inline def disjunction[derivation: SumReflection]
    :   derivation is Decodable in Xml =

      xml =>
        provide[Foci[Xml.Focus]]:
          provide[Tactic[XmlError]]:
            provide[Tactic[VariantError]]:
              val discriminable = infer[derivation is Discriminable in Xml]
              val labels: List[Text]    = variantLabels

              // `@name[Xml]` / bare `@name` variant renames: map the serialized
              // element label back to the variant name before delegating.
              val variantNames: Map[Text, Text] =
                variantRelabelling[derivation, Xml].map: (variant, wire) => wire -> variant

              val resolved: Optional[Text] =
                discriminable.discriminate(xml).let: wire =>
                  val discriminant = variantNames.getOrElse(wire, wire)
                  if labels.contains(discriminant) then discriminant else Unset

              resolved.let: discriminant =>
                delegate(discriminant): [variant <: derivation] =>
                  context => context.decoded(xml)

              . or:
                  summonFrom:
                    case derivationDefault: Default[`derivation`] =>
                      raise(XmlError()) yet derivationDefault()

                    case _ =>
                      abort(XmlError())

  // Wisteria-based encoder, the mirror of `DecodableDerivation`. A product
  // encodes to an `Element` labelled with the type's short name (`typeName`);
  // each field becomes a child `Element` labelled with the field name via
  // `wrap` — the exact inverse of the decoder reading a field from the child
  // element of that name. A sum encodes its selected variant and relabels the
  // resulting element to the variant name, so it round-trips through the
  // `Discriminable`-by-label default that `disjunction` reads back.
  //
  // A field marked `@attribute` is written to the element's attributes rather
  // than as a child element, and read back the same way by the decoder, so it
  // round-trips. The annotation is read at derivation time via adversaria's
  // `Annotated by attribute`.
  //
  // As in `DecodableDerivation`, `wisteria.label[Text]` is used in place of the
  // bare `label` identifier, which `Tag.Container`'s `label = "xml"` shadows.
  object EncodableDerivation extends Derivable[Encodable in Xml]:

    // Relabel an encoded field value to its field name and guarantee an
    // `Element` wrapper, so it decodes back from `<fieldName>…</fieldName>`.
    private def wrap(fieldName: Text, encoded: Xml): Node = encoded match
      case Element(_, attributes, children)           => Element(fieldName, attributes, children)
      case Fragment(Element(_, attributes, children)) => Element(fieldName, attributes, children)

      case Fragment(nodes*) =>
        Element(fieldName, Attributes.empty, nodes.toArray.immutable(using Unsafe))

      case node: Node =>
        Element(fieldName, Attributes.empty, IArray(node))

    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Encodable in Xml =

      value =>
        val attributeFields: Map[Text, Set[attribute]] = fieldAnnotations[derivation, attribute]

        // `@name[Xml]` / bare `@name` renames: field name -> element/attribute
        // name on the wire.
        val renames: Map[Text, Text] = relabelling[derivation, Xml]

        val attributes: scm.ArrayBuffer[(Text, Text)] = scm.ArrayBuffer()
        val children: scm.ArrayBuffer[Node] = scm.ArrayBuffer()

        fields(value): [field] =>
          field =>
            val fieldLabel: Text = wisteria.label[Text]
            val wireName: Text = renames.at(fieldLabel).or(fieldLabel)
            val encoded: Xml = contextual.encode(field)

            // `@attribute` fields become attributes carrying the encoded leaf's
            // text; every other field becomes a child element via `wrap`.
            if attributeFields.contains(fieldLabel)
            then attributes += wireName -> textOf(encoded).or(t"")
            else children += wrap(wireName, encoded)

        Element
          ( typeName,
            Attributes(attributes.toSeq*),
            children.toArray.immutable(using Unsafe) )

    inline def disjunction[derivation: SumReflection]: derivation is Encodable in Xml =
      value =>
        val discriminable = infer[derivation is Discriminable in Xml]

        // `@name[Xml]` / bare `@name` variant renames: variant name -> element
        // label, read back the same way by the decoder.
        val variantNames: Map[Text, Text] = variantRelabelling[derivation, Xml]

        variant(value): [variant <: derivation] =>
          value =>
            val label = wisteria.label[Text]
            discriminable.rewrite(variantNames.getOrElse(label, label), contextual.encode(value))

  case class attribute() extends StaticAnnotation

  case class XmlAttribute(label: Text, elements: Set[Text], global: Boolean):
    type Self <: Label
    type Topic
    type Plane <: Label

    def targets(tag: Text): Boolean = global || elements(tag)

    def merge(that: XmlAttribute): XmlAttribute =
      XmlAttribute(label, elements ++ that.elements, global || that.global)

  def header: Header = Header("1.0", Unset, Unset)

  extension (xml: Seq[Xml])
    def nodes: IArray[Node] =
      var count = 0

      for item <- xml do item match
        case fragment: Fragment => count += fragment.nodes.length
        case _                  => count += 1

      val array = new Array[Node](count)

      var index = 0

      for item <- xml do item match
        case Fragment(nodes*) =>
          for node <- nodes do
          array(index) = node

          index += 1

        case node: Node =>
          array(index) = node
          index += 1

      array.immutable(using Unsafe)

  inline given interpolator: Xml is Interpolable:
    type Result = Xml

    transparent inline def interpolate[parts <: Tuple, origins <: Tuple]
      ( inline insertions: Any* )
    :   Xml =

      ${xylophone.internal.interpolator[parts, origins]('insertions)}

  inline given extrapolator: Xml is Extrapolable:

    transparent inline def extrapolate[parts <: Tuple, origins <: Tuple](scrutinee: Xml)
    :   Boolean | Option[Tuple | Xml] =

      ${xylophone.internal.extractor[parts, origins]('scrutinee)}


  given aggregable: [content <: Label: Reifiable to List[String]] => (schema: XmlSchema)
  =>  Tactic[ParseError]
  =>  (Xml of content) is Aggregable by Text =

    input => XmlParser.fromIterator(input.iterator).parseXml(headers0 = false).of[content]

  given aggregable2: (schema: XmlSchema) => Tactic[ParseError] => Xml is Aggregable by Text =
    input => XmlParser.fromIterator(input.iterator).parseXml(headers0 = false)

  // HTTP content-type integration. `Abstractable across HttpStreams` makes an
  // `Xml` value usable as an HTTP request/response body (telekinesis derives
  // `Postable`/`Servable` from it); `Instantiable across HttpRequests` reads a
  // request/response body back into `Xml`.
  given abstractable: (encoder: CharEncoder)
  =>  Xml is Abstractable across HttpStreams to HttpStreams.Content =

    new Abstractable:
      type Self = Xml
      type Domain = HttpStreams
      type Result = HttpStreams.Content

      def genericize(xml: Xml): HttpStreams.Content =
        (t"application/xml; charset=${encoder.encoding.name}", Stream(xml.show.data))

  given instantiable: (schema: XmlSchema) => Tactic[ParseError]
  =>  Xml is Instantiable across HttpRequests from Text =

    text => Stream(text).read[Xml]

  // `source.read[Foo in Xml]` shorthand for
  // `source.read[Xml].as[Foo]`. Mirrors `jacinta`'s `aggregableDirect`
  // for `value in Json`. The `Form` type-tag is added by an
  // `asInstanceOf` cast — `value in Xml` is just `value { type
  // Form = Xml }` so the cast is a no-op at runtime.
  given aggregableIn: [value: Decodable in Xml] => (schema: XmlSchema)
  =>  ( Tactic[ParseError], Tactic[XmlError] )
  =>  (value in Xml) is Aggregable by Text =

    input =>
      XmlParser.fromIterator(input.iterator).parseXml(headers0 = false).as[value]
      . asInstanceOf[value in Xml]

  given loadable: (schema: XmlSchema) => Tactic[ParseError] => Xml is Loadable by Text = stream =>
    XmlParser.fromIterator(stream.iterator).parseXml(headers0 = true) match
      case Fragment((header: Header), rest*) =>
        if rest.nil then abort(ParseError(Xml, Position(1.u, 1.u), Issue.BadDocument))
        else if rest.length == 1 then Document(rest.head, header)
        else Document(Fragment(rest*), header)

      case _: Header =>
        abort(ParseError(Xml, Position(1.u, 1.u), Issue.BadDocument))

      case fragment: Fragment =>
        Document(fragment, Xml.header)

      case node: Node =>
        Document(node, Xml.header)

  given streamable: (Monitor, Probate) => Document[Xml] is Streamable by Text =
    emit(_).to(Stream)

  // Tracking-mode parse entry points. Build the parser with a line-feed
  // tracking `Lineation` in its cursor, parse, then bundle the resulting
  // `Xml` with the position index the parser emitted along the way.
  // `positionIndex` is `Unset` if the input contains no root element.
  def parseTracked(text: Text)(using schema: XmlSchema, tactic: Tactic[ParseError]): Tracked =
    val parser = XmlParser.fromTextTracked(text)
    val xml = parser.parseXml(headers0 = false)
    val index = parser.rootIndex
    Tracked(xml, PositionIndex(if index == null then IArray.empty[Int] else index))

  def parseTracked
    (input: Iterator[Text])(using schema: XmlSchema, tactic: Tactic[ParseError])
  :   Tracked =

    val parser = XmlParser.fromIteratorTracked(input)
    val xml = parser.parseXml(headers0 = false)
    val index = parser.rootIndex
    Tracked(xml, PositionIndex(if index == null then IArray.empty[Int] else index))

  def emit(document: Document[Xml])
    ( using formatting: Formatting, monitor: Monitor, probate: Probate )
  :   Iterator[Text] =

    val producer = Producer[Text](4096)

    async:
      writeDocument(producer, formatting, document)
      producer.finish()

    producer.iterator

  private def writeDocument
    ( producer: Producer[Text], formatting: Formatting, document: Document[Xml] )
  :   Unit =

    writeXml(producer, formatting, document.metadata, 0)
    if formatting.indent.present then producer.put("\n")
    writeXml(producer, formatting, document.root, 0)
    if formatting.trailingNewline then producer.put("\n")

  // Escape text content so the result is well-formed and round-trips exactly. `&` and `<` must be
  // escaped; `>` is escaped too so the `]]>` sequence can never appear; and a carriage return is
  // written as a character reference so XML line-ending normalization cannot rewrite it to `\n`.
  private def writeEscapedText(producer: Producer[Text], text: Text): Unit =
    val source = text.s
    val length = source.length
    var start = 0
    var index = 0

    inline def escape(entity: Text): Unit =
      if index > start then producer.put(text, start.z, index - start)
      producer.put(entity)
      start = index + 1

    while index < length do
      source.charAt(index) match
        case '&'  => escape(t"&amp;")
        case '<'  => escape(t"&lt;")
        case '>'  => escape(t"&gt;")
        case '\r' => escape(t"&#xD;")
        case _    => ()

      index += 1

    if length > start then producer.put(text, start.z, length - start)

  // Escape an attribute value delimited by double quotes. Besides the text escapes, the delimiter
  // and the whitespace characters tab, line feed and carriage return become character references,
  // since attribute-value normalization would otherwise collapse literal whitespace to spaces.
  private def writeEscapedAttribute(producer: Producer[Text], text: Text): Unit =
    val source = text.s
    val length = source.length
    var start = 0
    var index = 0

    inline def escape(entity: Text): Unit =
      if index > start then producer.put(text, start.z, index - start)
      producer.put(entity)
      start = index + 1

    while index < length do
      source.charAt(index) match
        case '&'  => escape(t"&amp;")
        case '<'  => escape(t"&lt;")
        case '>'  => escape(t"&gt;")
        case '"'  => escape(t"&quot;")
        case '\t' => escape(t"&#x9;")
        case '\n' => escape(t"&#xA;")
        case '\r' => escape(t"&#xD;")
        case _    => ()

      index += 1

    if length > start then producer.put(text, start.z, length - start)

  // The single, spec-correct XML serializer. `emit` drives it through a streaming `Producer` and
  // `showable` through a synchronous one, so the two never drift. When the `Formatting` carries
  // an `indent`, element-only content is laid out one child per indented line; an element that
  // contains any character data is kept inline so its text is never altered.
  private def writeXml(producer: Producer[Text], formatting: Formatting, node: Xml, depth: Int)
  :   Unit =

    node match
      case Fragment(nodes*) =>
        nodes.each(writeXml(producer, formatting, _, depth))

      case TextNode(text) =>
        writeEscapedText(producer, text)

      case Comment(comment) =>
        producer.put("<!--")
        producer.put(comment)
        producer.put("-->")

      case Cdata(text) =>
        producer.put("<![CDATA[")
        producer.put(text)
        producer.put("]]>")

      case Doctype(text) =>
        producer.put("<!DOCTYPE ")
        producer.put(text)
        producer.put(">")

      case ProcessingInstruction(target, data) =>
        producer.put("<?")
        producer.put(target)

        if !data.nil then
          producer.put(" ")
          producer.put(data)

        producer.put("?>")

      case Header(version, encoding, standalone) =>
        producer.put("<?xml version=\"")
        producer.put(version)
        producer.put("\"")

        encoding.let: encoding =>
          producer.put(" encoding=\"")
          producer.put(encoding)
          producer.put("\"")

        standalone.let: standalone =>
          producer.put(if standalone then " standalone=\"yes\"" else " standalone=\"no\"")

        producer.put("?>")

      case Element(label, attributes, children) =>
        producer.put("<")
        producer.put(label)

        if !attributes.nil then attributes.each: (key, value) =>
          producer.put(" ")
          producer.put(key)
          producer.put("=\"")
          writeEscapedAttribute(producer, value)
          producer.put("\"")

        if children.nil then producer.put("/>") else
          producer.put(">")

          if formatting.indent.present && !children.exists(textual) then
            children.each: child =>
              newline(producer, formatting, depth + 1)
              writeXml(producer, formatting, child, depth + 1)

            newline(producer, formatting, depth)
          else
            children.each(writeXml(producer, formatting, _, depth))

          producer.put("</")
          producer.put(label)
          producer.put(">")

  // Character data (text or CDATA) forces an element to be serialized inline, so indentation
  // whitespace can never alter its content.
  private def textual(node: Xml): Boolean = node match
    case _: TextNode => true
    case _: Cdata    => true
    case _           => false

  // In indented mode, emit a newline followed by `depth` indent units.
  private def newline(producer: Producer[Text], formatting: Formatting, depth: Int): Unit =
    formatting.indent.let: unit =>
      producer.put("\n")
      var i = 0

      while i < depth do
        producer.put(unit)
        i += 1

  given showable: [xml <: Xml] => (formatting: Formatting) => xml is Showable = node =>
    Producer.collect[Text](): producer =>
      writeXml(producer, formatting, node, 0)
      if formatting.trailingNewline then producer.put("\n")


  private enum Token:
    case Close, Comment, Empty, Open, Header, Cdata, Pi, Doctype

  private enum Level:
    case Ascend, Descend, Peer

  trait Populable:
    node: Element =>
      def apply(children: Optional[Xml of (? <: node.Transport)]*): Element of node.Topic =
        new Element(node.label, node.attributes, children.compact.nodes):
          type Topic = node.Topic

  import Issue.*
  def name: Text = t"XML"

  given text: [label >: "#text" <: Label] => Conversion[Text, Xml of label] =
    TextNode(_).of[label]

  given string: [label >: "#text" <: Label] => Conversion[String, Xml of label] =
    string => TextNode(string.tt).of[label]


  given conversion3: [label <: Label, content >: label <: Label]
  =>  Conversion[Xml of label, Xml of content] =

    _.of[content]


  given comment: [content <: Label] =>  Conversion[Comment, Xml of content] =
    _.of[content]

  given xmlConversion: [value: Encodable in Xml] => Conversion[value, Xml] =
    value.encoded(_)

  given sequences: [nodal, xml <: Xml] => (conversion: Conversion[nodal, xml])
  =>  Conversion[Seq[nodal], Seq[xml]] =

    (sequence: Seq[nodal]) =>
      sequence.map(conversion(_))

  enum Issue extends Format.Issue:
    case BadInsertion
    case ExpectedMore
    case BadDocument
    case UnquotedAttribute
    case InvalidTag(name: Text)
    case InvalidTagStart(prefix: Text)
    case DuplicateAttribute(name: Text)
    case InadmissibleTag(name: Text, parent: Text)
    case OnlyWhitespace(char: Char)
    case Unexpected(char: Char)
    case UnknownEntity(name: Text)
    case ForbiddenUnquoted(char: Char)
    case MismatchedTag(open: Text, close: Text)
    case UnopenedTag(close: Text)
    case Incomplete(tag: Text)
    case UnknownAttribute(name: Text)
    case UnknownAttributeStart(name: Text)
    case InvalidAttributeUse(attribute: Text, element: Text)

    def describe: Message = this match
      case BadInsertion                   => m"a value cannot be inserted into XML at this point"
      case ExpectedMore                   => m"the content ended prematurely"
      case BadDocument                    => m"the document did not contain a single root tag"
      case UnquotedAttribute              => m"the attribute value must be single- or double-quoted"
      case InvalidTag(name)               => m"<$name> is not a valid tag"
      case InvalidTagStart(prefix)        => m"there is no valid tag whose name starts $prefix"
      case DuplicateAttribute(name)       => m"the attribute $name already exists on this tag"
      case InadmissibleTag(name, parent)  => m"<$name> cannot be a child of <$parent>"
      case Unexpected(char)               => m"the character $char was not expected"
      case UnknownEntity(name)            => m"the entity &$name is not defined"
      case UnopenedTag(close)             => m"the tag </$close> has no corresponding opening tag"
      case Incomplete(tag)                => m"the content ended while the tag <$tag> was left open"
      case UnknownAttribute(name)         => m"$name is not a recognized attribute"
      case UnknownAttributeStart(name)    => m"there is no valid attribute whose name starts $name"
      case InvalidAttributeUse(name, tag) => m"the attribute $name cannot be used on the tag <$tag>"

      case MismatchedTag(open, close) =>
        m"the tag </$close> did not match the opening tag <$open>"

      case ForbiddenUnquoted(char) =>
        m"the character $char is forbidden in an unquoted attribute"

      case OnlyWhitespace(char) =>
        m"the character $char was found where only whitespace is permitted"

  case class Position
    ( line:                Ordinal,
      column:              Ordinal,
      override val offset: Optional[Int] = Unset,
      override val length: Optional[Int] = Unset )
  extends Format.Position:
    def describe: Text = t"line ${line.n1}, column ${column.n1}"
    override def span: Span = Span.line(line, column, length.or(0))

  // All internal references in a `PositionIndex` are stored as offsets
  // relative to the start of the containing element descriptor, so any
  // slice extracted at a descriptor boundary is itself a valid
  // `PositionIndex`. See `XmlParser` (tracking mode) for the layout and
  // `Xml.Tracked#locate` for the navigation algorithm.
  opaque type PositionIndex = IArray[Int]

  object PositionIndex:
    private[xylophone] def apply(data: IArray[Int]): PositionIndex = data

  extension (positionIndex: PositionIndex)
    private[xylophone] def ints: IArray[Int] = positionIndex

  // Focus value tracked by Xylophone's path-aware decoders / encoders.
  // `path` is the XPath to the current node; `position` is the source
  // line/column/length, populated when the root `Xml` was produced by
  // `parseTracked` and `Unset` otherwise.
  case class Focus(path: XPath, position: Optional[Xml.Position] = Unset)
  derives CanEqual:

    def withPosition(tracked: Tracked): Focus =
      copy(position = tracked.locate(path))

  object Tracked:
    // `XPath.path.descent` is stored leaf-first (Serpentine's `/`
    // prepends), so the walker iterates it in reverse to descend
    // root-to-leaf. The XPath convention is `/foo/bar/@attr`, so the
    // first element step names the *root* element itself (no descent
    // into children) and subsequent steps descend.
    private def walk
      ( xml:      Xml,
        data:     IArray[Int],
        offset:   Int,
        segments: IndexedSeq[Text],
        i:        Int )
    :   Optional[Position] =

      if i >= segments.length then
        Position(data(offset + 1).z, data(offset + 2).z, length = data(offset + 3))
      else
        val segment = segments(segments.length - 1 - i)

        XPath.parseStep(segment) match
          case Unset => Unset

          case step => step.asInstanceOf[Either[Text, (Text, Int)]] match
            case Left(attrName) =>
              xml match
                case element: Element => attrPosition(element, data, offset, attrName)
                case _                => Unset

            case Right((name, ordinal)) =>
              xml match
                case element: Element if i == 0 =>
                  // First step names the document's root element.
                  if element.label == name && ordinal == 1 then
                    walk(element, data, offset, segments, i + 1)
                  else
                    Unset

                case element: Element =>
                  descend(element, name, ordinal).let: childElementIndex =>
                    val attrCount = data(offset + 4)
                    val offSlot = offset + 6 + attrCount + childElementIndex
                    val childOff = data(offSlot)
                    val child = descendAst(element, name, ordinal).vouch
                    walk(child, data, offset + childOff, segments, i + 1)

                case _ =>
                  Unset

    private def attrPosition
      ( element:  Element,
        data:     IArray[Int],
        offset:   Int,
        attrName: Text )
    :   Optional[Position] =

      val keys: Vector[Text] = element.attributes.keys.toVector
      val i = keys.indexOf(attrName)

      if i < 0 then Unset
      else
        val attrOff = data(offset + 6 + i)
        val base = offset + attrOff
        Position(data(base + 1).z, data(base + 2).z, length = data(base + 3))

    // Find the position of the n-th (1-indexed) child element with the
    // given name among the child *elements only* (ignoring text, comment,
    // CDATA, PI and Doctype children). Returns the element-index used to
    // look up the offset in the element descriptor's offset table.
    private def descend(element: Element, name: Text, ordinal: Int): Optional[Int] =
      val children = element.children
      var i = 0
      var elementIndex = 0
      var seen = 0
      var found: Optional[Int] = Unset

      while i < children.length && found == Unset do
        children(i) match
          case child: Element =>
            if child.label == name then
              seen += 1
              if seen == ordinal then found = elementIndex

            elementIndex += 1

          case _ => ()

        i += 1

      found

    private def descendAst(element: Element, name: Text, ordinal: Int): Optional[Element] =
      val children = element.children
      var i = 0
      var seen = 0
      var found: Optional[Element] = Unset

      while i < children.length && found == Unset do
        children(i) match
          case child: Element if child.label == name =>
            seen += 1
            if seen == ordinal then found = child

          case _ => ()

        i += 1

      found

  // Wraps a parsed `Xml` with a parallel position index produced by
  // `parseTracked`. Untracked parses keep returning bare `Xml`.
  //
  // Element descriptor layout:
  //
  //   [ size, line, column, sourceLength,
  //     attrCount, elemCount,
  //     attrOff_0, …, attrOff_{a-1},
  //     elemOff_0, …, elemOff_{e-1},
  //     <attribute descriptors>,
  //     <child element descriptors> ]
  //
  // Attribute descriptor layout: [ size=4, line, column, length ].
  // All offsets are relative to the start of the containing element
  // descriptor; any slice at a descriptor boundary is itself a valid
  // `PositionIndex`.
  case class Tracked(value: Xml, positionIndex: PositionIndex):
    def locate(path: XPath): Optional[Xml.Position] =
      Tracked.walk(value, positionIndex.ints, 0, path.path.descent.toIndexedSeq, 0)

    // Decode like `Xml#as` but also populate `position` on every
    // accumulated `Xml.Focus` by looking up its XPath against this
    // tracked root's position index. Outside `validate[Xml.Focus]` the
    // ambient `Foci` is a no-op and `supplement` does nothing, so this
    // call stays cost-free.
    def as[result: Decodable in Xml]: result tracks Xml.Focus =
      val decoded = value match
        case Fragment(inner) => result.decoded(inner)
        case xml: Xml        => result.decoded(xml)

      val foci = summon[Foci[Xml.Focus]]
      val tracked = this
      foci.supplement(foci.length, _.let(_.withPosition(tracked)).vouch)
      decoded

  enum Hole:
    case Text, Tagbody, Comment
    case Element(tag: Text)
    case Attribute(tag: Text, attribute: Text)
    case Node(parent: Text)

  // ───────────────────────────────────────────────────────────────────────
  // Unified parser: a single algorithm split across substrates.
  //
  // The abstract `XmlParser` base implements the entire XML parsing
  // algorithm (tags, attributes, text, entities, comments, CDATA,
  // processing instructions, doctype, header) in terms of a small
  // substrate API: `more`/`peek`/`advance`, `position`, `begin`/`slice`/
  // `reset`/`appendSlice`, and `computePosition` (for lazy line/column on
  // error). Two concrete substrates supply that API:
  //
  //   * `XmlDirect`   — operates directly on an underlying `String` with a
  //                     `var pos`. Used by `aggregable` / `loadable`. Faster
  //                     because all primitives are trivial integer
  //                     arithmetic; `final` lets the JIT devirtualise them.
  //   * `XmlStreaming` — operates over a `Cursor[Text]` against an
  //                     `Iterator[Text]`. Used by macro interpolators
  //                     (which need callbacks for `\u0000` placeholders);
  //                     handles unbounded streaming inputs.
  //
  // Both substrates share the same parsing algorithm — schema-validation,
  // header parsing, error reporting, entity expansion etc. all live in the
  // base class.

  private[xylophone] object XmlParser:
    // Use untracked lineation in the cursor: avoids a per-`advance` branch
    // (newline detection) and a per-`mark` write into the cursor's parallel
    // offsets array. Errors still carry an accurate absolute `offset` /
    // `length` span (which is what tests assert on and what users need to
    // pinpoint the failure), but `line` / `column` stay at 1/1. Acceptable
    // trade: error quality remains useful while parsing-throughput improves.

    def fromText(text: Text)(using XmlSchema): XmlParser =
      new XmlParser(Cursor[Text](text), tracking = false)

    def fromIterator(input: Iterator[Text])(using XmlSchema): XmlParser =
      new XmlParser(Cursor[Text](input), tracking = false)

    // Tracking-mode constructors build the cursor with a `\n`-aware
    // `Lineation` so `cursor.line` / `cursor.column` reflect real source
    // coordinates as soon as `XmlParser.reconcileLineation()` is called.
    // The parser's hot loop still bypasses lineation via `unsafeAdvanceBy`;
    // reconciliation happens only at element / attribute capture points
    // and before any refill in `moreSlow`.
    def fromTextTracked(text: Text)(using XmlSchema): XmlParser =
      import zephyrine.lineation.linefeedChars
      new XmlParser(Cursor[Text](text), tracking = true)

    def fromIteratorTracked(input: Iterator[Text])(using XmlSchema): XmlParser =
      import zephyrine.lineation.linefeedChars
      new XmlParser(Cursor[Text](input), tracking = true)

  private[xylophone] final class XmlParser
    ( cursor:                   Cursor[Text],
     protected[xylophone] val tracking: Boolean )
    ( using schema: XmlSchema ):
    type Region = Cursor.Mark

    private var heldToken: Cursor.Held | Null = null

    // Parser-shared scratch buffer for attribute accumulation (lifetime of
    // the `XmlParser` instance). Stores key/value pairs interleaved as
    // `[k0, v0, k1, v1, ...]`. `readAttributes()` writes here and snapshots
    // the populated prefix into a freshly-sized `IArray[String]` to wrap
    // as the opaque `Attributes`. Geometric growth.
    private var attrBuf: Array[String] = new Array[String](16)

    // Pool of `ArrayBuffer[Node]` instances re-used across recursive
    // `readChildren` calls. Each nesting level borrows one, fills it, copies
    // its contents into an `IArray[Node]`, and returns it. Pool grows on
    // demand to the deepest nesting depth seen. Avoids one
    // `ArrayBuffer[Node]` allocation per element (plus its backing array)
    // for repetitive record-shaped XML.
    private var nodeBufferId: Int = -1

    private val nodeBuffers: scala.collection.mutable.ArrayBuffer
      [ scala.collection.mutable.ArrayBuffer[Node] ] =
      scala.collection.mutable.ArrayBuffer.empty

    // Small open-addressed cache for repeating tag names. Record-shape XML
    // (the dominant workload) reuses the same handful of element labels
    // hundreds of times per document. Names of up to 16 ASCII chars are
    // packed losslessly into a `(packedLow, packedHigh)` Long pair (one byte
    // per char, trailing positions zero). Since `isNameStart` requires a
    // letter/`_`/`:` and `isNameChar` excludes `\0`, every distinct ASCII
    // name produces a distinct pair, so the lookup is two Long equality
    // checks — no byte-by-byte compare, no hash-collision false positives.
    // Non-ASCII names (chars ≥ 128) and names longer than 16 chars bypass
    // the cache and allocate normally.
    private inline val TagCacheSize = 64
    private inline val TagCacheMaxChars = 16
    private val tagCache:     Array[Text | Null] = new Array(TagCacheSize)
    private val tagCacheLow:  Array[Long]        = new Array(TagCacheSize)
    private val tagCacheHigh: Array[Long]        = new Array(TagCacheSize)

    private inline def getNodeBuffer(): scala.collection.mutable.ArrayBuffer[Node] =
      nodeBufferId += 1

      if nodeBuffers.length <= nodeBufferId then
        val newBuffer = scala.collection.mutable.ArrayBuffer.empty[Node]
        nodeBuffers += newBuffer
        newBuffer
      else
        val buffer = nodeBuffers(nodeBufferId)
        buffer.clear()
        buffer

    private inline def relinquishNodeBuffer(): Unit = nodeBufferId -= 1

    // ─── tracking-mode bookkeeping ─────────────────────────────────────────
    //
    // Per-nesting-level pool of `ArrayBuffer[Int]` index buffers, mirroring
    // `nodeBuffers`. Each `readElementTracked` call acquires up to three
    // scratch buffers: one for attribute descriptors, one for child
    // element descriptors back-to-back, and one for child end positions
    // within the scratch. The buffer pool grows to the deepest nesting
    // depth seen and is reused across parses on the same `XmlParser`.
    private var indexBufferId: Int = -1

    private val indexBuffers: scala.collection.mutable.ArrayBuffer
      [ scala.collection.mutable.ArrayBuffer[Int] ] =
      scala.collection.mutable.ArrayBuffer.empty

    private inline def getIndexBuffer(): scala.collection.mutable.ArrayBuffer[Int] =
      indexBufferId += 1

      if indexBuffers.length <= indexBufferId then
        val nu = scala.collection.mutable.ArrayBuffer.empty[Int]
        indexBuffers += nu
        nu
      else
        val buf = indexBuffers(indexBufferId)
        buf.clear()
        buf

    private inline def relinquishIndexBuffer(): Unit = indexBufferId -= 1

    // Finalised root-level position index produced by the previous
    // tracking-mode parse. Reset on every parse entry. Read by the
    // `XmlParser.fromText/Iterator(Tracked)` callers.
    protected[xylophone] var rootIndex: IArray[Int] | Null = null

    // Local-buffer offset up to which `cursor.line` / `cursor.column` have
    // been brought up to date. The hot-loop `syncTo()` bypasses the
    // cursor's lineation tracking via `unsafeAdvanceBy`, so the parser
    // catches lineation up at tracking-mode capture points and before
    // any refill that would discard consumed bytes.
    private var lineationPos: Int = cursor.unsafePos(using Unsafe)

    private def reconcileLineation(): Unit =
      val end = cursor.unsafePos(using Unsafe)

      if lineationPos < end then
        var i = lineationPos
        var newlines = 0
        var lastNewlineAt = -1

        while i < end do
          if bytes(i) == '\n' then
            newlines += 1
            lastNewlineAt = i

          i += 1

        if newlines > 0 then
          cursor.unsafeBumpLine(newlines)(using Unsafe)
          cursor.unsafeSetColumn(end - lastNewlineAt - 1)(using Unsafe)
        else
          cursor.unsafeBumpColumn(end - lineationPos)(using Unsafe)

        lineationPos = end

    // Assemble an element descriptor in `out`. `attrDescs` and `attrEnds`
    // hold attribute descriptors back-to-back and their end positions
    // within `attrDescs`. `childDescs` and `childEnds` hold child element
    // descriptors / their end positions the same way. See the layout
    // comment on `Xml.Tracked`.
    private def emitElementDescriptor
      ( out:         scala.collection.mutable.ArrayBuffer[Int],
        attrDescs:   scala.collection.mutable.ArrayBuffer[Int],
        attrEnds:    scala.collection.mutable.ArrayBuffer[Int],
        childDescs:  scala.collection.mutable.ArrayBuffer[Int],
        childEnds:   scala.collection.mutable.ArrayBuffer[Int],
        startLine:   Int,
        startColumn: Int,
        startMark:   Long )
    :   Unit =

      syncTo()
      val attrCount = attrEnds.length
      val elemCount = childEnds.length
      val sourceLength = (cursor.position.n0 - startMark).toInt
      val sizeSlot = out.length

      out += 0
      out += startLine
      out += startColumn
      out += sourceLength
      out += attrCount
      out += elemCount

      val headerSize = 6 + attrCount + elemCount

      // Attribute offsets first, then element offsets.
      var i = 0
      var prevEnd = 0

      while i < attrCount do
        out += headerSize + prevEnd
        prevEnd = attrEnds(i)
        i += 1

      i = 0
      prevEnd = attrEnds.lastOption.getOrElse(0)
      val attrsTotal = attrEnds.lastOption.getOrElse(0)

      while i < elemCount do
        out += headerSize + attrsTotal + prevEnd
        prevEnd = childEnds(i)
        i += 1

      out ++= attrDescs
      out ++= childDescs
      out(sizeSlot) = out.length - sizeSlot

    // ─── parser-local snapshot of the cursor's buffer / position ───────────
    //
    // The cursor remains the source of truth at refill, mark, slice and
    // error points, but for the per-char hot loops (`peek`, `advance`,
    // `more`) the parser maintains its own snapshot of the current buffer
    // reference, read position, and write end. Keeping all three as parser
    // fields rather than re-reading them through cursor accessors on every
    // char lets the JIT keep them in registers across long inner loops —
    // the same trick Jacinta uses for its tight number / string scans.
    //
    // Invariant: between `syncTo()` and `syncFrom()` calls, `pos` is the
    // authoritative read position; `cursor.unsafePos` is allowed to lag.
    // Whenever a cursor operation that depends on `pos` is performed
    // (refill via `more`'s slow path, mark, slice, error reporting,
    // backtracking via `cue`) the parser pushes `pos` to the cursor first,
    // then refreshes its snapshot from the cursor afterwards — refill may
    // compact the buffer, reallocate it, or reset `pos`.
    private var bytes:  Array[Char] = cursor.buffer(using Unsafe)
    private var pos:    Int = cursor.unsafePos(using Unsafe)
    private var bufEnd: Int = cursor.unsafeWriteEnd(using Unsafe)

    private inline def syncTo(): Unit =
      cursor.unsafeAdvanceBy(pos - cursor.unsafePos(using Unsafe))(using Unsafe)

    private inline def syncFrom(): Unit =
      bytes  = cursor.buffer(using Unsafe)
      pos    = cursor.unsafePos(using Unsafe)
      bufEnd = cursor.unsafeWriteEnd(using Unsafe)
      lineationPos = pos

    protected inline def more: Boolean = pos < bufEnd || moreSlow()

    // Out-of-line slow path so `more`'s inline budget stays small enough
    // for the JIT to keep `pos < bufEnd` as one register comparison in
    // hot loops. In tracking mode, lineation is reconciled and the
    // parser-local `pos` is re-anchored even on EOF so that the next
    // `cursor.position` read reflects the compacted buffer's basePos.
    private def moreSlow(): Boolean =
      syncTo()
      if tracking then reconcileLineation()

      if cursor.more then { syncFrom(); true }
      else
        if tracking then syncFrom()
        false

    protected inline def peek: Char = bytes(pos)
    protected inline def advance(): Unit = pos += 1

    protected inline def position: Int =
      syncTo()
      cursor.position.n0

    // Non-`inline` so that `cursor.mark`'s expansion is emitted once in its
    // own method rather than re-expanded into every call site, keeping
    // `XmlParser`'s hot methods small enough for HotSpot's free-inline
    // budgets. The JIT can still inline at hot call sites via its own
    // heuristics.
    protected def begin(): Cursor.Mark =
      syncTo()
      cursor.mark(using heldToken.nn)

    protected def slice(start: Cursor.Mark): Text =
      syncTo()
      val end = cursor.mark(using heldToken.nn)
      cursor.grab(start, end).asInstanceOf[Text]

    protected def slice(start: Cursor.Mark, end: Cursor.Mark): Text =
      cursor.grab(start, end).asInstanceOf[Text]

    protected def reset(start: Cursor.Mark): Unit =
      syncTo()
      cursor.cue(start)
      syncFrom()

    protected def appendSlice(start: Cursor.Mark, buf: jl.StringBuilder): Unit =
      syncTo()
      val end = cursor.mark(using heldToken.nn)
      cursor.clone(start, end)(buf.asInstanceOf[cursor.addressable.Target])

    protected def computePosition(start: Optional[Cursor.Mark] = Unset): Position =
      // The cursor itself uses untracked lineation in the hot path (see the
      // import at `XmlParser`). On error, reconstruct (line, column) by
      // scanning the currently-buffered chars from the start of the buffer
      // up to the current read position, counting newlines. Errors are
      // rare, so the O(buffer) cost here doesn't matter; the parser stays
      // tight on the success path. For the loadable path (single-chunk
      // buffer), this is fully accurate. For multi-chunk streaming, lines
      // before the most recent compaction are not represented in the
      // buffer; we under-count by that amount but the absolute `offset`
      // remains correct, which is what the tests assert on.
      syncTo()
      var line = 1
      var col = 1
      var i = 0

      while i < pos do
        if bytes(i) == '\n' then
          line += 1
          col = 1
        else
          col += 1

        i += 1

      val end = cursor.position.n0
      val offset: Optional[Int] = start.let(_.absolute.toInt)
      val length: Optional[Int] = start.let: mark => end - mark.absolute.toInt
      Position(line.u, col.u, offset = offset, length = length)

    // Optional callback invoked when a `\u0000` placeholder is encountered.
    // Used by the macro interpolators to record hole positions; the
    // substrate must still recognise `\u0000` as a special character at
    // each insertion point. Default no-op for non-macro use.
    var callback: Optional[(Ordinal, Hole) => Unit] = Unset

    protected inline def fail(issue: Issue)(using Tactic[ParseError]): Nothing =
      abort(ParseError(Xml, computePosition(Unset), issue))

    protected def fail(issue: Issue, start: Cursor.Mark)(using Tactic[ParseError]): Nothing =
      abort(ParseError(Xml, computePosition(start), issue))

    protected inline def isAsciiLetter(c: Char): Boolean =
      ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

    protected inline def isAsciiDigit(c: Char): Boolean = '0' <= c && c <= '9'

    protected inline def isNameStart(c: Char): Boolean =
      isAsciiLetter(c) || c == '_' || c == ':' || (c > 127 && c.isLetter)

    protected inline def isNameChar(c: Char): Boolean =
      isAsciiLetter(c) || isAsciiDigit(c) || c == '_' || c == '-' || c == '.' || c == ':' ||
        (c > 127 && (c == '·' || c.isLetter || c.isDigit))

    protected inline def isWs(c: Char): Boolean =
      c == ' ' || c == '\n' || c == '\r' || c == '\t' || c == '\f'

    protected def skipWs(): Unit = while more && isWs(peek) do advance()

    protected def expectChar(chr: Char)(using Tactic[ParseError]): Unit =
      if !more then fail(Issue.ExpectedMore)
      if peek != chr then fail(Issue.Unexpected(peek))
      advance()

    protected def readName()(using Tactic[ParseError]): Text =
      val start = begin()
      if !more then fail(Issue.ExpectedMore, start)
      val first = peek
      if !isNameStart(first) then fail(Issue.Unexpected(first), start)
      advance()

      // Pack chars into a Long pair while scanning; track whether they all
      // stay in the 7-bit ASCII range. The pair is later used as the cache
      // key when both conditions (ascii + length ≤ 16) hold.
      var packedLow:  Long = first.toLong & 0xFFL
      var packedHigh: Long = 0L
      var len: Int = 1
      var ascii: Boolean = first < 128

      while more && isNameChar(peek) do
        val c = peek
        if c >= 128 then ascii = false

        if len < 8 then
          packedLow = packedLow | ((c.toLong & 0xFFL) << (len << 3))
        else if len < 16 then
          packedHigh = packedHigh | ((c.toLong & 0xFFL) << ((len - 8) << 3))

        len += 1
        advance()

      if !ascii || len > TagCacheMaxChars then slice(start)
      else
        val idx =
          ((packedLow.toInt ^ (packedLow >>> 32).toInt) ^
            (packedHigh.toInt ^ (packedHigh >>> 32).toInt)) & (TagCacheSize - 1)

        val cached = tagCache(idx)

        if cached != null && tagCacheLow(idx) == packedLow &&
          tagCacheHigh(idx) == packedHigh
        then cached.nn
        else
          val fresh = slice(start)
          tagCache(idx)     = fresh
          tagCacheLow(idx)  = packedLow
          tagCacheHigh(idx) = packedHigh
          fresh

    // Parse an entity reference. Position must be just after the '&'.
    // Returns the expansion as a Text; leaves position just after the ';'.
    protected def readEntity()(using Tactic[ParseError]): Text =
      if !more then fail(Issue.ExpectedMore)

      if peek == '#' then
        advance()
        if !more then fail(Issue.ExpectedMore)
        var value = 0

        if peek == 'x' || peek == 'X' then
          advance()

          while more && peek != ';' do
            val c = peek

            value =
              if '0' <= c && c <= '9' then 16*value + (c - '0')
              else if 'a' <= c && c <= 'f' then 16*value + (c - 87)
              else if 'A' <= c && c <= 'F' then 16*value + (c - 55)
              else fail(Issue.Unexpected(c))

            advance()
        else
          while more && peek != ';' do
            val c = peek

            if '0' <= c && c <= '9' then value = 10*value + (c - '0')
            else fail(Issue.Unexpected(c))

            advance()

        if !more then fail(Issue.ExpectedMore)
        advance()

        if value <= 0xffff then String.valueOf(value.toChar).nn.tt
        else String.valueOf(Character.toChars(value).nn).nn.tt
      else
        val nameStart = begin()

        while more && peek != ';' do
          val c = peek
          if !isNameChar(c) then fail(Issue.Unexpected(c), nameStart)
          advance()

        if !more then fail(Issue.ExpectedMore, nameStart)
        val name = slice(nameStart)
        advance()
        schema.entities(name).or(fail(Issue.UnknownEntity(name), nameStart))

    // Read attribute value enclosed in `quote`. Returns the unescaped
    // value as Text. Position starts just after the opening quote and
    // ends just after the closing quote.
    protected def readAttrValue(tag: Text, quote: Char)(using Tactic[ParseError]): Text =
      val start = begin()
      var hasEntity = false
      var hasHole = false

      while more && peek != quote do
        val c = peek
        if c == '<' then fail(Issue.Unexpected('<'), start)
        if c == '&' then hasEntity = true
        if c == '\u0000' then hasHole = true
        advance()

      if !more then fail(Issue.ExpectedMore, start)
      val end = begin()
      advance() // consume closing quote

      if !hasEntity && !hasHole then slice(start, end)
      else
        // Mixed: entities and/or holes. Walk again with a buffer.
        // We rewind to start and re-scan with appendSlice between events.
        val buf = jl.StringBuilder()
        reset(start)
        var segStart = begin()

        while more && peek != quote do
          val c = peek

          if c == '&' then
            appendSlice(segStart, buf)
            advance()
            buf.append(readEntity().s)
            segStart = begin()
          else if c == '\u0000' then
            // Macro hole inside attribute value. Per existing semantics,
            // we report it but include U+0000 in the value text so the
            // macro post-processor can locate it.
            appendSlice(segStart, buf)
            callback.let(_(position.z, Hole.Attribute(tag, t"")))
            buf.append('\u0000')
            advance()
            segStart = begin()
          else
            advance()

        if !more then fail(Issue.ExpectedMore, start)
        appendSlice(segStart, buf)
        advance() // consume closing quote
        buf.toString.nn.tt

    protected def readAttributes(tag: Text)(using Tactic[ParseError]): Attributes =
      // Append into the parser-shared interleaved scratch buffer (laid out as
      // `[k0, v0, k1, v1, ...]`); on close, snapshot the populated prefix
      // into a freshly-sized `IArray[String]` and wrap it as the opaque
      // `Attributes`.
      //
      // Duplicate detection uses a Bloom-filter-style cheap test before
      // falling back to a linear scan: maintain a running OR of the
      // hashCodes of all already-stored keys, and for each new key check
      // whether `(hashOr | h) == hashOr`. If the new hash has any bit
      // outside the accumulated envelope it cannot match any prior key and
      // the scan is skipped. Only when its bits are all already in the
      // envelope (rare for typical low-attribute-count elements with
      // disjoint label hashes) do we walk the existing keys to confirm.
      var n = 0
      var done = false
      var hashOr = 0

      inline def ensureCapacity(): Unit =
        if 2*n >= attrBuf.length then
          val nu = new Array[String](attrBuf.length*2)
          jl.System.arraycopy(attrBuf, 0, nu, 0, 2*n)
          attrBuf = nu

      while !done do
        skipWs()
        if !more then fail(Issue.ExpectedMore)
        val ch = peek

        if ch == '>' || ch == '/' || ch == '?' then done = true
        else if ch == '\u0000' then
          callback.let(_(position.z, Hole.Tagbody))
          advance()
          skipWs()
          ensureCapacity()
          attrBuf(2*n) = "\u0000"
          attrBuf(2*n + 1) = ""
          n += 1
        else
          val keyStart = begin()
          val key = readName()
          val keyStr: String = key.s
          val h: Int = keyStr.hashCode

          if (hashOr | h) == hashOr then
            var dup = 0

            while dup < 2*n do
              if attrBuf(dup) == keyStr then fail(Issue.DuplicateAttribute(key), keyStart)
              dup += 2

          hashOr |= h

          skipWs()
          expectChar('=')
          skipWs()
          if !more then fail(Issue.ExpectedMore, keyStart)
          val q = peek

          val value =
            if q == '\u0000' then
              callback.let(_(position.z, Hole.Attribute(tag, key)))
              advance()
              t"\u0000"
            else if q == '"' || q == '\'' then
              advance()
              readAttrValue(tag, q)
            else
              fail(Issue.UnquotedAttribute, keyStart)

          ensureCapacity()
          attrBuf(2*n) = keyStr
          attrBuf(2*n + 1) = value.s
          n += 1

      if n == 0 then Attributes.empty
      else
        val arr = new Array[String](2*n)
        jl.System.arraycopy(attrBuf, 0, arr, 0, 2*n)
        Attributes.fromInterleaved(arr.immutable(using Unsafe))

    // Read text up to the next '<'; returns the (possibly entity-expanded)
    // Text. Detects literal `]]>` as an error. Reports `\u0000` holes via
    // the callback.
    //
    // Single-pass: walk the text region tracking the `]]>` window with a
    // simple counter; if no entity/hole is encountered the result comes
    // from a single `slice`. The first `&` or `\u0000` hit lazily allocates
    // a `StringBuilder`, flushes the accumulated plain text into it, and
    // the loop continues in the same iteration, re-using the running
    // `bracketCount`. The previous form rescanned the whole region a
    // second time once an entity was detected.
    protected def readText(parentLabel: Text)(using Tactic[ParseError]): Text =
      val start = begin()
      var bracketCount = 0
      var buf: jl.StringBuilder | Null = null
      var segStart: Cursor.Mark = start

      while more && peek != '<' do
        val c = peek

        if c == ']' then bracketCount += 1
        else
          if bracketCount >= 2 && c == '>' then fail(Issue.Unexpected('>'), start)
          bracketCount = 0

        if c == '&' then
          if buf == null then buf = jl.StringBuilder()
          appendSlice(segStart, buf.nn)
          advance()
          buf.nn.append(readEntity().s)
          segStart = begin()
        else if c == '\u0000' then
          if buf == null then buf = jl.StringBuilder()
          appendSlice(segStart, buf.nn)
          callback.let(_(position.z, Hole.Node(parentLabel)))
          buf.nn.append('\u0000')
          advance()
          segStart = begin()
        else
          advance()

      if buf == null then slice(start)
      else
        appendSlice(segStart, buf.nn)
        buf.nn.toString.nn.tt

    protected def readComment()(using Tactic[ParseError]): Text =
      val start = begin()

      while
        if !more then fail(Issue.ExpectedMore, start)
        !(peek == '-')
      do advance()
      // Try to match `-->`
      val end = begin()
      advance()
      if !more then fail(Issue.ExpectedMore, start)
      if peek != '-' then
        // Not the end; continue from here
        readComment_continue(start)
      else
        advance()
        if !more then fail(Issue.ExpectedMore, start)
        if peek != '>' then fail(Issue.Unexpected(peek), start)
        advance()
        slice(start, end)

    private def readComment_continue(start: Region)(using Tactic[ParseError]): Text =
      // We saw '-' but the next wasn't '-' or '>'. Continue scanning.
      while more && peek != '-' do advance()
      if !more then fail(Issue.ExpectedMore, start)
      val end = begin()
      advance()
      if !more then fail(Issue.ExpectedMore, start)

      if peek != '-' then readComment_continue(start)
      else
        advance()
        if !more then fail(Issue.ExpectedMore, start)
        if peek != '>' then fail(Issue.Unexpected(peek), start)
        advance()
        slice(start, end)

    protected def readCdata()(using Tactic[ParseError]): Text =
      val start = begin()
      var done = false
      var endRegion: Region = start

      while !done do
        if !more then fail(Issue.ExpectedMore, start)

        if peek == ']' then
          val maybeEnd = begin()
          advance()

          if more && peek == ']' then
            advance()

            if more && peek == '>' then
              endRegion = maybeEnd
              advance()
              done = true
        else
          advance()

      slice(start, endRegion)

    // Position must be just after '<?'. Reads PI target + data, returning
    // the appropriate Node.
    protected def readProcessingInstruction()(using Tactic[ParseError]): Node =
      val nameStart = begin()
      if !more then fail(Issue.ExpectedMore, nameStart)
      val first = peek
      if !isNameStart(first) then fail(Issue.Unexpected(first), nameStart)
      advance()
      while more && isNameChar(peek) do advance()
      val target = slice(nameStart)

      val isXmlName =
        target.s.length == 3 &&
          (target.s.charAt(0) == 'x' || target.s.charAt(0) == 'X') &&
          (target.s.charAt(1) == 'm' || target.s.charAt(1) == 'M') &&
          (target.s.charAt(2) == 'l' || target.s.charAt(2) == 'L')

      if isXmlName then
        if !headers then fail(Issue.InvalidTag(target), nameStart)
        headers = false
        skipWs()
        val versionKey = readName()
        if versionKey != t"version" then fail(Issue.Unexpected(versionKey.s.charAt(0)), nameStart)
        skipWs()
        expectChar('=')
        skipWs()
        if !more then fail(Issue.ExpectedMore, nameStart)
        val q = peek
        if q != '"' && q != '\'' then fail(Issue.UnquotedAttribute, nameStart)
        advance()
        val version = readAttrValue(target, q)
        skipWs()
        var encoding: Optional[Text] = Unset
        var standalone: Optional[Boolean] = Unset

        if more && peek == 'e' then
          val key = readName()
          if key != t"encoding" then fail(Issue.Unexpected(key.s.charAt(0)), nameStart)
          skipWs()
          expectChar('=')
          skipWs()
          if !more then fail(Issue.ExpectedMore, nameStart)
          val q2 = peek
          if q2 != '"' && q2 != '\'' then fail(Issue.UnquotedAttribute, nameStart)
          advance()
          encoding = readAttrValue(target, q2)
          skipWs()

        if more && peek == 's' then
          val key = readName()
          if key != t"standalone" then fail(Issue.Unexpected(key.s.charAt(0)), nameStart)
          skipWs()
          expectChar('=')
          skipWs()
          if !more then fail(Issue.ExpectedMore, nameStart)
          val q2 = peek
          if q2 != '"' && q2 != '\'' then fail(Issue.UnquotedAttribute, nameStart)
          advance()
          val v = readAttrValue(target, q2)

          standalone = v.s match
            case "yes" => true
            case "no"  => false
            case _     => fail(Issue.Unexpected(v.s.charAt(0)), nameStart)

          skipWs()

        if !more then fail(Issue.ExpectedMore, nameStart)
        if peek != '?' then fail(Issue.Unexpected(peek), nameStart)
        advance()
        if !more then fail(Issue.ExpectedMore, nameStart)
        if peek != '>' then fail(Issue.Unexpected(peek), nameStart)
        advance()
        Header(version, encoding, standalone)
      else
        skipWs()
        val dataStart = begin()

        while
          if !more then fail(Issue.ExpectedMore, dataStart)
          !(peek == '?')
        do advance()
        // Now at '?'. Need '?>'.
        val dataEnd = begin()
        advance()
        if !more then fail(Issue.ExpectedMore, dataStart)
        if peek != '>' then
          // Not the terminator, continue
          readPiData(dataStart, target)
        else
          advance()
          val data = slice(dataStart, dataEnd)
          ProcessingInstruction(target, data)

    private def readPiData(dataStart: Region, target: Text)
      ( using Tactic[ParseError] )
    :   ProcessingInstruction =

      while more && peek != '?' do advance()
      if !more then fail(Issue.ExpectedMore, dataStart)
      val dataEnd = begin()
      advance()
      if !more then fail(Issue.ExpectedMore, dataStart)

      if peek != '>' then readPiData(dataStart, target)
      else
        advance()
        ProcessingInstruction(target, slice(dataStart, dataEnd))

    protected def readDoctype()(using Tactic[ParseError]): Text =
      skipWs()
      val start = begin()
      while more && peek != '>' do advance()
      if !more then fail(Issue.ExpectedMore, start)
      val end = begin()
      advance()
      slice(start, end)

    // Read a single element starting just after '<'.
    protected def readElement()(using Tactic[ParseError]): Element =
      // Detect `<\u0000` (macro element hole)
      if more && peek == '\u0000' then
        callback.let(_(position.z, Hole.Element(t"")))
        advance()
        if !more then fail(Issue.ExpectedMore)
        if peek != '>' then fail(Issue.Unexpected(peek))
        advance()
        Element(t"\u0000", Attributes.empty, IArray.empty[Node])
      else
        val name = readName()
        val attrs = readAttributes(name)
        if !more then fail(Issue.ExpectedMore)

        if peek == '/' then
          advance()
          if !more then fail(Issue.ExpectedMore)
          if peek != '>' then fail(Issue.Unexpected(peek))
          advance()
          Element(name, attrs, IArray.empty[Node])
        else
          if peek != '>' then fail(Issue.Unexpected(peek))
          advance()
          val children = readChildren(name)
          Element(name, attrs, children)

    protected def readChildren(parentName: Text)(using Tactic[ParseError]): IArray[Node] =
      val children = getNodeBuffer()
      var done = false

      while !done do
        if !more then fail(Issue.Incomplete(parentName))
        val c = peek

        if c == '<' then
          advance()
          if !more then fail(Issue.ExpectedMore)
          val c2 = peek

          if c2 == '/' then
            advance()
            val closeStart = begin()
            val close = readName()
            skipWs()
            if !more then fail(Issue.ExpectedMore, closeStart)
            if peek != '>' then fail(Issue.Unexpected(peek), closeStart)
            advance()
            if close != parentName then fail(Issue.MismatchedTag(parentName, close), closeStart)
            done = true
          else if c2 == '!' then
            advance()

            if more && peek == '-' then
              advance()
              if !more then fail(Issue.ExpectedMore)
              if peek != '-' then fail(Issue.Unexpected(peek))
              advance()
              children += Comment(readComment())
            else if more && peek == '[' then
              advance()
              consumeLiteral("CDATA[")
              children += Cdata(readCdata())
            else
              if !more then fail(Issue.ExpectedMore)
              fail(Issue.Unexpected(peek))
          else if c2 == '?' then
            advance()
            children += readProcessingInstruction()
          else
            children += readElement()
        else
          val text = readText(parentName)
          if text.length > 0 then children += TextNode(text)

      val result =
        if children.nil then IArray.empty[Node]
        else
          val arr = new Array[Node](children.length)
          var i = 0

          while i < children.length do
            arr(i) = children(i)
            i += 1

          arr.immutable(using Unsafe)

      relinquishNodeBuffer()
      result

    protected def consumeLiteral(literal: String)(using Tactic[ParseError]): Unit =
      var i = 0

      while i < literal.length do
        if !more then fail(Issue.ExpectedMore)
        if peek != literal.charAt(i) then fail(Issue.Unexpected(peek))
        advance()
        i += 1

    private var headers: Boolean = false

    def parseXml(headers0: Boolean)(using Tactic[ParseError]): Xml =
      cursor.hold:
        heldToken = summon[Cursor.Held]

        try
          if tracking then parseXmlTracked0(headers0) else parseXml0(headers0)
        finally heldToken = null

    // Tracked variants of `readElement` / `readAttributes` / `readChildren`,
    // building a parallel `IArray[Int]` position index as they parse. The
    // structural logic mirrors the untracked variants byte-for-byte; only
    // the position bookkeeping differs. Splitting keeps the untracked
    // hot path free of any tracking-related branches.

    private def parseXmlTracked0(headers0: Boolean)(using Tactic[ParseError]): Xml =
      headers = headers0
      skipWs()
      val nodes = getNodeBuffer()
      val rootBuf = getIndexBuffer()

      while more do
        if peek != '<' then
          val text = readText(t"")
          if text.length > 0 then nodes += TextNode(text)
        else
          syncTo()
          reconcileLineation()
          val startLine = cursor.line.n0
          val startColumn = cursor.column.n0
          val startMark = cursor.position.n0.toLong

          advance()
          if !more then fail(Issue.ExpectedMore)
          val c2 = peek

          if c2 == '!' then
            advance()

            if more && peek == '-' then
              advance()
              if !more then fail(Issue.ExpectedMore)
              if peek != '-' then fail(Issue.Unexpected(peek))
              advance()
              nodes += Comment(readComment())
            else if more && (peek == 'D' || peek == 'd') then
              consumeLiteralCi("DOCTYPE")
              nodes += Doctype(readDoctype())
            else if more && peek == '[' then
              advance()
              consumeLiteral("CDATA[")
              nodes += Cdata(readCdata())
            else
              if !more then fail(Issue.ExpectedMore)
              fail(Issue.Unexpected(peek))
          else if c2 == '?' then
            advance()
            nodes += readProcessingInstruction()
          else if c2 == '/' then
            advance()
            val closeStart = begin()
            val close = readName()
            fail(Issue.UnopenedTag(close), closeStart)
          else
            nodes += readElementTracked(rootBuf, startLine, startColumn, startMark)

        skipWs()

      val result =
        if nodes.length == 1 then nodes(0)
        else Fragment(nodes.toSeq*)

      relinquishNodeBuffer()
      rootIndex = IArray.from(rootBuf)
      relinquishIndexBuffer()
      result

    // Read a tracked element starting just after '<'. `startLine`,
    // `startColumn`, `startMark` were captured by the caller at the `<`.
    // `out` is the parent's index buffer; the element's descriptor is
    // appended to it.
    private def readElementTracked
      ( out:         scala.collection.mutable.ArrayBuffer[Int],
        startLine:   Int,
        startColumn: Int,
        startMark:   Long )
      ( using Tactic[ParseError] )
    :   Element =

      // Macro element holes can't carry meaningful positions; emit an empty
      // attribute / child set and a zero-length descriptor.
      if more && peek == ' ' then
        callback.let(_(position.z, Hole.Element(t"")))
        advance()
        if !more then fail(Issue.ExpectedMore)
        if peek != '>' then fail(Issue.Unexpected(peek))
        advance()
        val attrDescs = getIndexBuffer()
        val attrEnds  = getIndexBuffer()
        val childDescs = getIndexBuffer()
        val childEnds  = getIndexBuffer()

        emitElementDescriptor
          ( out, attrDescs, attrEnds, childDescs, childEnds, startLine, startColumn, startMark )

        relinquishIndexBuffer()
        relinquishIndexBuffer()
        relinquishIndexBuffer()
        relinquishIndexBuffer()
        Element(t" ", Attributes.empty, IArray.empty[Node])
      else
        val attrDescs = getIndexBuffer()
        val attrEnds  = getIndexBuffer()
        val childDescs = getIndexBuffer()
        val childEnds  = getIndexBuffer()

        val name = readName()
        val attrs = readAttributesTracked(name, attrDescs, attrEnds)
        if !more then fail(Issue.ExpectedMore)

        val result =
          if peek == '/' then
            advance()
            if !more then fail(Issue.ExpectedMore)
            if peek != '>' then fail(Issue.Unexpected(peek))
            advance()
            Element(name, attrs, IArray.empty[Node])
          else
            if peek != '>' then fail(Issue.Unexpected(peek))
            advance()
            val children = readChildrenTracked(name, childDescs, childEnds)
            Element(name, attrs, children)

        emitElementDescriptor
          ( out, attrDescs, attrEnds, childDescs, childEnds, startLine, startColumn, startMark )

        relinquishIndexBuffer()
        relinquishIndexBuffer()
        relinquishIndexBuffer()
        relinquishIndexBuffer()
        result

    private def readAttributesTracked
      ( tag:       Text,
        attrDescs: scala.collection.mutable.ArrayBuffer[Int],
        attrEnds:  scala.collection.mutable.ArrayBuffer[Int] )
      ( using Tactic[ParseError] )
    :   Attributes =

      var n = 0
      var done = false
      var hashOr = 0

      inline def ensureCapacity(): Unit =
        if 2*n >= attrBuf.length then
          val nu = new Array[String](attrBuf.length*2)
          jl.System.arraycopy(attrBuf, 0, nu, 0, 2*n)
          attrBuf = nu

      while !done do
        skipWs()
        if !more then fail(Issue.ExpectedMore)
        val ch = peek

        if ch == '>' || ch == '/' || ch == '?' then done = true
        else if ch == ' ' then
          callback.let(_(position.z, Hole.Tagbody))
          advance()
          skipWs()
          ensureCapacity()
          attrBuf(2*n) = " "
          attrBuf(2*n + 1) = ""
          n += 1
        else
          // Capture attribute start position before reading the name.
          syncTo()
          reconcileLineation()
          val attrLine = cursor.line.n0
          val attrColumn = cursor.column.n0
          val attrStartMark = cursor.position.n0.toLong

          val keyStart = begin()
          val key = readName()
          val keyStr: String = key.s
          val h: Int = keyStr.hashCode

          if (hashOr | h) == hashOr then
            var dup = 0

            while dup < 2*n do
              if attrBuf(dup) == keyStr then fail(Issue.DuplicateAttribute(key), keyStart)
              dup += 2

          hashOr |= h

          skipWs()
          expectChar('=')
          skipWs()
          if !more then fail(Issue.ExpectedMore, keyStart)
          val q = peek

          val value =
            if q == ' ' then
              callback.let(_(position.z, Hole.Attribute(tag, key)))
              advance()
              t" "
            else if q == '"' || q == '\'' then
              advance()
              readAttrValue(tag, q)
            else
              fail(Issue.UnquotedAttribute, keyStart)

          ensureCapacity()
          attrBuf(2*n) = keyStr
          attrBuf(2*n + 1) = value.s
          n += 1

          // Emit attribute descriptor [size=4, line, column, length].
          syncTo()
          val attrLength = (cursor.position.n0 - attrStartMark).toInt
          attrDescs += 4
          attrDescs += attrLine
          attrDescs += attrColumn
          attrDescs += attrLength
          attrEnds  += attrDescs.length

      if n == 0 then Attributes.empty
      else
        val arr = new Array[String](2*n)
        jl.System.arraycopy(attrBuf, 0, arr, 0, 2*n)
        Attributes.fromInterleaved(arr.immutable(using Unsafe))

    private def readChildrenTracked
      ( parentName: Text,
        childDescs: scala.collection.mutable.ArrayBuffer[Int],
        childEnds:  scala.collection.mutable.ArrayBuffer[Int] )
      ( using Tactic[ParseError] )
    :   IArray[Node] =

      val children = getNodeBuffer()
      var done = false

      while !done do
        if !more then fail(Issue.Incomplete(parentName))
        val c = peek

        if c == '<' then
          // Capture the `<` position now in case this turns out to be a
          // child element. Non-element branches (comment, CDATA, PI, close)
          // simply ignore the captured values.
          syncTo()
          reconcileLineation()
          val childLine = cursor.line.n0
          val childColumn = cursor.column.n0
          val childStartMark = cursor.position.n0.toLong

          advance()
          if !more then fail(Issue.ExpectedMore)
          val c2 = peek

          if c2 == '/' then
            advance()
            val closeStart = begin()
            val close = readName()
            skipWs()
            if !more then fail(Issue.ExpectedMore, closeStart)
            if peek != '>' then fail(Issue.Unexpected(peek), closeStart)
            advance()
            if close != parentName then fail(Issue.MismatchedTag(parentName, close), closeStart)
            done = true
          else if c2 == '!' then
            advance()

            if more && peek == '-' then
              advance()
              if !more then fail(Issue.ExpectedMore)
              if peek != '-' then fail(Issue.Unexpected(peek))
              advance()
              children += Comment(readComment())
            else if more && peek == '[' then
              advance()
              consumeLiteral("CDATA[")
              children += Cdata(readCdata())
            else
              if !more then fail(Issue.ExpectedMore)
              fail(Issue.Unexpected(peek))
          else if c2 == '?' then
            advance()
            children += readProcessingInstruction()
          else
            children += readElementTracked(childDescs, childLine, childColumn, childStartMark)
            childEnds += childDescs.length
        else
          val text = readText(parentName)
          if text.length > 0 then children += TextNode(text)

      val result =
        if children.nil then IArray.empty[Node]
        else
          val arr = new Array[Node](children.length)
          var i = 0

          while i < children.length do
            arr(i) = children(i)
            i += 1

          arr.immutable(using Unsafe)

      relinquishNodeBuffer()
      result

    private def parseXml0(headers0: Boolean)(using Tactic[ParseError]): Xml =
      headers = headers0
      skipWs()
      val nodes = getNodeBuffer()

      while more do
        if peek != '<' then
          val text = readText(t"")
          if text.length > 0 then nodes += TextNode(text)
        else
          advance()
          if !more then fail(Issue.ExpectedMore)
          val c2 = peek

          if c2 == '!' then
            advance()

            if more && peek == '-' then
              advance()
              if !more then fail(Issue.ExpectedMore)
              if peek != '-' then fail(Issue.Unexpected(peek))
              advance()
              nodes += Comment(readComment())
            else if more && (peek == 'D' || peek == 'd') then
              consumeLiteralCi("DOCTYPE")
              nodes += Doctype(readDoctype())
            else if more && peek == '[' then
              advance()
              consumeLiteral("CDATA[")
              nodes += Cdata(readCdata())
            else
              if !more then fail(Issue.ExpectedMore)
              fail(Issue.Unexpected(peek))
          else if c2 == '?' then
            advance()
            nodes += readProcessingInstruction()
          else if c2 == '/' then
            advance()
            val closeStart = begin()
            val close = readName()
            fail(Issue.UnopenedTag(close), closeStart)
          else
            nodes += readElement()

        skipWs()

      val result =
        if nodes.length == 1 then nodes(0)
        else Fragment(nodes.toSeq*)

      relinquishNodeBuffer()
      result

    protected def consumeLiteralCi(literal: String)(using Tactic[ParseError]): Unit =
      var i = 0

      while i < literal.length do
        if !more then fail(Issue.ExpectedMore)
        val expected = literal.charAt(i)
        val got = peek

        val matches =
          got == expected ||
            isAsciiLetter(expected) &&
            (got == (expected | 0x20).toChar || got == (expected & ~0x20).toChar)

        if !matches then fail(Issue.Unexpected(got))
        advance()
        i += 1

  // ───────────────────────────────────────────────────────────────────────
  // Public entry points.

  // Back-compat for macro interpolators: matches the previous cursor-based
  // signature (Iterator[Text] + callback).
  private[xylophone] def parse[schema <: XmlSchema]
    ( input:    Iterator[Text],
      root:     Tag,
      callback: Optional[(Ordinal, Hole) => Unit] = Unset,
      headers0: Boolean                           = false )
    ( using schema: XmlSchema )
  :   Xml raises ParseError =

    val parser = XmlParser.fromIterator(input)
    parser.callback = callback
    parser.parseXml(headers0)


sealed into trait Xml extends Dynamic, Topical, Documentary, Formal:
  type Topic <: Label
  type Transport <: Label
  type Metadata = Header
  type Chunks = Text
  type Form <: XmlSchema

  private[xylophone] def of[topic <: Label]: this.type of topic = asInstanceOf[this.type of topic]
  private[xylophone] def in[form]: this.type in form = asInstanceOf[this.type in form]

  private[xylophone] def over[transport <: Label]: this.type over transport =
    asInstanceOf[this.type over transport]

  // Decode this `Xml` to a `result` value. `Decodable in Xml` is resolved
  // via the `decodable` summonFrom (textual decoder, else Wisteria
  // derivation). Errors registered inside the decoder carry `Xml.Focus`
  // values describing the XPath of the failing field. Position information
  // stays `Unset`; use `Xml.Tracked#as` against a `parseTracked` root if
  // you also want source line / column.
  def as[result: Decodable in Xml]: result tracks Xml.Focus = this match
    case Fragment(value) => result.decoded(value)
    case xml: Xml        => result.decoded(xml)

  // Dynamic navigation. `xml.foo` selects every child element named `foo`,
  // flattening across all element-nodes in the current `Fragment` (XML tags
  // are not unique, so a dereference yields a `Fragment` of zero or more
  // matches). `xml.foo(ordinal)` picks a single one; the ordinal defaults to
  // `Prim`, so `xml.foo()` is the first match. Both are gated by an imported
  // `DynamicXmlEnabler` (see `dynamicXmlAccess.enabled`).

  private def selfNodes: IArray[Node] = this match
    case Fragment(nodes*) => IArray.from(nodes)
    case node: Node       => IArray(node)

  private def childElements(name: String): IArray[Node] =
    val buffer = scm.ArrayBuffer[Node]()
    val nodes = selfNodes
    var i = 0

    while i < nodes.length do
      nodes(i) match
        case Element(_, _, children) =>
          var j = 0

          while j < children.length do
            children(j) match
              case child: Element if child.label == name.tt => buffer.append(child)
              case _                                        => ()

            j += 1

        case _ =>
          ()

      i += 1

    IArray.from(buffer)

  def selectDynamic(name: String)(using erased DynamicXmlEnabler): Fragment =
    new Fragment(childElements(name)*)

  def applyDynamic(name: String)(ordinal: Ordinal = Prim)(using erased DynamicXmlEnabler)
  :   Fragment =

    childElements(name).at(ordinal).lay(new Fragment())(new Fragment(_))

sealed trait Node extends Xml

case class Comment(text: Text) extends Node:
  override def hashCode: Int = text.hashCode*31 + 0x436F6D6D

  override def equals(that: Any): Boolean = that match
    case Comment(text0)           => text0 == text
    case Fragment(Comment(text0)) => text0 == text
    case _                        => false

case class Doctype(text: Text) extends Node:
  override def hashCode: Int = text.hashCode*31 + 0x44637470

  override def equals(that: Any): Boolean = that match
    case Doctype(text0)           => text0 == text
    case Fragment(Doctype(text0)) => text0 == text
    case _                        => false

case class Cdata(text: Text) extends Node:
  override def hashCode: Int = text.hashCode*31 + 0x43646174

  override def equals(that: Any): Boolean = that match
    case Cdata(text0)           => text0 == text
    case Fragment(Cdata(text0)) => text0 == text
    case _                      => false

case class ProcessingInstruction(target: Text, data: Text) extends Node:
  override def hashCode: Int = (target.hashCode*31 + data.hashCode)*31 + 0x50494E73

  override def equals(that: Any): Boolean = that match
    case ProcessingInstruction(target0, data0)           => target0 == target && data0 == data
    case Fragment(ProcessingInstruction(target0, data0)) => target0 == target && data0 == data
    case _                                               => false

case class TextNode(text: Text) extends Node:
  type Topic = "#text"

  override def hashCode: Int = text.hashCode*31 + 0x54657874

  override def equals(that: Any): Boolean = that match
    case Fragment(textual: TextNode) => this == textual
    case TextNode(text0)             => text0 == text
    case _                           => false

case class Element
  ( label:      Text,
    attributes: Attributes,
    children:   IArray[Node] )
extends Node, Topical, Transportive:
  override def toString(): String =
    s"<$label>${children.mkString}</$label>"

  override def equals(that: Any): Boolean = that match
    case Fragment(node: Element) => this == node

    case Element(label, attributes, children) =>
      label == this.label && attributes.equalsAttributes(this.attributes) &&
        ju.Arrays.equals(children.mutable(using Unsafe), this.children.mutable(using Unsafe))

    case _ =>
      false

  override def hashCode: Int =
    ju.Arrays.hashCode(children.mutable(using Unsafe)) ^ attributes.hashAttributes ^ label.hashCode


  def selectDynamic(name: Label)(using attribute: name.type is Xml.XmlAttribute on Topic in Form)
  :   Optional[Text] =

    attributes.at(name.tt)


  def updateDynamic(name: Label)(using attribute: name.type is Xml.XmlAttribute in Form)
    ( value: Text )
  :   Element of Topic over Transport in Form =

    Element(label, attributes.updated(name, value), children)
    . of[Topic]
    . over[Transport]
    . in[Form]

object Fragment:
  @targetName("make")
  def apply[topic <: Label](nodes: Xml of (? <: topic)*): Fragment of topic =
    new Fragment(nodes.nodes*).of[topic]

case class Fragment(nodes: Node*) extends Xml:
  override def hashCode: Int = if nodes.length == 1 then nodes(0).hashCode else nodes.hashCode

  override def equals(that: Any): Boolean = that match
    case Fragment(nodes0*) => nodes0 == nodes
    case node: Xml         => nodes.length == 1 && nodes(0) == node
    case _                 => false

case class Header(version: Text, encoding: Optional[Text], standalone: Optional[Boolean])
extends Node:
  override def hashCode: Int =
    ((version.hashCode*31 + encoding.hashCode)*31 + standalone.hashCode)*31 + 0x48646572

  override def equals(that: Any): Boolean = that match
    case Fragment(header: Header) => equals(header)

    case Header(version0, encoding0, standalone0) =>
      version0 == version && encoding0 == encoding && standalone0 == standalone

    case _ =>
      false
