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

import scala.collection.immutable.Vector

import scala.collection.immutable.Seq

import scala.caps


import scala.language.dynamics

import java.lang as jl
import java.util as ju

import scala.collection.Factory
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
import symbolism.*

import Xml.Error.Reason

object Xml extends Tag.Container
  ( label = "xml", admissible = Set("head", "body") ), Format, Xml2:
  private type BaseText = anticipation.Text

  // Controls how an `Xml` tree is serialized. `indent` is the whitespace unit emitted per nesting
  // level when indenting element-only content; `Unset` (the default) keeps everything on a single
  // line. `trailingNewline` appends a final newline. Bundled as `formatting.compactFormatting`
  // (the default) and `formatting.indentedFormatting`.
  object Formatting:
    def apply(indent: Optional[BaseText], trailingNewline: Boolean): Formatting =
      Basic(indent, trailingNewline)

    private case class Basic(indent: Optional[BaseText], trailingNewline: Boolean)
    extends Formatting

    given default: Formatting = apply(Unset, trailingNewline = false)

  trait Formatting extends zephyrine.Formatting:
    def indent: Optional[BaseText]
    def trailingNewline: Boolean

  type Topic = "xml"
  type Transport = "head" | "body"

  // The namespace bindings in scope at an element; `internal.Scope`, reached here because the
  // umbrella already exports a `Scope` of its own.
  export xylophone.internal.Scope

  // Whether a prefix with no binding in scope is a parse error, as the Namespaces
  // recommendation requires, or resolves to no namespace: strict unless
  // `namespaceOptions.lenientNamespaces` is in scope.
  object Namespacing:
    given default: Namespacing = Strict

  enum Namespacing:
    case Strict, Lenient

  // The bindings an element's scope makes for the prefixes it and its attributes use (and for
  // its default namespace) which differ from what `declared` binds — as attribute pairs, the
  // default namespace keyed by the empty prefix. An element declaring nothing of its own has
  // an empty scope, and needs nothing.
  private[xylophone] def undeclared(element: Element, declared: Scope): Attributes =
    val scope = element.scope

    if scope.isEmpty then Attributes.empty else
      val buffer = scm.ArrayBuffer[(BaseText, BaseText)]()

      def consider(prefix: Optional[BaseText]): Unit =
        if scope.binds(prefix) then
          val uri = scope.resolve(prefix)
          val key = prefix.or(t"")

          if uri != declared.resolve(prefix) && !buffer.exists(_(0) == key)
          then buffer += ((key, uri.or(t"")))

      val (prefix, _) = Xml.Name.split(element.label)
      consider(prefix)

      element.attributes.eachPair: (key, _) =>
        val (attributePrefix, _) = Xml.Name.split(key)
        if attributePrefix.present && attributePrefix != t"xmlns" then consider(attributePrefix)

      Attributes(buffer.toSeq*)

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

    // The local name: a prefix is lexical, so `<s:Book>` is the `Book` variant
    def discriminate(xml: Xml): Optional[BaseText] = xml match
      case element: Element           => element.localName
      case Fragment(element: Element) => element.localName
      case _                          => Unset

    def rewrite(kind: BaseText, xml: Xml): Xml = xml match
      case Element(_, attrs, children)           => Element(kind, attrs, children)
      case Fragment(Element(_, attrs, children)) => Element(kind, attrs, children)
      case other                                 => other

    def variant(xml: Xml): Xml = xml

  // Identity-checked sentinel used by `DecodableDerivation.conjunction` to
  // signal "this field was absent from the XML". The textual decoder and
  // the primitive `Decodable in Xml` instances detect it by reference
  // equality and `raise` an `Xml.Error` while continuing with a zero / empty
  // sentinel, so multi-error accrual can register every missing field in
  // one decode rather than aborting at the first.
  private[xylophone] val Absent: Xml = new Fragment()

  // Extract the textual content of a leaf-shaped Xml node. Returns `Unset`
  // for non-text shapes (other Elements, Comments, the `Absent` sentinel,
  // etc.). Used by the primitive decoders so they can decide whether to
  // parse the content or `raise` for a missing / wrong-shape input.
  private def textOf(xml: Xml): Optional[BaseText] = xml match
    case _ if xml eq Absent               => Unset
    case Text(text)                       => text
    case Element(_, _, Array(Text(text))) => text
    case Element(_, _, Array())           => t""
    case Fragment(node: Node)             => textOf(node)
    case _                                => Unset

  // The primitive decoders capture the tactic they raise through.
  //
  // Explicit `Decodable in Xml` for the common primitive scalars. These
  // *raise + continue* (record an `Xml.Error` on the ambient `Foci` and
  // return a zero / false sentinel) rather than `abort`ing, so a case
  // class with multiple bad fields accrues one error per field instead of
  // bailing at the first.
  //
  // Specific givens here shadow the generic `decodable` summonFrom below
  // for these types — that branch's `summon[Decodable in Text]` for
  // `Int` would otherwise `abort` with `Number.Error` and break accrual.
  given int: (tactic: Tactic[Xml.Error]) => ((Int is Decodable in Xml)^{tactic}) = xml =>
    textOf(xml).let: text =>
      try Integer.parseInt(text.s).nn
      catch case _: NumberFormatException =>
        raise(Xml.Error(Reason.Malformed(text, t"Int"))) yet 0

    . or:
        raise(Xml.Error(Reason.Untextual(t"Int"))) yet 0

  given long: (tactic: Tactic[Xml.Error]) => ((Long is Decodable in Xml)^{tactic}) = xml =>
    textOf(xml).let: text =>
      try jl.Long.parseLong(text.s).nn
      catch case _: NumberFormatException =>
        raise(Xml.Error(Reason.Malformed(text, t"Long"))) yet 0L

    . or:
        raise(Xml.Error(Reason.Untextual(t"Long"))) yet 0L

  given short: (tactic: Tactic[Xml.Error]) => ((Short is Decodable in Xml)^{tactic}) = xml =>
    textOf(xml).let: text =>
      try jl.Short.parseShort(text.s).nn
      catch case _: NumberFormatException =>
        raise(Xml.Error(Reason.Malformed(text, t"Short"))) yet 0.toShort

    . or:
        raise(Xml.Error(Reason.Untextual(t"Short"))) yet 0.toShort

  given byte: (tactic: Tactic[Xml.Error]) => ((Byte is Decodable in Xml)^{tactic}) = xml =>
    textOf(xml).let: text =>
      try jl.Byte.parseByte(text.s).nn
      catch case _: NumberFormatException =>
        raise(Xml.Error(Reason.Malformed(text, t"Byte"))) yet 0.toByte

    . or:
        raise(Xml.Error(Reason.Untextual(t"Byte"))) yet 0.toByte

  given double: (tactic: Tactic[Xml.Error]) => ((Double is Decodable in Xml)^{tactic}) = xml =>
    textOf(xml).let: text =>
      try jl.Double.parseDouble(text.s).nn
      catch case _: NumberFormatException =>
        raise(Xml.Error(Reason.Malformed(text, t"Double"))) yet 0.0

    . or:
        raise(Xml.Error(Reason.Untextual(t"Double"))) yet 0.0

  given float: (tactic: Tactic[Xml.Error]) => ((Float is Decodable in Xml)^{tactic}) = xml =>
    textOf(xml).let: text =>
      try jl.Float.parseFloat(text.s).nn
      catch case _: NumberFormatException =>
        raise(Xml.Error(Reason.Malformed(text, t"Float"))) yet 0.0f

    . or:
        raise(Xml.Error(Reason.Untextual(t"Float"))) yet 0.0f

  given boolean: (tactic: Tactic[Xml.Error]) => ((Boolean is Decodable in Xml)^{tactic}) = xml =>
    textOf(xml).let: text =>
      text.s match
        case "true"  => true
        case "false" => false
        case _       => raise(Xml.Error(Reason.Malformed(text, t"Boolean"))) yet false

    . or:
        raise(Xml.Error(Reason.Untextual(t"Boolean"))) yet false

  // The encoding counterpart of the `boolean` decodable above. The other
  // primitives encode through the blanket `encodable`'s `Encodable in Text`
  // branch, but no `Boolean is Encodable in Text` exists, so without this a
  // `Boolean` field cannot be written at all despite reading fine.
  given booleanEncodable: Boolean is Encodable in Xml =
    value => Text(if value then t"true" else t"false")

  // Marks a `Decodable in Xml` / `Encodable in Xml` as *repeatable* —
  // xylophone's counterpart of stratiform's `Tel.Decodable.repeatable` flag.
  // The wire form of a repeatable (collection) field is *all* the child
  // elements sharing the field's name, one per collection element, rather
  // than a single first-matching child. The `distillate` codec traits are
  // format-neutral and cannot carry the flag themselves, so it rides on this
  // mixin; a codec without the mixin is implicitly `repeatable = false`, and
  // the collection codecs below mix it in with the flag left `true`.
  trait Repeatable:
    def repeatable: Boolean = true

  // AST-path collection decoding: a field of type `List[value]` / `Set[value]`
  // etc. decodes from *all* the child elements labelled with the field's name,
  // in document order, each element decoding as one collection element. The
  // product derivation (`buildWith`) recognises the `Repeatable` mixin and
  // hands over a synthetic `Fragment` of the matching children — stratiform's
  // synthetic-document idea. A lone `Element` decodes as a single-element
  // collection (the shape `Xml#as` sees at a document root, where the parsed
  // `Fragment(root)` wraps it).
  given collectionDecodable: [collection <: Iterable, element]
  =>  ( factory: Factory[element, collection[element]] )
  =>  ( element0: => (element is Decodable in Xml)^ )
  =>  collection[element] is Decodable in Xml =

    // Sealed per the codec-thunk pattern: a by-name parameter cannot be
    // named in a capture set, so the honest capture of `element0` cannot be
    // expressed; see rep/DECISIONS.md.
    // [by-name-capture] by-name element codec cannot be named in capture set
    caps.unsafe.unsafeAssumePure:
      new distillate.Decodable with Repeatable:
        type Self = collection[element]
        type Form = Xml

        def decoded(xml: Xml): collection[element] =
          val builder = factory.newBuilder

          xml match
            case Fragment(nodes*) =>
              // The synthetic wrapper (or the `Absent` sentinel, an empty
              // `Fragment`): each element node is one collection element.
              // Zero nodes build the empty collection — a missing repeated
              // field is empty, never an error.
              nodes.each:
                case child: Element => builder += element0.decoded(child)
                case _              => ()

            case node: Node =>
              builder += element0.decoded(node)

          builder.result()

  // Alias counterparts of `collectionDecodable`: the opaque prelude collections
  // do not conform to `Iterable`, so each gets its own instance built at the
  // underlying stdlib type and cast (a no-op at erasure).
  given listDecodable: [list <: List, element]
  =>  ( element0: => (element is Decodable in Xml)^ )
  =>  list[element] is Decodable in Xml =
    collectionDecodable[scala.collection.immutable.List, element]
    . asInstanceOf[list[element] is Decodable in Xml]

  given setDecodable: [set <: Set, element]
  =>  ( element0: => (element is Decodable in Xml)^ )
  =>  set[element] is Decodable in Xml =
    collectionDecodable[scala.collection.immutable.Set, element]
    . asInstanceOf[set[element] is Decodable in Xml]

  given seriesDecodable: [sequence <: Sequence, element]
  =>  ( element0: => (element is Decodable in Xml)^ )
  =>  sequence[element] is Decodable in Xml =
    collectionDecodable[Vector, element]
    . asInstanceOf[sequence[element] is Decodable in Xml]

  // The mirror of `collectionDecodable`: a collection encodes to a `Fragment`
  // holding one node per element, which the product derivation (recognising
  // the `Repeatable` mixin) flattens into repeated same-named children.
  given collectionEncodable: [collection <: Iterable, element]
  =>  ( encodable: => (element is Encodable in Xml)^ )
  =>  collection[element] is Encodable in Xml =

    // Sealed per the codec-thunk pattern, as in `collectionDecodable`.
    // [by-name-capture] by-name element encoder captured
    caps.unsafe.unsafeAssumePure:
      new anticipation.Encodable with Repeatable:
        type Self = collection[element]
        type Form = Xml

        def encoded(values: collection[element]): Xml =
          val nodes: scm.ArrayBuffer[Node] = scm.ArrayBuffer()

          values.each: value =>
            encodable.encoded(value) match
              case node: Node           => nodes += node
              case Fragment(node: Node) => nodes += node

              // A multi-node fragment (itself a repeated form — a nested
              // collection) has no per-element XML shape; it nests under an
              // unnamed element, relabelled by the enclosing product.
              case Fragment(nested*) =>
                nodes += Element(t"", Attributes.empty, Array.from(nested))

          Fragment(nodes.toSeq*)

  // Alias counterparts of `collectionEncodable` (see `listDecodable`).
  given listEncodable: [list <: List, element]
  =>  ( encodable: => (element is Encodable in Xml)^ )
  =>  list[element] is Encodable in Xml =
    collectionEncodable[scala.collection.immutable.List, element]
    . asInstanceOf[list[element] is Encodable in Xml]

  given setEncodable: [set <: Set, element]
  =>  ( encodable: => (element is Encodable in Xml)^ )
  =>  set[element] is Encodable in Xml =
    collectionEncodable[scala.collection.immutable.Set, element]
    . asInstanceOf[set[element] is Encodable in Xml]

  given seriesEncodable: [sequence <: Sequence, element]
  =>  ( encodable: => (element is Encodable in Xml)^ )
  =>  sequence[element] is Encodable in Xml =
    collectionEncodable[Vector, element]
    . asInstanceOf[sequence[element] is Encodable in Xml]

  // An `Optional[value]` field: the `Absent` sentinel — a missing child
  // element or attribute — decodes to `Unset` (or raises `Missing` under
  // `optionalityOptions.strictXmlAbsence`); anything present decodes as the
  // inner type, under `tactic.tolerate` when faults are lenient. XML has no
  // null: an empty element is a present value. Not `Repeatable`: a present
  // optional is the first matching child, exactly as a mandatory field is,
  // so `Optional[List[element]]` is not a supported shape (a `List` field is
  // already empty when absent).
  given optionalDecodable: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( absence: Decodable.Absence in Xml,
        fault:   Decodable.Fault in Xml,
        tactic:  Tactic[Xml.Error] )
  =>  ( decodable0: => (inner is Decodable in Xml)^ )
  =>  value is Decodable in Xml =

    // Sealed per the codec-thunk pattern, as in `collectionDecodable`.
    // [by-name-capture] by-name inner decodable captured
    caps.unsafe.unsafeAssumePure:
      new distillate.Decodable:
        type Self = value
        type Form = Xml

        def decoded(xml: Xml): value =
          if xml eq Absent then
            if absence.strict then abort(Xml.Error(Reason.Missing)) else Unset
          else if fault.strict then
            decodable0.decoded(xml)
          else
            tactic.tolerate(decodable0.decoded(xml)).or(Unset)

  // The mirror of `optionalDecodable`. `Unset` encodes to an *empty*
  // `Fragment`, and a present value to a one-node `Fragment`: the encoder is
  // `Repeatable`, so the product encoder flattens the former to no child
  // element at all (or, for an `@attribute` field, to no attribute) and the
  // latter to the single relabelled child a mandatory field would produce.
  given optionalEncodable: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( encodable: => (inner is Encodable in Xml)^ )
  =>  value is Encodable in Xml =

    // Sealed per the codec-thunk pattern, as in `collectionDecodable`.
    // [by-name-capture] by-name inner encodable captured
    caps.unsafe.unsafeAssumePure:
      new anticipation.Encodable with Repeatable:
        type Self = value
        type Form = Xml

        def encoded(value: value): Xml =
          value.let(_.asInstanceOf[inner]).lay(Fragment()): present =>
            encodable.encoded(present) match
              case node: Node         => Fragment(node)
              case fragment: Fragment => fragment

  // A `Map[key, value]` field encodes as repeated child elements — one per
  // entry, each relabelled with the field's wire name by the product encoder
  // through the `Repeatable` mixin — every entry holding a `<key>` and a
  // `<value>` child, the shape of stratiform's `entries`. Keys and values are
  // any XML-encodable type, so a key need not be a valid element name.
  given mapEncodable: [key, value]
  =>  ( keyEncodable:   => (key is Encodable in Xml)^,
        valueEncodable: => (value is Encodable in Xml)^ )
  =>  Map[key, value] is Encodable in Xml =

    // Sealed per the codec-thunk pattern, as in `collectionDecodable`.
    // [by-name-capture] by-name key/value encoders captured
    caps.unsafe.unsafeAssumePure:
      new anticipation.Encodable with Repeatable:
        type Self = Map[key, value]
        type Form = Xml

        private def child(label: BaseText, encoded: Xml): Node = encoded match
          case element: Element           => Element(label, element.attributes, element.children, element.scope)
          case Fragment(element: Element) => Element(label, element.attributes, element.children, element.scope)
          case node: Node                 => Element(label, Attributes.empty, Array(node))
          case Fragment(nodes*)           => Element(label, Attributes.empty, Array.unsafeFrozen(nodes.toArray))

        def encoded(map: Map[key, value]): Xml =
          val entries: scm.ArrayBuffer[Node] = scm.ArrayBuffer()

          map.keys.to[List].each: key =>
            map(key).let: value =>
              val pair: Array[Node]^{} =
                Array(child(t"key", keyEncodable.encoded(key)), child(t"value", valueEncodable.encoded(value)))

              entries += Element(t"", Attributes.empty, pair)

          Fragment(entries.toSeq*)

  // The mirror of `mapEncodable`: every gathered entry element contributes
  // one mapping, its `<key>` and `<value>` children decoding as the key and
  // value types. A missing child is handed the `Absent` sentinel, so a
  // primitive registers a focused error and an `Optional` value reads as
  // `Unset`, exactly as a missing product field does.
  given mapDecodable: [key, value]
  =>  ( keyDecodable:   => (key is Decodable in Xml)^,
        valueDecodable: => (value is Decodable in Xml)^ )
  =>  Map[key, value] is Decodable in Xml =

    // Sealed per the codec-thunk pattern, as in `collectionDecodable`.
    // [by-name-capture] by-name key/value decoders captured
    caps.unsafe.unsafeAssumePure:
      new distillate.Decodable with Repeatable:
        type Self = Map[key, value]
        type Form = Xml

        private def childOf(entry: Element, label: BaseText): Xml =
          val found = entry.children.readable.find:
            case element: Element => element.label == label
            case _                => false

          found match
            case Some(node) => node
            case None       => Absent

        def decoded(xml: Xml): Map[key, value] =
          var accumulator = Map.empty[key, value]

          def entry(element: Element): Unit =
            val key = keyDecodable.decoded(childOf(element, t"key"))
            val value = valueDecodable.decoded(childOf(element, t"value"))
            accumulator = accumulator.define(key, value)

          xml match
            case Fragment(nodes*) =>
              nodes.each:
                case element: Element => entry(element)
                case _                => ()

            case element: Element => entry(element)
            case _                => ()

          accumulator

  // Single entry-point for resolving `Decodable in Xml`. Prefers a textual
  // decoder when one exists (so any `Decodable in Text` value works as a
  // field type); otherwise falls back to Wisteria-derived case-class /
  // sealed-trait decoding via `DecodableDerivation`.
  //
  // The textual branch raises `Xml.Error` (and continues with `""`) on the
  // `Absent` sentinel and on wrong-shape input. The inner
  // `Decodable in Text` is then called with that sentinel `""`; primitive
  // numerics have explicit `Decodable in Xml` givens above that pre-empt
  // this branch entirely, so the surviving callers here are types whose
  // textual decoders accept the empty string (e.g., `Text` itself,
  // user-defined identifiers).
  inline given decodable: [value] => value is Decodable in Xml = summonFrom:
    case given (`value` is Decodable in BaseText) =>
      xml =>
        provide[Tactic[Xml.Error]]:
          val text: BaseText =
            if xml eq Absent then raise(Xml.Error(Reason.Missing)) yet t""
            else textOf(xml).or(raise(Xml.Error(Reason.Empty)) yet t"")

          summon[`value` is Decodable in BaseText].decoded(text)

    case given Reflection[`value`] =>
      DecodableDerivation.derived

  // Single entry-point for resolving `Encodable in Xml`, the exact mirror of
  // `decodable` above. Prefers a textual encoder when one exists (so any
  // `Encodable in Text` value — `Text`, `Int`, `Long`, user identifiers, … —
  // becomes a leaf `Xml.Text`); otherwise falls back to Wisteria-derived
  // case-class / sealed-trait encoding via `EncodableDerivation`.
  inline given encodable: [value] => value is Encodable in Xml = summonFrom:
    case given (`value` is Encodable in BaseText) =>
      value => Text(value.encode)

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

      // The capabilities are summoned at the derivation site and supplied
      // explicitly to `decodeElement`, rather than re-summoned inside the
      // decoder body via nested `provide`s. Under capture checking the
      // nested context-function `provide`s minted distinct root capabilities
      // that failed to unify; a `Decodable` instance is `Pure`, so the SAM
      // closing over the summoned capabilities contributes nothing to its
      // capture set.
      xml =>
        decodeElement[derivation](xml)
          ( using infer[ProductReflection[derivation]],
                  infer[Foci[Xml.Focus]],
                  infer[Tactic[Xml.Error]] )

    private inline def decodeElement[derivation <: Product]
      ( xml: Xml )
      ( using ProductReflection[derivation], Foci[Xml.Focus], Tactic[Xml.Error] )
    :   derivation =

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
              raise(Xml.Error(Reason.AbsentProduct(typeName)))
              derivationDefault()

            case _ =>
              raise(Xml.Error(Reason.AbsentProduct(typeName)))
              buildWith[derivation](Element(t"", Attributes.empty, Array.empty))

    // Scans the venture slots and constructs positionally through the threaded `Mirror` — a
    // plain method: the argument buffer must not be allocated inside an inline expansion,
    // where its fresh root capability leaks into the expansion site's capture sets. Returns
    // an unused null when any slot failed: the caller's accruing scope is tainted, so the
    // result is discarded.
    private def gate[derivation <: Product]
      ( reflection: ProductReflection[derivation],
        slots:      Array[Venture[Any]]^{},
        active:     Boolean )
    :   derivation =

      // `spot` stops at the first unready slot rather than scanning them all, and its index is
      // confined to `slots`, so the read needs no bounds check.
      val failed = active && slots.spot(slot => !slots(slot).ready).present
      var slot = 0

      if failed then null.asInstanceOf[derivation]
      else
        val arguments = Array.allocate[Any](slots.length)
        slot = 0

        while slot < slots.length do
          arguments(slot) = slots.readUnchecked(slot).vouch
          slot += 1

        Xml.Parsable.assemble[derivation](reflection, Array.freeze(arguments))

    private inline def buildWith[derivation <: Product: ProductReflection]
      ( element: Element )
      ( using foci: Foci[Xml.Focus], tactic: Tactic[Xml.Error] )
    :   derivation =

      // Fields marked `@attribute` are read from the element's attributes
      // rather than its child elements, mirroring the encoder.
      val attributeFields: Map[BaseText, Set[attribute]] = fieldAnnotations[derivation, attribute]

      val unqualifiedFields: Map[BaseText, Set[unqualified]] =
        fieldAnnotations[derivation, unqualified]

      // A namespaced type's fields are matched by resolved name, so a document may use any
      // prefix for the namespace; an unqualified field, or a type with no namespace, matches
      // the raw label.
      val namespace: Optional[BaseText] = namespaceOf[derivation].let: (uri, qualified) =>
        if qualified then uri else Unset

      def qualifiedField(fieldLabel: BaseText): Boolean =
        namespace.present && !unqualifiedFields.defines(fieldLabel)

      // A type with no namespace of its own has no opinion about prefixes, so a field matches
      // a child by raw label or, failing that, by local name
      def matches(child: Element, fieldLabel: BaseText, wireName: BaseText): Boolean =
        if qualifiedField(fieldLabel) then child.qualified == Xml.Name(namespace, wireName)
        else child.label == wireName || child.localName == wireName

      // `@name[Xml]` / bare `@name` renames: field name -> element/attribute
      // name on the wire. Read back the same way they are written.
      val renames: Map[BaseText, BaseText] = relabelling[derivation, Xml]

      // The first child by raw label, and — for a namespaced type — by local name among the
      // children in the namespace
      val children: scm.HashMap[String, Element] = scm.HashMap.empty
      val localChildren: scm.HashMap[String, Element] = scm.HashMap.empty
      val namespacedChildren: scm.HashMap[String, Element] = scm.HashMap.empty
      var i = 0

      while i < element.children.length do
        element.children.readUnchecked(i) match
          case child: Element =>
            val childLabel = child.label.s
            val local = child.localName.s

            if !children.contains(childLabel) then
              children.update(childLabel, child)

            if !localChildren.contains(local) then localChildren.update(local, child)

            if namespace.present && child.namespace == namespace then
              if !namespacedChildren.contains(local) then namespacedChildren.update(local, child)

          case _ => ()

        i += 1

      // A SINGLE field traversal serving both modes, branching on `foci.active` per field. One
      // traversal is load-bearing: every wisteria traversal re-summons each field's decoder
      // (`summonInline`), recursively deriving nested types, so traversals multiply
      // EXPONENTIALLY with nesting depth at compile time.
      //
      // Accruing mode: each slot is marked failed if its decode registered any focus, and the
      // product is constructed only when every slot is clean — user constructor code never
      // sees a garbage fallback value. On failure the returned value is never used (the
      // caller's scope is tainted), and siblings keep accruing in the meantime. Fail-fast mode:
      // the first recorded error escapes through the tactic, so no focus bookkeeping and no
      // failure scan are needed.
      val active = foci.active

      val slots: Array[Venture[Any]]^{} =
        contexts[derivation]()[Venture[Any]]: [field] =>
          context =>
            val fieldLabel: BaseText = wisteria.label[BaseText]
            val wireName: BaseText = renames(fieldLabel).or(fieldLabel)

            def decodeNow(): field =
              if attributeFields.defines(fieldLabel) then
                // `@attribute` field: decode from the matching attribute as a
                // `Xml.Text`; a missing attribute falls back to the declared
                // default, else the `Absent` sentinel (raise + continue).
                element.attributes(wireName).lay(default.or(context.decoded(Absent))): text =>
                  context.decoded(Text(text))
              else
                // The `AnyRef` cast (rather than `asMatchable`) sidesteps the
                // capture-refined singleton type the checker would otherwise
                // require of the scrutinee.
                context.asInstanceOf[AnyRef] match
                  case marked: Repeatable if marked.repeatable =>
                    // A repeatable (collection) field gathers *all* same-label
                    // children, in document order, into a synthetic `Fragment`
                    // for the collection decoder — stratiform's synthetic-
                    // document idea. Zero matches decode the empty fragment
                    // (an empty collection): the declared default is never
                    // consulted, exactly as on the direct path.
                    val gathered: scm.ArrayBuffer[Node] = scm.ArrayBuffer()
                    var child = 0

                    while child < element.children.length do
                      element.children.readUnchecked(child) match
                        case node: Element =>
                          if matches(node, fieldLabel, wireName) then gathered += node

                        case _ =>
                          ()

                      child += 1

                    context.decoded(Fragment(gathered.toSeq*))

                  case _ =>
                    val found =
                      if qualifiedField(fieldLabel) then namespacedChildren.get(wireName.s)
                      else children.get(wireName.s).orElse(localChildren.get(wireName.s))

                    found match
                      case Some(child) => context.decoded(child)
                      // Missing field: fall back to the case-class declared
                      // default (Wisteria's `default`); if absent, hand the
                      // `Absent` sentinel to the field's decoder. Primitives
                      // detect it and `raise + continue`; nested conjunctions
                      // detect it and may further short-circuit via a
                      // user-supplied `Default[Nested]`.
                      case None => default.or(context.decoded(Absent))

            if !active then Venture(decodeNow())
            else
              focus({
                // Each outer `focus` runs *after* the inner one, so we
                // extend `prior` at the root side (`/outer/inner`), not the
                // leaf side that `XPath#element` would use.
                val base = prior.let(_.path).or(XPath())
                Xml.Focus(base.prepend(wireName, 1))
              }):
                val before = foci.length
                val value: field = decodeNow()
                if foci.length > before then Venture.failed else Venture(value)

      gate[derivation](infer[ProductReflection[derivation]], slots, active)

    // Sealed-trait disjunction picks a variant by element label. We screen
    // the discriminator against `variantLabels` *before* `delegate`-ing so
    // an unrecognised label doesn't punch through as `Variant.Error` — it
    // gets the same `Default[derivation]`-or-abort treatment as a missing
    // discriminator. When the user supplies `Default[derivation]` we
    // register one error at the current focus and continue with the
    // sentinel; without one we abort.
    inline def disjunction[derivation: SumReflection]
    :   derivation is Decodable in Xml =

      // The label list and the `@name[Xml]` / bare `@name` variant-rename map
      // (serialized element label back to the variant name) are
      // per-derivation constants: built once here rather than on every
      // decode call, whose profile they dominated (map building plus
      // generic-equality lookups, per occurrence) — jacinta's map hoist.
      val labels: List[BaseText] = variantLabels

      val variantNames: Map[BaseText, BaseText] =
        variantRelabelling[derivation, Xml].remap: (variant, wire) => wire -> variant

      xml =>
        provide[Foci[Xml.Focus]]:
          provide[Tactic[Xml.Error]]:
            provide[Tactic[Variant.Error]]:
              val discriminable = infer[derivation is Discriminable in Xml]

              // Hoisted so the failure branch can name the discriminant it did not recognise.
              val wireName: Optional[BaseText] = discriminable.discriminate(xml)

              val resolved: Optional[BaseText] =
                wireName.let: wire =>
                  val discriminant = variantNames(wire).or(wire)
                  if labels.has(discriminant) then discriminant else Unset

              def unknown: Xml.Error.Reason =
                wireName.lay(Reason.AbsentVariant(typeName)): wire =>
                  Reason.UnknownVariant(wire, typeName)

              resolved.let: discriminant =>
                delegate(discriminant): [variant <: derivation] =>
                  context => context.decoded(xml)

              . or:
                  summonFrom:
                    case derivationDefault: Default[`derivation`] =>
                      raise(Xml.Error(unknown)) yet derivationDefault()

                    case _ =>
                      // Under an accruing scope, record ONE error and skip the variant decode
                      // without killing the whole scope: the returned value is never used (the
                      // caller sees the focus delta, or the tracking scope is tainted), so
                      // siblings keep accruing. Fail-fast scopes abort as before.
                      if infer[Foci[Xml.Focus]].active
                      then raise(Xml.Error(unknown)) yet null.asInstanceOf[derivation]
                      else abort(Xml.Error(unknown))

  // A sum discriminated by an attribute on the value's element —
  // `<payment type="Card">…</payment>`. This is the shape a sum nested
  // inside a product needs: the product encoder relabels the variant
  // element with the *field's* name, which destroys the label-based default
  // discriminator, so the variant rides in an attribute instead. A named,
  // recognisable class (like jacinta's `DiscriminantField`), so staged and
  // inlined sum parsers can dispatch on the attribute straight off the open
  // tag, without materializing the element.
  final class DiscriminantAttribute[derivation](val attribute: BaseText) extends Discriminable:
    type Self = derivation
    type Form = xylophone.Xml

    def discriminate(xml: xylophone.Xml): Optional[BaseText] = xml match
      case Element(_, attributes, _)           => attributes(attribute)
      case Fragment(Element(_, attributes, _)) => attributes(attribute)
      case _                                   => Unset

    def rewrite(kind: BaseText, xml: xylophone.Xml): xylophone.Xml = xml match
      case Element(label, attributes, children) =>
        Element(label, attributes.updated(attribute, kind), children)

      case Fragment(Element(label, attributes, children)) =>
        Element(label, attributes.updated(attribute, kind), children)

      case other =>
        other

    def variant(xml: xylophone.Xml): xylophone.Xml = xml

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
    private def wrap(fieldName: BaseText, encoded: Xml, scope: Optional[Scope]): Node =
      // The encoded element keeps the scope its own encoder gave it, unless the field is
      // unqualified, whose element is in no namespace
      def relabel(element: Element): Element =
        Element(fieldName, element.attributes, element.children, scope.or(element.scope))

      encoded match
        case element: Element           => relabel(element)
        case Fragment(element: Element) => relabel(element)

        case Fragment(nodes*) =>
          val children = Array.unsafeFrozen(nodes.toArray)
          Element(fieldName, Attributes.empty, children, scope.or(Scope.empty))

        case node: Node =>
          Element(fieldName, Attributes.empty, Array(node), scope.or(Scope.empty))

    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Encodable in Xml =

      value =>
        val attributeFields: Map[BaseText, Set[attribute]] = fieldAnnotations[derivation, attribute]

        val unqualifiedFields: Map[BaseText, Set[unqualified]] =
          fieldAnnotations[derivation, unqualified]

        val namespaced: Optional[(BaseText, Boolean)] = namespaceOf[derivation]

        // The element's scope binds its namespace as the default; a field's element in no
        // namespace, under a namespaced type, undeclares it
        val scope: Scope = namespaced.lay(Scope.empty): (uri, _) => Scope(t"" -> uri)
        val undeclaring: Scope = Scope(t"" -> t"")

        // `@name[Xml]` / bare `@name` renames: field name -> element/attribute
        // name on the wire.
        val renames: Map[BaseText, BaseText] = relabelling[derivation, Xml]

        val attributes: scm.ArrayBuffer[(BaseText, BaseText)] = scm.ArrayBuffer()
        val children: scm.ArrayBuffer[Node] = scm.ArrayBuffer()

        fields(value): [field] =>
          field =>
            val fieldLabel: BaseText = wisteria.label[BaseText]
            val wireName: BaseText = renames(fieldLabel).or(fieldLabel)
            val encoder: field is Encodable in Xml = wisteria.contextual
            val encoded: Xml = encoder.encoded(field)

            val fieldScope: Optional[Scope] = namespaced.let: (_, qualified) =>
              if !qualified || unqualifiedFields.defines(fieldLabel) then undeclaring else Unset

            // `@attribute` fields become attributes carrying the encoded leaf's
            // text — unless the leaf is an absent `Optional`, whose empty
            // `Fragment` yields no attribute at all; every other field becomes
            // a child element via `wrap`.
            if attributeFields.defines(fieldLabel) then
              encoded match
                case Fragment() => ()
                case _          => attributes += wireName -> textOf(encoded).or(t"")
            else
              // The `AnyRef` cast (rather than `asMatchable`) sidesteps the
              // capture-refined singleton type the checker would otherwise
              // require of the scrutinee.
              encoder.asInstanceOf[AnyRef] match
                case marked: Repeatable if marked.repeatable =>
                  // A repeatable (collection) field encodes as repeated
                  // child elements — one per collection element, each
                  // relabelled with the field's wire name — the exact
                  // inverse of the decoder gathering all same-label
                  // children.
                  encoded match
                    case Fragment(nodes*) =>
                      nodes.each: node =>
                        children += wrap(wireName, node, fieldScope)

                    case other =>
                      children += wrap(wireName, other, fieldScope)

                case _ =>
                  children += wrap(wireName, encoded, fieldScope)

        Element
          ( typeName,
            Attributes(attributes.toSeq*),
            Array.unsafeFrozen(children.toArray),
            scope )

    inline def disjunction[derivation: SumReflection]: derivation is Encodable in Xml =
      value =>
        val discriminable = infer[derivation is Discriminable in Xml]

        // `@name[Xml]` / bare `@name` variant renames: variant name -> element
        // label, read back the same way by the decoder.
        val variantNames: Map[BaseText, BaseText] = variantRelabelling[derivation, Xml]

        variant(value): [variant <: derivation] =>
          value =>
            val label = wisteria.label[BaseText]
            discriminable.rewrite(variantNames(label).or(label), contextual.encode(value))

  // ── Direct parsing ─────────────────────────────────────────────────────
  //
  // The shared substance of `Xml.Parsable` and `Xml.Field`, mirroring
  // jacinta's `Json.Parsing`. The two subtraits add nothing: they exist so
  // that neither is a subtype of the other — a subtype relation in either
  // direction would make one family's givens candidates for the other's
  // queries (nominal instances clashing ambiguously with the field fallback,
  // and Wisteria's codec probe finding a generic type's own `Parsable` given
  // while deriving it).
  //
  // `parse` is invoked with the current element just *opened* on the reader
  // (its name and attributes consumed) and must consume the element's
  // content and close tag in full. Unlike jacinta's `Json.Parsing`, there is
  // no `shape()`: xylophone's codecs are shape-free (`Decodable in Xml`
  // carries no `Morphology`), so their direct counterparts are too.
  trait Parsing extends distillate.Parsable:
    type Transport = Xml
    type Reader = Xml.Reader

    // True for collection parsers: a repeated field, gathered occurrence by
    // occurrence by the derived product parser — the direct counterpart of a
    // `Repeatable`-marked `Decodable in Xml`.
    def repeatable: Boolean = false

    // What a field of this type yields when no child element (or attribute)
    // carries its name — the direct counterpart of the AST derivation
    // handing the `Absent` sentinel to the field's decoder: an abort unless
    // overridden (the primitives raise and continue with a zero sentinel;
    // the bridge delegates to its decoder over `Absent`; a derived product
    // expands per sub-field). Takes the read-site `Foci` so that per-sub-
    // field expansion registers the same paths as the AST path's.
    def absent()(using Tactic[Xml.Error], Foci[Xml.Focus]): Self =
      abort(Xml.Error(Reason.Missing))

    // How a field of this type reads from an attribute value — the direct
    // counterpart of the AST derivation decoding `TextNode(text)` for a
    // field marked `@attribute`. For any shape that cannot read from a text
    // leaf, a `Xml.Text` is wrong-shape input on the AST path, which is
    // exactly `absent()`'s behavior — hence the default.
    def attribute(text: BaseText)(using Tactic[Xml.Error], Foci[Xml.Focus]): Self = absent()

  object Parsable:
    // The base of generated parsers: generated code is capture-erased, so
    // the body receives the reader as a neutral carrier, and the capability
    // is asserted here at the rim — the audited point — like the reader's
    // own accessors. (A generated override of `parse` itself would narrow
    // the trait's `Xml.Reader^` parameter to a pure type, which capture
    // checking rejects at the instantiation site.)
    abstract class Direct[value] extends Xml.Parsable:
      type Self = value

      protected def parseCarrier(reader: AnyRef): value

      def parse(reader: Xml.Reader^): value = parseCarrier(reader.asInstanceOf[AnyRef])

    // The call points for a nominal `Parsing` instance in a field position
    // of a *generated* parser (a recursive record's own `Parsable`, a
    // hand-written one, or the `Xml.Field` fallback chain). Both travel as
    // neutral carriers — generated code is capture-erased — and the
    // capability is reasserted here, at the audited point.
    def parseField[value](parsing: AnyRef, reader: AnyRef): value =
      parsing.asInstanceOf[value is Xml.Parsing].parse(reader.asInstanceOf[Xml.Reader^])

    def absentField[value](parsing: AnyRef)(using Tactic[Xml.Error], Foci[Xml.Focus]): value =
      parsing.asInstanceOf[value is Xml.Parsing].absent()

    def apply[value](parser: (reader: Xml.Reader^) => value)
    :   ((value is Xml.Parsable)^{parser}) =

      new Xml.Parsable:
        type Self = value
        def parse(reader: Xml.Reader^): value = parser(reader)

    // The universal bridge from the AST world: materialize one element as an
    // `Xml` and decode it. Field types with only a `Decodable in Xml` keep
    // working through this, and it is the user's one-line escape hatch when
    // a custom decoder must beat a derived direct parser. Absence and
    // attribute reads delegate to the decoder over the same sentinel inputs
    // the AST derivation would hand it, so the two paths stay identical by
    // construction.
    def fromDecodable[value](decodable: (value is Decodable in Xml)^)
    :   ((value is Xml.Parsable)^{decodable}) =

      // The `AnyRef` cast (rather than `asMatchable`) sidesteps the capture-
      // refined singleton type the checker would otherwise require of the
      // scrutinee.
      val repeated: Boolean = decodable.asInstanceOf[AnyRef] match
        case marked: Repeatable => marked.repeatable
        case _                  => false

      // A repeatable decoder (a custom collection) keeps its gathering
      // semantics: each occurrence is materialized separately and the
      // occurrences are decoded together as a synthetic fragment, exactly as
      // the AST derivation hands them over.
      if repeated then
        new Xml.Parsable with Gathering:
          type Self = value
          override def repeatable: Boolean = true
          def parse(reader: Xml.Reader^): value = decodable.decoded(reader.element())

          override def absent()(using Tactic[Xml.Error], Foci[Xml.Focus]): value =
            decodable.decoded(Absent)

          override def attribute(text: BaseText)(using Tactic[Xml.Error], Foci[Xml.Focus]): value =
            decodable.decoded(Text(text))

          def parseElement(reader: Xml.Reader^): Any = reader.element()

          def gathered(elements: List[Any]): value =
            // `Xml.Reader.element()` materializes exactly one `Element`.
            decodable.decoded(Fragment(elements.map(_.asInstanceOf[Node])*))
      else
        new Xml.Parsable:
          type Self = value
          def parse(reader: Xml.Reader^): value = decodable.decoded(reader.element())

          override def absent()(using Tactic[Xml.Error], Foci[Xml.Focus]): value =
            decodable.decoded(Absent)

          override def attribute(text: BaseText)(using Tactic[Xml.Error], Foci[Xml.Focus]): value =
            decodable.decoded(Text(text))

    // The one-line opt-in to direct parsing for a structural type:
    // `given MyType is Xml.Parsable = Xml.Parsable.derived` — a
    // Wisteria-derived direct parser, wrapped as a *nominal* `Parsable` so
    // it participates in the read trigger. Deliberately a method, not a
    // blanket given: a type parses directly only once it has opted in.
    inline def derived[value](using Reflection[value]): value is Xml.Parsable =
      fromField(ParsableDerivation.derivedOne[value])

    // The staged counterpart of `derived`: a macro-generated monomorphic
    // parser whose field values live in typed locals, whose child elements
    // dispatch through packed-`Long` literal comparisons, and whose record
    // is built by a direct constructor call — no `Array[Any]` buffer, no
    // `Mirror`, no per-field boxing. Semantics (wire names, `@attribute`
    // fields, gathering, first-match-wins duplicates, defaults, absents,
    // error foci) mirror `derived` exactly. Requires a top-level or
    // object-nested case class with a single parameter list — sums,
    // method-local classes and other shapes use `derived`.
    inline def staged[value]: value is Xml.Parsable =
      ${ xylophone.internal.stagedParsable[value]('{ adversaria.relabelling[value, Xml] }) }

    def fromField[value](field0: (value is Xml.Parsing)^)
    :   ((value is Xml.Parsable)^{field0}) =

      new Xml.Parsable:
        type Self = value
        def parse(reader: Xml.Reader^): value = field0.parse(reader)
        override def repeatable: Boolean = field0.repeatable
        override def absent()(using Tactic[Xml.Error], Foci[Xml.Focus]): value = field0.absent()

        override def attribute(text: BaseText)(using Tactic[Xml.Error], Foci[Xml.Focus]): value =
          field0.attribute(text)

    // The element-wise hooks of a repeatable (collection) parser. The
    // derived product parser gathers each same-label occurrence through
    // `parseElement` and builds the collection once the element's children
    // end — the direct counterpart of the AST derivation collecting all
    // matching children into a synthetic `Fragment` for the collection
    // decoder. The self type is capturing so implementing instances may
    // capture their element parser or decoder, like any other `Parsing`
    // instance.
    private[xylophone] trait Gathering:
      self: Xml.Parsing^ =>
      def parseElement(reader: Xml.Reader^): Any
      def gathered(elements: List[Any]): Self

    // Shared collection implementation, backing the `fieldCollection` given
    // (elements resolve through the field fallback chain, so nested products
    // still parse directly). Sealed per the codec-thunk pattern: a by-name
    // parameter cannot be named in a capture set.
    def iterable[collection <: Iterable, element]
      ( field: => (element is Xml.Parsing)^ )
      ( using factory: Factory[element, collection[element]] )
    :   collection[element] is Xml.Parsable =

      // [by-name-capture] by-name field parser cannot be named
      caps.unsafe.unsafeAssumePure:
        new Xml.Parsable with Gathering:
          type Self = collection[element]
          override def repeatable: Boolean = true

          def parseElement(reader: Xml.Reader^): Any = field.parse(reader)

          def gathered(elements: List[Any]): collection[element] =
            val builder = factory.newBuilder
            elements.each: item => builder += item.asInstanceOf[element]
            builder.result()

          // A single element read as a collection: one element — the AST
          // collection decoder's behavior when handed a lone element (a
          // whole-document read of a collection value).
          def parse(reader: Xml.Reader^): collection[element] =
            val builder = factory.newBuilder
            builder += field.parse(reader)
            builder.result()

          // A missing collection field is the empty collection on both
          // paths (the AST decoder receives an empty synthetic fragment);
          // the engine routes repeatable fields through `gathered`, so this
          // covers only the wrong-shape and root fallbacks.
          override def absent()(using Tactic[Xml.Error], Foci[Xml.Focus])
          :   collection[element] =

            factory.newBuilder.result()

    // Field instances travel wrapped in the `Field.Adapter`; the engine
    // looks through it for repeatability and the element-wise hooks.
    private def unwrap(parsing: Xml.Parsing): Xml.Parsing = parsing match
      case adapter: Xml.Field.Adapter[?] => adapter.source
      case other                         => other

    // Sentinel for the derived product parser's value buffer: a slot still
    // `AbsentSlot` after the child loop had no matching element
    // (`null`-checking would be unsound — a null-backed `Unset` is a
    // legitimate stored value).
    private[xylophone] val AbsentSlot: AnyRef = new Object

    // Positional construction through the threaded `Mirror`, from the value
    // buffer the parse loop filled. `fromProduct` is the only construction
    // form that works for method-local and object-nested case classes.
    def assemble[derivation <: Product]
      ( reflection: ProductReflection[derivation], values: Array[Any]^{} )
    :   derivation =

      reflection.fromProduct(ArrayProduct(values))

    private final class ArrayProduct(values: Array[Any]^{}) extends Product:
      def canEqual(that: Any): Boolean = true
      def productArity: Int = values.length
      def productElement(index: Int): Any = values.readUnchecked(index)

    // The prior focus's path, extended by one step — evaluated only at
    // error-registration time, exactly as the AST derivation builds its
    // per-field focus (`base.prepend(wireName, 1)`).
    private def descend(base0: Optional[Xml.Focus], name: BaseText): Xml.Focus =
      Xml.Focus(base0.let(_.path).or(XPath()).prepend(name, 1))

    // Support points for staged parsers, which are generated into user
    // modules and so may only reference public members.

    // The wire names of a product's fields, `@name` renames applied.
    def wireNames(names: Array[String]^{}, renames: Map[BaseText, BaseText]): Array[String]^{} =
      names.remap { name => renames(name.tt).or(name.tt).s }

    // A required primitive field whose name never arrived: the primitives'
    // `absent()` semantics — raise and continue with the sentinel.
    def missing[value](sentinel: value)(using Tactic[Xml.Error]): value =
      raise(Xml.Error(Reason.Missing)) yet sentinel

    // Focus bookkeeping for one field read, compiled away when the read
    // site's `Foci` is the inert default — the same short-circuit as the
    // derived parser's loop.
    inline def focusing[result](foci: Foci[Xml.Focus], name: BaseText)(inline block: => result)
    :   result =
      if foci.active then focus(using foci)(descend(prior, name))(block) else block

    // Linear child dispatch for the general step — an unpackable child name,
    // or any child of a `@name`-annotated record. An `@attribute` field
    // never matches a child element, exactly as the derived engine's
    // `indexOf` skips them.
    def childIndex(keys: Array[String]^{}, attributes: Array[Boolean]^{}, label: BaseText): Int =
      val count = keys.length
      val name: String = label.s
      var index = 0

      while index < count do
        if !attributes.readUnchecked(index) && keys.readUnchecked(index) == name then return index
        index += 1

      -1

    // The repeatable-field hooks, looking through the `Field.Adapter` — for
    // staged parsers, which cannot name the private `Gathering` trait. A
    // field gathers only when its (unwrapped) instance is a repeatable
    // `Gathering`, exactly the derived engine's test.
    def repeats(parsing: Xml.Parsing): Boolean =
      val actual = unwrap(parsing)
      actual.repeatable && actual.isInstanceOf[Gathering]

    def parseElement(parsing: Xml.Parsing, reader: Xml.Reader^): Any =
      (unwrap(parsing): @unchecked) match
        case gathering: Gathering => gathering.parseElement(reader)

    def gathered[value](parsing: Xml.Parsing, elements: List[Any]): value =
      (unwrap(parsing): @unchecked) match
        case gathering: Gathering => gathering.gathered(elements).asInstanceOf[value]

    // The derived product parser's engine. `fields0` is an explicit thunk
    // (nameable in the capture set, unlike a by-name) evaluated lazily, so
    // recursive derivation can defer sibling resolution; it yields, per
    // field, the wire name (`@name`-aware), the field's parser, its declared
    // default (or `Unset`) and whether it is an `@attribute` field.
    // `fallback` is the user-supplied `Default[derivation]` sentinel thunk,
    // when one exists. The parse loop lives here, in an ordinary method body
    // — no Wisteria per-field lambda ever closes over the reader.
    //
    // Unlike jacinta's engine, no `Tactic` or `Foci` is captured at
    // derivation time: every raise and focus goes through the capabilities
    // the reader carries, which are resolved at the *read* site — so a
    // `Parsable` given instantiated outside a `validate` boundary still
    // accrues to it, exactly like the AST path (whose derivation is
    // inline-expanded at the `.as` call).
    def product[derivation]
      ( fields0:  () => Array[(String, Xml.Parsing, Any, Boolean)]^{},
        fallback: Optional[() => derivation],
        make:     Array[Any]^{} -> derivation )
    :   ((derivation is Xml.Field)^{fields0, fallback}) =

      new Xml.Field:
        type Self = derivation

        private lazy val fields: Array[(String, Xml.Parsing, Any, Boolean)]^{} = fields0()
        private lazy val keys: Array[String]^{} = fields.remap(_(0))

        // Element dispatch. An `@attribute` field never matches a child
        // element: the AST derivation checks the annotation before looking
        // at the children, so a like-named child is simply ignored there —
        // skipped here.
        private def indexOf(label: BaseText): Int =
          val named = keys
          val count = named.length
          val name: String = label.s
          var index = 0

          while index < count do
            if !fields.readUnchecked(index)(3) && named.readUnchecked(index) == name then return index
            index += 1

          -1

        def parse(reader: Xml.Reader^): derivation =
          given foci: Foci[Xml.Focus] = reader.foci
          given tactic: Tactic[Xml.Error] = reader.errorTactic
          val entries = fields
          val count = entries.length
          val values = Array.allocate[Any](count)
          var index = 0

          while index < count do
            values(index) = AbsentSlot
            index += 1

          // With the inert default `Foci`, per-field `focus` wrapping would
          // observably do nothing, so the hot paths skip it.
          val focused = foci.active

          // `@attribute` fields first: they are available from the element's
          // open tag, before any child is consumed. A missing attribute is
          // handled by the absent-fill loop below, exactly like a missing
          // child element — both mirror the AST's
          // `default.or(context.decoded(Absent))`.
          val attributes = reader.attributes()
          index = 0

          while index < count do
            if entries.readUnchecked(index)(3) then
              attributes(keys.readUnchecked(index).tt).let: text =>
                values(index) =
                  if focused
                  then focus(descend(prior, keys.readUnchecked(index).tt))(entries.readUnchecked(index)(1).attribute(text))
                  else entries.readUnchecked(index)(1).attribute(text)

            index += 1

          var scanning: Boolean = true

          while scanning do reader.nextChild() match
            case Unset => scanning = false

            case label: BaseText =>
              val found = indexOf(label)

              if found < 0 then reader.skipElement()
              else Xml.Parsable.unwrap(entries.readUnchecked(found)(1)) match
                case gathering: Gathering if entries.readUnchecked(found)(1).repeatable =>
                  // Every occurrence of a repeatable field accumulates, in
                  // document order — the AST derivation's gather-all
                  // semantics.
                  val buffer = values.readable(found) match
                    case buffer: scm.ListBuffer[?] => buffer.asInstanceOf[scm.ListBuffer[Any]]

                    case _ =>
                      val buffer = scm.ListBuffer.empty[Any]
                      values(found) = buffer
                      buffer

                  buffer +=
                    ( if focused
                      then focus(descend(prior, keys.readUnchecked(found).tt))(gathering.parseElement(reader))
                      else gathering.parseElement(reader) )

                case _ =>
                  // Unknown children are skipped, and a duplicate child keeps
                  // the first occurrence — the AST derivation's `HashMap`
                  // inserts only when the label is not yet present.
                  if !(values.readable(found).asInstanceOf[AnyRef] eq AbsentSlot)
                  then reader.skipElement()
                  else values(found) =
                    if focused
                    then focus(descend(prior, keys.readUnchecked(found).tt))(entries.readUnchecked(found)(1).parse(reader))
                    else entries.readUnchecked(found)(1).parse(reader)



          index = 0

          while index < count do
            Xml.Parsable.unwrap(entries.readUnchecked(index)(1)) match
              case gathering: Gathering if entries.readUnchecked(index)(1).repeatable =>
                // A repeatable field never consults the declared default:
                // zero occurrences build the empty collection, exactly as
                // the AST derivation decodes an empty synthetic fragment.
                val elements: List[Any] = values.readable(index) match
                  case buffer: scm.ListBuffer[?] => buffer.to(List)
                  case _                         => Nil

                values(index) =
                  if focused
                  then focus(descend(prior, keys.readUnchecked(index).tt))(gathering.gathered(elements))
                  else gathering.gathered(elements)

              case _ =>
                if values.readable(index).asInstanceOf[AnyRef] eq AbsentSlot then
                  val declared = entries.readUnchecked(index)(2).asInstanceOf[Optional[Any]]

                  values(index) =
                    if declared.present then declared
                    else if focused
                    then focus(descend(prior, keys.readUnchecked(index).tt))(entries.readUnchecked(index)(1).absent())
                    else entries.readUnchecked(index)(1).absent()

            index += 1

          make(Array.freeze(values))

        // A missing (or wrong-shape) product value: one raise at the current
        // focus, then the user-supplied `Default[derivation]` sentinel, or
        // an absent-build in which every sub-field takes its declared
        // default or raises at its own focus — exactly the AST derivation's
        // `decodeElement` wrong-shape fallback, so a missing nested case
        // class expands per sub-field on both paths (or collapses to one
        // error under a `Default`).
        override def absent()(using tactic: Tactic[Xml.Error], foci: Foci[Xml.Focus])
        :   derivation =

          raise(Xml.Error(Reason.AbsentProduct(typeName)))
          fallback.lay(absentBuild()): instantiate => instantiate()

        private def absentBuild()(using tactic: Tactic[Xml.Error], foci: Foci[Xml.Focus])
        :   derivation =

          val entries = fields
          val count = entries.length
          val values = Array.allocate[Any](count)
          val focused = foci.active
          var index = 0

          while index < count do
            values(index) = Xml.Parsable.unwrap(entries.readUnchecked(index)(1)) match
              // A repeatable field builds the empty collection, exactly as
              // the AST derivation's wrong-shape fallback gathers zero
              // children — the declared default is never consulted.
              case gathering: Gathering if entries.readUnchecked(index)(1).repeatable =>
                if focused
                then focus(descend(prior, keys.readUnchecked(index).tt))(gathering.gathered(Nil))
                else gathering.gathered(Nil)

              case _ =>
                val declared = entries.readUnchecked(index)(2).asInstanceOf[Optional[Any]]

                if declared.present then declared
                else if focused
                then focus(descend(prior, keys.readUnchecked(index).tt))(entries.readUnchecked(index)(1).absent())
                else entries.readUnchecked(index)(1).absent()

            index += 1

          make(Array.freeze(values))

  // The direct-parsing counterpart of `Decodable in Xml`: consumes elements
  // from an `Xml.Reader` instead of walking a materialized `Xml`, so
  // `read[value in Xml]` can instantiate values without building the tree.
  // `Parsable` is the opt-in surface: explicit instances,
  // `Xml.Parsable.derived`, and the read trigger. It has no blanket fallback
  // given, so no read changes behavior until a type opts in; the fallback
  // belongs to its operational sibling, `Xml.Field`.
  trait Parsable extends Parsing

  object Field:
    // Adapts an opted-in nominal instance (or any other `Parsing`) for use
    // as a field parser. A named class (with the wrapped instance held as a
    // neutral carrier and reasserted at the rim, preserving the declared
    // capture), following jacinta's `Json.Field.Adapter`.
    private[xylophone] final class Adapter[value](source0: AnyRef) extends Xml.Field:
      type Self = value

      private[xylophone] def source: value is Xml.Parsing =
        source0.asInstanceOf[value is Xml.Parsing]

      def parse(reader: Xml.Reader^): value = source.parse(reader)
      override def repeatable: Boolean = source.repeatable

      override def absent()(using Tactic[Xml.Error], Foci[Xml.Focus]): value = source.absent()

      override def attribute(text: BaseText)(using Tactic[Xml.Error], Foci[Xml.Focus]): value =
        source.attribute(text)

    def apply[value](parsing: (value is Xml.Parsing)^): ((value is Xml.Field)^{parsing}) =
      Adapter[value](parsing.asInstanceOf[AnyRef])
      . asInstanceOf[(value is Xml.Field)^{parsing}]

  // The operational face of direct parsing: how a field's value is read from
  // an `Xml.Reader`, whether directly or through the AST bridge. This is the
  // typeclass the product derivation resolves per field; `Xml3` carries the
  // universal fallback — declared as an (inherited) member of this companion
  // so Wisteria's wrapper detection excludes the fallback during codec
  // probing.
  trait Field extends Parsing

  // Direct-parsing primitives, mirroring the primitive `Decodable in Xml`
  // givens above exactly: a missing or wrong-shape element and an
  // unparseable value are *raised* (not aborted) with the same zero / false
  // sentinel continuation, so per-field accrual under a
  // `validate[Xml.Focus]` boundary behaves identically on both paths.
  // Genuinely pure — parse-time raising happens through the tactics the
  // reader carries.
  private def primitiveParsable[value](sentinel: value, expected: BaseText)
    ( convert: BaseText -> Optional[value] )
  :   value is Xml.Parsable =

    new Xml.Parsable:
      type Self = value

      def parse(reader: Xml.Reader^): value =
        reader.text().lay(reader.fault(Reason.Untextual(expected)) yet sentinel): text =>
          convert(text).or(reader.fault(Reason.Malformed(text, expected)) yet sentinel)

      override def absent()(using Tactic[Xml.Error], Foci[Xml.Focus]): value =
        raise(Xml.Error(Reason.Absent(expected))) yet sentinel

      override def attribute(text: BaseText)(using Tactic[Xml.Error], Foci[Xml.Focus]): value =
        convert(text).or(raise(Xml.Error(Reason.Malformed(text, expected))) yet sentinel)

  // `Int`/`Long`/`Double`/`Boolean` read their value straight from the
  // buffered content chars (`reader.int()`/`long()`/`double()`/`boolean()`),
  // so a valid scalar never materializes the value `Text` that `text()`
  // would — only exotic content does, through the reader's internal general
  // fallback, which parses exactly as the primitives here would have.
  given intParsable: Int is Xml.Parsable = new Xml.Parsable:
    type Self = Int
    def parse(reader: Xml.Reader^): Int =
      reader.int().or(reader.fault(Reason.Untextual(t"Int")) yet 0)

    override def absent()(using Tactic[Xml.Error], Foci[Xml.Focus]): Int =
      raise(Xml.Error(Reason.Absent(t"Int"))) yet 0

    override def attribute(text: BaseText)(using Tactic[Xml.Error], Foci[Xml.Focus]): Int =
      try Integer.parseInt(text.s)
      catch case _: NumberFormatException =>
          raise(Xml.Error(Reason.Malformed(text, t"Int"))) yet 0

  given longParsable: Long is Xml.Parsable = new Xml.Parsable:
    type Self = Long
    def parse(reader: Xml.Reader^): Long =
      reader.long().or(reader.fault(Reason.Untextual(t"Long")) yet 0L)

    override def absent()(using Tactic[Xml.Error], Foci[Xml.Focus]): Long =
      raise(Xml.Error(Reason.Absent(t"Long"))) yet 0L

    override def attribute(text: BaseText)(using Tactic[Xml.Error], Foci[Xml.Focus]): Long =
      try jl.Long.parseLong(text.s)
      catch case _: NumberFormatException =>
          raise(Xml.Error(Reason.Malformed(text, t"Long"))) yet 0L

  given shortParsable: Short is Xml.Parsable = primitiveParsable(0.toShort, t"Short"): text =>
    try jl.Short.parseShort(text.s) catch case _: NumberFormatException => Unset

  given byteParsable: Byte is Xml.Parsable = primitiveParsable(0.toByte, t"Byte"): text =>
    try jl.Byte.parseByte(text.s) catch case _: NumberFormatException => Unset

  given doubleParsable: Double is Xml.Parsable = new Xml.Parsable:
    type Self = Double
    def parse(reader: Xml.Reader^): Double =
      reader.double().or(reader.fault(Reason.Untextual(t"Double")) yet 0.0)

    override def absent()(using Tactic[Xml.Error], Foci[Xml.Focus]): Double =
      raise(Xml.Error(Reason.Absent(t"Double"))) yet 0.0

    override def attribute(text: BaseText)(using Tactic[Xml.Error], Foci[Xml.Focus]): Double =
      try jl.Double.parseDouble(text.s)
      catch case _: NumberFormatException =>
          raise(Xml.Error(Reason.Malformed(text, t"Double"))) yet 0.0

  given floatParsable: Float is Xml.Parsable = primitiveParsable(0.0f, t"Float"): text =>
    try jl.Float.parseFloat(text.s) catch case _: NumberFormatException => Unset

  given booleanParsable: Boolean is Xml.Parsable = new Xml.Parsable:
    type Self = Boolean
    def parse(reader: Xml.Reader^): Boolean =
      reader.boolean().or(reader.fault(Reason.Untextual(t"Boolean")) yet false)

    override def absent()(using Tactic[Xml.Error], Foci[Xml.Focus]): Boolean =
      raise(Xml.Error(Reason.Absent(t"Boolean"))) yet false

    override def attribute(text: BaseText)(using Tactic[Xml.Error], Foci[Xml.Focus]): Boolean =
      text.s match
        case "true"  => true
        case "false" => false
        case _       => raise(Xml.Error(Reason.Malformed(text, t"Boolean"))) yet false

  // Element-wise `Xml.Field` for collections, resolved during derivation:
  // the element's own parser comes from the fallback chain, so nested
  // products still parse directly. Declared here (not in `Xml3`) so it beats
  // the fallback, and collection types never reach its `Reflection` case (a
  // `List`'s own `Mirror` would otherwise derive it as a sum). The instance
  // is repeatable: the product engine gathers every same-label occurrence,
  // exactly as the AST derivation collects all matching children.
  given fieldCollection: [collection <: Iterable, element]
  =>  ( factory: Factory[element, collection[element]] )
  =>  ( field: => (element is Xml.Field)^ )
  =>  collection[element] is Xml.Field =
    Xml.Field(Xml.Parsable.iterable[collection, element](field))

  // Alias counterparts: the opaque prelude collections do not conform to
  // `Iterable`, so each gets its own instance built at the underlying stdlib
  // type and cast (a no-op at erasure).
  given fieldList: [list <: List, element]
  =>  ( field: => (element is Xml.Field)^ )
  =>  list[element] is Xml.Field =
    Xml.Field(Xml.Parsable.iterable[scala.collection.immutable.List, element](field))
    . asInstanceOf[list[element] is Xml.Field]

  given fieldSet: [set <: Set, element]
  =>  ( field: => (element is Xml.Field)^ )
  =>  set[element] is Xml.Field =
    Xml.Field(Xml.Parsable.iterable[scala.collection.immutable.Set, element](field))
    . asInstanceOf[set[element] is Xml.Field]

  given fieldSeries: [sequence <: Sequence, element]
  =>  ( field: => (element is Xml.Field)^ )
  =>  sequence[element] is Xml.Field =
    Xml.Field(Xml.Parsable.iterable[Vector, element](field))
    . asInstanceOf[sequence[element] is Xml.Field]

  // The direct read of a field type carried by a plain text codec — the
  // direct counterpart of the `decodable` blanket's `Decodable in Text`
  // branch, including its absence behavior: a missing field raises and then
  // decodes the empty text, exactly as the AST branch decodes the `Absent`
  // sentinel.
  private[xylophone] def textCodecParsable[value]
    ( using codec: value is Decodable in BaseText )
  :   value is Xml.Parsable =

    new Xml.Parsable:
      type Self = value

      def parse(reader: Xml.Reader^): value =
        codec.decoded(reader.text().or(reader.fault(Reason.Empty) yet t""))

      override def absent()(using Tactic[Xml.Error], Foci[Xml.Focus]): value =
        raise(Xml.Error(Reason.Missing))
        codec.decoded(t"")

      override def attribute(text: BaseText)(using Tactic[Xml.Error], Foci[Xml.Focus]): value =
        codec.decoded(text)

  object ParsableDerivation extends Derivable[Xml.Field]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Xml.Field =

      // Like `DecodableDerivation.conjunction`: a single `contexts`
      // traversal collects, per field, its wire name (`@name`-aware), its
      // parser (via the `Field` fallback chain), its declared default and
      // its `@attribute` marking; `Xml.Parsable.product` owns the parse
      // loop, so no per-field lambda ever closes over the reader. Sealed per
      // the codec-thunk pattern: the field parsers the thunk resolves may
      // capture resolution-scoped capabilities (the AST bridge does).
      // [field-purity] derived product field codec, codec-thunk seal
      caps.unsafe.unsafeAssumePure:
        val reflection = infer[ProductReflection[derivation]]

        // A user-supplied `Default[derivation]` collapses a missing nested
        // value to a single error, exactly as `decodeElement`'s wrong-shape
        // fallback does on the AST path.
        val fallback: Optional[() => derivation] = summonFrom:
          case derivationDefault: Default[`derivation`] =>
            val instantiate: () => derivation = () => derivationDefault()
            Optional(instantiate)

          case _ =>
            Unset

        Xml.Parsable.product[derivation](
          { () =>
            val attributeFields: Map[BaseText, Set[attribute]] =
              fieldAnnotations[derivation, attribute]

            // `@name[Xml]` / bare `@name` renames: field name ->
            // element/attribute name on the wire, read back the same way
            // they are written.
            val renames: Map[BaseText, BaseText] = relabelling[derivation, Xml]

            contexts[derivation]():
              [field] => context =>
                val fieldLabel: BaseText = wisteria.label[BaseText]

                ( renames(fieldLabel).or(fieldLabel).s,
                  context: Xml.Parsing,
                  default[Optional[field]]: Any,
                  attributeFields.defines(fieldLabel) )
          },
          fallback,
          values => Xml.Parsable.assemble(reflection, values))

    inline def disjunction[derivation: SumReflection]: derivation is Xml.Field =
      // A sum's variant is the element's own label, which the AST path
      // resolves through `Discriminable` (possibly a custom instance), with
      // `variantLabels` screening and `Default`-or-abort handling for an
      // unknown discriminator — so a sum always takes the AST bridge over
      // its derived (or custom) decoder, keeping the two paths identical by
      // construction, as stratiform's TEL derivation does. Sealed per the
      // codec-thunk pattern: the instance captures a resolution-scoped
      // decoder.
      // [field-purity] derived sum field codec, codec-thunk seal
      caps.unsafe.unsafeAssumePure:
        Xml.Field(Xml.Parsable.fromDecodable(infer[derivation is Decodable in Xml]))

  case class attribute() extends StaticAnnotation

  // The namespace of a derived type's elements: `@xmlns("urn:x") case class Order(...)` encodes
  // as `<Order xmlns="urn:x">` with its fields' elements in the same namespace, and decodes
  // from any document whose elements resolve to that namespace, whatever prefixes it uses.
  // `qualified = false` (or `@unqualified` on a field) leaves the fields' elements in no
  // namespace, as an XML Schema with `elementFormDefault="unqualified"` has them.
  case class xmlns(uri: BaseText, qualified: Boolean = true) extends StaticAnnotation
  case class unqualified() extends StaticAnnotation

  // The namespace of a type which cannot be annotated: `given Rect is Xml.Namespaced =
  // Xml.Namespaced("http://www.w3.org/2000/svg")` takes precedence over an `@xmlns`.
  object Namespaced:
    def apply[value](uri: BaseText, qualified: Boolean = true): value is Namespaced =
      Bound[value](uri, qualified)

    class Bound[value](val namespace: BaseText, val qualified: Boolean) extends Namespaced:
      type Self = value

  trait Namespaced extends Typeclass.Pure:
    def namespace: BaseText
    def qualified: Boolean

  // The namespace a derived codec puts a type's elements in, and whether its fields' elements
  // share it: from a `Namespaced` given, else from the type's `@xmlns`, else none
  private[xylophone] inline def namespaceOf[derivation]: Optional[(BaseText, Boolean)] = summonFrom:
    case namespaced: (`derivation` is Namespaced) =>
      (namespaced.namespace, namespaced.qualified)

    case _ =>
      summonInline[derivation is Annotated by xmlns] match
        case fields: Annotated.AnnotatedFields[Xml.xmlns, ?, ?, ?] @unchecked =>
          fields.annotations.stdlib.headOption
          . map { annotation => (annotation.uri, annotation.qualified) }
          . optional

        case _ =>
          Unset

  case class XmlAttribute(label: BaseText, elements: Set[BaseText], global: Boolean):
    type Self <: Label
    type Topic
    type Plane <: Label

    def targets(tag: BaseText): Boolean = global || elements.has(tag)

    def merge(that: XmlAttribute): XmlAttribute =
      XmlAttribute(label, elements + that.elements, global || that.global)

  // A resolved name: the namespace URI, if any, and the local part, as the pair by which
  // elements and attributes are identified once prefixes have been resolved. `Name("a")` is
  // an unqualified name; `Name.of(label)` splits a raw `prefix:local` label without resolving it.
  object Name:
    def apply(local: BaseText): Name = Name(Unset, local)

    // The prefix and local part of a raw label; a label with no colon has no prefix
    def split(label: BaseText): (Optional[BaseText], BaseText) =
      val colon = label.s.indexOf(':')

      if colon < 0 then (Unset, label)
      else (label.keep(colon), label.skip(colon + 1))

    // Clark notation, `{uri}local`, or the bare local part
    given showable: Name is Showable = name =>
      name.namespace.lay(name.local)(uri => t"{$uri}${name.local}")

    given inspectable: Name is Inspectable = name => t"Name(${name.show.inspect})"

  case class Name(namespace: Optional[BaseText], local: BaseText)

  def header: Header = Header("1.0", Unset, Unset)

  extension (xml: List[Xml])
    def nodes: Array[Node]^{} =
      var count = 0

      xml.each: item =>
        item match
          case fragment: Fragment => count += fragment.nodes.length
          case _                  => count += 1

      val array = Array.allocate[Node](count)

      var index = 0

      xml.each: item =>
        item match
          case Fragment(nodes*) =>
            for node <- nodes do
              array(index) = node
              index += 1

          case node: Node =>
            array(index) = node
            index += 1

      Array.freeze(array)

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


  // The parser reads UTF-8 bytes, so a byte source is parsed as it arrives; a text source
  // is encoded first (see `XmlParser`).
  given aggregable: [content <: Label: Reifiable to List[String]]
  =>  (schema: XmlSchema, scope: Scope, namespacing: Namespacing)
  =>  (tactic: Tactic[Parse.Error])
  =>  (((Xml of content) is Aggregable by Data)^{tactic}) =

    input => XmlParser.fromDataChain(input).parseXml(headers0 = false).of[content]

  given aggregable2: (schema: XmlSchema, scope: Scope, namespacing: Namespacing)
  =>  (tactic: Tactic[Parse.Error])
  =>  ((Xml is Aggregable by Data)^{tactic}) =
    input => XmlParser.fromDataChain(input).parseXml(headers0 = false)

  given aggregableText: [content <: Label: Reifiable to List[String]]
  =>  (schema: XmlSchema, scope: Scope, namespacing: Namespacing)
  =>  (tactic: Tactic[Parse.Error])
  =>  (((Xml of content) is Aggregable by BaseText)^{tactic}) =

    input => XmlParser.fromChain(input).parseXml(headers0 = false).of[content]

  given aggregable2Text: (schema: XmlSchema, scope: Scope, namespacing: Namespacing)
  =>  (tactic: Tactic[Parse.Error])
  =>  ((Xml is Aggregable by BaseText)^{tactic}) =
    input => XmlParser.fromChain(input).parseXml(headers0 = false)

  // HTTP content-type integration. `Abstractable across HttpStreams` makes an
  // `Xml` value usable as an HTTP request/response body (telekinesis derives
  // `Postable`/`Servable` from it); `Instantiable across HttpRequests` reads a
  // request/response body back into `Xml`.
  given abstractable: (encoder: Codepage)
  =>  Xml is Abstractable across HttpStreams to HttpStreams.Content =

    new Abstractable:
      type Self = Xml
      type Domain = HttpStreams
      type Result = HttpStreams.Content

      def genericize(xml: Xml): HttpStreams.Content =
        (t"application/xml; charset=${encoder.encoding.name}", HttpStreams.Body(xml.show.in[Data]))

  given instantiable: (schema: XmlSchema, scope: Scope, namespacing: Namespacing)
  =>  (tactic: Tactic[Parse.Error])
  =>  ((Xml is Instantiable across HttpRequests from BaseText)^{tactic}) =

    text => Chain(text).read[Xml]

  // Direct parsing: when the value knows how to consume elements itself, the
  // `Xml` tree is never materialized. Declared here (not in `Xml2`, where the
  // `Decodable`-based `aggregableIn` now lives) so it wins whenever an
  // `Xml.Parsable` exists, and is otherwise inapplicable — existing code
  // resolves exactly as before. The read-site `Foci` travels with the reader
  // so direct decode errors accrue with the same field foci as the AST
  // path's.
  given aggregableParsed: [value]
  =>  ( parsable: (value is Xml.Parsable)^ )
  =>  ( schema: XmlSchema, scope: Scope, namespacing: Namespacing )
  =>  ( tactic: Tactic[Parse.Error], xmlTactic: Tactic[Xml.Error], foci: Foci[Xml.Focus] )
  =>  ( ((value in Xml) is Aggregable by Data)^{parsable, tactic, xmlTactic} ) =

    input => parseDirectData(input, parsable).asInstanceOf[value in Xml]

  given aggregableParsedText: [value]
  =>  ( parsable: (value is Xml.Parsable)^ )
  =>  ( schema: XmlSchema, scope: Scope, namespacing: Namespacing )
  =>  ( tactic: Tactic[Parse.Error], xmlTactic: Tactic[Xml.Error], foci: Foci[Xml.Focus] )
  =>  ( ((value in Xml) is Aggregable by BaseText)^{parsable, tactic, xmlTactic} ) =

    input => parseDirect(input, parsable).asInstanceOf[value in Xml]

  // Whole-value direct reads: when the entire content is already in hand,
  // parse it in place rather than wrapping it in a one-element stream —
  // jacinta's `readableParsed` precedent. Concrete in `Data` / `Text`, so
  // they beat the composed pipelines by specificity.
  given readableParsed: [value]
  =>  ( parsable: (value is Xml.Parsable)^ )
  =>  ( schema: XmlSchema, scope: Scope, namespacing: Namespacing )
  =>  ( tactic: Tactic[Parse.Error], xmlTactic: Tactic[Xml.Error], foci: Foci[Xml.Focus] )
  =>  ( (Data is Readable to (value in Xml))^{parsable, tactic, xmlTactic} ) =

    data => parseDirectData(data, parsable).asInstanceOf[value in Xml]

  given readableParsedText: [value]
  =>  ( parsable: (value is Xml.Parsable)^ )
  =>  ( schema: XmlSchema, scope: Scope, namespacing: Namespacing )
  =>  ( tactic: Tactic[Parse.Error], xmlTactic: Tactic[Xml.Error], foci: Foci[Xml.Focus] )
  =>  ( (BaseText is Readable to (value in Xml))^{parsable, tactic, xmlTactic} ) =

    text => parseDirect(text, parsable).asInstanceOf[value in Xml]

  // Direct-parsing counterpart of `Xml2.aggregableIn`: drives an
  // `Xml.Parsable` instance over the input through an `Xml.Reader`, so no
  // tree is built for the values the instance reads directly. Like
  // `aggregableIn`, the direct read path does not thread position tracking
  // (there is no `PositionIndex` — the result is the caller's value, not an
  // `Xml`).
  //
  // The root dispatch mirrors `parseXml(headers0 = false)` composed with
  // `Xml#as`: a root element is opened and handed to the instance; root
  // character data decodes as a text leaf (a `Fragment(TextNode(…))` on the
  // AST path); trailing content — a multi-node `Fragment`, which `as`
  // decodes as wrong-shape — and an absent root take the instance's
  // `absent()` fallback, the exact wrong-shape continuation of the AST
  // derivation. (A degenerate root — a comment, PI or doctype where the
  // value should be — also lands on `absent()`, where the AST path may
  // instead report a `Parse.Error`; the divergence is confined to inputs
  // that fail on both paths.)
  private def parseDirect[value](input: Chain[BaseText], parsable: (value is Xml.Parsable)^)
    ( using schema:      XmlSchema,
            scope:       Scope,
            namespacing: Namespacing,
            tactic:      Tactic[Parse.Error],
            xmlTactic:   Tactic[Xml.Error],
            foci:        Foci[Xml.Focus] )
  :   value =

    parseWith(XmlParser.fromChain(input), parsable)

  // The byte forms, the parser's own input: a chain of blocks, and a whole `Data`.
  private def parseDirectData[value](input: Chain[Data], parsable: (value is Xml.Parsable)^)
    ( using schema:      XmlSchema,
            scope:       Scope,
            namespacing: Namespacing,
            tactic:      Tactic[Parse.Error],
            xmlTactic:   Tactic[Xml.Error],
            foci:        Foci[Xml.Focus] )
  :   value =

    parseWith(XmlParser.fromDataChain(input), parsable)

  private def parseDirectData[value](input: Data, parsable: (value is Xml.Parsable)^)
    ( using schema:      XmlSchema,
            scope:       Scope,
            namespacing: Namespacing,
            tactic:      Tactic[Parse.Error],
            xmlTactic:   Tactic[Xml.Error],
            foci:        Foci[Xml.Focus] )
  :   value =

    parseWith(XmlParser.fromData(input), parsable)

  // The legacy interoperation shape: a stdlib `Iterator` of chunks.
  private def parseDirect[value](input: Iterator[BaseText], parsable: (value is Xml.Parsable)^)
    ( using schema:      XmlSchema,
            scope:       Scope,
            namespacing: Namespacing,
            tactic:      Tactic[Parse.Error],
            xmlTactic:   Tactic[Xml.Error],
            foci:        Foci[Xml.Focus] )
  :   value =

    parseWith(XmlParser.fromIterator(input), parsable)

  // Whole-`Text` form: a pre-filled single-chunk cursor, no iterator
  // plumbing (the `readableParsed` entry).
  private def parseDirect[value](input: BaseText, parsable: (value is Xml.Parsable)^)
    ( using schema:      XmlSchema,
            scope:       Scope,
            namespacing: Namespacing,
            tactic:      Tactic[Parse.Error],
            xmlTactic:   Tactic[Xml.Error],
            foci:        Foci[Xml.Focus] )
  :   value =

    parseWith(XmlParser.fromText(input), parsable)

  private def parseWith[value](parser: XmlParser^, parsable: (value is Xml.Parsable)^)
    ( using schema:      XmlSchema,
            scope:       Scope,
            namespacing: Namespacing,
            tactic:      Tactic[Parse.Error],
            xmlTactic:   Tactic[Xml.Error],
            foci:        Foci[Xml.Focus] )
  :   value =

    // [by-name-receiver] The session body captures the parser that is also `directSession`'s
    // receiver, which separation checking reports as an overlap; both are the one owner, and
    // the body runs only within the receiver's call.
    scala.caps.unsafe.unsafeAssumeSeparate:
     parser.directSession:
      parser.directRoot() match
        case 0 =>
          val result = parsable.parse(Xml.Reader(parser, tactic, xmlTactic, foci))
          if parser.directTrailing() then parsable.absent() else result

        case 1 =>
          val text = parser.directRootText()
          if parser.directTrailing() then parsable.absent() else parsable.attribute(text)

        case _ =>
          parsable.absent()

  // The single place `.load[Xml]` branches on position tracking. With
  // `parsing.trackPositions` in scope the parser records source positions and the
  // resulting `Document[Xml]` carries them in its `Header` metadata, locatable via
  // `document.locate(path)`; otherwise the untracked throughput path is unchanged.
  // `headers0 = true` accepts a leading `<?xml …?>` declaration, which is lifted
  // into the metadata `Header` and dropped from the tree, so the tracked value has
  // the same shape as a header-less load (keeping it aligned with the index, which
  // is built from the root element alone).
  given loadable: (schema: XmlSchema, scope: Scope, namespacing: Namespacing)
  =>  (tactic: Tactic[Parse.Error], tracking: PositionTracking)
  =>  ((Xml is Loadable by BaseText)^{tactic}) = stream =>
    // The chunk chain view of the pull endpoint (the audited bridge); the parser encodes
    // each chunk to UTF-8 as it reaches it.
    val chunks =
      zephyrine.chain(stream.asInstanceOf[AnyRef].asInstanceOf[(Stream[BaseText] over Credit)^])

    val parser: XmlParser^ = tracking match
      case PositionTracking.On  => XmlParser.fromChainTracked(chunks)
      case PositionTracking.Off => XmlParser.fromChain(chunks)

    loaded(parser, tracking)

  // The byte form, the parser's own input: a byte source — a file, an HTTP body — is parsed
  // as it arrives, with no decoding. Positions count bytes (see `XmlParser`).
  given loadableData: (schema: XmlSchema, scope: Scope, namespacing: Namespacing)
  =>  (tactic: Tactic[Parse.Error], tracking: PositionTracking, buffering: Buffering)
  =>  ((Xml is Loadable by Data)^{tactic}) = stream =>
    // The non-consume `load` crosses to the consuming cursor as a neutral reference.
    val bytes = stream.asInstanceOf[AnyRef].asInstanceOf[(Stream[Data] over Credit)^]

    val parser: XmlParser^ = tracking match
      case PositionTracking.On  => XmlParser.fromStreamTracked(bytes)
      case PositionTracking.Off => XmlParser.fromStream(bytes)

    loaded(parser, tracking)

  // The document a parser yields: a leading declaration is lifted into the metadata
  // `Header` and dropped from the tree, with the `PositionIndex` when tracking is on.
  private def loaded(parser: XmlParser^, tracking: PositionTracking)
    ( using Tactic[Parse.Error] )
  :   Document[Xml] =

    val parsed = parser.parseXml(headers0 = true)

    val positionIndex: Optional[PositionIndex] = tracking match
      case PositionTracking.On =>
        val index = parser.rootIndex
        PositionIndex(if index == null then Array.empty[Int] else index)

      case PositionTracking.Off =>
        Unset

    def withIndex(header: Header): Header = header.copy(positionIndex = positionIndex)

    parsed match
      case Fragment((header: Header), rest*) =>
        if rest.isEmpty then abort(Parse.Error(Xml, Position(1.u, 1.u), Issue.BadDocument))
        else if rest.length == 1 then Document(rest.head, withIndex(header))
        else Document(Fragment(rest*), withIndex(header))

      case _: Header =>
        abort(Parse.Error(Xml, Position(1.u, 1.u), Issue.BadDocument))

      case fragment: Fragment =>
        Document(fragment, withIndex(Xml.header))

      case node: Node =>
        Document(node, withIndex(Xml.header))

  // `^{monitor}` only: `Probate` is not capture-tracked (see rep/REVIEW.md).
  given streamable: (monitor: Monitor, probate: Probate)
  =>  ((Document[Xml] is Streamable by BaseText over Credit)^{monitor}) =
    document => zephyrine.Stream(emit(document))

  // Serializes on a fiber, handing out text as it is produced, so a large document can be
  // written to a socket or file before it is fully rendered.
  def emit(document: Document[Xml])
    ( using formatting: Formatting, monitor: Monitor, probate: Probate )
  :   Iterator[BaseText] =

    val producer = Producer[BaseText]()
    val output = producer.iterator

    producer.transfer: (producer, _, _) ?=>
      writeDocument(new Textual(producer()), formatting, document)
      producer().finish()

    output

  // The push form: serializes on the caller's thread, handing each block to `deliver` as it
  // fills, so no fiber is involved — the right shape for writing to a file, a socket or an
  // `OutputStream`, where the pull form above pays a thread handoff per block. The medium is
  // chosen by the type argument: `emit[Text]` delivers text, and `emit[Data]` delivers UTF-8
  // bytes encoded straight from the serializer, with no intermediate `Text` per block.
  def emit[medium: Emitter](document: Document[Xml], deliver: medium => Unit)
    ( using formatting: Formatting, buffering: Buffering )
  :   Unit =

    summon[Emitter[medium]].emit(document, formatting, deliver)

  // How the push form of `emit` reaches its consumer: as text blocks through a `Producer[Text]`,
  // or as UTF-8 blocks written directly by the byte-level writer.
  object Emitter:
    given text: Emitter[BaseText]:
      def emit(document: Document[Xml], formatting: Formatting, deliver: BaseText => Unit)
        ( using Buffering )
      :   Unit =

        Producer.sink[BaseText](deliver): producer =>
          writeDocument(new Textual(producer), formatting, document)

    given data: Emitter[Data]:
      def emit(document: Document[Xml], formatting: Formatting, deliver: Data => Unit)
        ( using buffering: Buffering )
      :   Unit =

        given Formatting = formatting

        lend(document): region => interval => deliver(region.materialize(interval))

  trait Emitter[medium]:
    def emit(document: Document[Xml], formatting: Formatting, deliver: medium => Unit)
      ( using Buffering )
    :   Unit

  // The borrowing form of the push `emit`: serializes to UTF-8 on the caller's thread, and lends
  // each filled block to `lending` as a `Region[Data]` with its branded extent, valid only for
  // the duration of the call — the discipline of `Stream.lend` — so nothing is copied. A
  // consumer that must keep the bytes materializes them itself; `emit[Data]` is that consumer
  // for the common case.
  def lend(document: Document[Xml])(lending: Producer.Lending[Data])
    ( using formatting: Formatting, buffering: Buffering )
  :   Unit =

    Producer.Utf8Writer.lend(lending): writer =>
      writeDocument(new Bytes(writer), formatting, document)

  private def writeDocument(out: Out^, formatting: Formatting, document: Document[Xml]): Unit =
    writeXml(out, formatting, document.metadata, 0)
    if formatting.indent.present then out.ascii("\n")
    writeXml(out, formatting, document.root, 0)
    if formatting.trailingNewline then out.ascii("\n")

  // The leaf operations of the serializer, per output medium; the traversal in `writeXml` is
  // shared. `Textual` puts text into a `Producer[Text]`, so `show` and the pull form of `emit`
  // render through the same code; `Bytes` writes UTF-8 straight into a byte block through
  // zephyrine's `Utf8Writer`, escaping and encoding each string in one pass over its characters,
  // and lends each block as it fills.
  private[xylophone] trait Out extends caps.ExclusiveCapability, caps.Stateful:
    // Markup known to be ASCII: delimiters and keywords.
    update def ascii(text: String): Unit

    // An element or attribute name, or a namespace prefix, written verbatim; a document repeats
    // its names, so a byte-level output may cache their encodings.
    update def name(text: String): Unit

    // Text written verbatim, escaped for nothing: the content of comments, CDATA sections,
    // doctypes and processing instructions.
    update def raw(text: String): Unit

    // Character data, escaped for text content.
    update def text(text: String): Unit

    // An attribute value, escaped for a double-quoted attribute.
    update def attribute(text: String): Unit

  private[xylophone] final class Textual(producer: (Producer[BaseText])^) extends Out:
    update def ascii(text: String): Unit = producer.put(text.tt)
    update def name(text: String): Unit = producer.put(text.tt)
    update def raw(text: String): Unit = producer.put(text.tt)
    update def text(text: String): Unit = writeEscaped(text, false)
    update def attribute(text: String): Unit = writeEscaped(text, true)

    // Escapes character data, or a double-quoted attribute value when `attribute`. `&` and `<`
    // must be escaped; `>` is escaped too so the `]]>` sequence can never appear; and a carriage
    // return is written as a character reference so XML line-ending normalization cannot
    // rewrite it to `\n`. In an attribute, the delimiter and the whitespace characters tab and
    // line feed become character references too, since attribute-value normalization would
    // otherwise collapse literal whitespace to spaces.
    private update def writeEscaped(source: String, attribute: Boolean): Unit =
      val text = source.tt
      val length = source.length
      var start = 0
      var index = 0

      inline def escape(entity: BaseText): Unit =
        if index > start then producer.put(text, start.z, index - start)
        producer.put(entity)
        start = index + 1

      while index < length do
        source.charAt(index) match
          case '&'                => escape(t"&amp;")
          case '<'                => escape(t"&lt;")
          case '>'                => escape(t"&gt;")
          case '\r'               => escape(t"&#xD;")
          case '"' if attribute   => escape(t"&quot;")
          case '\t' if attribute  => escape(t"&#x9;")
          case '\n' if attribute  => escape(t"&#xA;")
          case _                  => ()

        index += 1

      if length > start then producer.put(text, start.z, length - start)

  private[xylophone] object Bytes:
    // The same escapes as `Textual`'s, as tables.
    val textEscapes: Producer.Utf8Writer.Escapes =
      Producer.Utf8Writer.escapes('&' -> "&amp;", '<' -> "&lt;", '>' -> "&gt;", '\r' -> "&#xD;")

    val attributeEscapes: Producer.Utf8Writer.Escapes =
      Producer.Utf8Writer.escapes
        ( '&' -> "&amp;", '<' -> "&lt;", '>' -> "&gt;", '\r' -> "&#xD;", '"' -> "&quot;",
          '\t' -> "&#x9;", '\n' -> "&#xA;" )

  private[xylophone] final class Bytes(writer: Producer.Utf8Writer^) extends Out:
    update def ascii(text: String): Unit = writer.ascii(text)
    update def name(text: String): Unit = writer.name(text)
    update def raw(text: String): Unit = writer.text(text)
    update def text(text: String): Unit = writer.escaped(text, Bytes.textEscapes)
    update def attribute(text: String): Unit = writer.escaped(text, Bytes.attributeEscapes)

  // The single, spec-correct XML serializer. `emit`, `lend` and `showable` all drive it, so they
  // never drift. When the `Formatting` carries an `indent`, element-only content is laid out
  // one child per indented line; an element that contains any character data is kept inline so
  // its text is never altered. Elements and text are tested first, since nearly every node of
  // a document is one or the other.
  private def writeXml
    ( out: Out^, formatting: Formatting, node: Xml, depth: Int, declared: Scope = Scope.xml )
  :   Unit =

    node match
      case element: Element =>
        writeElement(out, formatting, element, depth, declared)

      case node: Text =>
        out.text(node.text.s)

      case Fragment(nodes*) =>
        nodes.each(writeXml(out, formatting, _, depth, declared))

      case Comment(comment) =>
        out.ascii("<!--")
        out.raw(comment.s)
        out.ascii("-->")

      case Cdata(text) =>
        out.ascii("<![CDATA[")
        out.raw(text.s)
        out.ascii("]]>")

      case Doctype(text) =>
        out.ascii("<!DOCTYPE ")
        out.raw(text.s)
        out.ascii(">")

      case ProcessingInstruction(target, data) =>
        out.ascii("<?")
        out.name(target.s)

        if !data.nil then
          out.ascii(" ")
          out.raw(data.s)

        out.ascii("?>")

      case Header(version, encoding, standalone, _) =>
        out.ascii("<?xml version=\"")
        out.raw(version.s)
        out.ascii("\"")

        encoding.let: encoding =>
          out.ascii(" encoding=\"")
          out.raw(encoding.s)
          out.ascii("\"")

        standalone.let: standalone =>
          out.ascii(if standalone then " standalone=\"yes\"" else " standalone=\"no\"")

        out.ascii("?>")

  // `declared` holds the bindings the output has declared above this element. An element whose
  // scope binds a prefix it uses (or its default namespace) differently from what is declared,
  // and does not declare it among its own attributes, has the declaration written for it, so a
  // subtree built in code or cut from a document serializes namespace-well-formed; a parsed
  // document carries its declarations as attributes, and is written back exactly as read. An
  // element whose scope is the one declared needs no search: the parser shares a scope between
  // an element and each child that declares nothing, and passing the element's own scope down
  // keeps the identity test cheap for its descendants.
  private def writeElement
    ( out: Out^, formatting: Formatting, element: Element, depth: Int, declared: Scope )
  :   Unit =

    val label = element.label.s
    val attributes = element.attributes
    val children = element.children
    val scope = element.scope
    out.ascii("<")
    out.name(label)

    val own =
      if attributes.declaresNamespace then Scope.declared(declared, attributes) else declared

    val inner =
      if scope.isEmpty then own
      else if scope.same(own) then scope
      else
        val missing = Xml.undeclared(element, own)

        missing.eachPair: (prefix, uri) =>
          out.ascii(" xmlns")

          if !prefix.nil then
            out.ascii(":")
            out.name(prefix.s)

          out.ascii("=\"")
          out.attribute(uri.s)
          out.ascii("\"")

        if missing.nil then own else own ++ Scope.fromAttributes(missing)

    if !attributes.nil then attributes.eachPair: (key, value) =>
      out.ascii(" ")
      out.name(key.s)
      out.ascii("=\"")
      out.attribute(value.s)
      out.ascii("\"")

    if children.nil then out.ascii("/>") else
      out.ascii(">")

      // `iterate` rather than `each`: `each` counts its ordinal through a boxed closure, which
      // measured as a third of the time to write a document.
      if formatting.indent.present && !children.exists(textual) then
        children.iterate: index =>
          newline(out, formatting, depth + 1)
          writeXml(out, formatting, children(index), depth + 1, inner)

        newline(out, formatting, depth)
      else
        children.iterate: index => writeXml(out, formatting, children(index), depth, inner)

      out.ascii("</")
      out.name(label)
      out.ascii(">")

  // Character data (text or CDATA) forces an element to be serialized inline, so indentation
  // whitespace can never alter its content.
  private def textual(node: Xml): Boolean = node match
    case _: Text  => true
    case _: Cdata => true
    case _        => false

  // In indented mode, emit a newline followed by `depth` indent units.
  private def newline(out: Out^, formatting: Formatting, depth: Int): Unit =
    formatting.indent.let: unit =>
      out.ascii("\n")

      repeat(depth):
        out.raw(unit.s)

  given showable: [xml <: Xml] => (formatting: Formatting) => xml is Showable = node =>
    Producer.collect[BaseText](): producer =>
      writeXml(new Textual(producer), formatting, node, 0)
      if formatting.trailingNewline then producer.put("\n")

  // `Element`, `Fragment`, `Xml.Text`, `Cdata`, `Comment`, `Doctype`, `ProcessingInstruction` and
  // `Header` are all covered here, in the companion of the trait they share, exactly as `showable`
  // above covers them. The `Showable` needs a `Formatting` which a debugger cannot supply, so
  // inspection fixes the compact one and renders the node's own XML source, escaped into an
  // `xml"…"` literal: compact and on one line, showing the tag, its attributes and its children,
  // and distinguishable from the `Text` holding the same markup and from honeycomb's `html"…"`.
  given inspectable: [xml <: Xml] => xml is Inspectable = node =>
    val formatting: Formatting = Formatting(Unset, trailingNewline = false)

    val markup: BaseText = Producer.collect[BaseText](): producer =>
      writeXml(new Textual(producer), formatting, node, 0)

    val builder: StringBuilder = new StringBuilder()
    markup.each { char => builder.append(Inspectable.escape(char).s) }

    ("xml\""+builder.toString+"\"").tt

  private enum Token:
    case Close, Comment, Empty, Open, Header, Cdata, Pi, Doctype

  private enum Level:
    case Ascend, Descend, Peer

  trait Vacuiscible:
    node: Element =>
      def apply(children: Optional[Xml of (? <: node.Transport)]*): Element of node.Topic =
        new Element(node.label, node.attributes, List.from(children.compact).nodes):
          type Topic = node.Topic

  import Issue.*
  def name: BaseText = t"XML"

  given text: [label >: "#text" <: Label] => Conversion[BaseText, Xml of label] =
    Text(_).of[label]

  given string: [label >: "#text" <: Label] => Conversion[String, Xml of label] =
    string => Text(string.tt).of[label]

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
    case InvalidTag(name: BaseText)
    case InvalidTagStart(prefix: BaseText)
    case DuplicateAttribute(name: BaseText)
    case InadmissibleTag(name: BaseText, parent: BaseText)
    case OnlyWhitespace(char: Char)
    case Unexpected(char: Char)
    case UnknownEntity(name: BaseText)
    case ForbiddenUnquoted(char: Char)
    case MismatchedTag(open: BaseText, close: BaseText)
    case UnopenedTag(close: BaseText)
    case Incomplete(tag: BaseText)
    case UnknownAttribute(name: BaseText)
    case UnknownAttributeStart(name: BaseText)
    case InvalidAttributeUse(attribute: BaseText, element: BaseText)
    case UnboundPrefix(prefix: BaseText)
    case BadEncoding

    def describe: Message = this match
      case BadInsertion                   => m"a value cannot be inserted into XML at this point"
      case UnboundPrefix(prefix)          => m"the prefix $prefix is not bound to a namespace"
      case ExpectedMore                   => m"the content ended prematurely"
      case BadEncoding                    => m"the input is not valid UTF-8"
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
    def describe: BaseText = t"line ${line.n1}, column ${column.n1}"
    override def span: Span = Span.line(line, column, length.or(0))

  // All internal references in a `PositionIndex` are stored as offsets
  // relative to the start of the containing element descriptor, so any
  // slice extracted at a descriptor boundary is itself a valid
  // `PositionIndex`. See `XmlParser` (tracking mode) for the layout and
  // `Xml.Locator#walk` for the navigation algorithm.
  // Represented as the stdlib's immutable array (pure by construction): the frozen
  // `Array[Int]^{}` form makes every `PositionIndex`-holding field carry a fresh `any.rd`.
  opaque type PositionIndex = scala.IArray[Int]

  object PositionIndex:
    private[xylophone] def apply(data: Array[Int]^{}): PositionIndex = data.readable

  extension (positionIndex: PositionIndex)
    private[xylophone] def ints: Array[Int]^{} = Array.frozen(positionIndex)

  // Focus value tracked by Xylophone's path-aware decoders / encoders.
  // `path` is the XPath to the current node; `position` is the source
  // line/column/length, populated when the document was loaded with
  // `parsing.trackPositions` in scope and `Unset` otherwise.
  case class Focus(path: XPath, position: Optional[Xml.Position] = Unset)
  derives CanEqual:

    def withPosition(document: Document[Xml]): Focus =
      copy(position = document.locate(path))

  // Walks a `Document[Xml]`'s position index (held in its `Header` metadata)
  // to resolve an `XPath` to a source `Position`; see the descriptor-layout
  // comment on `PositionIndex` below.
  private object Locator:
    // The path arrives as `XPath.Location`s, root-first. The XPath convention
    // is `/foo/bar/@attr`, so the first element step names the *root* element
    // itself (no descent into children) and subsequent steps descend.
    private[xylophone] def walk
      ( xml:      Xml,
        data:     Array[Int]^{},
        offset:   Int,
        segments: List[XPath.Location],
        first:    Boolean )
    :   Optional[Position] =

      segments match
        case Nil =>
          Position(data.readUnchecked(offset + 1).z, data.readUnchecked(offset + 2).z, length = data.readUnchecked(offset + 3))

        case XPath.Location.Attribute(attrName) :: _ =>
          xml match
            case element: Element => attrPosition(element, data, offset, attrName)
            case _                => Unset

        case XPath.Location.Element(name, ordinal) :: rest =>
          xml match
            case element: Element if first =>
              // First step names the document's root element.
              if element.label == name && ordinal == 1 then
                walk(element, data, offset, rest, false)
              else
                Unset

            case element: Element =>
              descend(element, name, ordinal).let: childElementIndex =>
                val attrCount = data.readUnchecked(offset + 4)
                val offSlot = offset + 6 + attrCount + childElementIndex
                val childOff = data.readUnchecked(offSlot)

                descendAst(element, name, ordinal).let: child =>
                  walk(child, data, offset + childOff, rest, false)

            case _ =>
              Unset

    private def attrPosition
      ( element:  Element,
        data:     Array[Int]^{},
        offset:   Int,
        attrName: BaseText )
    :   Optional[Position] =

      val i = element.attributes.keys.indexOf(attrName)

      if i < 0 then Unset
      else
        val attrOff = data.readUnchecked(offset + 6 + i)
        val base = offset + attrOff
        Position(data.readUnchecked(base + 1).z, data.readUnchecked(base + 2).z, length = data.readUnchecked(base + 3))

    // Find the position of the n-th (1-indexed) child element with the
    // given name among the child *elements only* (ignoring text, comment,
    // CDATA, PI and Doctype children). Returns the element-index used to
    // look up the offset in the element descriptor's offset table.
    private def descend(element: Element, name: BaseText, ordinal: Int): Optional[Int] =
      val children = element.children
      var i = 0
      var elementIndex = 0
      var seen = 0
      var found: Optional[Int] = Unset

      while i < children.length && found.absent do
        children.readUnchecked(i) match
          case child: Element =>
            if child.label == name then
              seen += 1
              if seen == ordinal then found = elementIndex

            elementIndex += 1

          case _ => ()

        i += 1

      found

    private def descendAst(element: Element, name: BaseText, ordinal: Int): Optional[Element] =
      val children = element.children
      var i = 0
      var seen = 0
      var found: Optional[Element] = Unset

      while i < children.length && found.absent do
        children.readUnchecked(i) match
          case child: Element if child.label == name =>
            seen += 1
            if seen == ordinal then found = child

          case _ => ()

        i += 1

      found

  // The position index is produced when a document is loaded with
  // `parsing.trackPositions` in scope, and held in the `Document[Xml]`'s
  // `Header` metadata; untracked loads carry `Unset`.
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

  // Resolves an `XPath` to the source `Position` recorded in a tracked
  // `Document[Xml]`'s `PositionIndex`. Exposed uniformly as
  // `document.locate(path)` through zephyrine's `Positionable`; returns `Unset`
  // for a document loaded without `parsing.trackPositions`.
  given positionable: Document[Xml] is Positionable by XPath to Xml.Position =
    new Positionable:
      type Self    = Document[Xml]
      type Operand = XPath
      type Result  = Xml.Position

      def locate(document: Document[Xml], path: XPath): Optional[Xml.Position] =
        document.metadata.positionIndex.let: index =>
          path.locations.let: segments =>
            Locator.walk(document.root, index.ints, 0, segments, true)

      // XML has no distinct key positions, so there is nothing to locate by key.
      def locateKey(document: Document[Xml], path: XPath): Optional[Xml.Position] = Unset

  // Decode a tracked `Document[Xml]` like `Xml#as`, but also populate `position`
  // on every accumulated `Xml.Focus` by looking its XPath up against the document's
  // position index. Named `asTracked` rather than `as` because `as` is a generic
  // decoder extension (and clashes with import-renaming syntax); outside
  // `validate[Xml.Focus]` the ambient `Foci` is a no-op, so this stays cost-free
  // and yields `Unset` positions for an untracked document.
  extension (document: Document[Xml])
    def asTracked[result: Decodable in Xml]: result tracks Xml.Focus =
      val decoded = document.root match
        case Fragment(inner) => result.decoded(inner)
        case xml: Xml        => result.decoded(xml)

      val foci = summon[Foci[Xml.Focus]]
      foci.supplement(foci.length, _.let(_.withPosition(document)))
      decoded

  enum Hole:
    case Text, Tagbody, Comment
    case Element(tag: BaseText)
    case Attribute(tag: BaseText, attribute: BaseText)
    case Node(parent: BaseText)

  // ───────────────────────────────────────────────────────────────────────
  // The parser reads UTF-8 bytes from a `Cursor[Data]`, as jacinta's and stratiform's do:
  // the whole algorithm (tags, attributes, text, entities, comments, CDATA, processing
  // instructions, doctype, header) runs over a parser-local snapshot of the cursor's byte
  // buffer, scanning eight bytes per step for the stop bytes of a text run, an attribute
  // value, a comment or a CDATA section, and decodes a slice to `Text` only when it keeps
  // it — an all-ASCII slice through the JDK's Latin-1 constructor, anything else through
  // zephyrine's strict `Utf8`. Markup is ASCII, so bytes need no decoding to be
  // recognised; only a name that reaches a byte above 0x7F decodes the code point to
  // classify it. Text input is encoded to UTF-8 first (a whole `Text` by `getBytes`, a
  // stream through the UTF-8 `Codepage`), so a `Text` source costs one copy, as its
  // `char[]` cursor did.
  //
  // Positions (`Parse.Error`, the tracked `PositionIndex`) count bytes: lines by line
  // feeds, columns by code points, offsets and lengths in bytes — which is what an editor
  // needs to underline a span in a UTF-8 file, and equals the char count for ASCII. The
  // one exception is a `Parse.Error`'s `offset`/`length` for a whole-`Text` source, which
  // `computePosition` converts back to chars while the input is still buffered, so the
  // interpolator's error spans stay right for a literal with non-ASCII content.

  private[xylophone] object XmlParser:
    // Exact powers of ten for the Clinger double fast path: every entry is
    // exactly representable, so `mantissa.toDouble / TenPow(scale)` is
    // correctly rounded whenever the mantissa fits in 53 bits.
    private[xylophone] val TenPow: Array[Double]^{} =
      scala.Array(1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11, 1e12, 1e13, 1e14, 1e15)
      . asInstanceOf[Array[Double]^{}]

    // `true` and `false` packed LSB-first, for the boolean content fast path.
    private[xylophone] val TrueWord: Long =
      ('t'.toLong & 0xFF) | (('r'.toLong & 0xFF) << 8) | (('u'.toLong & 0xFF) << 16)
        | (('e'.toLong & 0xFF) << 24)

    private[xylophone] val FalseWord: Long =
      ('f'.toLong & 0xFF) | (('a'.toLong & 0xFF) << 8) | (('l'.toLong & 0xFF) << 16)
        | (('s'.toLong & 0xFF) << 24) | (('e'.toLong & 0xFF) << 32)

    // The stop bytes of the SWAR scans, replicated into every lane (see `zephyrine.Words`).
    private[xylophone] val LtRepl:       Long = Words.replicate('<')
    private[xylophone] val GtRepl:       Long = Words.replicate('>')
    private[xylophone] val AmpRepl:      Long = Words.replicate('&')
    private[xylophone] val BracketRepl:  Long = Words.replicate(']')
    private[xylophone] val DashRepl:     Long = Words.replicate('-')
    private[xylophone] val QuestionRepl: Long = Words.replicate('?')

    private val Utf8Charset: java.nio.charset.Charset = java.nio.charset.StandardCharsets.UTF_8.nn

    // Text input reaches the byte parser encoded as UTF-8 by `getBytes` (the JDK's intrinsified
    // encoder): a whole `Text` in one call, a chain of chunks one chunk at a time, lazily. A
    // surrogate pair split across two chunks is carried whole: a chunk ending in a high
    // surrogate keeps it back and prepends it to the next. (Not the `Codepage` duct, whose
    // encoder, staging buffer and block are a fixed cost per parse that a small document
    // notices.)
    private[xylophone] def utf8(text: BaseText): Data = Array.unsafeFrozen(text.s.getBytes(Utf8Charset).nn)

    private[xylophone] def utf8(chain: Chain[BaseText]): Chain[Data] =
      def encode(todo: Chain[BaseText], carry: String): Chain[Data] =
        if todo.nil then
          if carry.isEmpty then Chain.empty else Chain(utf8(carry.tt))
        else
          val chunk = carry + Chain.head(todo).s
          val length = chunk.length

          val (whole, rest) =
            if length > 0 && Character.isHighSurrogate(chunk.charAt(length - 1))
            then (chunk.substring(0, length - 1).nn, chunk.substring(length - 1).nn)
            else (chunk, "")

          utf8(whole.tt) #:: encode(Chain.tail(todo), rest)

      Chain.defer(encode(chain, ""))

    // Use untracked lineation in the cursor: avoids a per-`advance` branch
    // (newline detection) and a per-`mark` write into the cursor's parallel
    // offsets array. Errors still carry an accurate absolute `offset` /
    // `length` span (which is what tests assert on and what users need to
    // pinpoint the failure), but `line` / `column` stay at 1/1. Acceptable
    // trade: error quality remains useful while parsing-throughput improves.

    def fromData(data: Data)(using XmlSchema, Scope, Namespacing): XmlParser^ =
      new XmlParser(Cursor[Data](data), tracking = false)

    def fromDataChain(input: Chain[Data])(using XmlSchema, Scope, Namespacing): XmlParser^ =
      new XmlParser(Cursor[Data](input), tracking = false)

    // A pull endpoint: the cursor reads each region in place (see `Cursor.apply`).
    def fromStream(consume input: (Stream[Data] over Credit)^)(using Buffering)
      ( using XmlSchema, Scope, Namespacing )
    :   XmlParser^ =

      new XmlParser(Cursor[Data](input), tracking = false)

    // The text-input forms encode to UTF-8 and read the bytes; their error offsets are
    // converted back to chars (`charOffsets`), the units of the text supplied.
    def fromText(text: BaseText)(using XmlSchema, Scope, Namespacing): XmlParser^ =
      new XmlParser(Cursor[Data](utf8(text)), tracking = false, charOffsets = true)

    def fromChain(input: Chain[BaseText])(using XmlSchema, Scope, Namespacing): XmlParser^ =
      new XmlParser(Cursor[Data](utf8(input)), tracking = false, charOffsets = true)

    // The legacy interoperation shape: a stdlib `Iterator` of chunks.
    def fromIterator(input: Iterator[BaseText])(using XmlSchema, Scope, Namespacing): XmlParser^ =
      fromChain(Chain.from(input))

    // Tracking-mode constructors build the cursor with a `\n`-aware
    // `Lineation` so `cursor.line` / `cursor.column` reflect real source
    // coordinates as soon as `XmlParser.reconcileLineation()` is called.
    // The parser's hot loop still bypasses lineation via `unsafeAdvanceBy`;
    // reconciliation happens only at element / attribute capture points
    // and before any refill in `moreSlow`.
    def fromDataTracked(data: Data)(using XmlSchema, Scope, Namespacing): XmlParser^ =
      import zephyrine.lineation.linefeedByte
      new XmlParser(Cursor[Data](data), tracking = true)

    def fromDataChainTracked(input: Chain[Data])(using XmlSchema, Scope, Namespacing): XmlParser^ =
      import zephyrine.lineation.linefeedByte
      new XmlParser(Cursor[Data](input), tracking = true)

    def fromStreamTracked(consume input: (Stream[Data] over Credit)^)(using Buffering)
      ( using XmlSchema, Scope, Namespacing )
    :   XmlParser^ =

      import zephyrine.lineation.linefeedByte
      new XmlParser(Cursor[Data](input), tracking = true)

    def fromTextTracked(text: BaseText)(using XmlSchema, Scope, Namespacing): XmlParser^ =
      import zephyrine.lineation.linefeedByte
      new XmlParser(Cursor[Data](utf8(text)), tracking = true, charOffsets = true)

    def fromChainTracked(input: Chain[BaseText])(using XmlSchema, Scope, Namespacing): XmlParser^ =
      import zephyrine.lineation.linefeedByte
      new XmlParser(Cursor[Data](utf8(input)), tracking = true, charOffsets = true)

    // The legacy interoperation shape: a stdlib `Iterator` of chunks.
    def fromIteratorTracked(input: Iterator[BaseText])(using XmlSchema, Scope, Namespacing)
    :   XmlParser^ =

      fromChainTracked(Chain.from(input))

  private[xylophone] final class XmlParser
    ( val cursor:               Cursor[Data, ?]^,
     protected[xylophone] val tracking: Boolean,
     callback:                  (Ordinal, Hole) ->{caps.any} Unit = (_, _) => (),
     charOffsets:               Boolean = false )
    ( using schema: XmlSchema, scope0: Scope, namespacing: Namespacing )
  extends caps.ExclusiveCapability, caps.Stateful:
    type Region = Cursor.Mark

    private var heldToken: Cursor.Held | Null = null

    // The namespace bindings: the document's root scope is the given one, always with the
    // reserved `xml` prefix; `scope` is the scope of the element being read, set on opening
    // an element and restored on closing it, so that every `Element` carries its own.
    private val rootScope: Scope = if scope0.binds(t"xml") then scope0 else Scope.xml ++ scope0

    private var scope: Scope = rootScope

    // Whether the attributes just read declared a namespace, or used a prefix — set by the
    // attribute readers so that an element without either costs no scan
    private var attrXmlns: Boolean = false
    private var attrPrefixed: Boolean = false

    // The scope of the element just opened: its parent's, extended by its own declarations,
    // and checked under strict namespacing for a prefix it uses without a binding
    private update def openScope(name: BaseText, attributes: Attributes)(using Tactic[Parse.Error])
    :   Scope =

      val own = if attrXmlns then Scope.declared(scope, attributes) else scope

      if namespacing == Namespacing.Strict then
        Scope.unbound(own, name, attributes, attrPrefixed).let: prefix =>
          fail(Issue.UnboundPrefix(prefix))

      own

    // Parser-shared scratch buffer for attribute accumulation (lifetime of
    // the `XmlParser` instance). Stores key/value pairs interleaved as
    // `[k0, v0, k1, v1, ...]`. `readAttributes()` writes here and snapshots
    // the populated prefix into a freshly-sized `Array[String]^{}` to wrap
    // as the opaque `Attributes`. Geometric growth.
    private var attrBuf: scala.Array[String]^ = new scala.Array[String](16)


    // Pool of `ArrayBuffer[Node]` instances re-used across recursive
    // `readChildren` calls. Each nesting level borrows one, fills it, copies
    // its contents into an `Array[Node]^{}`, and returns it. Pool grows on
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
    private val tagCache:     scala.Array[BaseText | Null]^ = new scala.Array(TagCacheSize)

    private val tagCacheLow:  scala.Array[Long]^ = new scala.Array(TagCacheSize)

    private val tagCacheHigh: scala.Array[Long]^ = new scala.Array(TagCacheSize)


    // Fingerprint of the name most recently read by `readName` — the packed
    // words it computes anyway for the tag cache, and whether they identify
    // the name losslessly (ASCII, at most `TagCacheMaxChars` chars).
    private var nameLow:      Long = 0L
    private var nameHigh:     Long = 0L
    private var namePackable: Boolean = false

    // Not `inline`: inline expansion propagates a refinement whose fresh reach capabilities
    // differ per call site, which the capture checker rejects.
    private update def getNodeBuffer(): scala.collection.mutable.ArrayBuffer[Node] =
      nodeBufferId += 1

      if nodeBuffers.length <= nodeBufferId then
        val newBuffer = scala.collection.mutable.ArrayBuffer.empty[Node]
        nodeBuffers += newBuffer
        newBuffer
      else
        val buffer = nodeBuffers(nodeBufferId)
        buffer.clear()
        buffer

    private inline update def relinquishNodeBuffer(): Unit = nodeBufferId -= 1

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

    // Not `inline`, as `getNodeBuffer` above.
    private update def getIndexBuffer(): scala.collection.mutable.ArrayBuffer[Int] =
      indexBufferId += 1

      if indexBuffers.length <= indexBufferId then
        val nu = scala.collection.mutable.ArrayBuffer.empty[Int]
        indexBuffers += nu
        nu
      else
        val buf = indexBuffers(indexBufferId)
        buf.clear()
        buf

    private inline update def relinquishIndexBuffer(): Unit = indexBufferId -= 1

    // Finalised root-level position index produced by the previous
    // tracking-mode parse. Reset on every parse entry. Read by the
    // `XmlParser.fromText/Iterator(Tracked)` callers.
    protected[xylophone] var rootIndex: Array[Int]^{} | Null = null

    // Local-buffer offset up to which `cursor.line` / `cursor.column` have
    // been brought up to date. The hot-loop `syncTo()` bypasses the
    // cursor's lineation tracking via `unsafeAdvanceBy`, so the parser
    // catches lineation up at tracking-mode capture points and before
    // any refill that would discard consumed bytes.
    private var lineationPos: Int = cursor.unsafePos(using Unsafe)

    private update def reconcileLineation(): Unit =
      val end = cursor.unsafePos(using Unsafe)

      if lineationPos < end then
        var i = lineationPos
        var newlines = 0
        var lastNewlineAt = -1

        // Columns count code points, not bytes: a continuation byte adds nothing.
        var continuations = 0

        while i < end do
          val b = bytes(i)

          if b == '\n' then
            newlines += 1
            lastNewlineAt = i
            continuations = 0
          else if Utf8.continuation(b) then
            continuations += 1

          i += 1

        if newlines > 0 then
          cursor.unsafeBumpLine(newlines)(using Unsafe)
          cursor.unsafeSetColumn(end - lastNewlineAt - 1 - continuations)(using Unsafe)
        else
          cursor.unsafeBumpColumn(end - lineationPos - continuations)(using Unsafe)

        lineationPos = end

    // Assemble an element descriptor in `out`. `attrDescs` and `attrEnds`
    // hold attribute descriptors back-to-back and their end positions
    // within `attrDescs`. `childDescs` and `childEnds` hold child element
    // descriptors / their end positions the same way. See the layout
    // comment on `Xml.PositionIndex`.
    private update def emitElementDescriptor
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
    // Held as an `AnyRef` field with an exclusive-view accessor (the `Tel.Reader.parser0`
    // pattern): a typed array field's snapshot of the cursor's buffer trips both the
    // classifier and the consume checks.
    // [cursor-snapshot] parser's AnyRef snapshot of cursor buffer
    @scala.caps.unsafe.untrackedCaptures
    private var bytes0: AnyRef = cursor.unsafeDataBuffer(using Unsafe).asInstanceOf[AnyRef]

    private inline def bytes: scala.Array[Byte]^ = bytes0.asInstanceOf[scala.Array[Byte]^]
    private var pos:    Int = cursor.unsafePos(using Unsafe)
    private var bufEnd: Int = cursor.unsafeWriteEnd(using Unsafe)

    private inline update def syncTo(): Unit =
      cursor.unsafeAdvanceBy(pos - cursor.unsafePos(using Unsafe))(using Unsafe)

    private inline update def syncFrom(): Unit =
      bytes0 = cursor.unsafeDataBuffer(using Unsafe).asInstanceOf[AnyRef]
      pos    = cursor.unsafePos(using Unsafe)
      bufEnd = cursor.unsafeWriteEnd(using Unsafe)
      lineationPos = pos

    protected inline update def more: Boolean = pos < bufEnd || moreSlow()

    // Out-of-line slow path so `more`'s inline budget stays small enough
    // for the JIT to keep `pos < bufEnd` as one register comparison in
    // hot loops. In tracking mode, lineation is reconciled and the
    // parser-local `pos` is re-anchored even on EOF so that the next
    // `cursor.position` read reflects the compacted buffer's basePos.
    private update def moreSlow(): Boolean =
      syncTo()
      if tracking then reconcileLineation()

      if cursor.more then { syncFrom(); true }
      else
        if tracking then syncFrom()
        false

    protected inline def peek: Byte = bytes(pos)
    protected inline update def advance(): Unit = pos += 1

    // The current byte for an error message: an ASCII byte as itself, a multi-byte
    // sequence as its code point's first char, and a malformed one as U+FFFD.
    protected update def peekChar: Char =
      val b = peek

      if b >= 0 then b.toChar
      else
        ensureAvailable(4)
        val point = Utf8.point(bytes, pos, bufEnd)
        if point < 0 then '\ufffd' else Character.toChars(point).nn(0)

    // Makes at least `n` bytes available from the current position, unless the input ends
    // first — for the look-ahead a multi-byte sequence needs at a refill boundary. Marks,
    // advances through the cursor to force the refills, and cues back: inside the parse's
    // `hold`, the mark keeps the bytes resident. (The `Tel.Parser.ensureLookahead` pattern.)
    private update def ensureAvailable(n: Int): Unit =
      if pos + n > bufEnd then
        syncTo()
        if tracking then reconcileLineation()
        val mark = cursor.mark(using heldToken.nn)
        var steps = 0

        while steps < n && cursor.more do
          cursor.advance()
          steps += 1

        cursor.cue(mark)
        syncFrom()

    // Skips whole words while `clear` finds no stop byte in them, leaving `pos` on the word
    // holding the first stop byte (or fewer than eight bytes from the buffer's end) for the
    // byte-by-byte loop that follows to examine.
    private inline update def skipWords(inline clear: Long => Boolean): Unit =
      while pos + 8 <= bufEnd && clear(Words.load(bytes, pos)) do pos += 8

    // Not `inline`: nested inside another inline update method's expansion, the parser's `this`
    // is bound as a read-only proxy, and the cursor sync in `syncTo` is then rejected.
    protected update def position: Int =
      syncTo()
      cursor.position.n0

    // Non-`inline` so that `cursor.mark`'s expansion is emitted once in its
    // own method rather than re-expanded into every call site, keeping
    // `XmlParser`'s hot methods small enough for HotSpot's free-inline
    // budgets. The JIT can still inline at hot call sites via its own
    // heuristics.
    protected update def begin(): Cursor.Mark =
      syncTo()
      cursor.mark(using heldToken.nn)

    protected update def slice(start: Cursor.Mark)(using Tactic[Parse.Error]): BaseText =
      syncTo()
      val end = cursor.mark(using heldToken.nn)
      slice(start, end)

    // As `slice`, for a scan that saw every byte of the region and knows whether any was
    // non-ASCII: an ASCII region is copied without the decoder's own scan.
    protected update def slice(start: Cursor.Mark, ascii: Boolean)(using Tactic[Parse.Error]): BaseText =
      syncTo()
      val end = cursor.mark(using heldToken.nn)

      if ascii
      then cursor.slice(start, end): (storage, offset, length) =>
        Utf8.ascii(storage.asInstanceOf[scala.Array[Byte]], offset, length)
      else slice(start, end)

    // The slice decoded: an all-ASCII one through the Latin-1 `String` constructor, any other
    // through the strict decoder, which rejects malformed UTF-8 as a parse error.
    protected update def slice(start: Cursor.Mark, end: Cursor.Mark)(using Tactic[Parse.Error]): BaseText =
      cursor.slice(start, end): (storage, offset, length) =>
        Utf8.decode(storage.asInstanceOf[scala.Array[Byte]], offset, length)
        . or(fail(Issue.BadEncoding, start))

    protected update def reset(start: Cursor.Mark): Unit =
      syncTo()
      cursor.cue(start)
      syncFrom()

    protected update def appendSlice(start: Cursor.Mark, buf: jl.StringBuilder)
      ( using Tactic[Parse.Error] )
    :   Unit =

      syncTo()
      val end = cursor.mark(using heldToken.nn)

      cursor.slice(start, end): (storage, offset, length) =>
        if !Utf8.append(storage.asInstanceOf[scala.Array[Byte]], offset, length, buf)
        then fail(Issue.BadEncoding, start)

    protected update def computePosition(start: Optional[Cursor.Mark] = Unset): Position =
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
        val b = bytes(i)

        if b == '\n' then
          line += 1
          col = 1
        else if !Utf8.continuation(b) then
          col += 1

        i += 1

      val end = cursor.position.n0
      val base = end - pos

      // Offsets are byte offsets, except for a text source, whose offsets are converted to
      // char offsets — the units of the `Text` supplied — while the input is still buffered
      // from its start (always so for a whole `Text`).
      def chars(byteOffset: Int): Int =
        if !charOffsets || base != 0 || byteOffset > pos then byteOffset else
          var count = 0
          var j = 0

          while j < byteOffset do
            val b = bytes(j)
            // A four-byte sequence is a surrogate pair: two chars.
            if !Utf8.continuation(b) then count += (if (b & 0xf8) == 0xf0 then 2 else 1)
            j += 1

          count

      val offset: Optional[Int] = start.let: mark => chars(mark.absolute.toInt)
      val length: Optional[Int] = start.let: mark => chars(end) - chars(mark.absolute.toInt)
      Position(line.u, col.u, offset = offset, length = length)

    protected inline def fail(issue: Issue)(using Tactic[Parse.Error]): Nothing =
      abort(Parse.Error(Xml, computePosition(Unset), issue))

    protected update def fail(issue: Issue, start: Cursor.Mark)(using Tactic[Parse.Error]): Nothing =
      abort(Parse.Error(Xml, computePosition(start), issue))

    protected inline def isAsciiLetter(c: Byte): Boolean =
      ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

    protected inline def isAsciiDigit(c: Byte): Boolean = '0' <= c && c <= '9'

    // The ASCII name characters; a byte above 0x7F is classified by `nameWidth`.
    protected inline def isNameStart(c: Byte): Boolean = isAsciiLetter(c) || c == '_' || c == ':'

    protected inline def isNameChar(c: Byte): Boolean =
      isAsciiLetter(c) || isAsciiDigit(c) || c == '_' || c == '-' || c == '.' || c == ':'

    // The width of the multi-byte sequence at the current position if it encodes a name
    // character (a letter to start a name; a letter, a digit or U+00B7 within one), or 0 if
    // it does not, or is malformed or incomplete at the end of the input.
    private update def nameWidth(start: Boolean): Int =
      ensureAvailable(4)
      val point = Utf8.point(bytes, pos, bufEnd)

      val named =
        point >= 0 &&
          (if start then Character.isLetter(point)
           else point == 0xb7 || Character.isLetterOrDigit(point))

      if named then Utf8.width(peek & 0xff) else 0

    protected inline def isWs(c: Byte): Boolean =
      c == ' ' || c == '\n' || c == '\r' || c == '\t' || c == '\f'

    protected update def skipWs(): Unit = while more && isWs(peek) do advance()

    protected update def expectChar(chr: Char)(using Tactic[Parse.Error]): Unit =
      if !more then fail(Issue.ExpectedMore)
      if peek != chr then fail(Issue.Unexpected(peekChar))
      advance()

    protected update def readName()(using Tactic[Parse.Error]): BaseText =
      val start = begin()
      if !more then fail(Issue.ExpectedMore, start)
      val first = peek

      // Pack bytes into a Long pair while scanning; track whether they all
      // stay in the 7-bit ASCII range. The pair is later used as the cache
      // key when both conditions (ascii + length ≤ 16) hold.
      var packedLow:  Long = first.toLong & 0xFFL
      var packedHigh: Long = 0L
      var len: Int = 1
      var ascii: Boolean = first >= 0

      if first >= 0 then
        if !isNameStart(first) then fail(Issue.Unexpected(peekChar), start)
        advance()
      else
        val width = nameWidth(start = true)
        if width == 0 then fail(Issue.Unexpected(peekChar), start)
        pos += width

      // The ASCII loop is the hot one; it leaves only at a non-name byte or a high byte, and
      // the outer loop resumes it after a multi-byte name character.
      var scanning = true

      while scanning do
        while more && { val c = peek; c >= 0 && isNameChar(c) } do
          val c = peek

          if len < 8 then
            packedLow = packedLow | ((c.toLong & 0xFFL) << (len << 3))
          else if len < 16 then
            packedHigh = packedHigh | ((c.toLong & 0xFFL) << ((len - 8) << 3))

          len += 1
          advance()

        if more && peek < 0 then
          val width = nameWidth(start = false)

          if width == 0 then scanning = false
          else
            ascii = false
            len += 1
            pos += width
        else scanning = false

      nameLow = packedLow
      nameHigh = packedHigh
      namePackable = ascii && len <= TagCacheMaxChars

      if !ascii || len > TagCacheMaxChars then slice(start, ascii)
      else
        val idx =
          ((packedLow.toInt ^ (packedLow >>> 32).toInt) ^
            (packedHigh.toInt ^ (packedHigh >>> 32).toInt)) & (TagCacheSize - 1)

        val cached = tagCache(idx)

        if cached != null && tagCacheLow(idx) == packedLow &&
          tagCacheHigh(idx) == packedHigh
        then cached.nn
        else
          val fresh = slice(start, ascii = true)
          tagCache(idx)     = fresh
          tagCacheLow(idx)  = packedLow
          tagCacheHigh(idx) = packedHigh
          fresh

    // Parse an entity reference. Position must be just after the '&'.
    // Returns the expansion as a Text; leaves position just after the ';'.
    protected update def readEntity()(using Tactic[Parse.Error]): BaseText =
      if !more then fail(Issue.ExpectedMore)

      if peek == '#' then
        advance()
        if !more then fail(Issue.ExpectedMore)
        var value = 0

        if peek == 'x' || peek == 'X' then
          advance()

          while more && peek != ';' do
            val c = peek

            // Reuse the digit-value subtraction as its own range check: the
            // difference, masked to 16 bits by `.toChar`, is in range iff small,
            // so the value the decoder already needs doubles as the bounds test.
            // The hex-letter subtraction is only computed when `c` isn't a digit.
            val dec = (c - '0').toChar

            value =
              if dec <= 9 then 16*value + dec
              else
                val hex = ((c | 0x20) - 'a').toChar
                if hex <= 5 then 16*value + hex + 10 else fail(Issue.Unexpected(peekChar))

            advance()
        else
          while more && peek != ';' do
            val c = peek
            val dec = (c - '0').toChar

            if dec <= 9 then value = 10*value + dec
            else fail(Issue.Unexpected(peekChar))

            advance()

        if !more then fail(Issue.ExpectedMore)
        advance()

        if value <= 0xffff then String.valueOf(value.toChar).nn.tt
        else String.valueOf(Character.toChars(value).nn).nn.tt
      else
        val nameStart = begin()

        while more && peek != ';' do
          if !isNameChar(peek) then fail(Issue.Unexpected(peekChar), nameStart)
          advance()

        if !more then fail(Issue.ExpectedMore, nameStart)
        val name = slice(nameStart)
        advance()
        schema.entities(name).or(fail(Issue.UnknownEntity(name), nameStart))

    // Read attribute value enclosed in `quote`. Returns the unescaped
    // value as Text. Position starts just after the opening quote and
    // ends just after the closing quote.
    protected update def readAttrValue(tag: BaseText, quote: Byte)(using Tactic[Parse.Error]): BaseText =
      val start = begin()
      var hasEntity = false
      var hasHole = false
      val quoteRepl = Words.replicate(quote)
      var scanning = true
      var ascii = true

      // Eight bytes per step past the plain content; the stop bytes — the quote, `<`, `&`
      // and the hole marker — are examined one at a time. The scan notes any non-ASCII
      // byte, so an ASCII value is copied out without a second scan.
      while scanning do
        skipWords: word =>
          if Words.nonAscii(word) != 0L then ascii = false

          (Words.matches(word, quoteRepl) | Words.matches(word, XmlParser.LtRepl) |
            Words.matches(word, XmlParser.AmpRepl) | Words.zeroes(word)) == 0L

        if !more then fail(Issue.ExpectedMore, start)
        val c = peek

        if c == quote then scanning = false
        else
          if c == '<' then fail(Issue.Unexpected('<'), start)
          if c == '&' then hasEntity = true
          if c == '\u0000' then hasHole = true
          if c < 0 then ascii = false
          advance()

      val end = begin()
      advance() // consume closing quote

      if !hasEntity && !hasHole then
        if ascii
        then cursor.slice(start, end): (storage, offset, length) =>
          Utf8.ascii(storage.asInstanceOf[scala.Array[Byte]], offset, length)
        else slice(start, end)
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
            callback(position.z, Hole.Attribute(tag, t""))
            buf.append('\u0000')
            advance()
            segStart = begin()
          else
            advance()

        if !more then fail(Issue.ExpectedMore, start)
        appendSlice(segStart, buf)
        advance() // consume closing quote
        buf.toString.nn.tt

    protected update def readAttributes(tag: BaseText)(using Tactic[Parse.Error]): Attributes =
      // Append into the parser-shared interleaved scratch buffer (laid out as
      // `[k0, v0, k1, v1, ...]`); on close, snapshot the populated prefix
      // into a freshly-sized `Array[String]^{}` and wrap it as the opaque
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
      attrXmlns = false
      attrPrefixed = false

      inline def ensureCapacity(): Unit =
        if 2*n >= attrBuf.length then
          // `Arrays.copyOf` (a Java method) yields an array that adapts to the pure field
          // type; a Scala-side fresh array could not be assigned inside the parse loop.
          attrBuf = java.util.Arrays
          . copyOf(attrBuf.asInstanceOf[scala.Array[AnyRef | Null]], attrBuf.length*2)
          . nn.asInstanceOf[scala.Array[String]]

      while !done do
        skipWs()
        if !more then fail(Issue.ExpectedMore)
        val ch = peek

        if ch == '>' || ch == '/' || ch == '?' then done = true
        else if ch == '\u0000' then
          callback(position.z, Hole.Tagbody)
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
          if keyStr.startsWith("xmlns") then attrXmlns = true
          else if keyStr.indexOf(':') >= 0 then attrPrefixed = true

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
              callback(position.z, Hole.Attribute(tag, key))
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
        val arr = Array.allocate[String](2*n)
        jl.System.arraycopy(attrBuf, 0, arr.raw, 0, 2*n)
        Attributes.fromInterleaved(Array.freeze(arr))

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
    protected update def readText(parentLabel: BaseText)(using Tactic[Parse.Error]): BaseText =
      val start = begin()
      var bracketCount = 0
      var buf: jl.StringBuilder | Null = null
      var segStart: Cursor.Mark = start
      var scanning = true
      var ascii = true

      // Eight bytes per step past the plain content; `<`, `&`, `]` and the hole marker are
      // examined one at a time, and after a `]` every byte is, so that a `]]>` is seen. The
      // scan notes any non-ASCII byte, so an ASCII run is copied out without a second scan.
      while scanning do
        if bracketCount == 0 then
          skipWords: word =>
            if Words.nonAscii(word) != 0L then ascii = false

            (Words.matches(word, XmlParser.LtRepl) | Words.matches(word, XmlParser.AmpRepl) |
              Words.matches(word, XmlParser.BracketRepl) | Words.zeroes(word)) == 0L

        if !more then scanning = false
        else
          val c = peek

          if c == '<' then scanning = false
          else
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
              callback(position.z, Hole.Node(parentLabel))
              buf.nn.append('\u0000')
              advance()
              segStart = begin()
            else
              if c < 0 then ascii = false
              advance()

      if buf == null then slice(start, ascii)
      else
        appendSlice(segStart, buf.nn)
        buf.nn.toString.nn.tt

    protected update def readComment()(using Tactic[Parse.Error]): BaseText =
      val start = begin()
      var result: BaseText | Null = null

      // Eight bytes per step to each `-`, then a byte at a time to see whether `-->` follows.
      while result == null do
        skipWords(Words.matches(_, XmlParser.DashRepl) == 0L)
        if !more then fail(Issue.ExpectedMore, start)

        if peek == '-' then
          val end = begin()
          advance()
          if !more then fail(Issue.ExpectedMore, start)

          if peek == '-' then
            advance()
            if !more then fail(Issue.ExpectedMore, start)
            if peek != '>' then fail(Issue.Unexpected(peekChar), start)
            advance()
            result = slice(start, end)
        else
          advance()

      result.nn

    protected update def readCdata()(using Tactic[Parse.Error]): BaseText =
      val start = begin()
      var done = false
      var endRegion: Region = start

      while !done do
        skipWords(Words.matches(_, XmlParser.BracketRepl) == 0L)
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
    protected update def readProcessingInstruction()(using Tactic[Parse.Error]): Node =
      val nameStart = begin()
      val target = readName()

      val isXmlName =
        target.length == 3 &&
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
        var encoding: Optional[BaseText] = Unset
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
        if peek != '?' then fail(Issue.Unexpected(peekChar), nameStart)
        advance()
        if !more then fail(Issue.ExpectedMore, nameStart)
        if peek != '>' then fail(Issue.Unexpected(peekChar), nameStart)
        advance()
        Header(version, encoding, standalone)
      else
        skipWs()
        val dataStart = begin()
        var result: ProcessingInstruction | Null = null

        // Eight bytes per step to each `?`, then a byte to see whether `?>` follows.
        while result == null do
          skipWords(Words.matches(_, XmlParser.QuestionRepl) == 0L)
          if !more then fail(Issue.ExpectedMore, dataStart)

          if peek == '?' then
            val dataEnd = begin()
            advance()
            if !more then fail(Issue.ExpectedMore, dataStart)

            if peek == '>' then
              advance()
              result = ProcessingInstruction(target, slice(dataStart, dataEnd))
          else
            advance()

        result.nn

    protected update def readDoctype()(using Tactic[Parse.Error]): BaseText =
      skipWs()
      val start = begin()
      skipWords(Words.matches(_, XmlParser.GtRepl) == 0L)
      while more && peek != '>' do advance()
      if !more then fail(Issue.ExpectedMore, start)
      val end = begin()
      advance()
      slice(start, end)

    // Read a single element starting just after '<'.
    protected update def readElement()(using Tactic[Parse.Error]): Element =
      // Detect `<\u0000` (macro element hole)
      if more && peek == '\u0000' then
        callback(position.z, Hole.Element(t""))
        advance()
        if !more then fail(Issue.ExpectedMore)
        if peek != '>' then fail(Issue.Unexpected(peekChar))
        advance()
        Element(t"\u0000", Attributes.empty, Array.empty[Node])
      else
        val name = readName()
        val attrs = readAttributes(name)
        if !more then fail(Issue.ExpectedMore)
        val parent = scope
        val own = openScope(name, attrs)

        if peek == '/' then
          advance()
          if !more then fail(Issue.ExpectedMore)
          if peek != '>' then fail(Issue.Unexpected(peekChar))
          advance()
          Element(name, attrs, Array.empty[Node], own)
        else
          if peek != '>' then fail(Issue.Unexpected(peekChar))
          advance()
          scope = own
          val children = readChildren(name)
          scope = parent
          Element(name, attrs, children, own)

    protected update def readChildren(parentName: BaseText)(using Tactic[Parse.Error]): Array[Node]^{} =
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
            if peek != '>' then fail(Issue.Unexpected(peekChar), closeStart)
            advance()
            if close != parentName then fail(Issue.MismatchedTag(parentName, close), closeStart)
            done = true
          else if c2 == '!' then
            advance()

            if more && peek == '-' then
              advance()
              if !more then fail(Issue.ExpectedMore)
              if peek != '-' then fail(Issue.Unexpected(peekChar))
              advance()
              children += Comment(readComment())
            else if more && peek == '[' then
              advance()
              consumeLiteral("CDATA[")
              children += Cdata(readCdata())
            else
              if !more then fail(Issue.ExpectedMore)
              fail(Issue.Unexpected(peekChar))
          else if c2 == '?' then
            advance()
            children += readProcessingInstruction()
          else
            children += readElement()
        else
          val text = readText(parentName)
          if text.length > 0 then children += Text(text)

      val result =
        if children.nil then Array.empty[Node]
        else
          val arr = Array.allocate[Node](children.length)
          var i = 0

          while i < children.length do
            arr(i) = children(i)
            i += 1

          Array.freeze(arr)

      relinquishNodeBuffer()
      result

    protected update def consumeLiteral(literal: String)(using Tactic[Parse.Error]): Unit =
      var i = 0

      while i < literal.length do
        if !more then fail(Issue.ExpectedMore)
        if peek != literal.charAt(i) then fail(Issue.Unexpected(peekChar))
        advance()
        i += 1

    private var headers: Boolean = false

    update def parseXml(headers0: Boolean)(using Tactic[Parse.Error]): Xml =
      cursor.hold:
        heldToken = summon[Cursor.Held]

        try
          if tracking then parseXmlTracked0(headers0) else parseXml0(headers0)
        finally heldToken = null

    // Tracked variants of `readElement` / `readAttributes` / `readChildren`,
    // building a parallel `Array[Int]^{}` position index as they parse. The
    // structural logic mirrors the untracked variants byte-for-byte; only
    // the position bookkeeping differs. Splitting keeps the untracked
    // hot path free of any tracking-related branches.

    private update def parseXmlTracked0(headers0: Boolean)(using Tactic[Parse.Error]): Xml =
      headers = headers0
      skipWs()
      val nodes = getNodeBuffer()
      val rootBuf = getIndexBuffer()

      while more do
        if peek != '<' then
          val text = readText(t"")
          if text.length > 0 then nodes += Text(text)
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
              if peek != '-' then fail(Issue.Unexpected(peekChar))
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
              fail(Issue.Unexpected(peekChar))
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
      rootIndex = Array.from(rootBuf)
      relinquishIndexBuffer()
      result

    // Read a tracked element starting just after '<'. `startLine`,
    // `startColumn`, `startMark` were captured by the caller at the `<`.
    // `out` is the parent's index buffer; the element's descriptor is
    // appended to it.
    private update def readElementTracked
      ( out:         scala.collection.mutable.ArrayBuffer[Int],
        startLine:   Int,
        startColumn: Int,
        startMark:   Long )
      ( using Tactic[Parse.Error] )
    :   Element =

      // Macro element holes can't carry meaningful positions; emit an empty
      // attribute / child set and a zero-length descriptor.
      if more && peek == '\u0000' then
        callback(position.z, Hole.Element(t""))
        advance()
        if !more then fail(Issue.ExpectedMore)
        if peek != '>' then fail(Issue.Unexpected(peekChar))
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
        Element(t"\u0000", Attributes.empty, Array.empty[Node])
      else
        val attrDescs = getIndexBuffer()
        val attrEnds  = getIndexBuffer()
        val childDescs = getIndexBuffer()
        val childEnds  = getIndexBuffer()

        val name = readName()
        val attrs = readAttributesTracked(name, attrDescs, attrEnds)
        if !more then fail(Issue.ExpectedMore)
        val parent = scope
        val own = openScope(name, attrs)

        val result =
          if peek == '/' then
            advance()
            if !more then fail(Issue.ExpectedMore)
            if peek != '>' then fail(Issue.Unexpected(peekChar))
            advance()
            Element(name, attrs, Array.empty[Node], own)
          else
            if peek != '>' then fail(Issue.Unexpected(peekChar))
            advance()
            scope = own
            val children = readChildrenTracked(name, childDescs, childEnds)
            scope = parent
            Element(name, attrs, children, own)

        emitElementDescriptor
          ( out, attrDescs, attrEnds, childDescs, childEnds, startLine, startColumn, startMark )

        relinquishIndexBuffer()
        relinquishIndexBuffer()
        relinquishIndexBuffer()
        relinquishIndexBuffer()
        result

    private update def readAttributesTracked
      ( tag:       BaseText,
        attrDescs: scala.collection.mutable.ArrayBuffer[Int],
        attrEnds:  scala.collection.mutable.ArrayBuffer[Int] )
      ( using Tactic[Parse.Error] )
    :   Attributes =

      var n = 0
      var done = false
      var hashOr = 0
      attrXmlns = false
      attrPrefixed = false

      inline def ensureCapacity(): Unit =
        if 2*n >= attrBuf.length then
          // `Arrays.copyOf` (a Java method) yields an array that adapts to the pure field
          // type; a Scala-side fresh array could not be assigned inside the parse loop.
          attrBuf = java.util.Arrays
          . copyOf(attrBuf.asInstanceOf[scala.Array[AnyRef | Null]], attrBuf.length*2)
          . nn.asInstanceOf[scala.Array[String]]

      while !done do
        skipWs()
        if !more then fail(Issue.ExpectedMore)
        val ch = peek

        if ch == '>' || ch == '/' || ch == '?' then done = true
        else if ch == '\u0000' then
          callback(position.z, Hole.Tagbody)
          advance()
          skipWs()
          ensureCapacity()
          attrBuf(2*n) = "\u0000"
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
          if keyStr.startsWith("xmlns") then attrXmlns = true
          else if keyStr.indexOf(':') >= 0 then attrPrefixed = true

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
              callback(position.z, Hole.Attribute(tag, key))
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
        val arr = Array.allocate[String](2*n)
        jl.System.arraycopy(attrBuf, 0, arr.raw, 0, 2*n)
        Attributes.fromInterleaved(Array.freeze(arr))

    private update def readChildrenTracked
      ( parentName: BaseText,
        childDescs: scala.collection.mutable.ArrayBuffer[Int],
        childEnds:  scala.collection.mutable.ArrayBuffer[Int] )
      ( using Tactic[Parse.Error] )
    :   Array[Node]^{} =

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
            if peek != '>' then fail(Issue.Unexpected(peekChar), closeStart)
            advance()
            if close != parentName then fail(Issue.MismatchedTag(parentName, close), closeStart)
            done = true
          else if c2 == '!' then
            advance()

            if more && peek == '-' then
              advance()
              if !more then fail(Issue.ExpectedMore)
              if peek != '-' then fail(Issue.Unexpected(peekChar))
              advance()
              children += Comment(readComment())
            else if more && peek == '[' then
              advance()
              consumeLiteral("CDATA[")
              children += Cdata(readCdata())
            else
              if !more then fail(Issue.ExpectedMore)
              fail(Issue.Unexpected(peekChar))
          else if c2 == '?' then
            advance()
            children += readProcessingInstruction()
          else
            children += readElementTracked(childDescs, childLine, childColumn, childStartMark)
            childEnds += childDescs.length
        else
          val text = readText(parentName)
          if text.length > 0 then children += Text(text)

      val result =
        if children.nil then Array.empty[Node]
        else
          val arr = Array.allocate[Node](children.length)
          var i = 0

          while i < children.length do
            arr(i) = children(i)
            i += 1

          Array.freeze(arr)

      relinquishNodeBuffer()
      result

    private update def parseXml0(headers0: Boolean)(using Tactic[Parse.Error]): Xml =
      headers = headers0
      skipWs()
      val nodes = getNodeBuffer()

      while more do
        if peek != '<' then
          val text = readText(t"")
          if text.length > 0 then nodes += Text(text)
        else
          advance()
          if !more then fail(Issue.ExpectedMore)
          val c2 = peek

          if c2 == '!' then
            advance()

            if more && peek == '-' then
              advance()
              if !more then fail(Issue.ExpectedMore)
              if peek != '-' then fail(Issue.Unexpected(peekChar))
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
              fail(Issue.Unexpected(peekChar))
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

    protected update def consumeLiteralCi(literal: String)(using Tactic[Parse.Error]): Unit =
      var i = 0

      while i < literal.length do
        if !more then fail(Issue.ExpectedMore)
        val expected = literal.charAt(i)
        val got = peek

        val matches =
          got == expected ||
            isAsciiLetter(expected.toByte) &&
            (got == (expected | 0x20).toChar || got == (expected & ~0x20).toChar)

        if !matches then fail(Issue.Unexpected(peekChar))
        advance()
        i += 1

    // ── Direct parsing rim ─────────────────────────────────────────────────
    //
    // The pull-event surface behind `Xml.Reader`, letting an `Xml.Parsable`
    // instance consume the input element by element without materializing
    // the document's tree. The rim reuses the tree-building parser's own
    // token readers (`readName`, `readAttributes`, `readText`,
    // `readComment`, `readCdata`, `readProcessingInstruction`,
    // `readChildren`), so tokenization, entity expansion and error
    // reporting are identical on both paths. Position tracking is not
    // threaded through the direct path (there is no `PositionIndex` — the
    // result is the caller's value, not an `Xml`), as in jacinta and
    // stratiform.
    //
    // The stepping protocol: `directRoot()` or `directNextChild()` *opens*
    // an element — consuming `<name attrs…>` and pushing the name onto an
    // explicit stack — and exactly one content consumer (`directNextChild`
    // until `null`, `directText`, `directSkipElement` or `directElement`)
    // then consumes its content *and its close tag*, validating the close
    // name against the stack (the `MismatchedTag` check) and popping it. A
    // self-closing element is opened with `directEmpty` set, and the first
    // content consumer pops it immediately without touching the input.

    private val directNames: scala.collection.mutable.ArrayBuffer[BaseText] =
      scala.collection.mutable.ArrayBuffer.empty

    private var directAttributes1: Attributes = Attributes.empty
    private var directEmpty: Boolean = false

    // The most recently opened child's name fingerprint (snapshotted in
    // `directOpen` before `readAttributes` clobbers `readName`'s), and the
    // child's interned name for the `NameOpaque` general dispatch.
    private var directChildLow:      Long = 0L
    private var directChildHigh:     Long = 0L
    private var directChildPackable: Boolean = false
    private var directChildName:     BaseText = t""

    // The scopes of the open elements, parallel to `directNames`
    private val directScopes: scala.collection.mutable.ArrayBuffer[Scope] =
      scala.collection.mutable.ArrayBuffer.empty

    private update def directPop(): BaseText =
      directScopes.remove(directScopes.length - 1)
      scope = if directScopes.isEmpty then rootScope else directScopes(directScopes.length - 1)
      directNames.remove(directNames.length - 1)

    // The resolved name of the element opened most recently
    private[xylophone] def directName(): Xml.Name =
      val (prefix, local) = Xml.Name.split(directNames(directNames.length - 1))
      Xml.Name(scope.resolve(prefix), local)

    // Establishes the cursor hold for a whole direct-parsing session,
    // exactly as `parseXml` does for one tree-building parse: every rim
    // method uses `begin()`/`slice()`, which require the held token.
    private[xylophone] update def directSession[result](body: => result): result =
      cursor.hold:
        heldToken = summon[Cursor.Held]

        try body finally heldToken = null

    // Positions the parser at the document's root value, mirroring
    // `parseXml0(headers0 = false)`'s dispatch composed with `Xml#as`'s
    // shape rules:
    //   0 — a root element was opened (its name and attributes consumed);
    //   1 — the root begins with character data (`directRootText` reads
    //       it), the direct counterpart of decoding `Fragment(TextNode…)`;
    //   2 — nothing decodable is present: the end of the input, or a root
    //       comment / PI / doctype / close tag, which the AST path decodes
    //       as a wrong-shape `Fragment` — the caller continues with
    //       `absent()`.
    private[xylophone] update def directRoot()(using Tactic[Parse.Error]): Int =
      headers = false
      skipWs()

      if !more then 2
      else if peek != '<' then 1
      else
        advance()
        if !more then fail(Issue.ExpectedMore)
        val c2 = peek

        if c2 == '/' || c2 == '!' || c2 == '?' then 2
        else
          directOpen()
          0

    // The root character data, up to the next markup or the end of the
    // input — read exactly as `parseXml0` reads a root-level text run.
    private[xylophone] update def directRootText()(using Tactic[Parse.Error]): BaseText = readText(t"")

    // The attributes of the element opened most recently. Valid until the
    // next element is opened.
    private[xylophone] def directAttributes(): Attributes = directAttributes1

    // True when non-whitespace content follows the root value — the direct
    // counterpart of the AST path's multi-node `Fragment`, which decodes as
    // wrong-shape. (`parseXml0` consumes root-level whitespace with
    // `skipWs` between nodes.)
    private[xylophone] update def directTrailing(): Boolean =
      skipWs()
      more

    // Opens an element whose `<` has been consumed: reads the name and the
    // attributes, consumes `>` or `/>`, and pushes the name. Reuses
    // `readName` and `readAttributes`, so validation (name syntax,
    // duplicate attributes) is identical to `readElement`'s.
    private update def directOpen()(using Tactic[Parse.Error]): BaseText =
      val name = readName()
      directChildLow = nameLow
      directChildHigh = nameHigh
      directChildPackable = namePackable
      directAttributes1 = readAttributes(name)
      if !more then fail(Issue.ExpectedMore)
      val own = openScope(name, directAttributes1)

      if peek == '/' then
        advance()
        if !more then fail(Issue.ExpectedMore)
        if peek != '>' then fail(Issue.Unexpected(peekChar))
        advance()
        directEmpty = true
      else
        if peek != '>' then fail(Issue.Unexpected(peekChar))
        advance()
        directEmpty = false

      directNames += name
      directScopes += own
      scope = own
      name

    // Consumes and validates the current element's close tag; the position
    // is just after `</`. Mirrors `readChildren`'s close-tag arm, including
    // the `MismatchedTag` check against the opening name.
    private update def directClose()(using Tactic[Parse.Error]): Unit =
      val parent = directNames(directNames.length - 1)
      val closeStart = begin()
      val close = readName()
      skipWs()
      if !more then fail(Issue.ExpectedMore, closeStart)
      if peek != '>' then fail(Issue.Unexpected(peekChar), closeStart)
      advance()
      if close != parent then fail(Issue.MismatchedTag(parent, close), closeStart)
      directPop()

    // Consumes a comment or a CDATA section, discarding it; the position is
    // just after `<!`. Mirrors `readChildren`'s `!` arm.
    private update def directBang()(using Tactic[Parse.Error]): Unit =
      if more && peek == '-' then
        advance()
        if !more then fail(Issue.ExpectedMore)
        if peek != '-' then fail(Issue.Unexpected(peekChar))
        advance()
        readComment()
      else if more && peek == '[' then
        advance()
        consumeLiteral("CDATA[")
        readCdata()
      else
        if !more then fail(Issue.ExpectedMore)
        fail(Issue.Unexpected(peekChar))

    // Steps to the current element's next child *element*, opening it and
    // returning its name, or consumes the close tag and returns `null` once
    // the element ends. Character data, comments, CDATA sections and
    // processing instructions between child elements are consumed and
    // discarded — mirroring the AST derivation, whose `buildWith` collects
    // nothing but `Element`s from `element.children`.
    private[xylophone] update def directNextChild()(using Tactic[Parse.Error]): BaseText | Null =
      if directEmpty then
        directEmpty = false
        directPop()
        null
      else
        val parent = directNames(directNames.length - 1)
        var result: BaseText | Null = null
        var done = false

        while !done do
          if !more then fail(Issue.Incomplete(parent))

          if peek == '<' then
            advance()
            if !more then fail(Issue.ExpectedMore)
            val c2 = peek

            if c2 == '/' then
              advance()
              directClose()
              done = true
            else if c2 == '!' then
              advance()
              directBang()
            else if c2 == '?' then
              advance()
              readProcessingInstruction()
            else
              result = directOpen()
              done = true
          else
            readText(parent)

        result

    // As `directNextChild`, but returning the child's name in packed form
    // for staged parsers that compare names against literal constants: the
    // packed low word (its high word from `directChildWordHigh`),
    // `Xml.Reader.NameEnd` once the close tag is consumed, or
    // `Xml.Reader.NameOpaque` when the name cannot pack (non-ASCII, or longer
    // than sixteen chars) — the child is still opened, and
    // `directChildLabel` identifies it for the general dispatch.
    private[xylophone] update def directNextChildWord()(using Tactic[Parse.Error]): Long =
      val name = directNextChild()

      if name == null then Xml.Reader.NameEnd
      else
        directChildName = name.nn
        if !directChildPackable then Xml.Reader.NameOpaque else directChildLow

    private[xylophone] def directChildWordHigh: Long = directChildHigh
    private[xylophone] def directChildLabel: BaseText = directChildName

    // The current element's text content, consumed together with its close
    // tag: the text when the content is exactly one text run (or empty), or
    // `null` for any other shape — mirroring `textOf`, which accepts only
    // `Element(_, _, Array(TextNode(text)))` and `Element(_, _, Array())`.
    // A CDATA section, a comment, a processing instruction or a child
    // element therefore makes a leaf wrong-shaped on both paths.
    private[xylophone] update def directText()(using Tactic[Parse.Error]): BaseText | Null =
      if directEmpty then
        directEmpty = false
        directPop()
        t""
      else
        val parent = directNames(directNames.length - 1)
        var nodes = 0
        var single: BaseText | Null = null
        var done = false

        while !done do
          if !more then fail(Issue.Incomplete(parent))

          if peek == '<' then
            advance()
            if !more then fail(Issue.ExpectedMore)
            val c2 = peek

            if c2 == '/' then
              advance()
              directClose()
              done = true
            else if c2 == '!' then
              advance()
              directBang()
              nodes += 1
            else if c2 == '?' then
              advance()
              readProcessingInstruction()
              nodes += 1
            else
              directOpen()
              directSkipElement()
              nodes += 1
          else
            val text = readText(parent)

            if text.length > 0 then
              if nodes == 0 then single = text
              nodes += 1

        if nodes == 0 then t"" else if nodes == 1 then single else null

    // ── Byte-parsed scalar content ─────────────────────────────────────────
    // The current element's text content parsed straight from the buffered
    // chars as a primitive, consumed together with its close tag, or `Unset`
    // for missing, wrong-shaped or unparseable content — saving the value
    // `Text` that `directText` would materialize only for the primitive to
    // re-scan. The fast path accepts only content whose parse is provably
    // identical to the `String`-parsing primitives'; anything else — an
    // entity, an interior node, an exotic spelling, a value outside the
    // exact range — rewinds to the content's start (the `readAttrValue`
    // precedent) and takes the general `directText` path, so the two routes
    // agree by construction.

    private update def directTextLongFallback()(using Tactic[Parse.Error]): Optional[Long] =
      directText() match
        case null       => Unset
        case text: BaseText =>
          try Optional(jl.Long.parseLong(text.s)) catch case _: NumberFormatException => Unset

    private[xylophone] update def directTextLong()(using Tactic[Parse.Error]): Optional[Long] =
      if directEmpty then
        directText()
        Unset
      else
        val start = begin()
        var value = 0L
        var digits = 0
        var neg = false
        var bad = false
        var c: Byte = 0

        while !bad && more && { c = peek; c != '<' } do
          if c >= '0' && c <= '9' then
            // Eighteen digits can never overflow; longer runs fall back.
            if digits == 18 then bad = true else
              value = value*10 + (c - '0')
              digits += 1
              advance()
          else if c == '-' && digits == 0 && !neg then
            neg = true
            advance()
          else bad = true

        if !bad && more && digits > 0 then
          advance()

          if more && peek == '/' then
            advance()
            directClose()
            Optional(if neg then -value else value)
          else
            reset(start)
            directTextLongFallback()
        else
          reset(start)
          directTextLongFallback()

    private[xylophone] update def directTextInt()(using Tactic[Parse.Error]): Optional[Int] =
      val value = directTextLong()

      value.let: long =>
        if long >= Int.MinValue.toLong && long <= Int.MaxValue.toLong
        then Optional(long.toInt)
        else Unset

    private update def directTextDoubleFallback()(using Tactic[Parse.Error]): Optional[Double] =
      directText() match
        case null       => Unset
        case text: BaseText =>
          try Optional(jl.Double.parseDouble(text.s))
          catch case _: NumberFormatException => Unset

    private[xylophone] update def directTextDouble()(using Tactic[Parse.Error]): Optional[Double] =
      if directEmpty then
        directText()
        Unset
      else
        val start = begin()
        var mantissa = 0L
        var digits = 0
        var decimals = -1
        var neg = false
        var bad = false
        var c: Byte = 0

        while !bad && more && { c = peek; c != '<' } do
          if c >= '0' && c <= '9' then
            // Fifteen mantissa digits stay below 2^53, so `toDouble` is
            // exact and the division below is correctly rounded — the
            // Clinger fast path, bit-identical to `Double.parseDouble`.
            if digits == 15 then bad = true else
              mantissa = mantissa*10 + (c - '0')
              digits += 1
              if decimals >= 0 then decimals += 1
              advance()
          else if c == '.' && decimals < 0 then
            decimals = 0
            advance()
          else if c == '-' && digits == 0 && decimals < 0 && !neg then
            neg = true
            advance()
          else bad = true

        if !bad && more && digits > 0 then
          advance()

          if more && peek == '/' then
            advance()
            directClose()
            val scale = if decimals > 0 then decimals else 0
            val magnitude = mantissa.toDouble/XmlParser.TenPow.readUnchecked(scale)
            Optional(if neg then -magnitude else magnitude)
          else
            reset(start)
            directTextDoubleFallback()
        else
          reset(start)
          directTextDoubleFallback()

    private update def directTextBooleanFallback()(using Tactic[Parse.Error]): Optional[Boolean] =
      directText() match
        case null       => Unset
        case text: BaseText => text.s match
          case "true"  => Optional(true)
          case "false" => Optional(false)
          case _       => Unset

    private[xylophone] update def directTextBoolean()(using Tactic[Parse.Error]): Optional[Boolean] =
      if directEmpty then
        directText()
        Unset
      else
        val start = begin()
        var word = 0L
        var length = 0
        var bad = false
        var c: Byte = 0

        while !bad && more && { c = peek; c != '<' } do
          if c >= 'a' && c <= 'z' && length < 5 then
            word |= (c.toLong & 0xFF) << (length*8)
            length += 1
            advance()
          else bad = true

        val isTrue = length == 4 && word == XmlParser.TrueWord
        val isFalse = length == 5 && word == XmlParser.FalseWord

        if !bad && more && (isTrue || isFalse) then
          advance()

          if more && peek == '/' then
            advance()
            directClose()
            Optional(isTrue)
          else
            reset(start)
            directTextBooleanFallback()
        else
          reset(start)
          directTextBooleanFallback()

    // Skips the current element's entire remaining subtree, consuming and
    // validating every close tag on the way, building nothing. Used for
    // unknown child elements and for duplicate occurrences of a field (the
    // AST derivation's first-match-wins `HashMap`).
    private[xylophone] update def directSkipElement()(using Tactic[Parse.Error]): Unit =
      if directEmpty then
        directEmpty = false
        directPop()
      else
        val baseDepth = directNames.length

        while directNames.length >= baseDepth do
          val parent = directNames(directNames.length - 1)
          if !more then fail(Issue.Incomplete(parent))

          if peek == '<' then
            advance()
            if !more then fail(Issue.ExpectedMore)
            val c2 = peek

            if c2 == '/' then
              advance()
              directClose()
            else if c2 == '!' then
              advance()
              directBang()
            else if c2 == '?' then
              advance()
              readProcessingInstruction()
            else
              directOpen()

              if directEmpty then
                directEmpty = false
                directPop()
          else
            readText(parent)

    // Materializes the current element as an `Element` — the bridge for
    // field types that only carry a `Decodable in Xml`. The children are
    // read with `readChildren`, so the materialized subtree (and its
    // close-tag validation) is exactly what `readElement` would have built.
    private[xylophone] update def directElement()(using Tactic[Parse.Error]): Element =
      val own = scope
      val name = directPop()
      val parent = scope
      val attributes = directAttributes1

      val children =
        if directEmpty then
          directEmpty = false
          Array.empty[Node]
        else
          scope = own
          val children = readChildren(name)
          scope = parent
          children

      Element(name, attributes, children, own)

  // ───────────────────────────────────────────────────────────────────────
  // Public entry points.

  // Back-compat for macro interpolators: matches the previous cursor-based
  // signature (Iterator[Text] + callback).
  private[xylophone] def parse[schema <: XmlSchema]
    ( input:    Iterator[BaseText],
      root:     Tag,
      callback: (Ordinal, Hole) => Unit                = (_, _) => (),
      headers0: Boolean                           = false )
    ( using schema: XmlSchema )
  :   (Tactic[Parse.Error]^) ?->{callback} Xml =

    // Lenient: a literal may use a prefix bound only by a `Namespace` given at its call site,
    // which the interpolator checks after parsing
    new XmlParser
      ( Cursor[Data](XmlParser.utf8(Chain.from(input))), tracking = false, callback,
        charOffsets = true )
      (using schema, Scope.xml, Namespacing.Lenient)
    . parseXml(headers0)

  // Selects the nodes matching an XPath: `//div[@id='x']` and friends,
  // evaluated against this tree. The result is a `Fragment` of the matching
  // tree nodes in document order; a path selecting attributes (`//a/@href`)
  // yields their values through `selectText` or `evaluate` instead, since
  // attributes are not tree nodes.
  extension (xml: Xml)
    def select(xpath: XPath)(using Tactic[XPath.Error]): Fragment =
      XPathEngine.evaluate(xml, xpath.expression, Map(), xpath.scope) match
        case XPath.Value.NodeSet(loci) =>
          val nodes = loci.bind: locus =>
            locus.attributeIndex match
              case _: Int => Nil

              case _ => locus.subject match
                case node: Node => List(node)
                case _          => Nil

          new Fragment(nodes*)

        case _ =>
          abort(XPath.Error(XPath.Error.Reason.NotNodeSet))

    // The string-value of the first matching node (`Unset` when nothing
    // matches), or of the expression's value for non-node-set results. This is
    // the way to read an attribute selected by path: `xml.selectText(xp"//a/@href")`.
    def selectText(xpath: XPath)(using Tactic[XPath.Error]): Optional[BaseText] =
      XPathEngine.evaluate(xml, xpath.expression, Map(), xpath.scope) match
        case XPath.Value.NodeSet(loci) => loci.prim.let(_.stringValue)

        case value =>
          value.text

    // Full XPath 1.0 expression evaluation, yielding one of the four value
    // types: `count(//div)` is a number, `//a` a node-set. Variables referenced
    // as `$name` resolve from `variables`, keyed by qualified name.
    def evaluate(xpath: XPath, variables: Map[BaseText, XPath.Value] = Map())
      ( using Tactic[XPath.Error] )
    :   XPath.Value =

      XPathEngine.evaluate(xml, xpath.expression, variables, xpath.scope)

  // XmlError → Xml.Error
  //
  // These are *raise-and-continue* errors: a record with several bad fields accrues one
  // per field rather than bailing at the first, so the reason is the only thing that
  // distinguishes them. `Foci[Xml.Focus]` carries *where*; the reason says *what*.
  object Error:
    enum Reason(val number: Int) extends Clarification:
      // A value was there, but wrong.
      case Malformed(text: BaseText, expected: BaseText)         extends Reason(1)
      case Untextual(expected: BaseText)                     extends Reason(2)

      // Nothing was there.
      case Absent(expected: BaseText)                        extends Reason(3)
      case AbsentProduct(product: BaseText)                  extends Reason(4)
      case AbsentVariant(sum: BaseText)                      extends Reason(5)

      // The shape was wrong.
      case UnknownVariant(discriminant: BaseText, sum: BaseText) extends Reason(6)

      // The type being read is not nameable at the raise site — the generic text bridges.
      case Empty                                         extends Reason(7)
      case Missing                                       extends Reason(8)

    given communicable: Reason is Communicable =
      case Reason.Malformed(text, expected) => m"the text $text is not a valid $expected"
      case Reason.Untextual(expected)       => m"the element has no text to read as $expected"

      case Reason.Absent(expected) =>
        m"no element or attribute supplied the required $expected"
      case Reason.Empty                     => m"the element has no text"
      case Reason.Missing                   => m"the element or attribute was not present"

      case Reason.AbsentProduct(product) =>
        m"no element supplied the required $product, which has no default"

      case Reason.AbsentVariant(sum) =>
        m"no element supplied the required $sum, which has no default"

      case Reason.UnknownVariant(discriminant, sum) =>
        m"$discriminant does not name a variant of $sum"

  case class Error(reason: Xml.Error.Reason)(using Diagnostics)
  extends fulminate.Error(149, reason.number)(m"the XML could not be read because $reason")

  // XmlReader → Xml.Reader
  object Reader:
    // Sentinels of `childWord()`; impossible as packed names, whose chars are
    // all 7-bit ASCII.
    inline final val NameEnd = -1L
    inline final val NameOpaque = -2L

    // Only xylophone's read path (`Xml.parseDirect`) constructs readers, so the
    // exclusivity of the wrapped parser and the resolution scope of the carried
    // capabilities are preserved by construction. The wrapped capabilities
    // travel as neutral carriers (jacinta's `Json.Reader` pattern): the fields
    // stay pure, and each accessor reasserts the type at the rim — the audited
    // point.
    private[xylophone] def apply
      ( parser:    Xml.XmlParser^,
        tactic:    Tactic[Parse.Error],
        xmlTactic: Tactic[Xml.Error],
        foci:      Foci[Xml.Focus] )
    :   Xml.Reader^ =

      new Xml.Reader
        ( parser.asInstanceOf[AnyRef],
          tactic.asInstanceOf[AnyRef],
          xmlTactic.asInstanceOf[AnyRef],
          foci.asInstanceOf[AnyRef] )

  // The public, restricted rim of the XML parser, handed to `Xml.Parsable`
  // instances so they can consume elements straight off the input without an
  // intermediate `Xml` tree. `parse` is invoked with the current element just
  // *opened* (its name and attributes consumed); exactly one of the content
  // consumers — `nextChild` until `Unset`, `text`, `skipElement` or `element` —
  // must then consume the element's content and its close tag in full.
  //
  // The reader carries its own `Tactic[Parse.Error]`, so malformed input aborts
  // through the read call's ambient tactic — and, unlike jacinta's reader, the
  // read-site `Tactic[Xml.Error]` and `Foci[Xml.Focus]` too, so decode errors
  // raised by `Parsable` instances accrue to the same `validate` boundary the
  // AST path's inline derivation uses, with the same field foci, even when the
  // `Parsable` given was instantiated outside the boundary.
  //
  // An exclusive, stateful capability, like the parser it wraps: it is owned
  // by one `Xml.Parsable.parse` call at a time, for the duration of that call,
  // and nothing of it may be retained afterwards.
  final class Reader private
    ( parser0: AnyRef, tactic0: AnyRef, xmlTactic0: AnyRef, foci0: AnyRef )
  extends caps.ExclusiveCapability, caps.Stateful:
    private inline def parser: Xml.XmlParser^ = parser0.asInstanceOf[Xml.XmlParser^]

    private[xylophone] inline def parseTactic: Tactic[Parse.Error] =
      tactic0.asInstanceOf[Tactic[Parse.Error]]

    // The read-site capabilities, public because staged parsers — generated
    // into user modules — bind them once per record for focus bookkeeping and
    // absent-field raising, exactly as the derived engine does.
    inline def errorTactic: Tactic[Xml.Error] =
      xmlTactic0.asInstanceOf[Tactic[Xml.Error]]

    inline def foci: Foci[Xml.Focus] = foci0.asInstanceOf[Foci[Xml.Focus]]

    // The attributes of the just-opened element, valid until the next element
    // is opened. The derived product parser reads them before its child loop,
    // so `@attribute` fields are filled before any child is consumed.
    update def attributes(): Attributes = parser.directAttributes()

    // The resolved name of the just-opened element: its namespace, through the bindings in
    // scope, and its local part
    update def name(): Xml.Name = parser.directName()

    // Steps within the current element: the name of the next child element
    // (opened — its name and attributes consumed), or `Unset` once the close
    // tag is consumed and validated. Character data, comments, CDATA sections
    // and processing instructions between child elements are consumed
    // transparently — the AST derivation looks only at `Element` children.
    //
    // The hot forwarders are `inline` (enabled by the toolchain's
    // inline-update receiver fix), so the derived engine's steps reach the
    // parser without a call through the rim. Methods that appear inside
    // xylophone's macro quotes stay non-inline: the spliced reader there is
    // capture-erased, and an inline update method requires an exclusive
    // receiver.
    update def nextChild(): Optional[BaseText] =
      val name = parser.directNextChild()(using parseTactic)
      if name == null then Unset else name.nn

    // The next child step in packed form, for parsers that compare names
    // against literal constants (staged parsers compile field names to
    // immediates): the packed low word of the child's name (its high word from
    // `childWordHigh`), `NameEnd` once the close tag is consumed, or
    // `NameOpaque` when the name cannot pack — the child is still opened, and
    // `childLabel` identifies it for a general dispatch. Public because staged
    // parsers are generated into user modules.
    update def childWord(): Long = parser.directNextChildWord()(using parseTactic)

    update def childWordHigh: Long = parser.directChildWordHigh

    update def childLabel: BaseText = parser.directChildLabel

    // The current element's text content, consumed together with its close
    // tag: `Unset` when the content is not exactly one text run (mirroring
    // `textOf`'s shape rules, under which CDATA is *not* text). Backs the
    // text-codec parsers.
    update def text(): Optional[BaseText] =
      val text = parser.directText()(using parseTactic)
      if text == null then Unset else text.nn

    // The current element's content parsed straight from the buffered chars
    // as a primitive (consumed with its close tag), or `Unset` for missing,
    // wrong-shaped or unparseable content — the byte-parsed counterparts of
    // `text()`, saving the value `Text` it would otherwise materialize only
    // for the primitive to re-scan. Values and failures agree with the
    // `String`-parsing primitives exactly: exotic content falls back to the
    // general path internally.
    update def int(): Optional[Int] = parser.directTextInt()(using parseTactic)
    update def long(): Optional[Long] = parser.directTextLong()(using parseTactic)
    update def double(): Optional[Double] = parser.directTextDouble()(using parseTactic)
    update def boolean(): Optional[Boolean] = parser.directTextBoolean()(using parseTactic)

    // Skips the current element's entire subtree, validating every close tag
    // on the way, building nothing — for unknown child elements and duplicate
    // occurrences of a field.
    update def skipElement(): Unit = parser.directSkipElement()(using parseTactic)

    // The fallback seam: materialize the current element as an `Xml` tree, for
    // field types that only carry a `Decodable in Xml`.
    update def element(): Xml = parser.directElement()(using parseTactic)

    // Raise an `Xml.Error` through the read-site tactic and continue — for leaf
    // instances that reject an element's content, preserving the AST
    // primitives' raise-and-continue accrual. The caller supplies the reason:
    // every call site knows which of `int()`/`text()`/a codec conversion failed,
    // and on what, so none of it needs to be discarded here.
    update def fault(reason: Xml.Error.Reason): Unit =
      raise(Xml.Error(reason))(using errorTactic)

  // `caps.Pure` because, under separation checking, a class nested in an object is otherwise given
  // an open capture set in its self type, which a node's pure parent rejects. And `this.` throughout
  // the nodes: `object Xml` is itself a `Tag.Container`, so its own `Topic`, `Transport` and `Form`
  // would otherwise be ambiguous with each node's inherited members.
  sealed trait Node extends Xml, caps.Pure

  case class Comment(text: BaseText) extends Node:
    override def hashCode: Int = text.hashCode*31 + 0x436F6D6D

    override def equals(that: Any): Boolean = that match
      case Comment(text0)           => text0 == text
      case Fragment(Comment(text0)) => text0 == text
      case _                        => false

  case class Doctype(text: BaseText) extends Node:
    override def hashCode: Int = text.hashCode*31 + 0x44637470

    override def equals(that: Any): Boolean = that match
      case Doctype(text0)           => text0 == text
      case Fragment(Doctype(text0)) => text0 == text
      case _                        => false

  case class Cdata(text: BaseText) extends Node:
    override def hashCode: Int = text.hashCode*31 + 0x43646174

    override def equals(that: Any): Boolean = that match
      case Cdata(text0)           => text0 == text
      case Fragment(Cdata(text0)) => text0 == text
      case _                      => false

  case class ProcessingInstruction(target: BaseText, data: BaseText) extends Node:
    override def hashCode: Int = (target.hashCode*31 + data.hashCode)*31 + 0x50494E73

    override def equals(that: Any): Boolean = that match
      case ProcessingInstruction(target0, data0)           => target0 == target && data0 == data
      case Fragment(ProcessingInstruction(target0, data0)) => target0 == target && data0 == data
      case _                                               => false

  case class Text(text: BaseText) extends Node:
    type Topic = "#text"

    override def hashCode: Int = text.hashCode*31 + 0x54657874

    override def equals(that: Any): Boolean = that match
      case Fragment(textual: Text) => this == textual
      case Text(text0)             => text0 == text
      case _                       => false

  // A plain class rather than a case class: the `scope` — the namespace bindings in force at the
  // element, filled by the parser, an interpolated literal or a derived encoder — is provenance,
  // like `Header.positionIndex`, and takes no part in equality, hashing or the extractor, so a
  // parsed element equals the same element built by hand, and `Element(label, attributes,
  // children)` patterns see the three fields they always did.
  // An element's `scope` defaults to `xylophone.internal.Scope.empty`, not the exported
  // `Scope.empty`: `Tag` is an element, so the default is evaluated while `object Xml` itself is
  // being constructed, before its export forwarders can be called.
  object Element:
    def apply
      ( label:      BaseText,
        attributes: Attributes,
        children:   Array[Node]^{},
        scope:      Scope          = xylophone.internal.Scope.empty )
    :   Element =

      new Element(label, attributes, children, scope)

    def unapply(element: Element): Some[(BaseText, Attributes, Array[Node]^{})] =
      Some((element.label, element.attributes, element.children))

  class Element
    ( val label:      BaseText,
      val attributes: Attributes,
      val children:   Array[Node]^{},
      val scope:      Scope = xylophone.internal.Scope.empty )
  extends Node, Topical, Transportive:
    override def toString(): String =
      s"<$label>${children.readable.mkString}</$label>"

    // The bindings in force at this element: its scope, extended by any declarations among its
    // own attributes — which are all a hand-built element has
    def bindings: Scope =
      if attributes.declaresNamespace then Scope.declared(scope, attributes) else scope

    def prefix: Optional[BaseText] = Xml.Name.split(label)(0)
    def localName: BaseText = Xml.Name.split(label)(1)

    // The URI bound to the prefix at this element, or `Unset` if it is unbound
    def resolve(prefix: Optional[BaseText]): Optional[BaseText] = bindings.resolve(prefix)

    def namespace: Optional[BaseText] = resolve(prefix)

    // The resolved name: the namespace URI, if any, and the local part. (`name` is taken: the
    // typed tags, which are elements, are `Format`s with a `name`.)
    def qualified: Xml.Name =
      val (prefix, local) = Xml.Name.split(label)
      Xml.Name(bindings.resolve(prefix), local)

    // The value of the attribute with the resolved name; an unprefixed attribute is in no
    // namespace, whatever the default namespace
    def attribute(name: Xml.Name): Optional[BaseText] = name.namespace.lay(attributes.fetch(name.local)):
      uri =>
        var found: Optional[BaseText] = Unset
        val bindings0 = bindings

        attributes.eachPair: (key, value) =>
          if found.absent then
            val (prefix, local) = Xml.Name.split(key)

            if prefix.present && local == name.local && bindings0.resolve(prefix) == uri
            then found = value

        found

    // Whether a child of this element with the label is the one `name` selects: by resolved
    // name when the name's prefix is bound in the scope or at this element, else by raw label
    private[xylophone] def selects(child: Element, name: BaseText)(using scope: Scope): Boolean =
      val (prefix, local) = Xml.Name.split(name)

      if prefix.absent then child.label == name else
        scope.resolve(prefix).or(resolve(prefix)).lay(child.label == name): uri =>
          child.qualified == Xml.Name(uri, local)

    override def equals(that: Any): Boolean = that match
      case Fragment(node: Element) => this == node

      case Element(label, attributes, children) =>
        label == this.label && attributes.equalsAttributes(this.attributes) &&
          ju.Arrays.equals(Array.unsafeJvm(children).asInstanceOf[scala.Array[Object | Null]], Array.unsafeJvm(this.children).asInstanceOf[scala.Array[Object | Null]])

      case _ =>
        false

    override def hashCode: Int =
      ju.Arrays.hashCode(Array.unsafeJvm(children).asInstanceOf[scala.Array[Object | Null]]) ^ attributes.hashAttributes ^ label.hashCode


    def selectDynamic(name: Label)
      ( using attribute: name.type is Xml.XmlAttribute on this.Topic in this.Form )
    :   Optional[BaseText] =

      attributes(name.tt)


    def updateDynamic(name: Label)(using attribute: name.type is Xml.XmlAttribute in this.Form)
      ( value: BaseText )
    :   Element of this.Topic over this.Transport in this.Form =

      Element(label, attributes.updated(name, value), children)
      . of[this.Topic]
      . over[this.Transport]
      . in[this.Form]

  object Fragment:
    @targetName("make")
    def apply[topic <: Label](nodes: Xml of (? <: topic)*): Fragment of topic =
      new Fragment(List.from(nodes).nodes*).of[topic]

  case class Fragment(nodes: Node*) extends Xml:
    override def hashCode: Int = if nodes.length == 1 then nodes(0).hashCode else nodes.hashCode

    override def equals(that: Any): Boolean = that match
      case Fragment(nodes0*) => nodes0 == nodes
      case node: Xml         => nodes.length == 1 && nodes(0) == node
      case _                 => false

  // The `positionIndex` rides in the document `Metadata` (a `Document[Xml]`'s
  // `metadata` is its `Header`), carrying the position index produced when the
  // document was loaded with `parsing.trackPositions` in scope. It is deliberately
  // excluded from `equals`/`hashCode`/serialization: it is parse provenance, not
  // part of the document's identity, so a tracked and an untracked load of the same
  // source compare equal.
  case class Header
      ( version:       BaseText,
        encoding:      Optional[BaseText],
        standalone:    Optional[Boolean],
        positionIndex: Optional[Xml.PositionIndex] = Unset )
  extends Node:
    override def hashCode: Int =
      ((version.hashCode*31 + encoding.hashCode)*31 + standalone.hashCode)*31 + 0x48646572

    override def equals(that: Any): Boolean = that match
      case Fragment(header: Header) => equals(header)

      case Header(version0, encoding0, standalone0, _) =>
        version0 == version && encoding0 == encoding && standalone0 == standalone

      case _ =>
        false


sealed into trait Xml extends Dynamic, Topical, Documentary, Formal:
  type Topic <: Label
  type Transport <: Label
  type Metadata = Xml.Header
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
  // stays `Unset`; decode a `Document[Xml]` loaded with `parsing.trackPositions`
  // in scope (`document.as[T]`) if you also want source line / column.
  //
  // Capturing evidence, as jacinta's `as` takes: a decoder built from a `Tactic` captures it, and
  // a context bound cannot say so. The `Foci` is a plain using-parameter, since a
  // context-function result may not hide the capability-typed evidence.
  def as[result](using decodable: (result is Decodable in Xml)^)(using Foci[Xml.Focus]): result =
    this match
      case Xml.Fragment(value) => decodable.decoded(value)
      case xml: Xml            => decodable.decoded(xml)

  // Dynamic navigation. `xml.foo` selects every child element named `foo`,
  // flattening across all element-nodes in the current `Fragment` (XML tags
  // are not unique, so a dereference yields a `Fragment` of zero or more
  // matches). `xml.foo(ordinal)` picks a single one; the ordinal defaults to
  // `Prim`, so `xml.foo()` is the first match. Both are gated by an erased
  // `Xml is Dynamical` (see `dynamicAccess.dynamicXml` and `dynamically`).

  private def selfNodes: Array[Xml.Node]^{} = this match
    case Xml.Fragment(nodes*) => Array.from(nodes)
    case node: Xml.Node       => Array(node)

  // The child elements the name selects: a prefixed name whose prefix is bound in the scope, or
  // at the parent, selects by resolved name, whatever prefix the child uses; otherwise the raw
  // label is matched.
  private def childElements(name: String)(using scope: Scope): Array[Xml.Node]^{} =
    matchingElements(_.selects(_, name.tt))

  private def namedElements(name: Xml.Name): Array[Xml.Node]^{} =
    matchingElements { (_, child) => child.qualified == name }

  // The child elements, of every element node here, which the predicate admits given their parent
  private def matchingElements(admits: (Xml.Element, Xml.Element) => Boolean): Array[Xml.Node]^{} =
    val buffer = scm.ArrayBuffer[Xml.Node]()
    val nodes = selfNodes
    var i = 0

    while i < nodes.length do
      nodes.readUnchecked(i) match
        case parent: Xml.Element =>
          val children = parent.children

          children.extent.each: j =>
            children(j) match
              case child: Xml.Element if admits(parent, child) => buffer.append(child)
              case _                                           => ()

        case _ =>
          ()

      i += 1

    Array.from(buffer)

  // Every child element with the resolved name, and the one at the ordinal
  def elements(name: Xml.Name): Xml.Fragment = new Xml.Fragment(namedElements(name)*)

  def element(name: Xml.Name, ordinal: Ordinal = Prim): Xml.Fragment =
    namedElements(name).at(ordinal).lay(new Xml.Fragment())(new Xml.Fragment(_))

  def selectDynamic(name: String)(using erased dynamical: (? >: Xml) is Dynamical, scope: Scope)
  :   Xml.Fragment =

    new Xml.Fragment(childElements(name)*)

  def applyDynamic(name: String)(ordinal: Ordinal = Prim)
    ( using erased dynamical: (? >: Xml) is Dynamical, scope: Scope )
  :   Xml.Fragment =

    childElements(name).at(ordinal).lay(new Xml.Fragment())(new Xml.Fragment(_))
