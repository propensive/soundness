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
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package stratiform

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import prepositional.*
import vacuous.*
import rudiments.`:+`
import denominative.asymptotics.linearSizeComplexity

// TELP, the TEL Path companion specification: a schema-aware textual path
// over the semantic model. The first character of a path selects its
// component delimiter (there is no escaping — a component containing one
// candidate delimiter is written under another); components resolve by
// keyword, and occurrences of repeatable members are selected by key
// value (§20 of the TEL specification) or by zero-based index, an
// all-digit component always being an index (the shadowing rule). A
// path's identity is its component sequence alone — `.a.b` and `/a/b`
// are the same path.
//
// `Telp` is also the focus pointer for multi-error accrual during
// decoding (`Tel.Focus`): a rendered focus like `/person/name` is a
// valid, resolvable TELP, since keywords are kebab-case and so never
// all-digits. The presentation-layer `Tel.Pointer` addressing of §22.2
// machine operations is deliberately separate (telp.md §9): TELP is a
// query mechanism over the semantic model and defines no operations.
object Telp:
  val Root: Telp = Telp(Nil)

  // §3: the sigil-valid set minus `-` and `'` — the two sigil-valid
  // characters that may appear inside a kebab-case identifier — leaving
  // twenty-one characters. `/` and `.` are the conventional choices.
  val delimiters: Text = Text("""!"#$%&*,./:;=?@\^_`|~""")

  object Error:
    // The failure kinds of telp.md §7. These are outcomes of the
    // resolving API, not document errors: they carry no E-codes.
    enum Reason:
      case Syntax, UnknownKeyword, NonStructDescent, AbsentMember, IndexOutOfRange,
        TypeNotKeyed, KeyNotFound

    given communicable: Reason is Communicable =
      case Reason.Syntax           => m"the path is not syntactically valid"
      case Reason.UnknownKeyword   => m"the component matches no keyword of the current struct"
      case Reason.NonStructDescent => m"the path descends below a value or flag"
      case Reason.AbsentMember     => m"the non-repeatable optional member is not present"
      case Reason.IndexOutOfRange  => m"the index selector exceeds the occurrence sequence"
      case Reason.TypeNotKeyed     => m"a key selector was applied to a type with no key field"
      case Reason.KeyNotFound      => m"no occurrence has the selected key value"

  // `index` is the 0-based index of the component at which
  // resolution or parsing failed (§7).
  case class Error(reason: Error.Reason, index: Int)(using Diagnostics)
  extends fulminate.Error(610, reason.ordinal + 1)(m"the TELP does not resolve because $reason")

  // A resolution outcome (§4): a single element, or one keyword's
  // ordered — possibly empty — occurrence sequence.
  enum Resolution:
    case One(element: Tel.Element)
    case Occurrences(elements: List[Tel.Element])

  // §3 grammar: the first character both selects and introduces the
  // delimiter; the remainder splits at every further occurrence. The
  // delimiter alone is the root path. Components are non-empty and may
  // not contain `LF` or `CR`.
  def parse(text: Text): Telp raises Telp.Error =
    val s = text.s
    if s.isEmpty then abort(Telp.Error(Error.Reason.Syntax, 0))
    val delimiter = s.charAt(0)
    if delimiters.s.indexOf(delimiter.toInt) < 0 then abort(Telp.Error(Error.Reason.Syntax, 0))

    if s.length == 1 then Root else
      val components = scala.collection.mutable.ListBuffer.empty[Text]
      var start = 1
      var i = 1

      while i <= s.length do
        if i == s.length || s.charAt(i) == delimiter then
          if i == start then abort(Telp.Error(Error.Reason.Syntax, components.length))
          val component = s.substring(start, i).nn
          if component.contains("\n") || component.contains("\r")
          then abort(Telp.Error(Error.Reason.Syntax, components.length))
          components += Text(component)
          start = i + 1

        i += 1

      Telp(List.from(components))

  given decodable: (tactic: Tactic[Telp.Error])
  =>  ((Telp is Decodable in Text)^{tactic}) = text => parse(text)

  // Renders under the first delimiter — `/`, then `.`, then the rest of
  // §3's set in order — that occurs in no component. A path whose
  // components exhaust all twenty-one delimiters is unaddressable (§8);
  // rendering falls back to `/` rather than failing.
  given encodable: Telp is Encodable in Text = path =>
    def free(delimiter: Char): Boolean = !path.components.stdlib.exists(_.s.indexOf(delimiter.toInt) >= 0)
    val candidates = t"/.".s + delimiters.s.filterNot { ch => ch == '/' || ch == '.' }
    val delimiter = Text(candidates).s.find(free(_)).getOrElse('/')
    Text(s"$delimiter${path.components.join(Text(delimiter.toString))}")

  // A member keyword's slot in the flat keyword order of a Struct: for
  // a Field, the field itself; for a SelectRef, one slot per variant of
  // the referenced SelectDefinition, keyed by the variant keyword.
  private case class Slot(flatIndex: Int, repeatable: Boolean, slotType: Tels.Type)

  private def definitionOf(reference: Text, schema: Tels): Optional[Tels.SelectDefinition] =
    schema.selects.readable.find(_.name == reference) match
      case scala.Some(definition) => definition
      case scala.None             => Unset

  private def resolved(t: Tels.Type, schema: Tels): Optional[Tels.Type] = t match
    case Tels.Reference(name) =>
      schema.records.readable.find(_.name == name) match
        case scala.Some(record) => Tels.Struct(record.members, record.validators)

        case scala.None => schema.scalars.readable.find(_.name == name) match
          case scala.Some(scalar) => Tels.Scalar(scalar.validators, scalar.encoding)

          case scala.None =>
            if name == Tels.Builtin.Flag then Tels.Flag
            else if name == Tels.Builtin.String || name == Tels.Builtin.Identifier
              || name == Tels.Builtin.TypeName || name == Tels.Builtin.Sigil
            then Tels.Scalar(Array.empty)
            else Unset

    case other => other

  private def widthOf(member: Tels.Member, schema: Tels): Int = member match
    case _: Tels.Field     => 1
    case _: Tels.Exclude   => 0
    case s: Tels.SelectRef => definitionOf(s.reference, schema).lay(0)(_.variants.length)

  // The slot a keyword-step component selects in `struct`, matched
  // against the keyword order: Field keywords and the variant keywords
  // of SelectRef-referenced SelectDefinitions alike (§4 step 1).
  private def slotOf(struct: Tels.Struct, component: Text, schema: Tels): Optional[Slot] =
    var flat = 0
    var result: Optional[Slot] = Unset
    var i = 0

    while i < struct.members.length && result.absent do
      struct.members.readUnchecked(i) match
        case f: Tels.Field =>
          if f.keyword == component
          then result = Slot(flat, f.repeatable == Tels.Polarity.Loose, f.fieldType)

        case s: Tels.SelectRef =>
          definitionOf(s.reference, schema).let: definition =>
            var v = 0

            while v < definition.variants.length && result.absent do
              val variant = definition.variants.readUnchecked(v)
              if variant.keyword == component
              then result = Slot(flat + v, s.repeatable == Tels.Polarity.Loose, variant.variantType)
              v += 1

        case _: Tels.Exclude => ()

      flat += widthOf(struct.members.readUnchecked(i), schema)
      i += 1

    result

  // The key value of an occurrence whose type is a key-carrying Struct
  // (§21.6): the semantic text of the Value filling its key field,
  // including a default-supplied value. `Unset` when the type has no
  // key field or the key field is unfilled.
  private def keyValueOf(element: Tel.Element, slotType: Tels.Type, schema: Tels)
  :   Optional[Text] =

    element match
      case node: Tel.Element.Node => resolved(slotType, schema) match
        case struct: Tels.Struct =>
          var keyFlat: Optional[Int] = Unset
          var flat = 0
          var i = 0

          while i < struct.members.length do
            struct.members.readUnchecked(i) match
              case f: Tels.Field => if f.key && keyFlat.absent then keyFlat = flat
              case _             => ()

            flat += widthOf(struct.members.readUnchecked(i), schema)
            i += 1

          keyFlat.let: keyIndex =>
            var value: Optional[Text] = Unset

            node.children.readable.foreach:
              case Tel.Element.Value(idx, _, text) =>
                if idx == keyIndex && value.absent then value = text

              case _ => ()

            value

        case _ => Unset

      case _ => Unset

  private def structKeyed(slotType: Tels.Type, schema: Tels): Boolean =
    resolved(slotType, schema) match
      case struct: Tels.Struct => struct.members.readable.exists:
        case f: Tels.Field => f.key
        case _             => false

      case _ => false

  private def allDigits(component: Text): Boolean =
    component.s.forall { ch => ch >= '0' && ch <= '9' }

  private def childrenAt(node: Tel.Element.Node, flatIndex: Int): List[Tel.Element] =
    List.from:
      node.children.readable.filter: element =>
        element match
          case Tel.Element.Node(idx, _, _)  => idx.or(-1) == flatIndex
          case Tel.Element.Value(idx, _, _) => idx == flatIndex

case class Telp(components: List[Text]) derives CanEqual:
  // Push a component onto the root (front) of the path. The product
  // derivation's outer `focus` blocks run *after* the inner ones
  // (contingency's try/finally order), so each level prepends its own
  // keyword to the root side, building a root-first descent like
  // `/person/name`.
  def prepend(component: Text): Telp = Telp(component :: components)

  def / (component: Text): Telp = Telp(components :+ component)

  // Resolution per telp.md §4, over the assigned semantic model: the
  // context element is a `Tel.Element` as produced by `Tel.Type.assign`
  // (defaults materialized, atom/compound realization erased), and
  // `schema` is the composed schema it was assigned under.
  def resolve(context: Tel.Element)(using schema: Tels): Telp.Resolution raises Telp.Error =
    // The pending state after a repeatable keyword: its occurrence
    // sequence and the keyword's declared type, awaiting a selector.
    var pendingType: Optional[Tels.Type] = Unset
    var pendingOccurrences: List[Tel.Element] = Nil
    var current: Tel.Element = context
    var i = 0

    components.stdlib.foreach: component =>
      pendingType.let: slotType =>
        // Selector step (§4 step 2). An all-digit component is always a
        // zero-based occurrence index (§5); otherwise the keyword's type
        // must be a key-carrying Struct, and the component selects the
        // first occurrence with that key value, code point for code point.
        if Telp.allDigits(component) then
          val index = component.s.toLong
          if index >= pendingOccurrences.stdlib.length
          then abort(Telp.Error(Telp.Error.Reason.IndexOutOfRange, i))
          current = pendingOccurrences.stdlib(index.toInt)
        else
          if !Telp.structKeyed(slotType, schema)
          then abort(Telp.Error(Telp.Error.Reason.TypeNotKeyed, i))

          current = pendingOccurrences.stdlib.find: occurrence =>
            Telp.keyValueOf(occurrence, slotType, schema).let(_ == component).or(false)
          . getOrElse(abort(Telp.Error(Telp.Error.Reason.KeyNotFound, i)))

        pendingType = Unset
        pendingOccurrences = Nil
      . or:
          // Keyword step (§4 step 1): `current` must be a Struct-typed
          // Node, and the component must match its keyword order.
          current match
            case node: Tel.Element.Node => Telp.resolved(node.elementType, schema) match
              case struct: Tels.Struct =>
                val slot = Telp.slotOf(struct, component, schema)
                . or(abort(Telp.Error(Telp.Error.Reason.UnknownKeyword, i)))

                val occurrences = Telp.childrenAt(node, slot.flatIndex)

                if slot.repeatable then
                  pendingType = slot.slotType
                  pendingOccurrences = occurrences
                else occurrences.stdlib.headOption match
                  case scala.Some(child) => current = child
                  case scala.None => abort(Telp.Error(Telp.Error.Reason.AbsentMember, i))

              case _ => abort(Telp.Error(Telp.Error.Reason.NonStructDescent, i))

            case _ => abort(Telp.Error(Telp.Error.Reason.NonStructDescent, i))

      i += 1

    if pendingType.present then Telp.Resolution.Occurrences(pendingOccurrences)
    else Telp.Resolution.One(current)

  // Convenience: type-assign `tel` under `schema` first, then resolve
  // against the resulting root.
  def resolve(tel: Tel)
    ( using schema: Tels )
    ( using Tactic[Telp.Error], Tactic[Tel.Error], Foci[Tel.Focus] )
  :   Telp.Resolution =

    resolve(Tel.Type.assign(tel, schema))
