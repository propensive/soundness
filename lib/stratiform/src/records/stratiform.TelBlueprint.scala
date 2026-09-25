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
package stratiform

import anticipation.*
import rudiments.*
import contingency.*
import gossamer.*
import polyvinyl.*
import prepositional.*
import vacuous.*

import strategies.throwUnsafely

// `stratiform.records` — a Polyvinyl Specification over a TEL schema.
// Each `TelBlueprint` instance binds a `Tels` schema to a structurally-
// typed `Record` whose accessor fields are derived from the schema's
// document struct, one field per Field member at the root.
//
// Usage:
//   object MyRecords extends TelBlueprint(myTels):
//     transparent inline def record(tel: Tel): Record = ${build('tel)}
//
//   val r = MyRecords.record(myTel)
//   r.name: Text     // for a `field name String` Scalar
//   r.`first-name`   // for a kebab-cased field
//
// The mapping from TEL validators to Scala types is provided by the
// `Intensional` typeclass instances in this file: "string",
// "identifier", "type-name" → `Text`; "sigil" → `Char`; "flag" →
// `Boolean`. Optional variants of each (suffixed with "?") yield
// `Optional[T]`.

object TelBlueprint:

  // A Tel returned from `access` is "absent" iff its wrapped Compound
  // has an empty keyword — `Tel.empty` is the sentinel returned when
  // a field is missing. Concrete present-but-no-value cases (e.g. a
  // Flag) have a non-empty keyword.
  private def absent(tel: Tel): Boolean = tel.keyword.s.isEmpty

  given string: ("string" is Intensional in TelBlueprint from Tel to Text) =
    TelBlueprint.intensional(_.primaryAtom)

  given optionalString: ("string?" is Intensional in TelBlueprint from Tel to Optional[Text]) =
    TelBlueprint.intensional: tel => if absent(tel) then Unset else tel.primaryAtom

  given identifier: ("identifier" is Intensional in TelBlueprint from Tel to Text) =
    TelBlueprint.intensional(_.primaryAtom)

  given optionalIdentifier
  :   ("identifier?" is Intensional in TelBlueprint from Tel to Optional[Text]) =

    TelBlueprint.intensional: tel => if absent(tel) then Unset else tel.primaryAtom

  given typeName: ("type-name" is Intensional in TelBlueprint from Tel to Text) =
    TelBlueprint.intensional(_.primaryAtom)

  given optionalTypeName
  :   ("type-name?" is Intensional in TelBlueprint from Tel to Optional[Text]) =

    TelBlueprint.intensional: tel => if absent(tel) then Unset else tel.primaryAtom

  given sigil: ("sigil" is Intensional in TelBlueprint from Tel to Char) =
    TelBlueprint.intensional: tel =>
      val text = tel.primaryAtom.s
      if text.length == 1 then text.charAt(0) else '#'

  given optionalSigil: ("sigil?" is Intensional in TelBlueprint from Tel to Optional[Char]) =
    TelBlueprint.intensional: tel =>
      if absent(tel) then Unset
      else
        val text = tel.primaryAtom.s
        if text.length == 1 then (text.charAt(0): Optional[Char]) else Unset

  // A Flag field is satisfied when the schema declares the keyword
  // *and* the parsed document contains a compound with that keyword.
  // Polyvinyl's accessor calls our `access` first, which returns
  // `Tel.empty` for an absent field; the Intensional below maps an
  // absent Tel to `false` and a present one to `true`.
  given flag: ("flag" is Intensional in TelBlueprint from Tel to Boolean) =
    TelBlueprint.intensional(!absent(_))

  given tel: ("tel" is Intensional in TelBlueprint from Tel to Tel) =
    TelBlueprint.intensional(identity)

  // Helper constructor for `Intensional` instances that ignore params.
  // A pure function (`->`): the instance retains it, and a capturing conversion would make
  // the typeclass instance itself a capability, which its pure self type (rightly) forbids.
  def intensional[name <: Label, value](accessor: Tel -> value)
  :   name is Intensional in TelBlueprint from Tel to value =

    new Intensional:
      type Self = name
      type Origin = Tel
      type Form = TelBlueprint
      type Result = value

      def access(tel: Tel): value = accessor(tel)
      def transform(tel: Tel, params: List[Text]): value = access(tel)

  // Build a polyvinyl Record from a Tel value and an accessor map.
  // Pure functions (`->`): the Record instance retains the accessor, and a capturing one would
  // make the record itself a capability, which polyvinyl's pure Record type (rightly) forbids.
  def record(data0: Tel, access0: Text -> Tel -> Any): Record = new Record:
    type Origin = Tel
    val data: Tel = data0
    def access: Text => Tel => Any = access0

  // Walk a Tels.Struct to produce the polyvinyl `Member` map. Each
  // Field at the top level contributes one entry whose `fieldType`
  // names the Intensional instance to look up. A member absent from
  // `base` — the struct as the schema's base declares it, before any
  // layer — was introduced by a layer, and is optional whatever the
  // layer declares: a document composed without that layer omits it.
  def fieldsOf
    ( struct: Tels.Struct, schema: Tels, base: Optional[Tels] = Unset,
      baseStruct: Optional[Tels.Struct] = Unset )
  :   List[(Text, Member)] =

    val declared: scala.collection.immutable.Set[Text] =
      baseStruct.lay(scala.collection.immutable.Set()): struct =>
        struct.members.readable.toList.collect { case field: Tels.Field => field.keyword }
        . to(scala.collection.immutable.Set)

    val builder = scala.collection.mutable.ListBuffer.empty[(Text, Member)]
    var i = 0

    while i < struct.members.length do
      struct.members.readable(i) match
        case f: Tels.Field =>
          val layered = base.present && !declared.contains(f.keyword)
          builder += f.keyword -> memberOf(f, schema, base, layered)

        case _ => ()

      i += 1

    builder.toList.to(List)

  // Map a single Tels.Field to its polyvinyl Member representation.
  // Scalar / Flag / Reference types are translated to a Value member
  // with the validator name (or built-in tag) used as the Intensional
  // lookup key. Optional fields (required = Loose) get the `?` suffix.
  //
  // Since §21.8 made `validate` optional, a scalar may be constrained by
  // patterns alone; it then has no validator name to key on and falls back to
  // `string`, which is right — a pattern narrows the accepted text but does not
  // change its Scala representation.
  private def memberOf(field: Tels.Field, schema: Tels, base: Optional[Tels], layered: Boolean)
  :   Member =

    val optional = layered || field.required == Tels.Polarity.Loose
    val suffix   = if optional then "?" else ""

    field.fieldType match
      case s: Tels.Scalar =>
        Member.Value(Text(s.validators.prim.or(t"string").s + suffix))

      case Tels.Flag =>
        Member.Value(t"flag")

      case Tels.Reference(name) =>
        schema.scalars.seek(_.name == name).lay:
          schema.records.seek(_.name == name).lay(Member.Value(t"tel")): rec =>
            // The record as the base declares it, if it does: a record a layer introduced
            // has every member optional, and one a layer refined has its additions optional.
            val baseRecord: Optional[Tels.Struct] = base.let: base =>
              base.records.seek(_.name == name)
              . let { record => Tels.Struct(record.members, record.validators) }
              . or(Tels.Struct(Array.empty, Array.empty))

            val members = Tels.Struct(rec.members, rec.validators)
            Member.Record(t"object", fieldsOf(members, schema, base, baseRecord))
        . apply: sc =>
          Member.Value(Text(sc.validators.prim.or(t"string").s + suffix))

      case _: Tels.Struct =>
        Member.Value(t"tel")

abstract class TelBlueprint(val tels: Tels) extends Specification:
  type Origin = Tel
  type Form = TelBlueprint

  // The schema's base — before any layer — and its full composition. A member the full
  // composition declares and the base does not was introduced by a layer, and reads as optional.
  private lazy val base: Tels = Tels.Layers.compose(tels, List())
  private lazy val composed: Tels = Tels.Layers.compose(tels)

  def fields: List[(Text, Member)] =
    TelBlueprint.fieldsOf(composed.document, composed, base, base.document)

  // The layers whose root members a document carries — the layers a value built from it was
  // composed with, for a writer serving an acceptance.
  def layersOf(tel: Tel): List[Text] =
    proscenium.List.from(tels.layers.readable.toList).filter: layer =>
      layer.overlay.members.readable.exists:
        case field: Tels.Field => tel.field(field.keyword).present
        case _                 => false
    . map(_.name)

  // The acceptance a reader of these records sends (BinTEL §8.4): the base alone is the
  // requirement, every layer is offered, and the base in self-contained mode is the fallback.
  def acceptance(lineage: SchemaSignature.Lineage)
    ( using Tactic[Bintel.Error], Tactic[Tels.Resolution.Error], Tactic[Tel.Acceptance.Error] )
  :   Tel.Acceptance =

    val components = lineage.layers.map { layer => Tel.Acceptance.Component(lineage.prefixOf(layer.hash)) }
    val requirement = Tel.Acceptance.Signature(lineage.signature(List()))

    Tel.Acceptance
      ( Tel.Acceptance.Alternative(requirement, components = components),
        Tel.Acceptance.Alternative(requirement, selfContained = true) )

  // A document received in reply to `acceptance`, presented for `record` with every layer the
  // writer included: resolved against the library, decoded under its composed schema. `Unset`
  // when no alternative's schema is a supertype of the document's.
  def receive(acceptance: Tel.Acceptance, library: SchemaSignature.Library, data: Data)
    ( using Tactic[Bintel.Error], Tactic[Tels.Resolution.Error] )
  :   Optional[Tel] =

    val framed = Bintel.unframe(data)

    Tel.Acceptance.served(acceptance, library, framed.signature).let: reading =>
      val element = Bintel.decode(framed.body, reading.document, Tel.Codec.Bindings.builtins)
      Bintel.present(element, reading.document)

  def access(name: Text, tel: Tel): Tel = tel.field(name).or(Tel.empty)

  def build(data: Tel, access: Text -> Tel -> Any): Record =
    TelBlueprint.record(data, access)
