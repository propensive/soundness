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
package stratiform


import anticipation.*
import contingency.*
import gossamer.*
import polyvinyl.*
import prepositional.*
import vacuous.*

import strategies.throwUnsafely

// `stratiform.records` — a Polyvinyl Specification over a TEL schema.
// Each `RecordSchema` instance binds a `Tels` schema to a structurally-
// typed `Record` whose accessor fields are derived from the schema's
// document struct, one field per Field member at the root.
//
// Usage:
//   object MyRecords extends RecordSchema(myTels):
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

object RecordSchema:

  // A Tel returned from `access` is "absent" iff its wrapped Compound
  // has an empty keyword — `Tel.empty` is the sentinel returned when
  // a field is missing. Concrete present-but-no-value cases (e.g. a
  // Flag) have a non-empty keyword.
  private def absent(tel: Tel): Boolean = tel.keyword.s.isEmpty

  given string: ("string" is Intensional in RecordSchema from Tel to Text) =
    RecordSchema.intensional(_.primaryAtom)

  given optionalString: ("string?" is Intensional in RecordSchema from Tel to Optional[Text]) =
    RecordSchema.intensional: tel =>
      if absent(tel) then Unset else tel.primaryAtom

  given identifier: ("identifier" is Intensional in RecordSchema from Tel to Text) =
    RecordSchema.intensional(_.primaryAtom)

  given optionalIdentifier
  :   ("identifier?" is Intensional in RecordSchema from Tel to Optional[Text]) =

    RecordSchema.intensional: tel =>
      if absent(tel) then Unset else tel.primaryAtom

  given typeName: ("type-name" is Intensional in RecordSchema from Tel to Text) =
    RecordSchema.intensional(_.primaryAtom)

  given optionalTypeName
  :   ("type-name?" is Intensional in RecordSchema from Tel to Optional[Text]) =

    RecordSchema.intensional: tel =>
      if absent(tel) then Unset else tel.primaryAtom

  given sigil: ("sigil" is Intensional in RecordSchema from Tel to Char) =
    RecordSchema.intensional: tel =>
      val text = tel.primaryAtom.s
      if text.length == 1 then text.charAt(0) else '#'

  given optionalSigil: ("sigil?" is Intensional in RecordSchema from Tel to Optional[Char]) =
    RecordSchema.intensional: tel =>
      if absent(tel) then Unset
      else
        val text = tel.primaryAtom.s
        if text.length == 1 then (text.charAt(0): Optional[Char]) else Unset

  // A Flag field is satisfied when the schema declares the keyword
  // *and* the parsed document contains a compound with that keyword.
  // Polyvinyl's accessor calls our `access` first, which returns
  // `Tel.empty` for an absent field; the Intensional below maps an
  // absent Tel to `false` and a present one to `true`.
  given flag: ("flag" is Intensional in RecordSchema from Tel to Boolean) =
    RecordSchema.intensional(tel => !absent(tel))

  given tel: ("tel" is Intensional in RecordSchema from Tel to Tel) =
    RecordSchema.intensional(identity)

  // Helper constructor for `Intensional` instances that ignore params.
  def intensional[name <: Label, value](accessor: Tel => value)
  :   name is Intensional in RecordSchema from Tel to value =

    new Intensional:
      type Self = name
      type Origin = Tel
      type Form = RecordSchema
      type Result = value

      def access(tel: Tel): value = accessor(tel)
      def transform(tel: Tel, params: List[Text]): value = access(tel)

  // Build a polyvinyl Record from a Tel value and an accessor map.
  def record(data0: Tel, access0: Text => Tel => Any): Record = new Record:
    type Origin = Tel
    val data: Tel = data0
    def access: Text => Tel => Any = access0

  // Walk a Tels.Struct to produce the polyvinyl `Member` map. Each
  // Field at the top level contributes one entry whose `fieldType`
  // names the Intensional instance to look up.
  def fieldsOf(struct: Tels.Struct, schema: Tels): Map[Text, Member] =
    val builder = scala.collection.mutable.LinkedHashMap.empty[Text, Member]
    var i = 0

    while i < struct.members.length do
      struct.members(i) match
        case f: Tels.Field => builder(f.keyword) = memberOf(f, schema)
        case _             => ()

      i += 1

    builder.toMap

  // Map a single Tels.Field to its polyvinyl Member representation.
  // Scalar / Flag / Reference types are translated to a Value member
  // with the validator name (or built-in tag) used as the Intensional
  // lookup key. Optional fields (required = Loose) get the `?` suffix.
  private def memberOf(field: Tels.Field, schema: Tels): Member =
    val optional = field.required == Tels.Polarity.Loose
    val suffix   = if optional then "?" else ""

    field.fieldType match
      case s: Tels.Scalar =>
        val validator = s.validators.headOption.getOrElse(t"string")
        Member.Value(Text(validator.s + suffix))

      case Tels.Flag =>
        Member.Value(t"flag")

      case Tels.Reference(name) =>
        schema.scalars.find(_.name == name) match
          case Some(sc) =>
            val validator = sc.validators.headOption.getOrElse(t"string")
            Member.Value(Text(validator.s + suffix))

          case None =>
            schema.records.find(_.name == name) match
              case Some(rec) =>
                Member.Record(t"object", fieldsOf(Tels.Struct(rec.members, rec.validators), schema))

              case None => Member.Value(t"tel")

      case _: Tels.Struct =>
        Member.Value(t"tel")

abstract class RecordSchema(val tels: Tels) extends Specification:
  type Origin = Tel
  type Form = RecordSchema

  def fields: Map[Text, Member] = RecordSchema.fieldsOf(tels.document, tels)

  def access(name: Text, tel: Tel): Tel = tel.field(name).or(Tel.empty)

  def build(data: Tel, access: Text => Tel => Any): Record =
    RecordSchema.record(data, access)
