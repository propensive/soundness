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

import scala.compiletime.*

import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*
import wisteria.*

// Phase-2 encode/decode typeclasses and Wisteria derivation. Mirrors
// jacinta.Json2 / DecodableDerivation / EncodableDerivation but produces
// TEL presentation structures instead of JSON ASTs.
//
// Encoding model: a scalar value (Text, Int, ...) produces a Tel wrapping
// a single Compound with one inline atom carrying the text. A product
// type (case class) produces a Tel wrapping a Compound whose children
// list contains one Compound per field, keyed by the field's label.
// Decoding inverts these mappings.

trait Tel2:
  inline given decodable: [value] => value is Decodable in Tel = summonFrom:
    case given (`value` is Decodable in Text) =>
      provide[Tactic[TelError]](_.primaryAtom.decode[value])

    case given Reflection[`value`] => DecodableDerivation.derived

  inline given encodable: [value] => value is Encodable in Tel = summonFrom:
    case given (`value` is Encodable in Text) =>
      v => Tel.scalar(v.encode)

    case given Reflection[`value`] => EncodableDerivation.derived

  object DecodableDerivation extends Derivable[Decodable in Tel]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Decodable in Tel =
      tel =>
        provide[Tactic[TelError]]:
          build: [field] =>
            ctx =>
              val match0 = tel.field(label)
              if match0.absent then default.or(ctx.decoded(Tel.empty))
              else ctx.decoded(match0.vouch)

    inline def disjunction[derivation: SumReflection]: derivation is Decodable in Tel =
      tel =>
        provide[Tactic[TelError]]:
          provide[Tactic[VariantError]]:
            val variantKeyword = tel.primaryAtom
            delegate(variantKeyword): [variant <: derivation] =>
              ctx => ctx.decoded(tel)

  object EncodableDerivation extends Derivable[Encodable in Tel]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Encodable in Tel = value =>
      val compounds = scala.collection.mutable.ArrayBuffer.empty[Tel.Compound]
      fields(value): [field] =>
        fieldValue =>
          val encoded = contextual.encode(fieldValue)
          encoded.subtree match
            case c: Tel.Compound =>
              compounds += c.copy(keyword = label)

            case d: Tel.Document =>
              compounds ++= d.children.flatMap(_.compounds)

      Tel.compound(Text(""), IArray.empty, IArray.from(compounds))

    inline def disjunction[derivation: SumReflection]: derivation is Encodable in Tel =
      value =>
        variant(value): [variant <: derivation] =>
          v => contextual.encode(v)

  // Primitive instances: Text/Int/Long/Double/Boolean as Compound + inline
  // atom. These mirror jacinta.Json's primitive decoders but go through
  // the atom text rather than a JSON AST.

  given textDecodable: Tactic[TelError] => Text is Decodable in Tel = _.primaryAtom

  given stringDecodable: Tactic[TelError] => String is Decodable in Tel =
    _.primaryAtom.s

  given intDecodable: Tactic[TelError] => Int is Decodable in Tel = tel =>
    try tel.primaryAtom.s.toInt
    catch case _: NumberFormatException => abort(TelError(TelError.Reason.BadVersion))

  given longDecodable: Tactic[TelError] => Long is Decodable in Tel = tel =>
    try tel.primaryAtom.s.toLong
    catch case _: NumberFormatException => abort(TelError(TelError.Reason.BadVersion))

  given doubleDecodable: Tactic[TelError] => Double is Decodable in Tel = tel =>
    try tel.primaryAtom.s.toDouble
    catch case _: NumberFormatException => abort(TelError(TelError.Reason.BadVersion))

  given booleanDecodable: Tactic[TelError] => Boolean is Decodable in Tel = tel =>
    tel.primaryAtom.s match
      case "true"  => true
      case "false" => false
      case _       => abort(TelError(TelError.Reason.BadVersion))

  given telDecodable: Tel is Decodable in Tel = identity(_)

  given textEncodable: Text is Encodable in Tel = text => Tel.scalar(text)
  given stringEncodable: String is Encodable in Tel = s => Tel.scalar(Text(s))
  given intEncodable: Int is Encodable in Tel = i => Tel.scalar(Text(i.toString))
  given longEncodable: Long is Encodable in Tel = l => Tel.scalar(Text(l.toString))
  given doubleEncodable: Double is Encodable in Tel = d => Tel.scalar(Text(d.toString))
  given booleanEncodable: Boolean is Encodable in Tel = b => Tel.scalar(Text(b.toString))
  given telEncodable: Tel is Encodable in Tel = identity(_)

  // Optional / List support — repeatable scalar fields produce multiple
  // compounds with the same keyword; we return a Document-rooted Tel
  // containing the list elements as siblings, which the product encoder
  // recognises and flattens with the field's label.

  given optionalEncodable: [value: Encodable in Tel] => Optional[value] is Encodable in Tel =
    opt => opt.lay(Tel.empty)(v => v.encode)

  given optionalDecodable: [value: Decodable in Tel] => Tactic[TelError]
  =>  Optional[value] is Decodable in Tel = tel =>
    if tel.childCompounds.isEmpty && tel.atomTexts.isEmpty then Unset else value.decoded(tel)

  // Helpers used by encoders to construct Tel values.

  def scalar(text: Text): Tel =
    Tel(Tel.Compound(Text(""), IArray(Tel.Atom.Inline(text, 1)), Unset, IArray.empty))

  def compound
       (keyword: Text, atoms: IArray[Tel.Atom], compounds: IArray[Tel.Compound])
  :     Tel =
    val children =
      if compounds.isEmpty then IArray.empty[Tel.Block]
      else IArray(Tel.Block(IArray.empty, Unset, compounds, 0))

    Tel(Tel.Compound(keyword, atoms, Unset, children))

  def empty: Tel = Tel(Tel.Compound(Text(""), IArray.empty, Unset, IArray.empty))

// `value.encode` (provided by the Encodable typeclass extension defined in
// anticipation) is the idiomatic call site producing a Tel from any
// encodable value. A `.tel` alias may be added later for symmetry with
// jacinta's `.json`.
