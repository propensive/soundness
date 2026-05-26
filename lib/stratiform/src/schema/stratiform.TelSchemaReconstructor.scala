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
import rudiments.*
import vacuous.*

import TelError.Reason
import TelSchema.*

// Inverse of the §20.5 schema-of-schemas: given a Tel.Document whose
// surface matches the canonical tel-schema vocabulary (record / scalar /
// select / document / layer at the top level, with the per-Definition
// bodies described in §20.5), reconstruct a TelSchema value. The §20.5
// self-consistency test parses the canonical document with the axiom
// then reconstructs and asserts structural equality with the axiom.

object TelSchemaReconstructor:

  // Deep structural equality for TelSchema values. The case classes
  // ship with a default `equals` that compares IArray fields by Array
  // identity rather than by element-wise content; this method recurses
  // and compares IArrays via `sameElements`, giving the structural
  // equivalence expected by the §20.5 self-consistency test.
  def equivalent(a: TelSchema, b: TelSchema): Boolean =
    a.name == b.name
      && a.sigil == b.sigil
      && structEq(a.document, b.document)
      && seqEq(a.records, b.records, recordEq)
      && seqEq(a.scalars, b.scalars, scalarEq)
      && seqEq(a.selects, b.selects, selectEq)
      && seqEq(a.layers,  b.layers,  layerEq)

  private def seqEq[T](a: IArray[T], b: IArray[T], eq: (T, T) => Boolean): Boolean =
    a.length == b.length && (0 until a.length).forall(i => eq(a(i), b(i)))

  private def structEq(a: Struct, b: Struct): Boolean =
    seqEq(a.members, b.members, memberEq)
      && seqEq(a.validators, b.validators, textEq)

  private def textEq(a: Text, b: Text): Boolean = a == b

  private def memberEq(a: Member, b: Member): Boolean = (a, b) match
    case (a: Field, b: Field) =>
      a.required == b.required && a.repeatable == b.repeatable
        && a.keyword == b.keyword && typeEq(a.fieldType, b.fieldType)
        && a.default == b.default

    case (a: SelectRef, b: SelectRef) =>
      a.required == b.required && a.repeatable == b.repeatable && a.reference == b.reference

    case (a: Exclude, b: Exclude) => a.keyword == b.keyword
    case _                        => false

  private def typeEq(a: Type, b: Type): Boolean = (a, b) match
    case (a: Struct, b: Struct)        => structEq(a, b)
    case (a: Scalar, b: Scalar)        => seqEq(a.validators, b.validators, textEq)
    case (Flag, Flag)                  => true
    case (Reference(n1), Reference(n2)) => n1 == n2
    case _                              => false

  private def recordEq(a: RecordDefinition, b: RecordDefinition): Boolean =
    a.name == b.name && seqEq(a.members, b.members, memberEq)
      && seqEq(a.validators, b.validators, textEq)

  private def scalarEq(a: ScalarDefinition, b: ScalarDefinition): Boolean =
    a.name == b.name && seqEq(a.validators, b.validators, textEq)

  private def selectEq(a: SelectDefinition, b: SelectDefinition): Boolean =
    a.name == b.name
      && seqEq(a.variants, b.variants, (x, y) => x.keyword == y.keyword
           && typeEq(x.variantType, y.variantType))
      && seqEq(a.validators, b.validators, textEq)

  private def layerEq(a: Layer, b: Layer): Boolean =
    a.name == b.name && structEq(a.overlay, b.overlay)
      && seqEq(a.records, b.records, recordEq)
      && seqEq(a.scalars, b.scalars, scalarEq)
      && seqEq(a.selects, b.selects, selectEq)

  def fromDocument(doc: Tel.Document): TelSchema raises TelError =
    val compounds: IArray[Tel.Compound] = doc.children.flatMap(_.compounds)

    var name: Optional[Text] = Unset
    var sigil: Optional[Char] = Unset
    var documentStruct: Optional[Struct] = Unset
    val records  = scala.collection.mutable.ArrayBuffer.empty[RecordDefinition]
    val scalars  = scala.collection.mutable.ArrayBuffer.empty[ScalarDefinition]
    val selects  = scala.collection.mutable.ArrayBuffer.empty[SelectDefinition]
    val layers   = scala.collection.mutable.ArrayBuffer.empty[Layer]

    var i = 0
    while i < compounds.length do
      val c = compounds(i)
      c.keyword.s match
        case "name"     => name = firstAtomText(c)
        case "sigil"    =>
          val s = firstAtomText(c)
          sigil = if s.absent then Unset else Optional(s.vouch.s.charAt(0))

        case "record"   => records  += parseRecord(c)
        case "scalar"   => scalars  += parseScalar(c)
        case "select"   => selects  += parseSelect(c)
        case "document" => documentStruct = parseBody(c)
        case "layer"    => layers   += parseLayer(c)
        case _          => abort(TelError(Reason.UnknownKeyword))

      i += 1

    // §20.4: the built-in scalars (Identifier, TypeName, Sigil, String)
    // are part of every schema's namespace but are not declared in the
    // surface syntax. The reconstructor injects them so the result
    // matches a hand-encoded axiom that includes them explicitly.
    val builtinScalars = IArray
     ( ScalarDefinition(Text("Identifier"), IArray(Text("identifier"))),
       ScalarDefinition(Text("TypeName"),   IArray(Text("type-name"))),
       ScalarDefinition(Text("Sigil"),      IArray(Text("sigil"))),
       ScalarDefinition(Text("String"),     IArray(Text("string"))) )

    TelSchema
     ( name     = name.or(abort(TelError(Reason.RequiredMemberAbsent))),
       document = documentStruct.or(abort(TelError(Reason.RequiredMemberAbsent))),
       layers   = IArray.from(layers),
       sigil    = sigil,
       records  = IArray.from(records),
       scalars  = builtinScalars ++ IArray.from(scalars),
       selects  = IArray.from(selects) )

  // Inline atoms only — the canonical never uses source/literal atoms at
  // schema positions, so we pull from the Inline atom list.
  private def firstAtomText(c: Tel.Compound): Optional[Text] =
    val texts = c.atoms.collect { case Tel.Atom.Inline(t, _) => t }
    if texts.isEmpty then Unset else texts(0): Optional[Text]

  private def atomTexts(c: Tel.Compound): IArray[Text] =
    c.atoms.collect { case Tel.Atom.Inline(t, _) => t }

  private def childCompounds(c: Tel.Compound): IArray[Tel.Compound] =
    c.children.flatMap(_.compounds)

  // Translate a type-name atom to its Type. The literal "Flag" is the
  // Flag case object; anything else is a Reference. Struct and Scalar
  // types never appear inline in the canonical — they're indirect via
  // Definition references.
  private def parseType(name: Text): Type =
    if name == Text("Flag") then Flag else Reference(name)

  // `record Foo` body: a sequence of Member declarations and optional
  // `validate <name>` validators.
  private def parseRecord(c: Tel.Compound): RecordDefinition raises TelError =
    val recName = firstAtomText(c).or(abort(TelError(Reason.RequiredMemberAbsent)))
    val (members, validators) = parseMembersAndValidators(childCompounds(c))
    RecordDefinition(recName, members, validators)

  // `scalar Foo` body: each child compound is a `validate <name>`.
  private def parseScalar(c: Tel.Compound): ScalarDefinition raises TelError =
    val scName = firstAtomText(c).or(abort(TelError(Reason.RequiredMemberAbsent)))
    val validators = childCompounds(c).flatMap: cc =>
      if cc.keyword == Text("validate") then atomTexts(cc) else IArray.empty[Text]

    ScalarDefinition(scName, validators)

  // `select Foo` body: variant / exclude / validate children.
  private def parseSelect(c: Tel.Compound): SelectDefinition raises TelError =
    val seName = firstAtomText(c).or(abort(TelError(Reason.RequiredMemberAbsent)))
    val variants   = scala.collection.mutable.ArrayBuffer.empty[Variant]
    val validators = scala.collection.mutable.ArrayBuffer.empty[Text]
    childCompounds(c).each: cc =>
      cc.keyword.s match
        case "variant"  =>
          val ats = atomTexts(cc)
          if ats.length < 2 then abort(TelError(Reason.RequiredMemberAbsent))
          variants += Variant(ats(0), parseType(ats(1)))

        case "validate" => validators ++= atomTexts(cc)
        case "exclude"  => ()
        case _          => abort(TelError(Reason.UnknownKeyword))

    SelectDefinition(seName, IArray.from(variants), IArray.from(validators))

  // The `document` block contains member declarations (same vocabulary
  // as a record body), not a name atom. It reconstructs into the
  // schema's document Struct.
  private def parseBody(c: Tel.Compound): Optional[Struct] raises TelError =
    val (members, validators) = parseMembersAndValidators(childCompounds(c))
    Optional(Struct(members, validators))

  // `layer Foo` body: records, scalars, selects, and an optional overlay.
  private def parseLayer(c: Tel.Compound): Layer raises TelError =
    val lyName = firstAtomText(c).or(abort(TelError(Reason.RequiredMemberAbsent)))
    val recs = scala.collection.mutable.ArrayBuffer.empty[RecordDefinition]
    val scs  = scala.collection.mutable.ArrayBuffer.empty[ScalarDefinition]
    val sels = scala.collection.mutable.ArrayBuffer.empty[SelectDefinition]
    var overlay: Optional[Struct] = Struct(IArray.empty, IArray.empty)

    childCompounds(c).each: cc =>
      cc.keyword.s match
        case "record"  => recs += parseRecord(cc)
        case "scalar"  => scs  += parseScalar(cc)
        case "select"  => sels += parseSelect(cc)
        case "overlay" => overlay = parseBody(cc)
        case _         => abort(TelError(Reason.UnknownKeyword))

    Layer
     ( name    = lyName,
       overlay = overlay.or(Struct(IArray.empty, IArray.empty)),
       records = IArray.from(recs),
       scalars = IArray.from(scs),
       selects = IArray.from(sels) )

  // Member declarations: `field kw Type [...flags]`, `select Ref
  // [...flags]`, or `validate <name>`.
  private def parseMembersAndValidators(compounds: IArray[Tel.Compound])
  :     (IArray[Member], IArray[Text]) raises TelError =
    val members    = scala.collection.mutable.ArrayBuffer.empty[Member]
    val validators = scala.collection.mutable.ArrayBuffer.empty[Text]

    compounds.each: cc =>
      cc.keyword.s match
        case "field"    => members += parseField(cc)
        case "select"   => members += parseSelectRef(cc)
        case "validate" => validators ++= atomTexts(cc)
        case "exclude"  =>
          val ats = atomTexts(cc)
          if ats.length >= 1 then members += Exclude(ats(0))

        case _          => abort(TelError(Reason.UnknownKeyword))

    (IArray.from(members), IArray.from(validators))

  // A `field` declaration: keyword + type atoms followed by polarity
  // flags and an optional `default` keyword introducing a default text.
  // The polarity flags are mapped to the per-axis Polarity enum.
  private def parseField(c: Tel.Compound): Field raises TelError =
    val ats = atomTexts(c)
    if ats.length < 2 then abort(TelError(Reason.RequiredMemberAbsent))
    val keyword = ats(0)
    val fieldType = parseType(ats(1))

    var required:   Polarity = Polarity.Implicit
    var repeatable: Polarity = Polarity.Implicit
    var default:    Optional[Text] = Unset

    var j = 2
    while j < ats.length do
      ats(j).s match
        case "optional"     => required   = Polarity.Loose
        case "required"     => required   = Polarity.Tight
        case "repeatable"   => repeatable = Polarity.Loose
        case "irrepeatable" => repeatable = Polarity.Tight
        case "default"      =>
          if j + 1 < ats.length then
            default = ats(j + 1): Optional[Text]
            j += 1

        case _              => ()

      j += 1

    Field(required, repeatable, keyword, fieldType, default)

  // A `select` member: SelectRef referencing a named SelectDefinition.
  // Polarity flags follow the reference name.
  private def parseSelectRef(c: Tel.Compound): SelectRef raises TelError =
    val ats = atomTexts(c)
    if ats.length < 1 then abort(TelError(Reason.RequiredMemberAbsent))
    val reference = ats(0)

    var required:   Polarity = Polarity.Implicit
    var repeatable: Polarity = Polarity.Implicit

    var j = 1
    while j < ats.length do
      ats(j).s match
        case "optional"     => required   = Polarity.Loose
        case "required"     => required   = Polarity.Tight
        case "repeatable"   => repeatable = Polarity.Loose
        case "irrepeatable" => repeatable = Polarity.Tight
        case _              => ()

      j += 1

    SelectRef(required, repeatable, reference)
