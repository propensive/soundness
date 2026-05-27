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
import vacuous.*

import Tels.*
import Tels.Polarity.*

// Hand-encoded `tel-schema` axiom per §20.5 of the TEL specification.
// This Scala literal MIRRORS the canonical `tel-schema.tel` document
// (saved at `res/test/stratiform/corpus/tel-schema.tel`) verbatim.
//
// AUTHORITATIVE: this is the source-of-truth schema-of-schemas for the
// stratiform parser. The self-consistency property — parsing the
// canonical document under this axiom and reconstructing a Tels
// value equal by structural equality — is the phase-3 merge blocker.

object TelsAxiom:

  private inline def kebab(s: String): Text = Text(s)

  private inline def field
       ( keyword:    String,
         fieldType:  Type,
         required:   Polarity = Implicit,
         repeatable: Polarity = Implicit,
         default:    Optional[Text] = Unset )
  :     Field =
    Field(required, repeatable, kebab(keyword), fieldType, default)

  private inline def selectRef
       ( reference:  String,
         required:   Polarity = Implicit,
         repeatable: Polarity = Implicit )
  :     SelectRef =
    SelectRef(required, repeatable, kebab(reference))

  private inline def variant(keyword: String, variantType: Type): Variant =
    Variant(kebab(keyword), variantType)

  private inline def record(name: String, members: Member*): RecordDefinition =
    RecordDefinition(kebab(name), IArray.from(members), IArray.empty)

  private inline def scalar(name: String, validators: String*): ScalarDefinition =
    ScalarDefinition(kebab(name), IArray.from(validators.map(kebab)))

  private inline def select(name: String, variants: Variant*): SelectDefinition =
    SelectDefinition(kebab(name), IArray.from(variants), IArray.empty)

  // Built-in scalar types referenced from member declarations.
  private val identifierRef: Type = Reference(kebab("Identifier"))
  private val typeNameRef:   Type = Reference(kebab("TypeName"))
  private val sigilRef:      Type = Reference(kebab("Sigil"))
  private val stringRef:     Type = Reference(kebab("String"))

  // The schema's root struct, mirroring the `document` block in the
  // canonical tel-schema.tel.
  private val documentStruct: Struct = Struct(
    members = IArray(
      field("name",     identifierRef),
      field("sigil",    sigilRef,                              required = Loose),
      field("record",   Reference(kebab("Record")), required = Loose, repeatable = Loose),
      field("scalar",   Reference(kebab("Scalar")), required = Loose, repeatable = Loose),
      field("select",   Reference(kebab("Select")), required = Loose, repeatable = Loose),
      field("document", Reference(kebab("Body"))),
      field("layer",    Reference(kebab("Layer")),  required = Loose, repeatable = Loose)),
    validators = IArray.empty)

  val tels: Tels = Tels(
    name     = kebab("tel-schema"),
    document = documentStruct,
    layers   = IArray.empty,
    sigil    = Unset,
    records  = IArray(
      record("Field",
        field("keyword",      identifierRef),
        field("type",         typeNameRef),
        field("optional",     Flag,       required = Loose),
        field("required",     Flag,       required = Loose),
        field("repeatable",   Flag,       required = Loose),
        field("irrepeatable", Flag,       required = Loose),
        field("default",      stringRef,  required = Loose)),

      record("SelectRef",
        field("reference",    typeNameRef),
        field("optional",     Flag, required = Loose),
        field("required",     Flag, required = Loose),
        field("repeatable",   Flag, required = Loose),
        field("irrepeatable", Flag, required = Loose)),

      record("Variant",
        field("keyword", identifierRef),
        field("type",    typeNameRef)),

      record("Record",
        field("name", typeNameRef),
        selectRef("Member", required = Loose, repeatable = Loose)),

      record("Scalar",
        field("name",     typeNameRef),
        field("validate", identifierRef, repeatable = Loose)),

      record("Select",
        field("name", typeNameRef),
        selectRef("SelectChild", repeatable = Loose)),

      record("Body",
        selectRef("Member", required = Loose, repeatable = Loose)),

      record("Layer",
        field("name",    identifierRef),
        field("record",  Reference(kebab("Record")), required = Loose, repeatable = Loose),
        field("scalar",  Reference(kebab("Scalar")), required = Loose, repeatable = Loose),
        field("select",  Reference(kebab("Select")), required = Loose, repeatable = Loose),
        field("overlay", Reference(kebab("Body")),   required = Loose))),
    scalars  = IArray(
      scalar("Identifier", "identifier"),
      scalar("TypeName",   "type-name"),
      scalar("Sigil",      "sigil"),
      scalar("String",     "string")),
    selects  = IArray(
      select("Member",
        variant("field",    Reference(kebab("Field"))),
        variant("select",   Reference(kebab("SelectRef"))),
        variant("validate", identifierRef)),
      select("SelectChild",
        variant("variant",  Reference(kebab("Variant"))),
        variant("exclude",  identifierRef),
        variant("validate", identifierRef))))
