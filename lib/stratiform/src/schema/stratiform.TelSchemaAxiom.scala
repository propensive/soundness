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

import TelSchema.*
import TelSchema.Polarity.*

// Hand-encoded `tel-schema` axiom per §20.5 of the TEL specification.
// This is the bootstrap: a TelSchema value describing the structure of
// schema documents themselves. When the runtime parses a `.tel-schema`
// document, the document is type-assigned against this axiom and the
// result is constructed into a TelSchema runtime value.
//
// IMPORTANT: this axiom is the AUTHORITATIVE definition of the schema
// language for stratiform. The self-consistency property — parsing the
// canonical `tel-schema.tel` document and comparing against this Scala
// literal — is the merge blocker for phase 3.
//
// Phase-3.x scope: this commit lays down the foundation (the keyword
// vocabulary at the top level). Construction of the full vocabulary
// (the recursive shape of `record`, `scalar`, `select`, `field`,
// `variant`, `layer`, ...) is incremental and tracked in the schema
// integration tests as they expand.

object TelSchemaAxiom:

  // Quick-reference factories that keep the literal compact below.
  private inline def field
       ( required:   Polarity,
         repeatable: Polarity,
         keyword:    String,
         fieldType:  Type,
         default:    Optional[Text] = Unset )
  :     Field =
    Field(required, repeatable, Text(keyword), fieldType, default)

  private inline def selectRef(required: Polarity, repeatable: Polarity, name: String)
  :     SelectRef =
    SelectRef(required, repeatable, Text(name))

  private inline def variant(keyword: String, variantType: Type): Variant =
    Variant(Text(keyword), variantType)

  private inline def record(name: String, members: Member*): RecordDefinition =
    RecordDefinition(Text(name), IArray.from(members), IArray.empty)

  private inline def scalar(name: String, validators: String*): ScalarDefinition =
    ScalarDefinition(Text(name), IArray.from(validators.map(Text(_))))

  private inline def select(name: String, variants: Variant*): SelectDefinition =
    SelectDefinition(Text(name), IArray.from(variants), IArray.empty)

  private val flagType:       Type = Flag
  private val identifierRef:  Type = Scalar(IArray(Text("identifier")))
  private val typeNameRef:    Type = Scalar(IArray(Text("type-name")))
  private val sigilRef:       Type = Scalar(IArray(Text("sigil")))
  private val stringRef:      Type = Scalar(IArray(Text("string")))

  // The schema's root document is a Struct whose members are the
  // top-level keywords used to write a schema document. The full
  // recursive shape (record body, layer body, select body, field
  // body, etc.) is captured by the Definitions below.
  private val documentStruct: Struct = Struct(
    members = IArray(
      field(Implicit, Implicit, "name",     identifierRef),
      field(Implicit, Implicit, "sigil",    sigilRef,           Unset),
      field(Implicit, Implicit, "document", Reference(Text("Struct"))),
      field(Loose,   Loose,   "layer",    Reference(Text("Layer"))),
      field(Loose,   Loose,   "record",   Reference(Text("RecordDef"))),
      field(Loose,   Loose,   "scalar",   Reference(Text("ScalarDef"))),
      field(Loose,   Loose,   "select",   Reference(Text("SelectDef")))),
    validators = IArray.empty)

  val telSchema: TelSchema = TelSchema(
    name     = Text("tel-schema"),
    document = documentStruct,
    layers   = IArray.empty,
    sigil    = Unset,
    records  = IArray(
      record("Struct",
        field(Loose, Loose, "field",  Reference(Text("Field"))),
        field(Loose, Loose, "select", Reference(Text("SelectDef"))),
        field(Loose, Loose, "validate", identifierRef)),

      record("Field",
        field(Implicit, Implicit, "keyword", identifierRef),
        field(Implicit, Implicit, "type",    typeNameRef),
        field(Loose,   Implicit, "optional",     flagType),
        field(Loose,   Implicit, "required",     flagType),
        field(Loose,   Implicit, "repeatable",   flagType),
        field(Loose,   Implicit, "irrepeatable", flagType),
        field(Loose,   Implicit, "default",      stringRef)),

      record("Layer",
        field(Implicit, Implicit, "name",    identifierRef),
        field(Loose,   Implicit, "overlay", Reference(Text("Struct"))),
        field(Loose,   Loose,   "record",  Reference(Text("RecordDef"))),
        field(Loose,   Loose,   "scalar",  Reference(Text("ScalarDef"))),
        field(Loose,   Loose,   "select",  Reference(Text("SelectDef")))),

      record("RecordDef",
        field(Implicit, Implicit, "name",     typeNameRef),
        field(Loose,   Loose,   "field",    Reference(Text("Field"))),
        field(Loose,   Loose,   "select",   Reference(Text("SelectDef"))),
        field(Loose,   Loose,   "validate", identifierRef)),

      record("ScalarDef",
        field(Implicit, Implicit, "name",     typeNameRef),
        field(Loose,   Loose,   "validate", identifierRef)),

      record("SelectDef",
        field(Implicit, Implicit, "name",     typeNameRef),
        field(Loose,   Loose,   "variant",  Reference(Text("Variant"))),
        field(Loose,   Loose,   "exclude",  identifierRef),
        field(Loose,   Loose,   "validate", identifierRef)),

      record("Variant",
        field(Implicit, Implicit, "keyword", identifierRef),
        field(Implicit, Implicit, "type",    typeNameRef))),
    scalars  = IArray(
      scalar("Identifier", "identifier"),
      scalar("TypeName",   "type-name"),
      scalar("Sigil",      "sigil"),
      scalar("String",     "string")),
    selects  = IArray.empty)
