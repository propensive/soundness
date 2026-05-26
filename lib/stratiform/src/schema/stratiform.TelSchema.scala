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

// Schema data model per §20 of the TEL specification. The data is a
// straightforward translation of the TypeScript interfaces given in the
// spec; behavioural code (type assignment, layer merging, validators)
// lives in companion modules.
//
// Notes on naming:
// - `Schema.name` and `Layer.name` are kebab-case identifiers carried
//   as Text values; conformance to the kebab-case grammar of §20.7 is
//   enforced by validators at parse time, not by the type.
// - Definition names (record / scalar / select) are PascalCase
//   TypeName identifiers, also Text at the data level.

case class TelSchema
   ( name:     Text,
     document: TelSchema.Struct,
     layers:   IArray[TelSchema.Layer],
     sigil:    Optional[Char],
     records:  IArray[TelSchema.RecordDefinition],
     scalars:  IArray[TelSchema.ScalarDefinition],
     selects:  IArray[TelSchema.SelectDefinition] )

object TelSchema:

  // Per-axis polarity tristate from §20: "default" means no flag was
  // declared, "loose" means a loosening flag (optional / repeatable)
  // was declared, "tight" means a tightening flag (required /
  // irrepeatable) was declared. Effective booleans are derived as
  //   required   = (member.required   != "loose")
  //   repeatable = (member.repeatable == "loose")
  enum Polarity:
    case Implicit, Loose, Tight

  // A schema's member sequence is a list of Member kinds. Field carries
  // its keyword and type at the use site; SelectRef references a named
  // SelectDefinition; Exclude is a layer-only operation that removes a
  // variant from the merged SelectDefinition.
  sealed trait Member

  case class Field
     ( required:   Polarity,
       repeatable: Polarity,
       keyword:    Text,
       fieldType:  Type,
       default:    Optional[Text] )
  extends Member

  case class SelectRef
     ( required:   Polarity,
       repeatable: Polarity,
       reference:  Text )
  extends Member

  case class Exclude(keyword: Text) extends Member

  // A Variant of a SelectDefinition: a kebab-case keyword paired with
  // any Type.
  case class Variant(keyword: Text, variantType: Type)

  // The four kinds of Type per §20:
  //   - Struct: an ordered Member list plus struct-level validators
  //   - Scalar: zero-or-more validators applied to the atom text
  //   - Flag:   value-less; identity from keyword alone
  //   - Reference: indirect to a named Definition by TypeName
  sealed trait Type

  case class Struct(members: IArray[Member], validators: IArray[Text]) extends Type

  case class Scalar(validators: IArray[Text]) extends Type

  case object Flag extends Type

  case class Reference(name: Text) extends Type

  // Definitions in the schema's namespace. They share a single namespace
  // (§20: E211 for cross-kind collisions). Each Definition optionally
  // carries validators applied to the entire instance.
  case class RecordDefinition
     ( name:       Text,
       members:    IArray[Member],
       validators: IArray[Text] )

  case class ScalarDefinition(name: Text, validators: IArray[Text])

  case class SelectDefinition
     ( name:       Text,
       variants:   IArray[Variant],
       validators: IArray[Text] )

  // A Layer applies incremental refinements per §20.3. `overlay` is the
  // (possibly empty) Struct merged into the document root; the three
  // Definition lists are merged into the composed namespace.
  case class Layer
     ( name:    Text,
       overlay: Struct,
       records: IArray[RecordDefinition],
       scalars: IArray[ScalarDefinition],
       selects: IArray[SelectDefinition] )

  // Predefined built-in type names per §20.5 / §21.5. Used by the
  // schema-of-schemas and any user schema that references them via
  // `Reference(TypeName)`.
  object Builtin:
    val String:     Text = Text("String")
    val Identifier: Text = Text("Identifier")
    val TypeName:   Text = Text("TypeName")
    val Sigil:      Text = Text("Sigil")
    val Flag:       Text = Text("Flag")
