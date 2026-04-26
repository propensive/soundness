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
package cellulose

import anticipation.*
import contingency.*
import distillate.*
import prepositional.*
import proscenium.*
import rudiments.*
import vacuous.*
import wisteria.*

trait protointernal:
  object EncodableDerivation extends ProductDerivable[Encodable in Codl]:
    inline def conjunction[derivation <: Product: ProductReflection]: derivation is Encodable in Codl =
      val mapping: Map[Text, Text] = compiletime.summonFrom:
        case relabelling: CodlRelabelling[derivation] => relabelling.relabelling()
        case _                                        => Map()

      product => Codl:
        List:
          val schemata: IArray[CodlSchema.Entry] =
            CodlSchematicDerivation.conjunction[derivation].schema().absolve match
              case Struct(elements, _) => IArray.from(elements)

          Codllike:
            IArray.from:
              fields(product):
                [field] => field =>
                  val label2 = mapping.at(label).or(label)
                  val schematic = infer[field is CodlSchematic]

                  contextual.encoded(field).list.map: value =>
                    CodlNode(Atom(label2, value.children, Layout.empty, schemata(index).schema))

                  . filter(!_.nil)

              . to(List).flatten

  class DecodableDerivation()(using Tactic[CodlError]) extends ProductDerivable[Decodable in Codl]:
    inline def conjunction[derivation <: Product: ProductReflection]: derivation is Decodable in Codl =
      values =>
        build: [field] => context =>
          val label2 = compiletime.summonFrom:
            case relabelling: CodlRelabelling[derivation] => relabelling(label).or(label)
            case _                                        => label

          context.decoded:
            Codl:
              values.list.prim.let: value =>
                value.absolve match
                  case doc: CodlDoc => doc.get(label2)
                  case data: Atom   => data.get(label2)

              . lest(CodlError(CodlError.Reason.BadFormat(label2)))
