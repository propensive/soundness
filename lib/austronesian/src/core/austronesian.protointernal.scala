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
package austronesian

import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import prepositional.*
import wisteria.*

object protointernal:
  object EncodableDerivation extends Derivation[[entity] =>> entity is Encodable in Pojo]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Encodable in _root_.austronesian.internal.Pojo =

      fields(_): [field] => contextual.encoded(_)
      . asInstanceOf[Pojo]


    inline def disjunction[derivation: SumReflection]: derivation is Encodable in Pojo = sum =>
      variant(sum): [variant <: derivation] => value =>
        Array.build[Pojo](2): array =>
          array(0) = label.s.asInstanceOf[Pojo]
          array(1) = contextual.encoded(value)

        . asInstanceOf[Pojo]

  object DecodableDerivation extends Derivable[Decodable in Pojo]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Decodable in Pojo =

      case array: scala.Array[Pojo @unchecked] =>
        provide[Tactic[PojoError]]:
          build[derivation]: [field] =>
            _.decoded(array(index))

      case other =>
        provide[Tactic[PojoError]](abort(PojoError()))


    inline def disjunction[derivation: SumReflection]: derivation is Decodable in Pojo =
      case scala.Array(label: String @unchecked, pojo: Pojo @unchecked) =>
        provide[Tactic[PojoError]]:
          provide[Tactic[VariantError]]:
            delegate(label): [variant <: derivation] => _.decoded(pojo)

      case other =>
        provide[Tactic[PojoError]](abort(PojoError()))
