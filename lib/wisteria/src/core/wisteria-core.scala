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
┃    Soundness, version 0.50.0.                                                                    ┃
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
package wisteria

import scala.deriving.*

import prepositional.*
import symbolism.*

type Reflection[derivation] = Mirror.Of[derivation]
type ProductReflection[derivation <: Product] = Mirror.ProductOf[derivation]
type SumReflection[derivation] = Mirror.SumOf[derivation]

type Derivable[derivation <: { type Self }] =
  Derivation[[self] =>> derivation { type Self = self }]

type ProductDerivable[derivation <: { type Self }] =
  ProductDerivation[[self] =>> derivation { type Self = self }]


object arithmetic:
  object AddableDerivation
  extends ProductDerivation[[value] =>> value is Addable by value to value]:

    inline def join[derivation <: Product: ProductReflection]
    : derivation is Addable by derivation to derivation =
        (left, right) =>
          construct:
            [field] => _.add(complement(left), complement(right))

  inline given addable: [value <: Product: ProductReflection]
               => value is Addable by value to value =
    AddableDerivation.derived[value]


  object SubtractableDerivation
  extends ProductDerivation[[value] =>> value is Subtractable by value to value]:

    inline def join[derivation <: Product: ProductReflection]
    : derivation is Subtractable by derivation to derivation =
        (left, right) =>
          construct:
            [field] => _.subtract(complement(left), complement(right))

  inline given subtractable: [value <: Product: ProductReflection]
                => value is Subtractable by value to value =
    SubtractableDerivation.derived[value]


  object MultiplicableDerivation
  extends ProductDerivation[[value] =>> value is Multiplicable by value to value]:

    inline def join[derivation <: Product: ProductReflection]
    : derivation is Multiplicable by derivation to derivation =
        (left, right) =>
          construct:
            [field] => _.multiply(complement(left), complement(right))

  inline given multiplicable: [value <: Product: ProductReflection]
                => value is Multiplicable by value to value =
    MultiplicableDerivation.derived[value]


  object DivisibleDerivation
  extends ProductDerivation[[value] =>> value is Divisible by value to value]:

    inline def join[derivation <: Product: ProductReflection]
    : derivation is Divisible by derivation to derivation =
        (left, right) =>
          construct:
            [field] => _.divide(complement(left), complement(right))

  inline given divisible: [value <: Product: ProductReflection]
                => value is Divisible by value to value =
    DivisibleDerivation.derived[value]
