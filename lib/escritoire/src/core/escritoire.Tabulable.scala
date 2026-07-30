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
package escritoire

import proscenium.compat.*

import scala.compiletime

import anticipation.*
import gossamer.*
import prepositional.*
import spectacular.*
import vacuous.*
import wisteria.*

object Tabulable extends ProductDerivation[[row] =>> row is Tabulable[Text]]:
  class JoinTabulable[derivation <: Product](columns: Array[Column[derivation, Text]]^{})
  extends Tabulable[Text]:
    type Self = derivation
    def table(): Scaffold[derivation, Text] = Scaffold[derivation](columns*)

  // Flattens the per-field column arrays; a plain method rather than part of the inline
  // derivation body, so the fresh `any.rd` of the intermediates is not minted at the
  // expansion site, where it would leak into the caller's capture sets.
  private def joinColumns[derivation <: Product]
    ( arrays: Array[Array[Column[derivation, Text]]^{}]^{scala.caps.any.rd} )
  :   JoinTabulable[derivation] =

    new JoinTabulable[derivation](Array.frozen(arrays.readable.map(_.readable).flatten))

  inline def conjunction[derivation <: Product: ProductReflection]: derivation is Tabulable[Text] =
    val labels: Map[Text, Text] = compiletime.summonFrom:
      case labels: TableRelabelling[derivation] => labels.relabelling()
      case _                                    => Map()

    joinColumns[derivation]:
      contexts[derivation]():
        [field] => tabulable =>
          tabulable.table().columns.map: element =>
            element.contramap(dereference).retitle:
              labels.stdlib.get(label).getOrElse(label.uncamel.join(t" ").capitalize)

  given int: Int is Tabulable[Text] = () =>
    Scaffold[Int, Text](Column(t"", TextAlignment.Right, Unset, columnar.Collapsible(0.3))(_.show))

  given double: Decimalizer => Double is Tabulable[Text] = () =>
    Scaffold[Double, Text]
      ( Column(t"", TextAlignment.Right, Unset, columnar.Collapsible(0.3))(_.show) )

  given text: Text is Tabulable[Text] = () =>
    Scaffold[Text, Text](Column(t"", TextAlignment.Left, Unset, columnar.Paragraph)(identity))

trait Tabulable[text] extends Typeclass:
  def table(): Scaffold[Self, text]
  private lazy val tableValue: Scaffold[Self, text] = table()
  def tabulate(data: List[Self]): Tabulation[text] = tableValue.tabulate(data)
