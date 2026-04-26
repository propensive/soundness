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
package escritoire

import anticipation.*
import gossamer.*
import prepositional.*
import proscenium.*
import spectacular.*
import vacuous.*
import wisteria.*, derivationContext.required

object Tabulable extends ProductDerivation[[row] =>> row is Tabulable[Text]]:
  class JoinTabulable[derivation <: Product](columns: IArray[Column[derivation, Text]])
  extends Tabulable[Text]:
    type Self = derivation
    def table(): Scaffold[derivation, Text] = Scaffold[derivation](columns*)

  inline def conjunction[derivation <: Product: ProductReflection]: derivation is Tabulable[Text] =
    val labels: Map[Text, Text] = compiletime.summonFrom:
      case labels: TableRelabelling[derivation] => labels.relabelling()
      case _                                    => Map()

    val columns: IArray[Column[derivation, Text]] =
      contexts:
        [field] => tabulable =>
          tabulable.table().columns.map(_.contramap(dereference).retitle:
            labels.get(label).getOrElse(label.uncamel.join(t" ").capitalize))

      . flatten

    new JoinTabulable[derivation](columns)

  given int: Int is Tabulable[Text] = () =>
    Scaffold[Int, Text](Column(t"", TextAlignment.Right, Unset, columnar.Collapsible(0.3))(_.show))

  given double: Decimalizer => Double is Tabulable[Text] = () =>
    Scaffold[Double, Text]
      ( Column(t"", TextAlignment.Right, Unset, columnar.Collapsible(0.3))(_.show) )

  given text: Text is Tabulable[Text] = () =>
    Scaffold[Text, Text](Column(t"", TextAlignment.Left, Unset, columnar.Prose)(identity))

trait Tabulable[text] extends Typeclass:
  def table(): Scaffold[Self, text]
  private lazy val tableValue: Scaffold[Self, text] = table()
  def tabulate(data: Seq[Self]): Tabulation[text] = tableValue.tabulate(data)
