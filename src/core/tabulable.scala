/*
    Escritoire, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package escritoire

import gossamer.*
import wisteria.*, derivationContext.required
import anticipation.*
import vacuous.*
import spectacular.*

extension [RowType](data: Seq[RowType])
  def table[TextType](using textual: Textual[TextType], tabulable: Tabulable[RowType, TextType])
        : Tabulation[TextType] =

    tabulable.tabulate(data)

trait Tabulable[RowType, TextType]:
  def table(): Table[RowType, TextType]
  private lazy val tableValue: Table[RowType, TextType] = table()
  def tabulate(data: Seq[RowType]): Tabulation[TextType] = tableValue.tabulate(data)

trait TableRelabelling[+TargetType]:
  def relabelling(): Map[Text, Text]
  private lazy val labels: Map[Text, Text] = relabelling()
  def apply(label: Text): Optional[Text] = if labels.contains(label) then labels(label) else Unset

object Tabulable extends ProductDerivation[[RowType] =>> Tabulable[RowType, Text]]:
  inline def join[DerivationType <: Product: ProductReflection]: Tabulable[DerivationType, Text] = () =>
    val columns: IArray[Column[DerivationType, Text]] =
      val labels: Map[Text, Text] = compiletime.summonFrom:
        case labels: TableRelabelling[DerivationType] => labels.relabelling()
        case _                                        => Map()

      contexts:
        [FieldType] => tabulable =>
          tabulable.table().columns.map(_.contramap(dereference).retitle:
            labels.get(label).getOrElse(label.capitalize))
      .flatten

    Table[DerivationType](columns*)
  
  given Tabulable[Int, Text] = () =>
    Table[Int, Text](Column(t"", TextAlignment.Right, Unset, columnSizing.Collapsible(0.3))(_.show))
  
  given (using Decimalizer): Tabulable[Double, Text] = () =>
    Table[Double, Text](Column(t"", TextAlignment.Right, Unset, columnSizing.Collapsible(0.3))(_.show))
  
  given Tabulable[Text, Text] = () =>
    Table[Text, Text](Column(t"", TextAlignment.Left, Unset, columnSizing.Prose)(identity))