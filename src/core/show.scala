/*
    Gossamer, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package gossamer

import rudiments.*
import spectacular.*

import scala.deriving.*
import scala.compiletime.*

import language.experimental.pureFunctions


case class SimpleTExtractor(text: Text):
  def unapply(scrutinee: Text): Boolean = text == scrutinee

extension (inline ctx: StringContext)
  transparent inline def txt(inline parts: Any*): Text =
    ${Interpolation.Text.expand('{Interpolation.Text}, 'ctx, 'parts)}
 
  transparent inline def t(inline parts: Any*): Text =
    ${Interpolation.T.expand('{Interpolation.T}, 'ctx, 'parts)}

extension (ctx: StringContext)
  def t = SimpleTExtractor(ctx.parts.head.show)

/*
object Debug:
  object any extends Debug[Any]:
  
  given [T: Debug]: Debug[Option[T]] =
    case None        => t"None"
    case Some(value) => t"Some(${value.debug})"

  given long: Debug[Long] = long => Text(s"${long}L")

  given Debug[Double] = double =>
    if double != double then t"Double.NaN"
    else if java.lang.Double.isInfinite(double)
    then t"Double.${if double < 0.0 then t"Negative" else t"Positive"}Infinity"
    else Showable(double).show
  
  given Debug[Float] = float =>
    if float != float then t"Float.NaN"
    else if java.lang.Float.isInfinite(float)
    then t"Float.${if float < 0.0f then t"Negative" else t"Positive"}Infinity"
    else t"${Showable(float).show}F"

  given Debug[Boolean] =
    case true  => t"true"
    case false => t"false"
  
  given Debug[ByteSize] = bs =>
    if bs.long > 10L*1024*1024*1024*1024 then Text(s"${(bs.long/1024*1024*1024*1024).show}.tb")
    else if bs.long > 10L*1024*1024*1024 then Text(s"${(bs.long/1024*1024*1024).show}.gb")
    else if bs.long > 10*1024*1024 then Text(s"${bs.long/1024*1024}.mb")
    else if bs.long > 10*1024 then Text(s"${bs.long/1024}.kb")
    else Text(s"${bs.long}.b")

  given [T: Debug]: Debug[Set[T]] = _.map(_.debug).join(t"Set(", t", ", t")")
  given [T: Debug]: Debug[List[T]] = _.map(_.debug).join(t"List(", t", ", t")")
  given [T: Debug]: Debug[Vector[T]] = _.map(_.debug).join(t"Vector(", t", ", t")")
  given [T: Debug]: Debug[Array[T]] = _.map(_.debug).join(t"Array(", t", ", t")")
  given [T: Debug]: Debug[IArray[T]] = _.map(_.debug).join(t"IArray(", t", ", t")")


  private transparent inline def deriveProduct[Labels <: Tuple](tuple: Tuple): List[Text] =
    inline tuple match
      case EmptyTuple => Nil
      case cons: (? *: ?) => cons match
        case head *: tail => inline erasedValue[Labels] match
          case _: (headLabel *: tailLabels) => inline valueOf[headLabel].asMatchable match
            case label: String =>
              val value = summonInline[Debug[head.type]].show(head)
              Text(label+" = "+value) :: deriveProduct[tailLabels](tail)

  private transparent inline def deriveSum[TupleType <: Tuple, DerivedType](ordinal: Int): Debug[DerivedType] =
    inline erasedValue[TupleType] match
      case _: (head *: tail) =>
        if ordinal == 0 then
          summonInline[Debug[head]].asInstanceOf[Debug[DerivedType]]
        else deriveSum[tail, DerivedType](ordinal - 1)


  inline given derived[P](using mirror: Mirror.Of[P]): Debug[P] = inline mirror match
    case given Mirror.ProductOf[P & Product] => (value: P) => value.asMatchable match
      case value: Product =>
        val elements = deriveProduct[mirror.MirroredElemLabels](Tuple.fromProductTyped(value))
        val typeName = Text(valueOf[mirror.MirroredLabel])
        typeName+elements.join(Text("("), Text(", "), Text(")"))
    
    case s: Mirror.SumOf[P] =>
      (value: P) => deriveSum[s.MirroredElemTypes, P](s.ordinal(value)).show(value)

object Show:
  given (using decimalizer: Decimalizer): Show[Double] = decimalizer.decimalize(_)
  given (using decimalizer: Decimalizer): Show[Float] = decimalizer.decimalize(_)
  
  given [T: Show]: Show[LazyList[T]] with
    private def recur(value: LazyList[T]): Text =
      if value.toString() == "LazyList(<not computed>)" then t"〜"
      else if value.isEmpty then t"¶"
      else t"${value.head} ⌗ ${recur(value.tail)}"

    def show(value: LazyList[T]): Text = recur(value.take(3))
  
  given [T: Show]: Show[Option[T]] =
    case None    => Text("none")
    case Some(v) => v.show

extension (inline ctx: StringContext)
  transparent inline def enc(inline parts: Any*): Encoding =
    ${EncodingPrefix.expand('EncodingPrefix, 'ctx, 'parts)}
*/