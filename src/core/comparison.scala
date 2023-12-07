/*
    Chiaroscuro, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package chiaroscuro

import rudiments.*
import gossamer.*
import dissonance.*
import escapade.*
import iridescence.*
import dendrology.*
import escritoire.*
import spectacular.*
import hieroglyph.*
import anticipation.*

import scala.deriving.*
import scala.compiletime.*

enum Semblance:
  case Identical(value: Text)
  case Different(left: Text, right: Text)
  case Breakdown(comparison: IArray[(Text, Semblance)], left: Text, right: Text)

object Semblance:
  given (using calc: TextWidthCalculator): Displayable[Semblance] =
    case Semblance.Breakdown(cmp, l, r) =>
      import tableStyles.horizontalGaps
      
      def children(comp: (Text, Semblance)): List[(Text, Semblance)] = comp(1) match
        case Identical(value)                 => Nil
        case Different(left, right)           => Nil
        case Breakdown(comparison, left, right) => comparison.to(List)
      
      case class Row(treeLine: Text, left: Output, right: Output)

      given TreeStyle[Row] = (tiles, row) =>
        row.copy(treeLine = tiles.map(treeStyles.default.text(_)).join+row.treeLine)

      def mkLine(data: (Text, Semblance)) =
        def line(bullet: Text): Text = t"$bullet ${data(0)}"
        
        data(1) match
          case Identical(v) =>
            Row(line(t"▪"), e"${rgb"#667799"}($v)", e"${rgb"#667799"}($v)")
          
          case Different(left, right) =>
            Row(line(t"▪"), e"${colors.YellowGreen}($left)", e"${colors.Crimson}($right)")
          
          case Breakdown(cmp, left, right) =>
            Row(line(t"■"), e"$left", e"$right")
      
      val table = Table[Row](
        Column(e"")(_.treeLine),
        Column(e"Expected", align = Alignment.Right)(_.left),
        Column(e"Found")(_.right)
      )

      val diagram = TreeDiagram[(Text, Semblance)](children, mkLine)

      table.tabulate(diagram(cmp).lines, maxWidth = 200).join(e"\n")
    
    case Different(left, right) =>
      val whitespace = if right.contains('\n') then e"\n" else e" "
      val whitespace2 = if left.contains('\n') then e"\n" else e" "
      e"The result$whitespace${colors.Crimson}($right)${whitespace}did not equal$whitespace${colors.YellowGreen}($left)"
    
    case Identical(value) =>
      e"The value ${colors.Gray}($value) was expected"

object Similarity:
  given [ValueType]: Similarity[ValueType] = (a, b) => a == b

trait Similarity[-ValueType]:
  def similar(a: ValueType, b: ValueType): Boolean

trait Contrast[-ValueType]:
  def apply(a: ValueType, b: ValueType): Semblance

extension [ValueType](left: ValueType)
  def contrastWith(right: ValueType)(using contrast: Contrast[ValueType]): Semblance =
    contrast(left, right)

trait FallbackContrast:
  given [ValueType]: Contrast[ValueType] = new Contrast[ValueType]:
    def apply(a: ValueType, b: ValueType): Semblance =
      if a == b then Semblance.Identical(a.debug)
      else Semblance.Different(a.debug, b.debug)

object Contrast extends FallbackContrast:
  def nothing[ValueType]: Contrast[ValueType] = (a, b) => Semblance.Identical(a.debug)
  
  inline given Contrast[Exception] = new Contrast[Exception]:
    def apply(left: Exception, right: Exception): Semblance =
      val leftMsg = Option(left.getMessage).fold(t"null")(_.nn.debug)
      val rightMsg = Option(right.getMessage).fold(t"null")(_.nn.debug)
      if left.getClass == right.getClass && leftMsg == rightMsg then Semblance.Identical(leftMsg)
      else Semblance.Different(leftMsg, rightMsg)

  inline def compareSeq
      [ValueType: Contrast: Similarity]
      (left: IndexedSeq[ValueType], right: IndexedSeq[ValueType], leftDebug: Text, rightDebug: Text)
      : Semblance =
    if left == right then Semblance.Identical(leftDebug)
    else
      val comparison = IArray.from:
        diff(left, right).rdiff(summon[Similarity[ValueType]].similar).changes.map:
          case Par(leftIndex, rightIndex, value) =>
            val label =
              if leftIndex == rightIndex then leftIndex.show
              else t"${leftIndex.show.superscript}⫽${rightIndex.show.subscript}"
            
            label -> Semblance.Identical(value.debug)
          
          case Ins(rightIndex, value) =>
            t" ⧸${rightIndex.show.subscript}" -> Semblance.Different(t"—", value.debug)
          
          case Del(leftIndex, value) =>
            t"${leftIndex.show.superscript}⧸" -> Semblance.Different(value.debug, t"—")
          
          case Sub(leftIndex, rightIndex, leftValue, rightValue) =>
            val label = t"${leftIndex.show.superscript}⫽${rightIndex.show.subscript}"
            
            label -> leftValue.contrastWith(rightValue)
      
      Semblance.Breakdown(comparison, leftDebug, rightDebug)
    

  inline given iarray[ValueType: Contrast: Similarity]: Contrast[IArray[ValueType]] =
    new Contrast[IArray[ValueType]]:
      def apply(left: IArray[ValueType], right: IArray[ValueType]): Semblance =
        compareSeq[ValueType](left.to(IndexedSeq), right.to(IndexedSeq), left.debug, right.debug)
  
  inline given list[ValueType: Contrast: Similarity]: Contrast[List[ValueType]] =
    new Contrast[List[ValueType]]:
      def apply(left: List[ValueType], right: List[ValueType]): Semblance =
        compareSeq[ValueType](left.to(IndexedSeq), right.to(IndexedSeq), left.debug, right.debug)
  
  inline given vector[ValueType: Contrast: Similarity]: Contrast[Vector[ValueType]] =
    new Contrast[Vector[ValueType]]:
      def apply(left: Vector[ValueType], right: Vector[ValueType]): Semblance =
        compareSeq[ValueType](left.to(IndexedSeq), right.to(IndexedSeq), left.debug, right.debug)
  
  private transparent inline def deriveSum
      [TupleType <: Tuple, DerivedType]
      (ordinal: Int)
      : Contrast[DerivedType] =
    inline erasedValue[TupleType] match
      case _: (head *: tail) =>
        inline if ordinal == 0 then summonInline[Contrast[head]].asInstanceOf[Contrast[DerivedType]]
        else deriveSum[tail, DerivedType](ordinal - 1)
      case _ => ???

  private transparent inline def deriveProduct
      [Labels <: Tuple]
      (left: Tuple, right: Tuple)
      : List[(Text, Semblance)] =
    inline left match
      case EmptyTuple => Nil
      case leftCons: (? *: ?) => leftCons match
        case leftHead *: leftTail => inline right match
          case rightCons: (? *: ?) => rightCons match
            case rightHead *: rightTail => inline erasedValue[Labels] match
              case _: (headLabel *: tailLabels) => inline valueOf[headLabel].asMatchable match
                case label: String =>
                  val item =
                    if leftHead == rightHead then Semblance.Identical(leftHead.debug)
                    else summonInline[Contrast[leftHead.type | rightHead.type]](leftHead, rightHead)
                  
                  (Text(label), item) :: deriveProduct[tailLabels](leftTail, rightTail)
        
  inline given derived
      [DerivationType](using mirror: Mirror.Of[DerivationType]): Contrast[DerivationType] =
    inline mirror match
      case given Mirror.ProductOf[DerivationType & Product] =>
        (left: DerivationType, right: DerivationType) => inline (left.asMatchable: @unchecked) match
          case leftProduct: Product => inline (right.asMatchable: @unchecked) match
            case rightProduct: Product =>
              val leftTuple = Tuple.fromProductTyped(leftProduct)
              val rightTuple = Tuple.fromProductTyped(rightProduct)
              val product = deriveProduct[mirror.MirroredElemLabels](leftTuple, rightTuple)
              Semblance.Breakdown(IArray.from(product), left.debug, right.debug)
     
      case mirror: Mirror.SumOf[DerivationType] => (left: DerivationType, right: DerivationType) =>
        if mirror.ordinal(left) == mirror.ordinal(right)
        then deriveSum[mirror.MirroredElemTypes, DerivationType](mirror.ordinal(left))(left, right)
        else Semblance.Different(left.debug, right.debug)
