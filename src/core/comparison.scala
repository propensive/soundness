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
import gossamer.*, defaultTextTypes.output
import dissonance.*
import escapade.*
import iridescence.*
import dendrology.*
import escritoire.*
import spectacular.*
import hieroglyph.*

import scala.deriving.*
import scala.compiletime.*

enum Accordance:
  case Accord(value: Text)
  case Discord(left: Text, right: Text)
  case Collation(comparison: IArray[(Text, Accordance)], left: Text, right: Text)

object Accordance:
  given (using calc: TextWidthCalculator): Display[Accordance] =
    case Accordance.Collation(cmp, l, r) =>
      import tableStyles.horizontalGaps
      import treeStyles.default
      
      def children(comp: (Text, Accordance)): List[(Text, Accordance)] = comp(1) match
        case Accord(value)                      => Nil
        case Discord(left, right)               => Nil
        case Collation(comparison, left, right) => comparison.to(List)
      
      case class Row(treeLine: Text, left: Output, right: Output)

      def mkLine(tiles: List[TreeTile], data: (Text, Accordance)): Row =
        def line(bullet: Text) = t"${tiles.map(_.text).join}$bullet ${data(0)}"
        
        data(1) match
          case Accord(v) =>
            Row(line(t"▪"), out"${rgb"#667799"}($v)", out"${rgb"#667799"}($v)")
          
          case Discord(left, right) =>
            Row(line(t"▪"), out"${colors.YellowGreen}($left)", out"${colors.Crimson}($right)")
          
          case Collation(cmp, left, right) =>
            Row(line(t"■"), out"$left", out"$right")
      
      val table = Table[Row](
        Column(out"")(_.treeLine),
        Column(out"Expected", align = Alignment.Right)(_.left),
        Column(out"Found")(_.right)
      )

      table.tabulate(drawTree(children, mkLine)(cmp), maxWidth = 200).join(out"\n")
    
    case Discord(left, right) =>
      out"The result ${colors.Crimson}($right) did not equal ${colors.YellowGreen}($left)"
    
    case Accord(value) =>
      out"The value ${colors.Gray}($value) was expected"

object Assimilable:
  given [ValueType]: Assimilable[ValueType] = (a, b) => a == b

trait Assimilable[-ValueType]:
  def similar(a: ValueType, b: ValueType): Boolean

trait Contrast[-ValueType]:
  def apply(a: ValueType, b: ValueType): Accordance

extension [ValueType](left: ValueType)
  def contrastWith(right: ValueType)(using contrast: Contrast[ValueType]): Accordance =
    contrast(left, right)

trait FallbackContrast:
  given [ValueType]: Contrast[ValueType] = new Contrast[ValueType]:
    def apply(a: ValueType, b: ValueType): Accordance =
      if a == b then Accordance.Accord(a.debug)
      else Accordance.Discord(a.debug, b.debug)

object Contrast extends FallbackContrast:
  def nothing[ValueType]: Contrast[ValueType] = (a, b) => Accordance.Accord(a.debug)
  
  inline given Contrast[Exception] = new Contrast[Exception]:
    def apply(left: Exception, right: Exception): Accordance =
      val leftMsg = Option(left.getMessage).fold(t"null")(_.nn.debug)
      val rightMsg = Option(right.getMessage).fold(t"null")(_.nn.debug)
      if left.getClass == right.getClass && leftMsg == rightMsg then Accordance.Accord(leftMsg)
      else Accordance.Discord(leftMsg, rightMsg)

  inline def compareSeq
      [ValueType: Contrast: Assimilable]
      (left: IndexedSeq[ValueType], right: IndexedSeq[ValueType], leftDebug: Text, rightDebug: Text)
      : Accordance =
    if left == right then Accordance.Accord(leftDebug)
    else
      val comparison = IArray.from:
        diff(left, right).rdiff(summon[Assimilable[ValueType]].similar).changes.map:
          case Par(leftIndex, rightIndex, value) =>
            val label =
              if leftIndex == rightIndex then leftIndex.show
              else t"${leftIndex.show.superscript}⫽${rightIndex.show.subscript}"
            
            label -> Accordance.Accord(value.debug)
          
          case Ins(rightIndex, value) =>
            t" ⧸${rightIndex.show.subscript}" -> Accordance.Discord(t"—", value.debug)
          
          case Del(leftIndex, value) =>
            t"${leftIndex.show.superscript}⧸" -> Accordance.Discord(value.debug, t"—")
          
          case Sub(leftIndex, rightIndex, leftValue, rightValue) =>
            val label = t"${leftIndex.show.superscript}⫽${rightIndex.show.subscript}"
            
            label -> leftValue.contrastWith(rightValue)
      
      Accordance.Collation(comparison, leftDebug, rightDebug)
    

  inline given iarray[ValueType: Contrast: Assimilable]: Contrast[IArray[ValueType]] =
    new Contrast[IArray[ValueType]]:
      def apply(left: IArray[ValueType], right: IArray[ValueType]): Accordance =
        compareSeq[ValueType](left.to(IndexedSeq), right.to(IndexedSeq), left.debug, right.debug)
  
  inline given list[ValueType: Contrast: Assimilable]: Contrast[List[ValueType]] =
    new Contrast[List[ValueType]]:
      def apply(left: List[ValueType], right: List[ValueType]): Accordance =
        compareSeq[ValueType](left.to(IndexedSeq), right.to(IndexedSeq), left.debug, right.debug)
  
  inline given vector[ValueType: Contrast: Assimilable]: Contrast[Vector[ValueType]] =
    new Contrast[Vector[ValueType]]:
      def apply(left: Vector[ValueType], right: Vector[ValueType]): Accordance =
        compareSeq[ValueType](left.to(IndexedSeq), right.to(IndexedSeq), left.debug, right.debug)
  
  private transparent inline def deriveSum
      [TupleType <: Tuple, DerivedType]
      (ordinal: Int)
      : Contrast[DerivedType] =
    inline erasedValue[TupleType] match
      case _: (head *: tail) =>
        if ordinal == 0 then summonInline[Contrast[head]].asInstanceOf[Contrast[DerivedType]]
        else deriveSum[tail, DerivedType](ordinal - 1)

  private transparent inline def deriveProduct
      [Labels <: Tuple]
      (left: Tuple, right: Tuple)
      : List[(Text, Accordance)] =
    inline left match
      case EmptyTuple => Nil
      case leftCons: (? *: ?) => leftCons match
        case leftHead *: leftTail => inline right match
          case rightCons: (? *: ?) => rightCons match
            case rightHead *: rightTail => inline erasedValue[Labels] match
              case _: (headLabel *: tailLabels) => inline valueOf[headLabel].asMatchable match
                case label: String =>
                  val item =
                    if leftHead == rightHead then Accordance.Accord(leftHead.debug)
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
              Accordance.Collation(IArray.from(product), left.debug, right.debug)
     
      case mirror: Mirror.SumOf[DerivationType] => (left: DerivationType, right: DerivationType) =>
        if mirror.ordinal(left) == mirror.ordinal(right)
        then deriveSum[mirror.MirroredElemTypes, DerivationType](mirror.ordinal(left))(left, right)
        else Accordance.Discord(left.debug, right.debug)
