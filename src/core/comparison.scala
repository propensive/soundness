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
import lithography.*

import scala.deriving.*
import scala.compiletime.*

enum Accordance:
  case Accord(value: Text)
  case Discord(left: Text, right: Text)
  case Collation(comparison: IArray[(Text, Accordance)], left: Text, right: Text)

extension [T](value: T)
  def contrastWith(other: T)(using contrast: Contrast[T]): Accordance = contrast(value, other)

object Accordance:
  given (using calc: TextWidthCalculator): Display[Accordance] =
    case Accordance.Collation(cmp, l, r) =>
      import tableStyles.horizontalGaps
      import treeStyles.default
      
      def children(comp: (Text, Accordance)): List[(Text, Accordance)] = comp(1) match
        case Accord(value)                         => Nil
        case Discord(left, right)              => Nil
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

object Contrast:
  def nothing[ValueType]: Contrast[ValueType] = (a, b) => Accordance.Accord(a.toString.debug)
  
  inline def simplistic[ValueType]: Contrast[ValueType] = new Contrast[ValueType]:
    def apply(a: ValueType, b: ValueType): Accordance =
      if a == b then Accordance.Accord(a.debug)
      else Accordance.Discord(a.debug, b.debug)

  inline given Contrast[Text] = simplistic(_, _)
  inline given Contrast[Int] = simplistic(_, _)
  inline given Contrast[Long] = simplistic(_, _)
  inline given Contrast[Double] = simplistic(_, _)
  inline given Contrast[Float] = simplistic(_, _)
  inline given [EnumType <: reflect.Enum]: Contrast[EnumType] = simplistic(_, _)

  inline given Contrast[Exception] = new Contrast[Exception]:
    def apply(left: Exception, right: Exception): Accordance =
      val leftMsg = Option(left.getMessage).fold(t"null")(_.nn.debug)
      val rightMsg = Option(right.getMessage).fold(t"null")(_.nn.debug)
      if left.getClass == right.getClass && leftMsg == rightMsg then Accordance.Accord(leftMsg)
      else Accordance.Discord(leftMsg, rightMsg)
  
  inline given seq[ValueType: Contrast: Assimilable, CollectionType[ElemType] <: Seq[ElemType]]
      : Contrast[CollectionType[ValueType]] =
    new Contrast[CollectionType[ValueType]]:
      def apply(left: CollectionType[ValueType], right: CollectionType[ValueType]): Accordance =
        val leftSeq = left.to(IndexedSeq)
        val rightSeq = right.to(IndexedSeq)
    
        if leftSeq == rightSeq then Accordance.Accord(leftSeq.map(_.debug).join(t"[", t", ", t"]"))
        else
          val comparison = IArray.from:
            diff(leftSeq, rightSeq).rdiff2(summon[Assimilable[ValueType]].similar).changes.map:
              case Change.Keep(lid, rid, v) =>
                (if lid == rid then lid.debug else t"${lid.show.superscript}⧸${rid.show.subscript}") -> Accordance.Accord(v.debug)
              
              case Change.Ins(rid, v) =>
                t" ⫽${rid.show.subscript}" -> Accordance.Discord(t"—", v.debug)
              
              case Change.Del(lid, v) =>
                t"${lid.show.superscript}⫽" -> Accordance.Discord(v.debug, t"—")
              
              case Change.Replace(lid, rid, lv, rv) =>
                t"${lid.show.superscript}⧸${rid.show.subscript}" -> summon[Contrast[ValueType]](lv, rv)
          
          Accordance.Collation(comparison, left.debug, right.debug)
  
  inline given iarray[ElemType: Contrast: Assimilable]: Contrast[IArray[ElemType]] =
    new Contrast[IArray[ElemType]]:
      def apply(left: IArray[ElemType], right: IArray[ElemType]): Accordance =
        seq[ElemType, IndexedSeq](left.to(IndexedSeq), right.to(IndexedSeq))

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
                    if leftHead == rightHead then Accordance.Accord(left.debug)
                    else summonInline[Contrast[leftHead.type]].asInstanceOf[Contrast[leftHead.type | rightHead.type]](leftHead, rightHead)
                  
                  (Text(label), item) :: deriveProduct[tailLabels](leftTail, rightTail)
        
  inline given derived
      [DerivationType](using mirror: Mirror.Of[DerivationType]): Contrast[DerivationType] =
    inline mirror match
      case given Mirror.ProductOf[DerivationType & Product] =>
        (left: DerivationType, right: DerivationType) => inline (left.asMatchable: @unchecked) match
          case left: Product => inline (right.asMatchable: @unchecked) match
            case right: Product =>
              val leftTuple = Tuple.fromProductTyped(left)
              val rightTuple = Tuple.fromProductTyped(right)
              Accordance.Collation(IArray.from(deriveProduct[mirror.MirroredElemLabels](leftTuple, rightTuple)), left.debug, right.debug)
     
      case mirror: Mirror.SumOf[DerivationType] => (left: DerivationType, right: DerivationType) =>
        if mirror.ordinal(left) == mirror.ordinal(right)
        then deriveSum[mirror.MirroredElemTypes, DerivationType](mirror.ordinal(left))(left, right)
        else Accordance.Discord(left.debug, right.debug)
