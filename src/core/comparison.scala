/*
    Chiaroscuro, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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
import wisteria.*
import escapade.*
import iridescence.*
import vacuous.*
import dendrology.*
import escritoire.*
import spectacular.*
import hieroglyph.*
import anticipation.*

import scala.deriving.*

enum Semblance:
  case Identical(value: Text)
  case Different(left: Text, right: Text)
  case Breakdown(comparison: IArray[(Text, Semblance)], left: Text, right: Text)

object Semblance:
  given (using calc: TextMetrics): Displayable[Semblance] =
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

      table.tabulate(TreeDiagram.by(children(_))(cmp*).render(mkLine), maxWidth = 200).join(e"\n")
    
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
  inline def contrastWith(right: ValueType): Semblance = compiletime.summonFrom:
    case contrast: Contrast[ValueType] => contrast(left, right)
    case _                             => Contrast.general(left, right)

object Contrast extends Derivation[Contrast]:
  def nothing[ValueType]: Contrast[ValueType] = (left, right) => Semblance.Identical(left.debug)
  
  inline def general[ValueType]: Contrast[ValueType] = (left, right) =>
    if left == right then Semblance.Identical(left.debug) else Semblance.Different(left.debug, right.debug)

  inline given Contrast[Exception] = new Contrast[Exception]:
    def apply(left: Exception, right: Exception): Semblance =
      val leftMsg = Option(left.getMessage).fold(t"null")(_.nn.debug)
      val rightMsg = Option(right.getMessage).fold(t"null")(_.nn.debug)

      if left.getClass == right.getClass && leftMsg == rightMsg then Semblance.Identical(leftMsg)
      else Semblance.Different(leftMsg, rightMsg)

  given Contrast[Char] = (left, right) =>
    if left == right then Semblance.Identical(left.show) else Semblance.Different(left.show, right.show)

  given Contrast[Text] = (left, right) => compareSeq[Char](left.chars, right.chars, left, right)

  inline def compareSeq
      [ValueType: Contrast: Similarity]
      (left: IndexedSeq[ValueType], right: IndexedSeq[ValueType], leftDebug: Text, rightDebug: Text)
      : Semblance =
    if left == right then Semblance.Identical(leftDebug) else
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
  
  inline def join[DerivationType <: Product: ProductReflection]: Contrast[DerivationType] = (left, right) =>
    val elements = fields(left, true): [FieldType] =>
      leftParam =>
        if leftParam == complement(right) then (label, Semblance.Identical(leftParam.debug))
        else (label, context(leftParam, complement(right)))
    
    Semblance.Breakdown(elements, left.debug, right.debug)
  
  inline def split[DerivationType: SumReflection]: Contrast[DerivationType] = (left, right) =>
    variant(left): [VariantType <: DerivationType] =>
      left => complement(right).let(left.contrastWith(_)).or(Semblance.Different(left.debug, right.debug))
    
