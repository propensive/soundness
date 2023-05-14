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

import wisteria.*
import rudiments.*
import gossamer.*, defaultTextTypes.output
import dissonance.*
import escapade.*
import iridescence.*
import dendrology.*
import escritoire.*
import spectacular.*
import lithography.*

enum Comparison:
  case Same(value: Text)
  case Different(left: Text, right: Text)
  case Structural(comparison: IArray[(Text, Comparison)], left: Text, right: Text)

extension [T](value: T)
  def compareTo(other: T)(using comparable: Comparable[T]): Comparison =
    comparable.compare(value, other)

object Comparison:
  given (using calc: TextWidthCalculator): Display[Comparison] =
    case Comparison.Structural(cmp, l, r) =>
      import tableStyles.horizontalGaps
      import treeStyles.default
      
      def children(comp: (Text, Comparison)): List[(Text, Comparison)] = comp(1) match
        case Same(value)                         => Nil
        case Different(left, right)              => Nil
        case Structural(comparison, left, right) => comparison.to(List)
      
      case class Row(treeLine: Text, left: Output, right: Output)

      def mkLine(tiles: List[TreeTile], data: (Text, Comparison)): Row =
        def line(bullet: Text) = t"${tiles.map(_.text).join}$bullet ${data(0)}"
        
        data(1) match
          case Same(v) =>
            Row(line(t"▪"), out"${rgb"#667799"}($v)", out"${rgb"#667799"}($v)")
          
          case Different(left, right) =>
            Row(line(t"▪"), out"${colors.YellowGreen}($left)", out"${colors.Crimson}($right)")
          
          case Structural(cmp, left, right) =>
            Row(line(t"■"), out"$left", out"$right")
      
      val table = Table[Row](
        Column(out"")(_.treeLine),
        Column(out"Expected", align = Alignment.Right)(_.left),
        Column(out"Found")(_.right)
      )

      table.tabulate(drawTree(children, mkLine)(cmp), maxWidth = 200).join(out"\n")
    
    case Different(left, right) =>
      out"The result ${colors.Crimson}($right) did not equal ${colors.YellowGreen}($left)"
    
    case Same(value) =>
      out"The value ${colors.Gray}($value) was expected"

object Similar:
  given [ValueType]: Similar[ValueType] = (a, b) => a == b

trait Similar[-ValueType]:
  def similar(a: ValueType, b: ValueType): Boolean

trait Comparable[-ValueType]:
  def compare(a: ValueType, b: ValueType): Comparison

object Comparable extends Derivation[Comparable]:
  def nothing[ValueType]: Comparable[ValueType] = (a, b) => Comparison.Same(a.toString.debug)
  
  inline def simplistic[ValueType]: Comparable[ValueType] = new Comparable[ValueType]:
    def compare(a: ValueType, b: ValueType): Comparison =
      if a == b then Comparison.Same(a.debug)
      else Comparison.Different(a.debug, b.debug)

  inline given Comparable[Text] = new Comparable[Text]:
    def compare(left: Text, right: Text): Comparison =
      if left == right then Comparison.Same(left.debug)
      else Comparison.Different(left.debug, right.debug)
  
  inline given Comparable[Int] = new Comparable[Int]:
    def compare(left: Int, right: Int): Comparison =
      if left == right then Comparison.Same(left.debug)
      else Comparison.Different(left.debug, right.debug)

  inline given Comparable[Exception] = new Comparable[Exception]:
    def compare(left: Exception, right: Exception): Comparison =
      val leftMsg = Option(left.getMessage).fold(t"null")(_.nn.debug)
      val rightMsg = Option(right.getMessage).fold(t"null")(_.nn.debug)
      if left.getClass == right.getClass && leftMsg == rightMsg then Comparison.Same(leftMsg)
      else Comparison.Different(leftMsg, rightMsg)
  
  inline given seq[ValueType: Comparable: Similar, CollectionType[ElemType] <: Seq[ElemType]]
      : Comparable[CollectionType[ValueType]] =
    new Comparable[CollectionType[ValueType]]:
      def compare(left: CollectionType[ValueType], right: CollectionType[ValueType]): Comparison =
        val leftSeq = left.to(IndexedSeq)
        val rightSeq = right.to(IndexedSeq)
    
        if leftSeq == rightSeq then Comparison.Same(leftSeq.map(_.debug).join(t"[", t", ", t"]"))
        else
          val comparison = IArray.from:
            diff(leftSeq, rightSeq).rdiff2(summon[Similar[ValueType]].similar).changes.map:
              case Change.Keep(lid, rid, v) =>
                (if lid == rid then lid.debug else t"$lid/$rid") -> Comparison.Same(v.debug)
              
              case Change.Ins(rid, v) =>
                t" /$rid" -> Comparison.Different(t"—", v.debug)
              
              case Change.Del(lid, v) =>
                t"$lid/" -> Comparison.Different(v.debug, t"—")
              
              case Change.Replace(lid, rid, lv, rv) =>
                t"$lid/$rid" -> summon[Comparable[ValueType]].compare(lv, rv)
          
          Comparison.Structural(comparison, left.debug, right.debug)
  
  inline given iarray[ElemType: Comparable: Similar]: Comparable[IArray[ElemType]] =
    new Comparable[IArray[ElemType]]:
      def compare(left: IArray[ElemType], right: IArray[ElemType]): Comparison =
        seq[ElemType, IndexedSeq].compare(left.to(IndexedSeq), right.to(IndexedSeq))
  
  def join[ValueType](caseClass: CaseClass[Comparable, ValueType]): Comparable[ValueType] =
    (left, right) =>
      val same = caseClass.params.forall: param =>
        param.deref(left) == param.deref(right)
  
      if same then Comparison.Same(left.toString.show)
      else
        val comparison = caseClass.params.map: param =>
          (Text(param.label), param.typeclass.compare(param.deref(left), param.deref(right)))
        Comparison.Structural(comparison, left.toString.show, right.toString.show)
  
  def split[ValueType](sealedTrait: SealedTrait[Comparable, ValueType]): Comparable[ValueType] =
    (left, right) =>
      sealedTrait.choose(left): subtype =>
        sealedTrait.choose(right): subtype2 =>
          if subtype.subtype.index == subtype2.subtype.index
          then subtype.typeclass.compare(subtype.cast(left), subtype.cast(right))
          else Comparison.Different(left.toString.show, right.toString.show)
