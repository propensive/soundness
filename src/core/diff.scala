/*
    Dissonance, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package dissonance

import rudiments.*
import fulminate.*
import perforate.*
import anticipation.*

import language.experimental.captureChecking

sealed trait Change[+ElemType] extends Product:
  def map[ElemType2](fn: ElemType => ElemType2): Change[ElemType2^{fn}] = this match
    case Sub(left, right, leftValue, rightValue) =>
      Sub(left, right, leftValue.let(fn), rightValue.let(fn))
    
    case edit: Edit[ElemType] =>
      edit.map(fn)

sealed trait Edit[+ElemType] extends Change[ElemType]:
  def value: Maybe[ElemType]

  override def map[ElemType2](fn: ElemType => ElemType2): Edit[ElemType2^{fn}] = this match
    case Par(left, right, value) => Par(left, right, value.let(fn))
    case Del(left, value)        => Del(left, value.let(fn))
    case Ins(right, value)       => Ins(right, fn(value))

case class Ins[+ElemType](right: Int, value: ElemType) extends Edit[ElemType]
case class Del[+ElemType](left: Int, value: Maybe[ElemType] = Unset) extends Edit[ElemType]

case class Par[+ElemType](left: Int, right: Int, value: Maybe[ElemType] = Unset)
extends Edit[ElemType]
 
case class Sub
    [+ElemType]
    (left: Int, right: Int, leftValue: Maybe[ElemType], rightValue: Maybe[ElemType])
extends Change[ElemType]

enum Region[ElemType]:
  case Changed(deletions: List[Del[ElemType]], insertions: List[Ins[ElemType]])
  case Unchanged(retentions: List[Par[ElemType]])

case class RDiff[ElemType](changes: Change[ElemType]*):
  def flip: RDiff[Maybe[ElemType]] =
    val changes2: Seq[Change[Maybe[ElemType]]] = changes.map:
      case Par(left, right, value)                 => Par(right, left, value)
      case Del(left, value)                        => Ins(left, value)
      case Ins(right, value)                       => Del(right, value)
      case Sub(left, right, leftValue, rightValue) => Sub(right, left, rightValue, leftValue)
    
    RDiff(changes2*)
  
  def map[ElemType2](fn: ElemType => ElemType2): RDiff[ElemType2^{fn}] =
    RDiff(changes.map(_.map(fn))*)

case class DiffParseError(lineNo: Int, line: Text)
extends Error(msg"could not read the diff at line $lineNo: $line")

object Diff:
  def parse(lines: LazyList[Text])(using Raises[DiffParseError]): Diff[Text] =
    def recur
        (todo: LazyList[Text], line: Int, edits: List[Edit[Text]], pos: Int, rpos: Int, target: Int)
        : Diff[Text] =
      if pos < target
      then recur(todo, line + 1, Par(pos, rpos, Unset) :: edits, pos + 1, rpos + 1, target)
      else todo match
        case head #:: tail =>
          if head == Text("---") then recur(tail, line + 1, edits, pos, rpos, 0)
          else if head.s.startsWith("< ")
          then recur(tail, line + 1, Del(pos, Text(head.s.drop(2))) :: edits, pos + 1, rpos, 0)
          else if head.s.startsWith("> ")
          then recur(tail, line + 1, Ins(rpos, Text(head.s.drop(2))) :: edits, pos, rpos + 1, 0)
          else
            def unpair(string: String): (Int, Int) =
              try string.split(",").nn.to(List) match
                case List(start, end) => (start.nn.toInt - 1, end.nn.toInt)
                case List(start)      => (start.nn.toInt - 1, start.nn.toInt)
                case _                => raise(DiffParseError(line, head))((0, 0))
              catch case err: NumberFormatException => raise(DiffParseError(line, head))((0, 0))

            val pairs =
              head.s.split("[acd]").nn.to(List) match
                case List(left, right) => Some((unpair(left.nn), unpair(right.nn)))
                case _                 => None
            
            pairs match
              case Some(((leftStart, leftEnd), (rightStart, rightEnd))) =>
                recur(tail, line + 1, edits, pos, rpos, leftStart)
              
              case None =>
                raise(DiffParseError(line, head))(recur(tail, line + 1, edits, pos, rpos, 0))
        
        case _ =>
          Diff(edits.reverse*)
    
    recur(lines, 1, Nil, 0, 0, 0)
          
case class Diff[ElemType](edits: Edit[ElemType]*):
  def flip: Diff[Maybe[ElemType]] =
    val edits2: Seq[Edit[Maybe[ElemType]]] = edits.map:
      case Par(left, right, value) => Par(right, left, value)
      case Del(left, value)        => Ins(left, value)
      case Ins(right, value)       => Del(right, value)
    
    Diff(edits2*)

  def map[ElemType2](fn: ElemType => ElemType2): Diff[ElemType2^{fn}] = Diff(edits.map(_.map(fn))*)

  def applyTo
      (seq: Seq[ElemType], update: (ElemType, ElemType) -> ElemType = { (left, right) => left })
      : LazyList[ElemType] =
    
    def recur(todo: List[Edit[ElemType]], seq: Seq[ElemType]): LazyList[ElemType] = todo match
      case Nil                   => seq.to(LazyList)
      case Ins(_, value) :: tail => value #:: recur(tail, seq)
      case Del(_, _) :: tail     => recur(tail, seq.tail)
      
      case Par(_, _, value) :: tail =>
        value.let(update(_, seq.head)).or(seq.head) #:: recur(tail, seq.tail)

    recur(edits.to(List), seq)

  def rdiff(similar: (ElemType, ElemType) -> Boolean, subSize: Int = 1): RDiff[ElemType] =
    val changes = collate.flatMap:
      case Region.Unchanged(pars)    => pars
      case Region.Changed(dels, Nil) => dels
      case Region.Changed(Nil, inss) => inss
      
      case Region.Changed(dels, inss) =>
        if inss.length == dels.length && inss.length <= subSize
        then dels.zip(inss).map { (del, ins) => Sub(del.left, ins.right, del.value, ins.value) }
        else
          val delsSeq = dels.map(_.value.vouch(using Unsafe)).to(IndexedSeq)
          val inssSeq = inss.map(_.value.vouch(using Unsafe)).to(IndexedSeq)
          
          diff(delsSeq, inssSeq, similar).edits.map:
            case Del(index, _) => Del(dels(index).left, dels(index).value)
            case Ins(index, _) => Ins(inss(index).right, inss(index).value)
            
            case Par(left, right, _) =>
              Sub(dels(left).left, inss(right).right, dels(left).value, inss(right).value)
    
    RDiff(changes*)
          
  def collate: List[Region[ElemType]] =
    edits.runsBy:
      case Par(_, _, _) => true
      case _            => false
    .map:
      case xs@(Par(_, _, _) :: _) => Region.Unchanged(xs.sift[Par[ElemType]])
      case xs                     => Region.Changed(xs.sift[Del[ElemType]], xs.sift[Ins[ElemType]])

  def chunks: LazyList[Chunk[ElemType]] =
    def recur(todo: List[Edit[ElemType]], pos: Int, rpos: Int): LazyList[Chunk[ElemType]] =
      todo match
        case Nil                         => LazyList()
        case Par(pos2, rpos2, _) :: tail => recur(tail, pos2 + 1, rpos2 + 1)
        
        case _ =>
          val dels = todo.takeWhile(_.is[Del[ElemType]]).collect:
            case del: Del[ElemType] => del
          
          val inss = todo.drop(dels.length).takeWhile(_.is[Ins[ElemType]]).collect:
            case ins: Ins[ElemType] => ins
          
          Chunk(pos, rpos, dels, inss) #::
              recur(todo.drop(dels.size + inss.size), pos + dels.length, pos + inss.length)

    recur(edits.to(List), 0, 0)

case class Chunk
    [ElemType]
    (pos: Int, rpos: Int, dels: List[Del[ElemType]], inss: List[Ins[ElemType]])

def diff
    [ElemType]
    (left: IndexedSeq[ElemType], right: IndexedSeq[ElemType],
        compare: (ElemType, ElemType) -> Boolean = { (a: ElemType, b: ElemType) => a == b })
    : Diff[ElemType] =

  type Edits = List[Edit[ElemType]]

  @tailrec
  def count(pos: Int, off: Int): Int =
    if pos < left.length && pos + off < right.length && compare(left(pos), right(pos + off))
    then count(pos + 1, off)
    else pos

  @tailrec
  def trace(deletes: Int, inserts: Int, focus: List[Int], rows: List[Array[Int]]): Diff[ElemType] =
    val delPos = if deletes == 0 then 0 else count(rows.head(deletes - 1) + 1, inserts - deletes)
    val insPos = if inserts == 0 then 0 else count(rows.head(deletes), inserts - deletes)
    val best = if deletes + inserts == 0 then count(0, 0) else delPos.max(insPos)
    
    if best == left.length && (best - deletes + inserts) == right.length
    then Diff(backtrack(left.length - 1, deletes, rows, Nil)*)
    else if inserts > 0 then trace(deletes + 1, inserts - 1, best :: focus, rows)
    else trace(0, deletes + 1, Nil, ((best :: focus).reverse).to(Array) :: rows)

  @tailrec
  def backtrack(pos: Int, deletes: Int, rows: List[Array[Int]], edits: Edits): Edits =
    val rpos = pos + rows.length - deletes*2
    lazy val ins = rows.head(deletes) - 1
    lazy val del = rows.head(deletes - 1)
    
    if pos == -1 && rpos == -1 then edits else if rows.isEmpty
    then backtrack(pos - 1, deletes, rows, Par(pos, rpos, left(pos)) :: edits)
    else if deletes < rows.length && (deletes == 0 || ins >= del)
    then
      if pos == ins then backtrack(pos, deletes, rows.tail, Ins(rpos, right(rpos)) :: edits)
      else backtrack(pos - 1, deletes, rows, Par(pos, rpos, left(pos)) :: edits)
    else
      if pos == del then backtrack(del - 1, deletes - 1, rows.tail, Del(pos, left(pos)) :: edits)
      else backtrack(pos - 1, deletes, rows, Par(pos, rpos, left(pos)) :: edits)
    
  trace(0, 0, Nil, Nil)

extension (diff: Diff[Text])
  def serialize: LazyList[Text] = diff.chunks.flatMap:
    case Chunk(left, right, dels, inss) =>
      def range(start: Int, end: Int): Text = s"$start${if start == end then "" else s",$end"}".tt
      
      val command: Text =
        if dels.isEmpty then s"${left}a${range(right + 1, right + inss.size)}".tt
        else if inss.isEmpty then s"${range(left + 1, left + dels.size)}d${right}".tt
        else s"${range(left + 1, left + dels.size)}c${range(right + 1, right + inss.size)}".tt

      val delSeq = dels.map { del => Text("< "+del.value) }
      val sep = if inss.size > 0 && dels.size > 0 then List(Text("---")) else List()
      val insSeq = inss.map { ins => Text("> "+ins.value) }
      
      command :: delSeq ::: sep ::: insSeq

  def casual: CasualDiff =
    def recur(todo: List[Region[Text]], acc: List[Replace], last: List[Text]): CasualDiff =
      todo match
        case Nil                            => CasualDiff(acc.reverse)
        case Region.Unchanged(pars) :: tail => recur(tail, acc, pars.map(_.value).vouched)
        
        case Region.Changed(deletions, insertions) :: tail =>
          val deletions2 = deletions.map(_.value).vouched
          val prelude = (last ++ deletions2).to(Array)
          
          def replace(deletions: List[Text], target: Int): Replace =
            val index = prelude.indexOfSlice(deletions)
            if index == target
            then Replace(deletions.dropRight(deletions2.length), deletions2, insertions.map(_.value))
            else
              def countback(n: Int): Int =
                if index - n > 0 && prelude(target - n) == prelude(index - n) then countback(n + 1)
                else n
              
              val n = countback(1)
              replace(prelude.slice(target - n, target).to(List) ++ deletions, target - n)

          recur(tail, replace(deletions2, last.length) :: acc, Nil)

    recur(diff.collate, Nil, Nil)
