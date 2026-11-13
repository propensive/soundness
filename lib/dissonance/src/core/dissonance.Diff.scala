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
package dissonance

import scala.language.experimental.pureFunctions

import anticipation.*
import contingency.*
import denominative.*
import denominative.dysasymptotics.linearSize
import fulminate.*
import prepositional.*
import rudiments.*
import symbolism.*
import turbulence.*
import vacuous.*

object Diff:
  given aggregable: (tactic: Tactic[Diff.Error])
  =>  ( (Diff[Text] is Aggregable by Text)^{tactic} ) = parse(_)

  private def parse(lines: Chain[Text]): Diff[Text] raises Diff.Error =
    def recur
      ( todo:          Chain[Text],
        line:          Int,
        edits:         List[Edit[Text]],
        position:      Int,
        rightPosition: Int,
        target:        Int )
    :   Diff[Text] =

      if position < target
      then
        recur
          ( todo,
            line + 1,
            Par(position, rightPosition, Unset) :: edits,
            position + 1,
            rightPosition + 1,
            target )

      else
        todo match
        case head #:: tail =>
          if head == Text("---") then recur(tail, line + 1, edits, position, rightPosition, 0)
          else if head.s.startsWith("< ")
          then
            recur
              ( tail,
                line + 1,
                Del(position, head.s.drop(2).tt) :: edits,
                position + 1,
                rightPosition,
                0 )

          else if head.s.startsWith("> ")
          then
            recur
              ( tail,
                line + 1,
                Ins(rightPosition, head.s.drop(2).tt) :: edits,
                position,
                rightPosition + 1,
                0 )

          else
            def unpair(string: String): (Int, Int) =
              try string.split(",").nn.iterator.to(List) match
                case List(start, end) => (start.nn.toInt - 1, end.nn.toInt)
                case List(start)      => (start.nn.toInt - 1, start.nn.toInt)
                case _                => abort(Diff.Error(line, head))
              catch case error: NumberFormatException => abort(Diff.Error(line, head))

            val pairs =
              head.s.split("[acd]").nn.iterator.to(List) match
                case List(left, right) => Some((unpair(left.nn), unpair(right.nn)))
                case _                 => None

            pairs match
              case Some(((leftStart, leftEnd), (rightStart, rightEnd))) =>
                recur(tail, line + 1, edits, position, rightPosition, leftStart)

              case None =>
                raise(Diff.Error(line, head))
                recur(tail, line + 1, edits, position, rightPosition, 0)

        case _ =>
          Diff(edits.reverse*)


    recur(lines, 1, Nil, 0, 0, 0)

  // Rendering a redraft reproduces every kept and deleted line verbatim, so it demands a `Diff`
  // whose edits provably carry their values: `parse`d diffs (whose `Par`s are `Unset`) do not
  // qualify; diffs from the Myers walk or a previous `resolve` do.
  extension (diff: Diff[Text] & Retained)
    def redraft(context: Redraft.Context = Redraft.Context.Minimal): Redraft =
      Redraft.render(diff, context)

  extension (diff: Diff[Text])
    def serialize: Chain[Text] = diff.chunks.flatMap:
      case Chunk(left, right, dels, inss) =>
        def range(start: Int, end: Int): Text =
          s"$start${if start == end then "" else s",$end"}".tt

        val command: Text =
          if dels.nil then s"${left}a${range(right + 1, right + inss.size)}".tt
          else if inss.nil then s"${range(left + 1, left + dels.size)}d${right}".tt
          else s"${range(left + 1, left + dels.size)}c${range(right + 1, right + inss.size)}".tt

        val delSeq = dels.map: del => Text("< "+del.value)

        val sep =
          if inss.size > 0 && dels.size > 0 then List(Text("---")) else List[Text]()

        val insSeq = inss.map: ins => Text("> "+ins.value)

        List(command) + delSeq + sep + insSeq

  // DiffError → Diff.Error
  case class Error(lineNo: Int, line: Text)(using Diagnostics)
  extends fulminate.Error(39, 0)(m"could not read the diff at line $lineNo: $line")

  sealed trait Change[+element] extends Product:
    import vacuous.*

    def map[element2](lambda: element => element2): Change[element2] = this match
      case Sub(left, right, leftValue, rightValue) =>
        Sub(left, right, leftValue.let(lambda), rightValue.let(lambda))

      case edit: Edit[element] =>
        edit.map(lambda)

  sealed trait Edit[+element] extends Change[element]:
    import vacuous.*

    def value: Optional[element]

    // Mapping preserves presence (`let` maps a present value to a present value), but not the
    // `Retained` brand: expressing `this.type & Retained`-conditional results here would entangle
    // the ADT with the marker, so a mapped edit must be re-minted (`retained`) where the brand is
    // still needed.
    override def map[element2](lambda: element => element2): Edit[element2] = this match
      case Par(left, right, value) => Par(left, right, value.let(lambda))
      case Del(left, value)        => Del(left, value.let(lambda))
      case Ins(right, value)       => Ins(right, lambda(value))

  case class Del[+element](left: Int, value: Optional[element] = Unset) extends Edit[element]
  case class Ins[+element](right: Int, value: element) extends Edit[element]

  case class Par[+element](left: Int, right: Int, value: Optional[element] = Unset)
  extends Edit[element]

  case class Sub[+element]
    ( left: Int, right: Int, leftValue: Optional[element], rightValue: Optional[element] )
  extends Change[element]

  case class Chunk[element]
    ( position: Int, rightPosition: Int, dels: List[Del[element]], inss: List[Ins[element]] )

case class Diff[element](edits: Diff.Edit[element]*):
  import Diff.{Change, Chunk, Del, Edit, Ins, Par, Sub}

  def size: Int = edits.count:
    case Par(_, _, _) => false
    case _            => true

  def flip: Diff[Optional[element]] =
    val edits2: List[Edit[Optional[element]]] = edits.to(List).map:
      case Par(left, right, value) => Par(right, left, value)
      case Del(left, value)        => Ins(left, value)
      case Ins(right, value)       => Del(right, value)

    Diff(edits2*)

  def map[element2](lambda: element => element2): Diff[element2] =
    Diff(edits.map(_.map(lambda))*)


  def patch(sequence: List[element], update: (element, element) -> element = (left, right) => left)
  :   Chain[element] =

    def recur(todo: List[Edit[element]], sequence: List[element]): Chain[element] = todo match
      case Nil                   => sequence.to[Chain]
      case Ins(_, value) :: tail => value #:: recur(tail, sequence)

      case Del(_, _) :: tail => sequence match
        case _ :: rest => recur(tail, rest)
        case _         => panic(m"the diff deletes beyond the end of the sequence")

      case Par(_, _, value) :: tail => sequence match
        case head :: rest => value.let(update(_, head)).or(head) #:: recur(tail, rest)
        case _            => panic(m"the diff retains beyond the end of the sequence")

    recur(edits.to(List), sequence)


  def rdiff(similar: (element, element) => Boolean, subSize: Int = 1): RDiff[element] =
    val changes = collate.bind:
      case Tract.Unchanged(pars)    => (pars: List[Change[element]])
      case Tract.Changed(dels, Nil) => (dels: List[Change[element]])
      case Tract.Changed(Nil, inss) => (inss: List[Change[element]])

      case Tract.Changed(dels, inss) =>
        if inss.size == dels.size && inss.size <= subSize
        then
          val subs = dels.zip(inss).map: (del, ins) =>
            Sub(del.left, ins.right, del.value, ins.value)

          (subs: List[Change[element]])
        else
          // This `Diff` may have been parsed, in which case its deletions carry no values; an
          // absent value simply never compares similar, honestly degrading that pairing to a
          // plain deletion and insertion instead of vouching for a value that is not there.
          // Both runs are read positionally at indices the inner `diff` reports, so a single
          // stdlib view of each is bound rather than a bounds proof threaded through every read.
          val delsRun = dels.stdlib
          val inssRun = inss.stdlib

          val delsSeq = Sequence.from(delsRun.map(_.value))
          val inssSeq = Sequence.from(inssRun.map { ins => ins.value: Optional[element] })

          val similar2: (Optional[element], Optional[element]) ->{similar} Boolean =
            (left, right) => left.lay(false): l =>
              right.lay(false): r => similar(l, r)

          val subs = dissonance.diff(delsSeq, inssSeq, similar2).edits.to(List).map:
            case Del(index, _) => Del(delsRun(index).left, delsRun(index).value)
            case Ins(index, _) => Ins(inssRun(index).right, inssRun(index).value)

            case Par(left, right, _) =>
              val del = delsRun(left)
              val ins = inssRun(right)

              Sub(del.left, ins.right, del.value, ins.value)

          (subs: List[Change[element]])

    RDiff(changes*)

  def collate: List[Tract[element]] =
    edits.to(List).runsBy:
      case Par(_, _, _) => true
      case _            => false

    . map:
        case xs@(Par(_, _, _) :: _) =>
          Tract.Unchanged(xs.sweep {  case par: Par[element] => par })

        case xs =>
          Tract.Changed
            ( xs.sweep {  case del: Del[element] => del },
              xs.sweep {  case ins: Ins[element] => ins } )

  def chunks: Chain[Chunk[element]] =
    def recur(todo: List[Edit[element]], position: Int, rightPosition: Int)
    :   Chain[Chunk[element]] =

      todo match
        case Nil                         => Chain()
        case Par(pos2, rpos2, _) :: tail => recur(tail, pos2 + 1, rpos2 + 1)

        case _ =>
          val dels = todo.keep(_.typed[Del[element]]).sweep:
            case del: Del[element] => del

          val inss = todo.skip(dels.size).keep(_.typed[Ins[element]]).sweep:
            case ins: Ins[element] => ins

          Chunk(position, rightPosition, dels, inss) #::
            recur
              ( todo.skip(dels.size + inss.size), position + dels.size, rightPosition + inss.size )

    recur(edits.to(List), 0, 0)
