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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import scala.collection.immutable as sci

import anticipation.*
import contingency.*
import proscenium.compat.*
import denominative.*
import fulminate.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*

object Diff:
  given aggregable: (tactic: Tactic[DiffError])
  =>  ((Diff[Text] is Aggregable by Text)^{tactic}) = parse(_)

  private def parse(lines: Progression[Text]): Diff[Text] raises DiffError =
    def recur
      ( todo:          Progression[Text],
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
                case _                => abort(DiffError(line, head))
              catch case error: NumberFormatException => abort(DiffError(line, head))

            val pairs =
              head.s.split("[acd]").nn.iterator.to(List) match
                case List(left, right) => Some((unpair(left.nn), unpair(right.nn)))
                case _                 => None

            pairs match
              case Some(((leftStart, leftEnd), (rightStart, rightEnd))) =>
                recur(tail, line + 1, edits, position, rightPosition, leftStart)

              case None =>
                raise(DiffError(line, head))
                recur(tail, line + 1, edits, position, rightPosition, 0)

        case _ =>
          Diff(edits.reverse*)


    recur(lines, 1, Nil, 0, 0, 0)

  extension (diff: Diff[Text])
    def redraft(context: Redraft.Context = Redraft.Context.Minimal): Redraft =
      Redraft.render(diff, context)

    // The inner line list is assembled with stdlib members (the `:::` chain otherwise trips the
    // opaque-`List` alias distinctness inside the umbrella `flatMap`) and wrapped once via
    // `List.of` so the umbrella flatMap sees a `Traversable` opaque `List`.
    def serialize: Progression[Text] = diff.chunks.flatMap:
      case Chunk(left, right, dels, inss) =>
        def range(start: Int, end: Int): Text =
          s"$start${if start == end then "" else s",$end"}".tt

        val command: Text =
          if dels.nil then s"${left}a${range(right + 1, right + inss.size)}".tt
          else if inss.nil then s"${range(left + 1, left + dels.size)}d${right}".tt
          else s"${range(left + 1, left + dels.size)}c${range(right + 1, right + inss.size)}".tt

        val delSeq = dels.stdlib.map: del => Text("< "+del.value)
        val sep =
          if inss.size > 0 && dels.size > 0 then sci.List(Text("---")) else sci.List()
        val insSeq = inss.stdlib.map: ins => Text("> "+ins.value)

        List.of(command +: (delSeq ++ sep ++ insSeq))

case class Diff[element](edits: Edit[element]*):
  def size: Int = edits.count:
    case Par(_, _, _) => false
    case _            => true

  def flip: Diff[Optional[element]] =
    val edits2: List[Edit[Optional[element]]] = edits.transmute[List].map:
      case Par(left, right, value) => Par(right, left, value)
      case Del(left, value)        => Ins(left, value)
      case Ins(right, value)       => Del(right, value)

    Diff(edits2*)

  def map[element2](lambda: element => element2): Diff[element2] =
    Diff(edits.map(_.map(lambda))*)


  def patch(sequence: List[element], update: (element, element) -> element = (left, right) => left)
  :   Progression[element] =

    def recur(todo: List[Edit[element]], sequence: List[element]): Progression[element] = todo match
      case Nil                   => sequence.stdlib.transmute[Progression]
      case Ins(_, value) :: tail => value #:: recur(tail, sequence)
      case Del(_, _) :: tail     => recur(tail, sequence.tail)

      case Par(_, _, value) :: tail =>
        value.let(update(_, sequence.head)).or(sequence.head) #:: recur(tail, sequence.tail)

    recur(edits.transmute[List], sequence)


  def rdiff(similar: (element, element) => Boolean, subSize: Int = 1): RDiff[element] =
    val changes = collate.bind:
      case Region.Unchanged(pars)    => (pars: List[Change[element]])
      case Region.Changed(dels, Nil) => (dels: List[Change[element]])
      case Region.Changed(Nil, inss) => (inss: List[Change[element]])

      case Region.Changed(dels, inss) =>
        if inss.length == dels.length && inss.length <= subSize
        then
          val subs = dels.zip(inss).map: (del, ins) =>
            Sub(del.left, ins.right, del.value, ins.value)

          (subs: List[Change[element]])
        else
          val delsSeq = Series.from(dels.stdlib.map(_.value.vouch))
          val inssSeq = Series.from(inss.stdlib.map(_.value))

          val subs = dissonance.diff(delsSeq, inssSeq, similar).edits.toList.map:
            case Del(index, _) => Del(dels(index).left, dels(index).value)
            case Ins(index, _) => Ins(inss(index).right, inss(index).value)

            case Par(left, right, _) =>
              Sub(dels(left).left, inss(right).right, dels(left).value, inss(right).value)

          (List.of(subs): List[Change[element]])

    RDiff(changes*)

  def collate: List[Region[element]] =
    edits.transmute[List].runsBy:
      case Par(_, _, _) => true
      case _            => false

    . map:
        case xs@(Par(_, _, _) :: _) =>
          Region.Unchanged(xs.collect { case par: Par[element] => par })

        case xs =>
          Region.Changed
            ( xs.collect { case del: Del[element] => del },
              xs.collect { case ins: Ins[element] => ins } )

  def chunks: Progression[Chunk[element]] =
    def recur(todo: List[Edit[element]], position: Int, rightPosition: Int)
    :   Progression[Chunk[element]] =

      todo match
        case Nil                         => Progression()
        case Par(pos2, rpos2, _) :: tail => recur(tail, pos2 + 1, rpos2 + 1)

        case _ =>
          val dels = todo.takeWhile(_.typed[Del[element]]).collect:
            case del: Del[element] => del

          val inss = todo.drop(dels.length).takeWhile(_.typed[Ins[element]]).collect:
            case ins: Ins[element] => ins

          Chunk(position, rightPosition, dels, inss) #::
            recur
              ( todo.drop(dels.size + inss.size), position + dels.length, position + inss.length )

    recur(edits.transmute[List], 0, 0)
