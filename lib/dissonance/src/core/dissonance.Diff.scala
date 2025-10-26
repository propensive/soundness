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
┃    Soundness, version 0.45.0.                                                                    ┃
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

import anticipation.*
import contingency.*
import fulminate.*
import prepositional.*
import proscenium.*
import rudiments.*
import turbulence.*
import vacuous.*

object Diff:
  given aggregable: Tactic[DiffError] => Diff[Text] is Aggregable by Text = parse(_)

  private def parse(lines: Stream[Text]): Diff[Text] raises DiffError =
    def recur
         (todo: Stream[Text], line: Int, edits: List[Edit[Text]], pos: Int, rpos: Int, target: Int)
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
                  case _                => raise(DiffError(line, head)) yet (0, 0)
                catch case err: NumberFormatException => raise(DiffError(line, head)) yet (0, 0)

              val pairs =
                head.s.split("[acd]").nn.to(List) match
                  case List(left, right) => Some((unpair(left.nn), unpair(right.nn)))
                  case _                 => None

              pairs match
                case Some(((leftStart, leftEnd), (rightStart, rightEnd))) =>
                  recur(tail, line + 1, edits, pos, rpos, leftStart)

                case None =>
                  raise(DiffError(line, head)) yet recur(tail, line + 1, edits, pos, rpos, 0)

          case _ =>
            Diff(edits.reverse*)


    recur(lines, 1, Nil, 0, 0, 0)

case class Diff[element](edits: Edit[element]*):
  def size: Int = edits.count:
    case Par(_, _, _) => false
    case _            => true

  def flip: Diff[Optional[element]] =
    val edits2: Seq[Edit[Optional[element]]] = edits.map:
      case Par(left, right, value) => Par(right, left, value)
      case Del(left, value)        => Ins(left, value)
      case Ins(right, value)       => Del(right, value)

    Diff(edits2*)

  def map[element2](lambda: element => element2): Diff[element2] =
    Diff(edits.map(_.map(lambda))*)


  def patch(seq: Seq[element], update: (element, element) => element = (left, right) => left)
  : Stream[element] =

      def recur(todo: List[Edit[element]], seq: Seq[element]): Stream[element] = todo match
        case Nil                   => seq.to(Stream)
        case Ins(_, value) :: tail => value #:: recur(tail, seq)
        case Del(_, _) :: tail     => recur(tail, seq.tail)

        case Par(_, _, value) :: tail =>
          value.let(update(_, seq.head)).or(seq.head) #:: recur(tail, seq.tail)

      recur(edits.to(List), seq)


  def rdiff(similar: (element, element) => Boolean, subSize: Int = 1): RDiff[element] =
    val changes = collate.flatMap:
      case Region.Unchanged(pars)    => pars
      case Region.Changed(dels, Nil) => dels
      case Region.Changed(Nil, inss) => inss

      case Region.Changed(dels, inss) =>
        if inss.length == dels.length && inss.length <= subSize
        then dels.zip(inss).map { (del, ins) => Sub(del.left, ins.right, del.value, ins.value) }
        else
          val delsSeq = dels.map(_.value.vouch).to(IndexedSeq)
          val inssSeq = inss.map(_.value).to(IndexedSeq)

          diff(delsSeq, inssSeq, similar).edits.map:
            case Del(index, _) => Del(dels(index).left, dels(index).value)
            case Ins(index, _) => Ins(inss(index).right, inss(index).value)

            case Par(left, right, _) =>
              Sub(dels(left).left, inss(right).right, dels(left).value, inss(right).value)

    RDiff(changes*)

  def collate: List[Region[element]] =
    edits.runsBy:
      case Par(_, _, _) => true
      case _            => false
    . map:
        case xs@(Par(_, _, _) :: _) => Region.Unchanged
                                        (xs.collect { case par: Par[element] => par })
        case xs                     => Region.Changed
                                        (xs.collect { case del: Del[element] => del },
                                         xs.collect { case ins: Ins[element] => ins })

  def chunks: Stream[Chunk[element]] =
    def recur(todo: List[Edit[element]], pos: Int, rpos: Int): Stream[Chunk[element]] =
      todo match
        case Nil                         => Stream()
        case Par(pos2, rpos2, _) :: tail => recur(tail, pos2 + 1, rpos2 + 1)

        case _ =>
          val dels = todo.takeWhile(_.is[Del[element]]).collect:
            case del: Del[element] => del

          val inss = todo.drop(dels.length).takeWhile(_.is[Ins[element]]).collect:
            case ins: Ins[element] => ins

          Chunk(pos, rpos, dels, inss) #::
              recur(todo.drop(dels.size + inss.size), pos + dels.length, pos + inss.length)

    recur(edits.to(List), 0, 0)
