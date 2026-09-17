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
package cartouche

import scala.annotation.tailrec

import rudiments.*

// Runs `body` twice. The first run records every `position`, `avoid` and `canvas` call and
// answers each `position` provisionally, with the label where it was asked for; its result is
// discarded. The arranger in scope then solves the whole arrangement, and the second run answers
// each `position` call with its solved position, matched to the first run's calls by their
// order. The body therefore has to be repeatable: the same calls in the same order both times,
// with no effects beyond them. Its second result is `arrange`'s.
def arrange[result](body: (Arranger.Pass^) ?=> result)(using arranger: Arranger): result =
  val first = Arranger.Pass(false, Sequence.empty)
  body(using first)
  val (captions, obstacles, canvas) = first.recorded
  val positions = arranger.arrange(captions, obstacles, canvas)

  @tailrec
  def pair
    ( captions:  List[Caption],
      positions: List[Caption.Position],
      acc:       List[(Caption, Caption.Position)] )
  :   List[(Caption, Caption.Position)] =

    captions match
      case caption :: captions2 => positions match
        case position :: positions2 => pair(captions2, positions2, (caption, position) :: acc)
        case _                      => acc.reverse

      case _ => acc.reverse

  body(using Arranger.Pass(true, pair(captions, positions, Nil).to[Sequence]))

// Asks for a place for a label. In the first pass of `arrange` the answer is provisional; in the
// second it is the solved position.
def position(caption: Caption)(using pass: Arranger.Pass^): Caption.Position = pass.record(caption)

def position
  ( width:       Double,
    height:      Double,
    x:           Double,
    y:           Double,
    attachments: List[Caption.Attachment] = Caption.Attachment.compass,
    standoff:    Double                   = 0.0,
    reach:       Double                   = 0.0,
    padding:     Double                   = 0.0,
    priority:    Int                      = 0,
    fallback:    Caption.Fallback         = Caption.Fallback.Overlap )
  ( using pass: Arranger.Pass^ )
:   Caption.Position =

  pass.record
    ( Caption
        ( width, height, x, y, attachments, standoff, reach, padding, priority, fallback ) )

// Declares things that no label may be set over.
def avoid(obstacles: Obstacle*)(using pass: Arranger.Pass^): Unit =
  List.from(obstacles).each(pass.avoid(_))

// Declares the area labels must stay within; the last declaration in a body wins.
def canvas(box: Obstacle.Box)(using pass: Arranger.Pass^): Unit = pass.canvas(box)

package arrangers:
  given greedyArranger: Arranger = Arranger.Greedy()
  given annealingArranger: Arranger = Arranger.Annealing()
