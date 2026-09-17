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

import rudiments.*
import vacuous.*

object Caption:
  object Attachment:
    // The cartographic preference: right of the point, then left, then the diagonals, then
    // directly above and below.
    val compass: List[Attachment] =
      List(East, West, Northeast, Northwest, Southeast, Southwest, North, South)

    val cardinal: List[Attachment] = List(East, West, North, South)

  // Which side of its target a label lies on, in a y-down frame, so `North` is above. `dx` and
  // `dy` are the unit offsets from the target to the label's near edge, which is what places
  // the label's footprint around its anchor point.
  enum Attachment(val dx: Int, val dy: Int):
    case North     extends Attachment(0, -1)
    case Northeast extends Attachment(1, -1)
    case East      extends Attachment(1, 0)
    case Southeast extends Attachment(1, 1)
    case South     extends Attachment(0, 1)
    case Southwest extends Attachment(-1, 1)
    case West      extends Attachment(-1, 0)
    case Northwest extends Attachment(-1, -1)
    case Center    extends Attachment(0, 0)

  // What becomes of a label that overlaps something wherever it is put: drawn where it overlaps
  // least, or not drawn at all.
  enum Fallback:
    case Overlap, Hide

  // A line from a displaced label's edge back to the target it describes.
  case class Leader(x1: Double, y1: Double, x2: Double, y2: Double)

  // The engine's answer for a caption: the anchor point to set the text at, the side of the
  // target it lies on (from which a renderer derives its own alignment), the footprint it
  // occupies, a leader if the label was moved far enough to need one, and whether it is to be
  // drawn at all.
  case class Position
    ( x:          Double,
      y:          Double,
      attachment: Attachment,
      box:        Obstacle.Box,
      leader:     Optional[Leader],
      visible:    Boolean )

  // The answer before any arrangement has happened: the label where it was asked for, on its
  // first-choice side. This is what the first pass of an `arrange` body receives.
  def provisional(caption: Caption): Position =
    val attachment = caption.sides.prim.or(Attachment.East)
    val (x, y) = caption.anchor(attachment, 0.0, 0.0)
    Position(x, y, attachment, caption.footprint(x, y, attachment), Unset, true)

// A request to place a label: its measured size, the point it describes, and how it may be set
// relative to that point. The engine sees only geometry; whoever draws the label measures it.
//
// `attachments` lists the sides the label may lie on, best first. `standoff` is the gap between
// the target and the label's near edge. `reach` is how far the label's anchor may be moved from
// its target when every side is blocked; zero pins it. `padding` is clearance demanded around
// the label. Captions with a higher `priority` are placed first, so they keep their targets.
case class Caption
  ( width:       Double,
    height:      Double,
    x:           Double,
    y:           Double,
    attachments: List[Caption.Attachment] = Caption.Attachment.compass,
    standoff:    Double                   = 0.0,
    reach:       Double                   = 0.0,
    padding:     Double                   = 0.0,
    priority:    Int                      = 0,
    fallback:    Caption.Fallback         = Caption.Fallback.Overlap ):

  // The sides to try; a caption with none is treated as one that may lie to the east.
  def sides: List[Caption.Attachment] = attachments match
    case Nil => List(Caption.Attachment.East)
    case _   => attachments

  // The anchor point of the label on the `attachment` side, displaced by (`dx`, `dy`) from the
  // target.
  def anchor(attachment: Caption.Attachment, dx: Double, dy: Double): (Double, Double) =
    (x + dx + attachment.dx*standoff, y + dy + attachment.dy*standoff)

  def footprint(anchorX: Double, anchorY: Double, attachment: Caption.Attachment)
  :   Obstacle.Box =

    Obstacle.Box.around(anchorX, anchorY, width, height, attachment)
