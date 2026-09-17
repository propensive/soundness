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

import soundness.*

import Caption.Attachment.*

object Tests extends Suite(m"Cartouche tests"):
  def diverged(using pass: Arranger.Pass): Boolean = pass.diverged

  def run(): Unit =
    val box = Obstacle.Box(0.0, 0.0, 10.0, 4.0)

    suite(m"Geometry"):
      test(m"Disjoint boxes have no overlap"):
        box.overlap(Obstacle.Box(10.0, 0.0, 5.0, 5.0))
      . assert(_ == 0.0)

      test(m"Overlapping boxes report the intersection area"):
        box.overlap(Obstacle.Box(8.0, 2.0, 5.0, 5.0))
      . assert(_ == 4.0)

      test(m"A box inside the canvas has no excess"):
        box.excess(Obstacle.Box(-1.0, -1.0, 20.0, 20.0))
      . assert(_ == 0.0)

      test(m"A box half outside the canvas has half its area in excess"):
        box.excess(Obstacle.Box(5.0, 0.0, 20.0, 20.0))
      . assert(_ == 20.0)

      test(m"A footprint to the east starts at its anchor"):
        Obstacle.Box.around(3.0, 5.0, 10.0, 4.0, East)
      . assert(_ == Obstacle.Box(3.0, 3.0, 10.0, 4.0))

      test(m"A footprint to the north ends at its anchor"):
        Obstacle.Box.around(3.0, 5.0, 10.0, 4.0, North)
      . assert(_ == Obstacle.Box(-2.0, 1.0, 10.0, 4.0))

      test(m"A centred footprint surrounds its anchor"):
        Obstacle.Box.around(5.0, 2.0, 10.0, 4.0, Center)
      . assert(_ == box)

      test(m"A line through a box overlaps by its length inside times its width"):
        Obstacle.Line(-5.0, 2.0, 15.0, 2.0, 2.0).overlap(box)
      . assert(_ == 20.0)

      test(m"A line missing a box has no overlap"):
        Obstacle.Line(-5.0, 6.0, 15.0, 6.0).overlap(box)
      . assert(_ == 0.0)

      test(m"A disc clear of a box has no overlap"):
        Obstacle.Disc(12.0, 2.0, 2.0).overlap(box)
      . assert(_ == 0.0)

      test(m"A disc near a box's corner but outside it has no overlap"):
        Obstacle.Disc(12.0, 6.0, 2.5).overlap(box)
      . assert(_ == 0.0)

      test(m"A disc over a box has an overlap"):
        Obstacle.Disc(5.0, 2.0, 2.0).overlap(box)
      . assert(_ > 0.0)

      test(m"The nearest perimeter point to an outside point is on the edge"):
        box.nearest(20.0, 2.0)
      . assert(_ == (10.0, 2.0))

    suite(m"Greedy arrangement"):
      test(m"A lone caption keeps its target"):
        arrange(position(10.0, 4.0, 50.0, 50.0))
      . assert(_ == Caption.Position(50.0, 50.0, East, Obstacle.Box(50.0, 48.0, 10.0, 4.0), Unset, true))

      test(m"The first pass answers provisionally and the second from the solution"):
        arrange:
          val one = position(10.0, 4.0, 50.0, 50.0)
          val two = position(10.0, 4.0, 50.0, 50.0)
          (one.attachment, two.attachment)
      . assert(_ == (East, West))

      test(m"A second caption at the same target takes the next side"):
        arrange:
          position(10.0, 4.0, 50.0, 50.0)
          position(10.0, 4.0, 50.0, 50.0)
      . assert(_.attachment == West)

      test(m"A caption restricted to one side is displaced instead"):
        arrange:
          position(10.0, 4.0, 50.0, 50.0, attachments = List(East))
          position(10.0, 4.0, 50.0, 50.0, attachments = List(East), reach = 20.0)
      . assert(_ == Caption.Position(50.0, 46.0, East, Obstacle.Box(50.0, 44.0, 10.0, 4.0), Unset, true))

      test(m"Displaced captions do not overlap the earlier one"):
        arrange:
          val first = position(10.0, 4.0, 50.0, 50.0, attachments = List(East))
          val second = position(10.0, 4.0, 50.0, 50.0, attachments = List(East), reach = 20.0)
          first.box.overlap(second.box)
      . assert(_ == 0.0)

      test(m"A pinned caption stays put and pushes a later one away"):
        arrange:
          val pinned = position(10.0, 4.0, 50.0, 50.0, attachments = List(East))
          val later = position(10.0, 4.0, 52.0, 50.0, reach = 20.0)
          (pinned.x, pinned.y, pinned.box.overlap(later.box), later.leader.absent)
      . assert(_ == (50.0, 50.0, 0.0, true))

      test(m"A box obstacle on the east side pushes a caption west"):
        arrange:
          avoid(Obstacle.Box(50.0, 40.0, 20.0, 20.0))
          position(10.0, 4.0, 50.0, 50.0)
      . assert(_.attachment == West)

      test(m"A disc obstacle pushes a caption off it"):
        arrange:
          avoid(Obstacle.Disc(55.0, 50.0, 3.0))
          position(10.0, 4.0, 50.0, 50.0)
      . assert(_.attachment == West)

      test(m"A line obstacle pushes a caption off it"):
        arrange:
          avoid(Obstacle.Line(55.0, 0.0, 55.0, 100.0))
          position(10.0, 4.0, 50.0, 50.0)
      . assert(_.attachment == West)

      test(m"A far displacement carries a leader ending at the target"):
        arrange:
          avoid(Obstacle.Box(0.0, 0.0, 100.0, 106.0))
          position(10.0, 4.0, 50.0, 98.0, attachments = List(South), reach = 40.0)
      . assert(_.leader == Caption.Leader(50.0, 106.0, 50.0, 98.0))

      test(m"A near displacement carries no leader"):
        arrange:
          position(10.0, 4.0, 50.0, 50.0, attachments = List(East))
          position(10.0, 4.0, 50.0, 50.0, attachments = List(East), reach = 20.0)
      . assert(_.leader == Unset)

      test(m"A later caption avoids an accepted leader"):
        arrange:
          avoid(Obstacle.Box(0.0, 0.0, 100.0, 106.0))
          val led = position(10.0, 4.0, 50.0, 98.0, attachments = List(South), reach = 40.0)
          val later = position(10.0, 4.0, 52.0, 102.0, attachments = List(South), reach = 40.0)

          val crossing = led.leader.lay(1.0): leader =>
            Obstacle.Line(leader.x1, leader.y1, leader.x2, leader.y2).overlap(later.box)

          (crossing, led.box.overlap(later.box))
      . assert(_ == (0.0, 0.0))

      test(m"A caption that fits nowhere is hidden when asked"):
        arrange:
          avoid(Obstacle.Box(-100.0, -100.0, 300.0, 300.0))
          position(10.0, 4.0, 50.0, 50.0, reach = 20.0, fallback = Caption.Fallback.Hide)
      . assert(!_.visible)

      test(m"A caption that fits nowhere is drawn where it overlaps least by default"):
        arrange:
          avoid(Obstacle.Box(0.0, 0.0, 60.0, 60.0))
          position(10.0, 4.0, 50.0, 50.0, reach = 8.0)
      . assert: position =>
          position == Caption.Position
            ( 58.0, 58.0, Southeast, Obstacle.Box(58.0, 58.0, 10.0, 4.0),
              Caption.Leader(58.0, 58.0, 50.0, 50.0), true )

      test(m"A hidden caption blocks nothing"):
        arrange:
          avoid(Obstacle.Box(0.0, 0.0, 60.0, 60.0))
          position(10.0, 4.0, 50.0, 50.0, fallback = Caption.Fallback.Hide)
          position(10.0, 4.0, 62.0, 50.0)
      . assert(_.attachment == East)

      test(m"The canvas keeps a caption inside"):
        arrange:
          canvas(Obstacle.Box(0.0, 0.0, 60.0, 60.0))
          position(10.0, 4.0, 55.0, 50.0)
      . assert(_.attachment == West)

      test(m"Padding keeps captions apart"):
        arrange:
          position(10.0, 4.0, 50.0, 50.0, attachments = List(East))
          val second = position(10.0, 4.0, 50.0, 50.0, attachments = List(East, West), reach = 20.0, padding = 2.0)
          (second.attachment, second.x)
      . assert(_ == (West, 46.0))

      test(m"Standoff moves the label away from its target"):
        arrange(position(10.0, 4.0, 50.0, 50.0, standoff = 3.0))
      . assert(_.x == 53.0)

      test(m"A higher priority caption keeps its target"):
        arrange:
          val low = position(10.0, 4.0, 50.0, 50.0, attachments = List(East), reach = 20.0)
          val high = position(10.0, 4.0, 50.0, 50.0, attachments = List(East), priority = 1)
          (low.y, high.y)
      . assert(_ == (46.0, 50.0))

      test(m"Arrangement is deterministic"):
        def run(): List[Caption.Position] = arrange:
          List
            ( position(10.0, 4.0, 50.0, 50.0, reach = 20.0),
              position(10.0, 4.0, 52.0, 51.0, reach = 20.0),
              position(10.0, 4.0, 48.0, 49.0, reach = 20.0) )

        run() == run()
      . assert(_ == true)

      test(m"The body's result is the arrangement's result"):
        arrange:
          position(10.0, 4.0, 50.0, 50.0)
          t"done"
      . assert(_ == t"done")

    suite(m"Pass identity"):
      test(m"An extra call in the second pass is answered provisionally"):
        var passes = 0

        arrange:
          passes += 1
          position(10.0, 4.0, 50.0, 50.0)
          if passes == 2 then position(10.0, 4.0, 50.0, 50.0).attachment else West
      . assert(_ == East)

      test(m"A call whose caption changed is answered provisionally"):
        var passes = 0

        arrange:
          passes += 1
          position(10.0, 4.0, 50.0, 50.0)
          position(if passes == 2 then 12.0 else 10.0, 4.0, 50.0, 50.0).attachment
      . assert(_ == East)

      test(m"Divergence is reported"):
        var passes = 0

        arrange:
          passes += 1
          position(10.0, 4.0, 50.0, 50.0)
          if passes == 2 then position(10.0, 4.0, 50.0, 50.0)
          diverged
      . assert(_ == true)

      test(m"A faithful body does not diverge"):
        arrange:
          position(10.0, 4.0, 50.0, 50.0)
          position(10.0, 4.0, 50.0, 50.0)
          diverged
      . assert(_ == false)

      test(m"A nested arrangement is independent of the outer one"):
        arrange:
          avoid(Obstacle.Box(50.0, 40.0, 20.0, 20.0))
          val outer = position(10.0, 4.0, 50.0, 50.0)
          val inner = arrange(position(10.0, 4.0, 50.0, 50.0))
          (outer.attachment, inner.attachment)
      . assert(_ == (West, East))

    suite(m"Annealing arrangement"):
      import arrangers.annealingArranger

      test(m"A lone caption keeps its target"):
        arrange(position(10.0, 4.0, 50.0, 50.0))
      . assert(_ == Caption.Position(50.0, 50.0, East, Obstacle.Box(50.0, 48.0, 10.0, 4.0), Unset, true))

      test(m"Annealing is deterministic for a fixed seed"):
        def run() = arrange:
          List.range(0, 12).map: n =>
            position(10.0, 4.0, 50.0 + n%3, 50.0 + n%4, reach = 20.0)

        run() == run()
      . assert(_ == true)

      test(m"A pinned caption never moves"):
        arrange:
          List.range(0, 6).map: n =>
            position(10.0, 4.0, 50.0 + n, 50.0, reach = 20.0)
          position(10.0, 4.0, 52.0, 50.0, attachments = List(East))
      . assert(_ == Caption.Position(52.0, 50.0, East, Obstacle.Box(52.0, 48.0, 10.0, 4.0), Unset, true))

      test(m"Annealing collides no more than greedy on a dense cluster"):
        def collisions(using Arranger.Pass^): Double =
          val positions = List.range(0, 9).map: n =>
            position(10.0, 4.0, 50.0 + n%3, 50.0 + n/3, reach = 12.0)

          var total = 0.0

          positions.each: one =>
            positions.each: other =>
              if one != other then total += one.box.overlap(other.box)

          total

        val greedy = arrange(collisions)(using Arranger.Greedy())
        val annealed = arrange(collisions)
        annealed <= greedy
      . assert(_ == true)

      test(m"Hiding removes remaining collisions"):
        arrange:
          avoid(Obstacle.Box(-100.0, -100.0, 300.0, 300.0))
          position(10.0, 4.0, 50.0, 50.0, reach = 8.0, fallback = Caption.Fallback.Hide)
      . assert(!_.visible)
