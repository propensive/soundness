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
package panopticon

import scala.quoted.*

import ambience.*, environments.javaBaseEnvironment, systems.javaBaseSystem
import anticipation.*
import contingency.*, strategies.throwUnsafely
import denominative.*
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContextClassloader
import probably.*
import proscenium.*
import quantitative.*
import rudiments.*
import sedentary.*
import symbolism.*
import temporaryDirectories.systemTemporaryDirectory
import vacuous.*

object Benchmarks extends Suite("optics-bench", m"Panopticon benchmarks"):
  given device: BenchmarkDevice = LocalhostDevice

  // ─── data shape ───────────────────────────────────────────────────────────

  case class Address(street: Text, city: Text, postcode: Text)
  case class Part(name: Text, lines: Int)
  case class Player(name: Text, age: Int, addr: Address, part: Part)
  case class Troupe(name: Text, lead: Player, players: List[Player])
  case class Theatre(name: Text, home: Address, troupes: List[Troupe])

  lazy val addr:    Address = Address(t"The Wood", t"Athens", t"AT1")
  lazy val part:    Part    = Part(t"Pyramus", 100)
  lazy val player:  Player  = Player(t"Bottom", 30, addr, part)
  lazy val troupe:  Troupe  = Troupe(t"Mechanicals", player, List(player, player, player))
  lazy val theatre: Theatre = Theatre(t"Palace", addr, List(troupe, troupe, troupe))

  // ─── optic applications ───────────────────────────────────────────────────

  // Each `lens` and `lensFold` call is made here rather than inside a quoted bench body: the
  // optic for each path segment comes from `Optic.deref`, a transparent inline macro given,
  // which a quote cannot expand.

  def singleDepth4(o: Theatre): Theatre =
    o.lens(_.troupes(Prim).lead.addr.city = t"X")

  def twoSharedDepth3(o: Theatre): Theatre =
    o.lens
      ( _.troupes(Prim).lead.addr.city     = t"X",
        _.troupes(Prim).lead.addr.postcode = t"Y" )

  def fourSharedDepth2(o: Theatre): Theatre =
    o.lens
      ( _.troupes(Prim).lead.addr.city     = t"X",
        _.troupes(Prim).lead.addr.postcode = t"Y",
        _.troupes(Prim).lead.part.name     = t"Z",
        _.troupes(Prim).lead.part.lines    = 99 )

  def fourDisjoint(o: Theatre): Theatre =
    o.lens
      ( _.name                            = t"A",
        _.home.city                       = t"B",
        _.troupes(Prim).name              = t"C",
        _.troupes(Prim).lead.part.lines   = 99 )

  def eightMixed(o: Theatre): Theatre =
    o.lens
      ( _.troupes(Prim).lead.addr.city     = t"X",
        _.troupes(Prim).lead.addr.postcode = t"Y",
        _.troupes(Prim).lead.part.name     = t"Z",
        _.troupes(Prim).lead.part.lines    = 99,
        _.home.street                      = t"S",
        _.home.city                        = t"C",
        _.home.postcode                    = t"P",
        _.name                             = t"N" )

  def eachTwoLeaves(o: Theatre): Theatre =
    o.lens
      ( _.troupes(Each).lead.part.name  = t"Wall",
        _.troupes(Each).lead.part.lines = 0 )

  // Same updates routed through the pre-fusion `lensFold` (the original `def lens`
  // body). Used to measure the speedup of singleton-traversal fusion.
  def eachTwoLeavesFold(o: Theatre): Theatre =
    o.lensFold
      ( _.troupes(Each).lead.part.name  = t"Wall",
        _.troupes(Each).lead.part.lines = 0 )

  // ─── field-only fusion targets (no traversals) ────────────────────────────

  // Single field-only update at depth 2 — this exercises the macro on the simplest
  // shape (one path, no fusion to perform but the same code emission as fusion).
  def singleFieldDepth2(o: Theatre): Theatre =
    o.lens(_.home.city = t"X")

  // Three updates sharing a depth-1 prefix (`home`). Foldleft rebuilds Address and Theatre
  // three times; fusion rebuilds each once.
  def threeSharedHome(o: Theatre): Theatre =
    o.lens
      ( _.home.street   = t"S",
        _.home.city     = t"C",
        _.home.postcode = t"P" )

  // Three top-level disjoint updates. Foldleft rebuilds Theatre three times; fusion
  // rebuilds it once.
  def threeDisjointTop(o: Theatre): Theatre =
    o.lens
      ( _.name    = t"N",
        _.home    = addr,
        _.troupes = Nil )

  // True pre-fusion baselines — call `lensFold` (the original `def lens` body),
  // bypassing the macro entirely. Single-call multi-lambda foldLeft semantics.

  def singleFieldDepth2Fold(o: Theatre): Theatre =
    o.lensFold(_.home.city = t"X")

  def threeSharedHomeFold(o: Theatre): Theatre =
    o.lensFold
      ( _.home.street   = t"S",
        _.home.city     = t"C",
        _.home.postcode = t"P" )

  def threeDisjointTopFold(o: Theatre): Theatre =
    o.lensFold
      ( _.name    = t"N",
        _.home    = addr,
        _.troupes = Nil )

  // ─── benchmarks ───────────────────────────────────────────────────────────

  def run(): Unit =
    val bench = Bench()

    suite(m"Single update"):
      bench(m"single update, depth 4")(target = 1*Second):
        '{ panopticon.Benchmarks.singleDepth4(panopticon.Benchmarks.theatre) }

    suite(m"Multi-update with shared prefixes"):
      bench(m"2 updates sharing depth-3 prefix")(target = 1*Second):
        '{ panopticon.Benchmarks.twoSharedDepth3(panopticon.Benchmarks.theatre) }

      bench(m"4 updates sharing depth-2 prefix")(target = 1*Second):
        '{ panopticon.Benchmarks.fourSharedDepth2(panopticon.Benchmarks.theatre) }

      bench(m"8 updates, 2 groups of 4 shared")(target = 1*Second):
        '{ panopticon.Benchmarks.eightMixed(panopticon.Benchmarks.theatre) }

    suite(m"Multi-update without shared prefix"):
      bench(m"4 updates, no shared prefix")(target = 1*Second):
        '{ panopticon.Benchmarks.fourDisjoint(panopticon.Benchmarks.theatre) }

    // The manual rows use direct `.copy(...)`: the optimum any optic library should be measured
    // against.
    suite(m"Traversal"):
      bench(m"Each ×2 — fused")(target = 1*Second):
        '{ panopticon.Benchmarks.eachTwoLeaves(panopticon.Benchmarks.theatre) }

      bench(m"Each ×2 — pre-fusion foldLeft")(target = 1*Second):
        '{ panopticon.Benchmarks.eachTwoLeavesFold(panopticon.Benchmarks.theatre) }

      bench(m"Each ×2 — manual .copy + .map (optimum)")(target = 1*Second):
        '{
            val theatre = panopticon.Benchmarks.theatre

            theatre.copy(troupes = theatre.troupes.map { troupe =>
              val part = troupe.lead.part
              val lead = troupe.lead
              troupe.copy(lead = lead.copy(part = part.copy(name = t"Wall", lines = 0)))
            })
        }

    suite(m"Field-only fusion (no traversals)"):
      bench(m"single update — fused")(target = 1*Second):
        '{ panopticon.Benchmarks.singleFieldDepth2(panopticon.Benchmarks.theatre) }

      bench(m"single update — pre-fusion foldLeft")(target = 1*Second):
        '{ panopticon.Benchmarks.singleFieldDepth2Fold(panopticon.Benchmarks.theatre) }

      bench(m"single update — manual .copy (optimum)")(target = 1*Second):
        '{
            val theatre = panopticon.Benchmarks.theatre
            theatre.copy(home = theatre.home.copy(city = t"X"))
        }

      bench(m"3 shared-prefix — fused")(target = 1*Second):
        '{ panopticon.Benchmarks.threeSharedHome(panopticon.Benchmarks.theatre) }

      bench(m"3 shared-prefix — pre-fusion foldLeft")(target = 1*Second):
        '{ panopticon.Benchmarks.threeSharedHomeFold(panopticon.Benchmarks.theatre) }

      bench(m"3 shared-prefix — manual .copy (optimum)")(target = 1*Second):
        '{
            val theatre = panopticon.Benchmarks.theatre
            theatre.copy(home = theatre.home.copy(street = t"S", city = t"C", postcode = t"P"))
        }

      bench(m"3 disjoint — fused")(target = 1*Second):
        '{ panopticon.Benchmarks.threeDisjointTop(panopticon.Benchmarks.theatre) }

      bench(m"3 disjoint — pre-fusion foldLeft")(target = 1*Second):
        '{ panopticon.Benchmarks.threeDisjointTopFold(panopticon.Benchmarks.theatre) }

      bench(m"3 disjoint — manual .copy (optimum)")(target = 1*Second):
        '{
            val theatre = panopticon.Benchmarks.theatre
            theatre.copy(name = t"N", home = panopticon.Benchmarks.addr, troupes = Nil)
        }
