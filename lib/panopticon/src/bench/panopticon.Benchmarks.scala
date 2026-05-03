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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import ambience.*, environments.java, systems.java
import anticipation.*
import contingency.*, strategies.throwUnsafely
import denominative.*
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContext
import probably.*
import proscenium.*
import quantitative.*
import sedentary.*
import symbolism.*
import temporaryDirectories.system
import vacuous.*

object Benchmarks extends Suite(m"Panopticon benchmarks"):
  given device: BenchmarkDevice = LocalhostDevice

  // ─── data shape ───────────────────────────────────────────────────────────

  case class Address(street: Text, city: Text, postcode: Text)
  case class Role(name: Text, count: Int)
  case class Employee(name: Text, age: Int, addr: Address, role: Role)
  case class Department(name: Text, lead: Employee, members: List[Employee])
  case class Org(name: Text, hq: Address, depts: List[Department])

  lazy val addr: Address = Address(t"1 Way", t"Townville", t"AA1")
  lazy val role: Role    = Role(t"CEO", 100)
  lazy val emp:  Employee = Employee(t"Alice", 30, addr, role)
  lazy val dept: Department = Department(t"Eng", emp, List(emp, emp, emp))
  lazy val org:  Org = Org(t"Acme", addr, List(dept, dept, dept))

  // ─── helpers (called from quoted bench bodies) ────────────────────────────

  def singleDepth4(o: Org): Org =
    o.lens(_.depts(Prim).lead.addr.city = t"X")

  def twoSharedDepth3(o: Org): Org =
    o.lens
     ( _.depts(Prim).lead.addr.city     = t"X",
       _.depts(Prim).lead.addr.postcode = t"Y" )

  def fourSharedDepth2(o: Org): Org =
    o.lens
     ( _.depts(Prim).lead.addr.city     = t"X",
       _.depts(Prim).lead.addr.postcode = t"Y",
       _.depts(Prim).lead.role.name     = t"Z",
       _.depts(Prim).lead.role.count    = 99 )

  def fourDisjoint(o: Org): Org =
    o.lens
     ( _.name                          = t"A",
       _.hq.city                       = t"B",
       _.depts(Prim).name              = t"C",
       _.depts(Prim).lead.role.count   = 99 )

  def eightMixed(o: Org): Org =
    o.lens
     ( _.depts(Prim).lead.addr.city     = t"X",
       _.depts(Prim).lead.addr.postcode = t"Y",
       _.depts(Prim).lead.role.name     = t"Z",
       _.depts(Prim).lead.role.count    = 99,
       _.hq.street                      = t"S",
       _.hq.city                        = t"C",
       _.hq.postcode                    = t"P",
       _.name                           = t"N" )

  def eachTwoLeaves(o: Org): Org =
    o.lens
     ( _.depts(Each).lead.role.name  = t"Boss",
       _.depts(Each).lead.role.count = 0 )

  // ─── field-only fusion targets (no traversals) ────────────────────────────

  // Single field-only update at depth 2 — this exercises the macro on the simplest
  // shape (one path, no fusion to perform but the same code emission as fusion).
  def singleFieldDepth2(o: Org): Org =
    o.lens(_.hq.city = t"X")

  // Three updates sharing a depth-1 prefix (`hq`). Foldleft rebuilds Address and Org
  // three times; fusion rebuilds each once.
  def threeSharedHq(o: Org): Org =
    o.lens
     ( _.hq.street   = t"S",
       _.hq.city     = t"C",
       _.hq.postcode = t"P" )

  // Three top-level disjoint updates. Foldleft rebuilds Org three times; fusion
  // rebuilds it once.
  def threeDisjointTop(o: Org): Org =
    o.lens
     ( _.name  = t"N",
       _.hq    = addr,
       _.depts = Nil )

  // Comparison baselines for the field-only fusion targets above. `Manual` uses direct
  // `.copy(...)` — the theoretical optimum any optic library should be measured against.

  def singleFieldDepth2Manual(o: Org): Org =
    o.copy(hq = o.hq.copy(city = t"X"))

  def threeSharedHqManual(o: Org): Org =
    o.copy(hq = o.hq.copy(street = t"S", city = t"C", postcode = t"P"))

  def threeDisjointTopManual(o: Org): Org =
    o.copy(name = t"N", hq = addr, depts = Nil)

  // True pre-fusion baselines — call `lensFold` (the original `def lens` body),
  // bypassing the macro entirely. Single-call multi-lambda foldLeft semantics.

  def singleFieldDepth2Fold(o: Org): Org =
    o.lensFold(_.hq.city = t"X")

  def threeSharedHqFold(o: Org): Org =
    o.lensFold
     ( _.hq.street   = t"S",
       _.hq.city     = t"C",
       _.hq.postcode = t"P" )

  def threeDisjointTopFold(o: Org): Org =
    o.lensFold
     ( _.name  = t"N",
       _.hq    = addr,
       _.depts = Nil )

  // ─── benchmarks ───────────────────────────────────────────────────────────

  def run(): Unit =
    val bench = Bench()

    suite(m"Single update"):
      bench(m"single update, depth 4")(target = 1*Second):
        '{ panopticon.Benchmarks.singleDepth4(panopticon.Benchmarks.org) }

    suite(m"Multi-update with shared prefixes"):
      bench(m"2 updates sharing depth-3 prefix")(target = 1*Second):
        '{ panopticon.Benchmarks.twoSharedDepth3(panopticon.Benchmarks.org) }

      bench(m"4 updates sharing depth-2 prefix")(target = 1*Second):
        '{ panopticon.Benchmarks.fourSharedDepth2(panopticon.Benchmarks.org) }

      bench(m"8 updates, 2 groups of 4 shared")(target = 1*Second):
        '{ panopticon.Benchmarks.eightMixed(panopticon.Benchmarks.org) }

    suite(m"Multi-update without shared prefix"):
      bench(m"4 updates, no shared prefix")(target = 1*Second):
        '{ panopticon.Benchmarks.fourDisjoint(panopticon.Benchmarks.org) }

    suite(m"Traversal"):
      bench(m"2 updates under shared Each traversal")(target = 1*Second):
        '{ panopticon.Benchmarks.eachTwoLeaves(panopticon.Benchmarks.org) }

    suite(m"Field-only fusion (no traversals)"):
      bench(m"single update — fused")(target = 1*Second):
        '{ panopticon.Benchmarks.singleFieldDepth2(panopticon.Benchmarks.org) }

      bench(m"single update — pre-fusion foldLeft")(target = 1*Second):
        '{ panopticon.Benchmarks.singleFieldDepth2Fold(panopticon.Benchmarks.org) }

      bench(m"single update — manual .copy (optimum)")(target = 1*Second):
        '{ panopticon.Benchmarks.singleFieldDepth2Manual(panopticon.Benchmarks.org) }

      bench(m"3 shared-prefix — fused")(target = 1*Second):
        '{ panopticon.Benchmarks.threeSharedHq(panopticon.Benchmarks.org) }

      bench(m"3 shared-prefix — pre-fusion foldLeft")(target = 1*Second):
        '{ panopticon.Benchmarks.threeSharedHqFold(panopticon.Benchmarks.org) }

      bench(m"3 shared-prefix — manual .copy (optimum)")(target = 1*Second):
        '{ panopticon.Benchmarks.threeSharedHqManual(panopticon.Benchmarks.org) }

      bench(m"3 disjoint — fused")(target = 1*Second):
        '{ panopticon.Benchmarks.threeDisjointTop(panopticon.Benchmarks.org) }

      bench(m"3 disjoint — pre-fusion foldLeft")(target = 1*Second):
        '{ panopticon.Benchmarks.threeDisjointTopFold(panopticon.Benchmarks.org) }

      bench(m"3 disjoint — manual .copy (optimum)")(target = 1*Second):
        '{ panopticon.Benchmarks.threeDisjointTopManual(panopticon.Benchmarks.org) }
