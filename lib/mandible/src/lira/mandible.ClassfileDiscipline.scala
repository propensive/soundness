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
package mandible

import anticipation.*
import contingency.*
import gossamer.*
import reliquary.*
import rudiments.*

// The `classfile/1` discipline: the JVM's *linkage* contract, as atoms.
//
// Registering it is a deliberate choice with a cost, and the cost is the reason `tasty/1` holds
// `.class` files atomless rather than doing this itself. Atoms feed the snapshot, and the
// snapshot is API identity (§12.1), so folding bytecode surface into atoms means a release whose
// source-level interface is untouched but whose bridge methods moved acquires a *different* API
// identity — breaking dependency satisfaction (§13.2) for consumers who only ever recompile.
// LIRA §11.6 records that trade-off and recommends the other side of it for an ecosystem whose
// primary contract is recompilation: `JvmProfile` computes the same predicates from the same
// atomizer without letting them near the snapshot. Register this discipline where linkage is the
// primary contract; declare the `jvm/1` profile where it is not.
//
// Registry order is load-bearing (§11.2): `Discipline.Registry` claims by first match, and
// `tasty/1` claims `.class` atomless, so this discipline must precede it —
// `Discipline.Registry(List(ClassfileDiscipline, Tasty))`. In the other order it claims nothing.
object ClassfileDiscipline extends Discipline:
  def id: Text = ClassfileAtomizer.id

  // Classfiles exist in the `jvm` universe alone; `sjsir` and `nir` have no counterpart to
  // compare against, so the cross-section invariant (§9.6) is vacuous here and correctly so. A
  // single-universe domain is exactly the case §11.2 requirement 1 exempts from it.
  def domain: Discipline.Domain = Discipline.Domain.Realms(Set(t"jvm"))

  // §11.2 requirement 4: a JVM call site names the receiver, not the declarer, so a type's
  // linkage surface includes members it inherits. Declaration keying would be unsound for a
  // discipline certifying linkage, and this one does certify it.
  def keying: Discipline.Keying = Discipline.Keying.Membership

  // Linkage only. Bytecode has erased the type arguments, variance and implicit specificity that
  // decide whether a consumer's *next compile* succeeds, so this discipline cannot certify
  // recompilation and must not claim it (§11.2 requirement 7). That level is `tasty/1`'s, and the
  // two are independent in both directions (Appendix D.1).
  def guarantees(universe: Text): Set[Discipline.Guarantee] = Set(Discipline.Guarantee.Linkage)

  def claims(path: TreePath, data: Data): Boolean = path.text.s.endsWith(".class")

  def atomize(content: List[(TreePath, Data)], context: Discipline.Context)
  :   Atomization raises DisciplineError =

    val classes = content.stdlib.map: (path, data) =>
      // Same erasure; the parser reads the bytes and retains nothing of them.
      val surface = ClassSurface(data.asInstanceOf[scala.IArray[Byte]])
      surface.name -> surface

    . to(scala.collection.immutable.Map)

    val outcome = ClassfileAtomizer.atomize(Map.of(classes), context.classpath)

    // An unresolvable supertype is fatal, never skipped: its member set is unknown, so the
    // presented set of everything below it is understated, and an understated interface is a
    // compatibility claim made over content nobody read.
    outcome.unresolved.stdlib.headOption.foreach: name =>
      abort(DisciplineError(id, DisciplineError.Reason.Unresolved(name)))

    Atomization.of(id, outcome.atoms)
