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
package probably

import java.util.concurrent.atomic as juca

import anticipation.*
import denominative.*
import gossamer.*
import rudiments.*
import vacuous.*

// A failed assertion is diagnosed AFTER it has failed, and only then. An assertion site holds no
// rendering logic: it records where its predicate is written and the values of the locals the
// predicate closes over. When the predicate fails, a `Diagnostician` — if one is installed, and
// the host asked for a diagnosis — recovers the predicate from the suite's own TASTy, compiles
// an instrumented copy of it, and applies that to the value which actually failed. The test's
// body is never run again.
object Diagnosis:
  // The ways a host may ask for a failure to be presented. Which typeclass renders a value is
  // decided when the diagnosis is compiled, so a suite contributes instances without knowing
  // whether any front-end will ask for them.
  enum Presentation:
    // Each value through `Inspectable`, falling back to `Showable`.
    case Inspect
    // Each value through `Showable`, falling back to `Inspectable`.
    case Show
    // For a predicate which is an equality, a structural comparison of its two sides.
    case Contrast
    // Every intermediate value of the predicate, as a tree.
    case Dissect

  object Request:
    val none: Request = Request(Nil)

    // A comma-separated list of presentations, as `--diagnose=dissect,contrast` carries it;
    // a word that names none is ignored.
    def parse(text: Text): Request =
      Request:
        text.cut(t",").flatMap:
          case t"inspect"  => List(Presentation.Inspect)
          case t"show"     => List(Presentation.Show)
          case t"contrast" => List(Presentation.Contrast)
          case t"dissect"  => List(Presentation.Dissect)
          case _           => Nil

  case class Request(presentations: List[Presentation]):
    def nil: Boolean = presentations.nil
    def has(presentation: Presentation): Boolean = presentations.has(presentation)

  // Where a predicate is written: the binary name of the top-level class enclosing it, and
  // the offsets of its first and last characters in that class's source file, which are the
  // offsets its tree has in the class's TASTy. `source` is the predicate as written.
  case class Site(owner: Text, start: Int, end: Int, source: Text):
    def known: Boolean = start >= 0 && end > start && owner != t""

object Diagnostician:
  private val installed: juca.AtomicReference[Diagnostician | Null] = juca.AtomicReference(null)

  // Makes `diagnostician` the one which failed assertions in this classloader are taken to.
  // A host does this before it runs a suite, when it has an engine to offer.
  def install(diagnostician: Diagnostician): Unit = installed.set(diagnostician)

  def apply(): Optional[Diagnostician] = installed.get() match
    case null                         => Unset
    case diagnostician: Diagnostician => diagnostician

trait Diagnostician:
  // The details of a failure at `site`, whose predicate was applied to `value` with the
  // locals in `captures` in scope, presented as `request` asks. An engine which cannot
  // diagnose the failure answers `Nil`; it never throws.
  def diagnose
    ( site: Diagnosis.Site, value: Any, captures: List[(Text, Any)], request: Diagnosis.Request )
  :   List[Verdict.Detail]
