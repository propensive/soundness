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
package frontier

import soundness.{every as _, *}
import gigantism.every

object Tests extends Suite(m"Frontier Tests"):
  trait Plug

  object NoPlugs:
    def explicit: Int = every[Plug].values.length
    def viaSummon: Int = summon[Every[Plug]].values.length

  object OnePlug:
    given p1: Plug = new Plug {}

    def explicit: Int = every[Plug].values.length
    def viaSummon: Int = summon[Every[Plug]].values.length

  object TwoPlugs:
    given p1: Plug = new Plug {}
    given p2: Plug = new Plug {}

    def explicit: Int = every[Plug].values.length
    def viaSummon: Int = summon[Every[Plug]].values.length

  def run(): Unit =
    test(m"every[X] returns empty Every when no givens in scope"):
      NoPlugs.explicit
    . assert(_ == 0)

    test(m"summon[Every[X]] returns empty Every when no givens in scope"):
      NoPlugs.viaSummon
    . assert(_ == 0)

    test(m"every[X] with one given in scope returns 1"):
      OnePlug.explicit
    . assert(_ == 1)

    test(m"summon[Every[X]] with one given in scope returns 1"):
      OnePlug.viaSummon
    . assert(_ == 1)

    test(m"every[X] collects two ambiguous givens in scope"):
      TwoPlugs.explicit
    . assert(_ == 2)

    test(m"summon[Every[X]] collects two ambiguous givens in scope"):
      TwoPlugs.viaSummon
    . assert(_ == 2)

    test(m"every[X] compiles cleanly with givens in scope"):
      demilitarize:
        trait Widget
        given w1: Widget = new Widget {}
        given w2: Widget = new Widget {}
        val all: Every[Widget] = every[Widget]
      . map(_.message)
    . assert(_ == Nil)

    test(m"summon[Every[X]] compiles cleanly with givens in scope"):
      demilitarize:
        trait Widget
        given w1: Widget = new Widget {}
        given w2: Widget = new Widget {}
        val all: Every[Widget] = summon[Every[Widget]]
      . map(_.message)
    . assert(_ == Nil)

    test(m"every[X] compiles cleanly when no givens are in scope"):
      demilitarize:
        trait Widget
        val all: Every[Widget] = every[Widget]
      . map(_.message)
    . assert(_ == Nil)

    test(m"catch-all is in scope via plain import soundness.* without a given selector"):
      demilitarize:
        import soundness.*
        trait Absent
        summon[Absent]
      . map(_.message)
    . assert(_.exists(_.contains("contextual value not found")))

    test(m"explainMissingContext lists classpath givens for missing implicit"):
      demilitarize:
        import frontier.context.explainMissingContext
        summon[rudiments.DecimalConverter]
      . map(_.message)
    . assert(_.exists(_.contains("decimalConverters.javaDecimalConverter")))

    test(m"explainMissingContext shows type-parameter bindings for polymorphic givens"):
      demilitarize:
        import frontier.context.explainMissingContext
        summon[Char is symbolism.Concatenable]
      . map(_.message)
    . assert(_.exists(_.contains("textual = Char")))

    // Diagnostic-behavior tests: the catch-all fails the search normally (it
    // is marked `@internal.diagnostic`, so its abort becomes the authoritative
    // failure message rather than a spurious success). In a using-clause chain
    // the surviving diagnostic is the one for the deepest missing implicit —
    // with `summon[Alpha]` ← `mkAlpha(using Beta)` ← `Beta?`, the user sees
    // the tree rooted at Beta, the innermost cause.

    test(m"explainMissingContext fires for a missing using-clause implicit"):
      demilitarize:
        import frontier.context.explainMissingContext
        trait A
        trait B
        given mkA(using B): A = new A {}
        summon[A]
      . map(_.message)
    . assert(_.exists(_.contains("contextual value not found")))

    test(m"explainMissingContext fires across a two-deep using-chain"):
      demilitarize:
        import frontier.context.explainMissingContext
        trait A
        trait B
        trait C
        given mkA(using B): A = new A {}
        given mkB(using C): B = new B {}
        summon[A]
      . map(_.message)
    . assert(_.exists(_.contains("contextual value not found")))

    test(m"explainMissingContext shows classpath alternatives at deepest in chain"):
      demilitarize:
        import frontier.context.explainMissingContext
        trait Holder
        given mkHolder(using rudiments.DecimalConverter): Holder = new Holder {}
        summon[Holder]
      . map(_.message)
    . assert: msgs =>
        msgs.exists: m =>
          m.contains("resolving") && m.contains("DecimalConverter")
          && m.contains("decimalConverters.javaDecimalConverter")

    // Regression tests for the old deferred-fail scheme, which made failing
    // searches spuriously succeed: with the catch-all in scope, constructs
    // that depend on ordinary search *failure* must still compile.

    test(m"explainMissingContext does not defeat NotGiven"):
      demilitarize:
        import frontier.context.explainMissingContext
        trait Absent
        summon[scala.util.NotGiven[Absent]]
      . filter(_.error).map(_.message)
    . assert(_ == Nil)

    test(m"explainMissingContext does not defeat default using arguments"):
      demilitarize:
        import frontier.context.explainMissingContext
        class Cfg
        def withDefault(using cfg: Cfg = new Cfg): Cfg = cfg
        withDefault
      . filter(_.error).map(_.message)
    . assert(_ == Nil)

    test(m"explainMissingContext does not defeat summonFrom fallback"):
      demilitarize:
        import frontier.context.explainMissingContext
        trait Absent
        inline def choose: Int =
          scala.compiletime.summonFrom:
            case _: Absent => 1
            case _         => 2
        val n: Int = choose
      . filter(_.error).map(_.message)
    . assert(_ == Nil)

    // The catch-all must never *succeed* as a candidate: the inliner instantiates
    // the open type variables of a tentative search (to `Any`) before the macro
    // runs, and a successful candidate would commit them. Frontier aborts even
    // when the search resolves without it, leaving inference of e.g. `join`'s
    // `element` to the compiler (#1942).

    test(m"a bare join over mapped elements compiles under import soundness.*"):
      demilitarize:
        import soundness.*
        val y = List(t"a", t"b").map(_.upper).join
      . filter(_.error).map(_.message)
    . assert(_ == Nil)

    test(m"a bare join compiles with the frontier.context catch-all in scope"):
      demilitarize:
        import frontier.context.explainMissingContext
        val y = List(t"a", t"b").map(_.upper).join
      . filter(_.error).map(_.message)
    . assert(_ == Nil)

    test(m"join with a separator compiles with the catch-all in scope"):
      demilitarize:
        import frontier.context.explainMissingContext
        val y = List(t"a", t"b").join(t", ")
      . filter(_.error).map(_.message)
    . assert(_ == Nil)

    test(m"a stdlib List joins with the catch-all in scope"):
      demilitarize:
        import frontier.context.explainMissingContext
        val y = scala.collection.immutable.List(t"a", t"b").map(_.upper).join
      . filter(_.error).map(_.message)
    . assert(_ == Nil)

    test(m"the catch-all does not change join's inferred type"):
      demilitarize:
        import frontier.context.explainMissingContext
        val y = List(t"a", t"b").join
        val z: Text = y
      . filter(_.error).map(_.message)
    . assert(_ == Nil)

    test(m"a later using clause still pins a type parameter left open earlier"):
      demilitarize:
        import frontier.context.explainMissingContext
        trait Pin[self] { type Operand }
        type PinBy[self, element] = Pin[self] { type Operand = element }
        object Pin:
          given any: [self, element] => PinBy[self, element] =
            new Pin[self] { type Operand = element }
        trait Only[t]
        object Only:
          given int: Only[Int] = new Only[Int] {}
        extension [self, element, wide >: element](value: self)
          (using pin: PinBy[self, element])
          (using only: Only[wide])
          def pinned: wide = ???
        val n = 1.pinned
        val m: Int = n
      . filter(_.error).map(_.message)
    . assert(_ == Nil)

    // `read` now resolves an ordinary `Readable` instance, so a missing
    // decoder is an ordinary failed implicit search that Frontier explains —
    // listing the `Readable` pipelines and what each still requires.
    test(m"explainMissingContext advises on a missing read instance"):
      demilitarize:
        import frontier.context.explainMissingContext
        import turbulence.*
        trait Widget
        t"data".read[Widget]
      . map(_.message)
    . assert: msgs =>
        msgs.exists: m =>
          m.contains("resolving") && m.contains("Readable")
          && m.contains("candidate") && m.contains("Aggregable")

