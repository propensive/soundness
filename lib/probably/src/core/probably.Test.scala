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

import scala.deriving.*
import scala.math
import scala.reflect
import anticipation.*
import digression.*
import distillate.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import hypotenuse.*
import nomenclature.*
import rudiments.*
import vacuous.*

object Test:
  // The `Test` may capture (its block can close over a capability); the methods accept a capturing
  // receiver so a test whose block uses a capability still type-checks, while `test` (the asserted
  // result type) stays whatever the block produced — pure, when the block produces a pure value.
  extension [test](test: Test[test]^)
    inline def aspire(inline predicate: test => Boolean): Unit =
      ${probably.internal.aspire[test]('test, 'predicate)}

    inline def aspire(): Unit =
      ${probably.internal.aspire[test]('test, '{probably.internal.succeed})}

    inline def assert(inline predicate: test => Boolean): Unit =
      ${probably.internal.assert[test]('test, 'predicate)}

    inline def check(inline predicate: test => Boolean): test =
      ${probably.internal.check[test]('test, 'predicate)}

    inline def assert(): Unit =
      ${probably.internal.assert[test]('test, '{probably.internal.succeed})}

    inline def check(): test =
      ${probably.internal.check[test]('test, '{probably.internal.succeed})}

    inline def matches(inline pf: test ~> Any): Unit = assert(pf.isDefinedAt(_))

  // TestId → Test.Id
  object Id:
    given ordering: Ordering[Test.Id] =
      math.Ordering.Implicits.seqOrdering[scala.collection.immutable.List, Text]
      . on(_.ids.stdlib.reverse)

  case class Id
    ( name:      Message,
      suite:     Optional[Testable],
      codepoint: Codepoint,
      moniker:   Optional[Name[Probing]] = Unset ):
    val timestamp: Long = System.currentTimeMillis

    import textMetrics.uniformMetric
    lazy val id: Text = (suite.lay(0)(_.hashCode) ^ name.hashCode).hex.pad(6, Rtl, '0').keep(6, Rtl)
    lazy val ids: List[Text] = id :: suite.let(_.id.ids).or(Nil)

    // The test block may capture a capability (e.g. an error tactic, a decoder); that capture
    // lands on the returned `Test` (`^{context}`), NOT on `result` — so a test that asserts a
    // *pure* value (`Text`, `Optional`, …) computed via a capability does not spuriously stamp
    // `^` on that pure result type at the `assert`/`check` site.
    def apply[result](context: Harness ?=> result): (Test[result])^{context} =
      Test[result](this, context(using _))

    // Spreads the test over one axis: the body is evaluated per axis value (an enum
    // companion object, a value list, or a range), producing one cell per defined value; a
    // partial body (`{ case … }`) leaves gaps at the values it does not define.
    def over[value, result](axis: Axis[value])(action: Harness ?=> (value ~> result))
    :   Spread[value, result]^{action} =

      Spread(this, axis, action(using _))

    def over[value <: reflect.Enum: Enumerable, result](companion: { def values: scala.Array[value] })
      ( action: Harness ?=> (value ~> result) )
    :   Spread[value, result]^{action} =

      Spread(this, Axis(companion), action(using _))

    // Spreads the test over two axes, producing a sparse grid of cells.
    def over[left, right, result](first: Axis[left], second: Axis[right])
      ( action: Harness ?=> (((left, right)) ~> result) )
    :   Spread2[left, right, result]^{action} =

      Spread2(this, first, second, action(using _))

    def over[left <: reflect.Enum: Enumerable, right, result]
      ( first: { def values: scala.Array[left] }, second: Axis[right] )
      ( action: Harness ?=> (((left, right)) ~> result) )
    :   Spread2[left, right, result]^{action} =

      Spread2(this, Axis(first), second, action(using _))

    def over[left, right <: reflect.Enum: Enumerable, result]
      ( first: Axis[left], second: { def values: scala.Array[right] } )
      ( action: Harness ?=> (((left, right)) ~> result) )
    :   Spread2[left, right, result]^{action} =

      Spread2(this, first, Axis(second), action(using _))

    def over[left <: reflect.Enum: Enumerable, right <: reflect.Enum: Enumerable, result]
      ( first: { def values: scala.Array[left] }, second: { def values: scala.Array[right] } )
      ( action: Harness ?=> (((left, right)) ~> result) )
    :   Spread2[left, right, result]^{action} =

      Spread2(this, Axis(first), Axis(second), action(using _))

    def depth: Int = suite.let(_.id.depth).or(0) + 1

case class Test[+result](id: Test.Id, action: Harness => result)
