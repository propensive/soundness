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
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package stratiform

import soundness.*

import proscenium.compat.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import charEncoders.utf8Encoder
import denominative.asymptotics.linearSizeComplexity

// Corpus-driven schema-validity (E2xx) and validation (E3xx) conformance:
// the sibling of `stratiform_test.scala`'s "Negative corpus (E1xx parsing)"
// suite, for the codes that require the schema component. Each negative
// case is parsed, its governing schema resolved by the corpus fixture
// convention (the tels URL selects the built-in axiom; any other URL tail
// names a sibling `<name>.tel` / `_<name>.tel` schema document), and every
// error raised during schema construction, schema validity checking, and
// type assignment is collected. The case passes when the collected codes
// meet the fixture's expected codes.
object SchemaCorpusTests extends Suite(m"Stratiform schema corpus tests"):

  // Codes this implementation can raise. Corpus cases whose expected codes
  // fall wholly outside this set are pending-implementation and skipped,
  // mirroring the E1xx suite's `< 200` gate. E312/E313 are excluded: codec
  // rejection needs a configured binding, which the corpus convention
  // cannot supply.
  val implemented: scala.collection.immutable.Set[Int] =
    scala.collection.immutable.Set
      ( 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217,
        218, 219, 220, 221,
        301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 314 )

  case class Collected(codes: List[Int] = Nil)(using Diagnostics)
  extends Error(m"${codes.length} collected codes"):
    def +(code: Int): Collected = Collected(codes :+ code)

  // The codes accrued type-assigning and validator-checking `tel` under
  // the composed `schema`.
  private def assignCodes(tel: Tel, schema: Tels): List[Int] =
    validate[Tel.Focus](Collected()):
      case error: Tel.Error => accrual + error.reason.number
    . protect:
        Tel.Type.assign(tel, schema, Tel.Validator.Registry.builtins)
        ()
    . codes

  // Every error code observable from a corpus document: E3xx from
  // assigning it under its governing schema, and — when the document is
  // itself a schema document (the tels URL) — E2xx from constructing and
  // checking the schema it defines. Schema-validity errors abort, so the
  // first is captured; assignment errors accrue.
  private def collectCodes(testcase: CorpusLoader.Case, category: Text): List[Int] =
    val tel = testcase.source.read[Tel]
    val document = testcase.source.utf8.load[Tel]
    val tail = document.metadata.pragma.let(_.schema).let(CorpusLoader.urlTail(_))

    tail.let: tail =>
      if tail == t"tels" then
        val assigned = assignCodes(tel, Tels.Axiom.tels)

        val constructed: scala.List[Int] =
          try
            Tels.Validation.validate(Tels.Reconstructor.fromTel(tel))
            scala.Nil
          catch case error: Tel.Error => scala.List(error.reason.number)

        proscenium.List.from(assigned.stdlib ::: constructed)
      else
        CorpusLoader.auxiliarySchema(category, tail).let: aux =>
          try assignCodes(tel, Tels.Validation.validate(Tels.Reconstructor.fromTel(aux.read[Tel])))
          catch case error: Tel.Error => proscenium.List(error.reason.number)
        . or(proscenium.List.empty[Int])
    . or(proscenium.List.empty[Int])

  def run(): Unit =
    suite(m"Negative corpus (E2xx schema validity, E3xx validation)"):
      CorpusLoader.negative.each: testcase =>
        val codes = CorpusLoader.expectedCodes(testcase)
        if codes.stdlib.nonEmpty && codes.stdlib.forall(_ >= 200)
            && codes.stdlib.exists(implemented.contains)
        then
          test(m"raises an expected error on ${testcase.stem}"):
            collectCodes(testcase, t"neg").stdlib.exists(codes.has(_))
          . assert(_ == true)

    suite(m"Positive corpus (schema-bearing cases stay clean)"):
      // Positive fixtures governed by a resolvable schema must produce no
      // errors at all through schema checking and assignment.
      CorpusLoader.positive.each: testcase =>
        test(m"no schema or validation errors on ${testcase.stem}"):
          collectCodes(testcase, t"pos")
        . assert(_.stdlib.isEmpty)
