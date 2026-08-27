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

import anticipation.*
import chiaroscuro.*
import denominative.dysasymptotics.linearSize
import digression.*
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

// The typed event stream a test run produces: the low-level counterpart of the `Report`
// accumulator, for consumers — a test-running tool such as fume — which render results
// themselves (as terminal output, HTML, or anything else). Events are RICH but WIRE-SAFE: every
// payload is a flat case class of scalars, `Optional`s, `List`s and `Map[Text, Text]`, so the
// whole enum derives a BinTEL schema and codec with no hand-written instances (nested sums,
// recursion, tuples and live `Exception`s are all excluded by construction). Recursive
// structures are flattened STRUCTURALLY, not pre-rendered: a stack trace becomes its cause
// chain in order, a `Juxtaposition` becomes depth-indexed rows from which a renderer can
// rebuild the tree, and a metrics `Ledger` becomes a list of labelled values. One deliberate
// simplification: a `Message`'s rope structure is rendered to `Text` at this boundary.
object TestEvent:
  // A test's (or suite's) identity on the wire: the stable 6-hex hash, the display name, the
  // optional moniker, the full name-or-moniker path from the root suite to this entry (`path`
  // includes the entry itself, so tree reconstruction needs no other context), and the
  // declaration site.
  case class Ref
    ( id:      Text,
      name:    Text,
      moniker: Optional[Text],
      path:    List[Text],
      file:    Text,
      line:    Int )

  object Ref:
    def of(id: Test.Id): Ref =
      def names(id: Test.Id): List[Text] =
        id.suite.let { suite => names(suite.id) }.or(Nil) :+ id.moniker.or(id.name.text)

      Ref(id.id, id.name.text, id.moniker, names(id), id.codepoint.source, id.codepoint.line)

  case class Frame(className: Text, method: Text, file: Text, line: Optional[Int])

  // One component of a flattened cause chain: the outermost exception first, each with its
  // class name, rendered message and frames.
  case class TraceComponent(className: Text, message: Text, frames: List[Frame])

  case class Trace(components: List[TraceComponent])

  object Trace:
    def of(stack: StackTrace): Trace =
      def component(stack: StackTrace): TraceComponent =
        val frames = stack.frames.map: frame =>
          Frame(frame.method.className, frame.method.method, frame.file, frame.line)

        TraceComponent(stack.className, stack.message.text, frames)

      def recur(stack: StackTrace, chain: List[TraceComponent]): List[TraceComponent] =
        stack.cause.lay((component(stack) :: chain).reverse): cause =>
          recur(cause, component(stack) :: chain)

      Trace(recur(stack, Nil))

  // A verdict without its live `Exception`: the outcome vocabulary is `pass`, `fail`,
  // `throws`, `check-throws`, `aspire-pass` or `aspire-fail`, with the stack trace present for
  // the two throwing outcomes.
  case class Outcome(outcome: Text, duration: Long, stack: Optional[Trace])

  object Outcome:
    def of(verdict: Verdict): Outcome = verdict match
      case Verdict.Pass(duration)       => Outcome(t"pass", duration, Unset)
      case Verdict.Fail(duration)       => Outcome(t"fail", duration, Unset)
      case Verdict.AspirePass(duration) => Outcome(t"aspire-pass", duration, Unset)
      case Verdict.AspireFail(duration) => Outcome(t"aspire-fail", duration, Unset)

      case Verdict.Throws(exception, duration) =>
        Outcome(t"throws", duration, Trace.of(StackTrace(exception)))

      case Verdict.CheckThrows(exception, duration) =>
        Outcome(t"check-throws", duration, Trace.of(StackTrace(exception)))

  // One row of a flattened `Juxtaposition`, pre-order with depth, from which the comparison
  // tree can be rebuilt: `kind` is `same`, `different` or `collation` (whose type name travels
  // in `difference`).
  case class CompareRow
    ( depth:      Int,
      label:      Text,
      kind:       Text,
      left:       Text,
      right:      Text,
      difference: Optional[Text] )

  object CompareRow:
    def flatten(juxtaposition: Juxtaposition): List[CompareRow] =
      def recur(label: Text, juxtaposition: Juxtaposition, depth: Int): List[CompareRow] =
        juxtaposition match
          case Juxtaposition.Same(value) =>
            List(CompareRow(depth, label, t"same", value, value, Unset))

          case Juxtaposition.Different(left, right, difference) =>
            List(CompareRow(depth, label, t"different", left, right, difference))

          case Juxtaposition.Collation(typeName, comparison, left, right) =>
            CompareRow(depth, label, t"collation", left, right, typeName)
              :: comparison.bind[List[CompareRow], CompareRow, List[CompareRow]]:
                   (entry: (Text, Juxtaposition)) => recur(entry(0), entry(1), depth + 1)

      recur(t"", juxtaposition, 0)

  // An axis coordinate: the `Axis.Spec` fields plus the `Value`, its enum flattened into three
  // `Optional`s of which exactly one is present, according to `domain` (`discrete`, `integral`
  // or `decimal`).
  case class Coordinate
    ( axis:     Text,
      domain:   Text,
      emergent: Boolean,
      discrete: Optional[Text],
      integral: Optional[Long],
      decimal:  Optional[Double] )

  object Coordinate:
    def of(spec: Axis.Spec, value: Value): Coordinate =
      val domain = spec.domain match
        case Axis.Domain.Discrete => t"discrete"
        case Axis.Domain.Integral => t"integral"
        case Axis.Domain.Decimal  => t"decimal"

      value match
        case Value.Discrete(label)  => Coordinate(spec.label, domain, spec.emergent, label, Unset, Unset)
        case Value.Integral(number) => Coordinate(spec.label, domain, spec.emergent, Unset, number, Unset)
        case Value.Decimal(number)  => Coordinate(spec.label, domain, spec.emergent, Unset, Unset, number)

    def of(coordinates: List[(Axis.Spec, Value)]): List[Coordinate] =
      coordinates.map { (coordinate: (Axis.Spec, Value)) => of(coordinate(0), coordinate(1)) }

  // One metric of a result, keyed by the `Metric` enum case's NAME (stable across versions in
  // a way its display label is not).
  case class MetricValue(metric: Text, value: Double)

  object MetricValue:
    def of(metrics: Ledger[Metric, Double]): List[MetricValue] =
      metrics.to[List].map { (entry: (Metric, Double)) => MetricValue(entry(0).toString.tt, entry(1)) }

  def kindName(kind: Entry.Kind): Text = kind match
    case Entry.Kind.Check   => t"check"
    case Entry.Kind.Bench   => t"bench"
    case Entry.Kind.Stress  => t"stress"
    case Entry.Kind.Profile => t"profile"

enum TestEvent:
  case SuiteStarted(suite: TestEvent.Ref, timestamp: Long)
  case SuiteEnded(suite: TestEvent.Ref, timestamp: Long)

  // Progress only: `TestStarted`/`TestEnded` bracket a test's execution (driving a live
  // display), while `TestCompleted` carries its result and follows `TestEnded`.
  case TestStarted(test: TestEvent.Ref, timestamp: Long)
  case TestEnded(test: TestEvent.Ref, timestamp: Long)

  case TestCompleted
    ( test:        TestEvent.Ref,
      kind:        Text,
      coordinates: List[TestEvent.Coordinate],
      outcome:     TestEvent.Outcome,
      metrics:     List[TestEvent.MetricValue],
      timestamp:   Long )

  case DetailCaptures(test: TestEvent.Ref, values: Map[Text, Text])
  case DetailCompare(test: TestEvent.Ref, expected: Text, found: Text, rows: List[TestEvent.CompareRow])
  case DetailMessage(test: TestEvent.Ref, message: Text)
  case DetailThrows(test: TestEvent.Ref, check: Boolean, stack: TestEvent.Trace)

  case BenchmarkRecorded
    ( test:          TestEvent.Ref,
      coordinates:   List[TestEvent.Coordinate],
      nanoseconds:   Long,
      iterations:    Long,
      runs:          Int,
      mean:          Double,
      min:           Double,
      max:           Double,
      sd:            Double,
      confidence:    Int,
      operationSize: Optional[Text],
      operationRate: Optional[Text],
      timestamp:     Long )

  case StrainRecorded
    ( test:        TestEvent.Ref,
      coordinates: List[TestEvent.Coordinate],
      concurrency: Int,
      operations:  Long,
      nanoseconds: Long,
      allocation:  Long,
      peakHeap:    Long,
      retained:    Long,
      gcCount:     Long,
      gcTime:      Long,
      p50:         Optional[Long],
      p90:         Optional[Long],
      p99:         Optional[Long],
      p999:        Optional[Long],
      compliance:  Optional[Double],
      sustained:   Boolean,
      timestamp:   Long )

  case HotspotsRecorded
    ( test:        TestEvent.Ref,
      coordinates: List[TestEvent.Coordinate],
      total:       Long,
      frames:      List[TestEvent.Frame],
      timestamp:   Long )

  case AnchorRecorded
    ( test:     TestEvent.Ref,
      axis:     Text,
      discrete: Optional[Text],
      integral: Optional[Long],
      decimal:  Optional[Double],
      compare:  Text,
      metric:   Text,
      mode:     Text )

  case NothingMatched(admitted: Int)
  case RunTerminated(error: TestEvent.Trace, active: List[TestEvent.Ref], timestamp: Long)
  case RunCompleted(passed: Boolean, timestamp: Long)
