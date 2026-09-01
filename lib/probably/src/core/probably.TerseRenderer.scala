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


import ambience.*
import anticipation.*
import contingency.*
import denominative.*
import denominative.dysasymptotics.linearAccess
import denominative.dysasymptotics.linearSize
import digression.*
import distillate.*
import escapade.*
import escritoire.*, columnAttenuation.ignoreAttenuation
import fulminate.*
import gossamer.*
import rudiments.*
import spectacular.*
import symbolism.*
import turbulence.*
import vacuous.*

import Format.measurable
import tableStyles.minimalTableStyle

import probably.decimalizers.fourDecimalPlaces

// The plain-text renderer, for machine-adjacent environments (Claude Code): the same
// document as the colour renderer, with word statuses, minimal tables and no banner.
private[probably] object TerseRenderer:
  import Doc.{Block, Datum, Document, Group, SummaryRow}
  import Report.Status

  def render(document: Document)(using stdio: Stdio, environment: Environment): Unit =
    val columns: Int = safely(Environment.columns.as[Int]).or(120)
    val totals = document.totals

    if totals.total == 0 then Out.println(t"No tests were run.")
    else
      val summary = t"${totals.passed} passed, ${totals.failed} failed, "
      val aspires = t"${totals.aspirePassed} aspire-passed, ${totals.aspireFailed} aspire-failed"
      Out.println(t"$summary$aspires, ${totals.total} total")

    document.groups.each(renderGroup(_, columns, document.verbosity.verbose))
    renderFailures(document, columns)
    renderFatal(document)

  private def time(n: Long): Teletype = figure(Format.scaled(n), Format.timeUnits)
  private def memory(n: Long): Teletype = figure(Format.scaled(n), Format.memoryUnits)

  // The unit tables and each table row below are read positionally (hence the `linearAccess`
  // import), and `keep(3)` over a stack trace measures the list (hence `linearSize`); every one
  // of those lists is a handful of elements, and the stdlib `List` this replaces indexed at
  // exactly the same cost.
  private def figure(figure: Format.Figure, units: List[Text]): Teletype =
    if figure.unit >= 3 then figure.whole.teletype
    else e"${figure.whole}.${figure.fraction} ${units.at(figure.unit.z).or(t"")}"

  private def datum(value: Datum): Teletype = value match
    case Datum.Blank                => e""
    case Datum.Gap                  => e"–"
    case Datum.Str(text)            => e"$text"
    case Datum.Hash(id)             => e"$id"
    case Datum.Title(name, depth)   => e"${t"  "*depth}$name"
    case Datum.Mark(status)         => e"${Format.statusWord(status)}"
    case Datum.Num(number)          => number.show.teletype
    case Datum.Time(nanos)          => time(nanos)
    case Datum.Memory(bytes)        => memory(bytes)
    case Datum.Rate(perSecond)      => e"$perSecond op/s"
    case Datum.Percent(fraction)    => e"${fraction*100}%"

    case Datum.Conf(percentile, basisPoints) =>
      e"P$percentile ±${Format.percent(basisPoints)}%"

    case Datum.Ratio(factor) =>
      if factor == 1.0 then e"★" else e"$factor"

    case Datum.Delta(inner, negative) =>
      val sign = if negative then e"-" else e"+"
      e"$sign${datum(inner)}"

  private def renderGroup(group: Group, columns: Int, verbose: Boolean)(using Stdio): Unit =
    Out.println(t"")
    val suiteName = group.suite.let(_.name.text).or(t"")
    if suiteName.length > 0 then Out.println(suiteName)
    group.blocks.each(renderBlock(_, columns))
    if verbose then group.detail.each(renderBlock(_, columns))

  private def renderBlock(block: Block, columns: Int)(using Stdio): Unit = block match
    case Block.Table(title, tableColumns, rows) =>
      title.let: id => Out.println(t"${id.id}  ${id.name.text}")

      val numbered: List[(Doc.Column, Ordinal)] = tableColumns.indexed

      val tableColumns2 = numbered.map: (column, index) =>
        val align = if column.numeric then TextAlignment.Right else TextAlignment.Left

        escritoire.Column[List[Datum], Teletype, Teletype](column.title.teletype, align):
          row => datum(row.at(index).or(Datum.Blank))

      Scaffold[List[Datum]](tableColumns2*)
      . tabulate(rows).grid(columns).render.each(Out.println(_))

    case Block.Sparkline(steps, sequence) =>
      val labelWidths: List[Int] = sequence.map(_.label.length)
      val stepWidths: List[Int] = steps.map(_.show.length)
      val labelWidth = labelWidths.most.or(0)
      val stepWidth = stepWidths.most.or(0) + 2

      // Hoisted out of the lambdas below: a `t"…"` interpolation evaluated inside a combinator
      // lambda trips the compiler's `wildApprox` assertion.
      val blank = t"·".pad(stepWidth, Rtl)
      def block(level: Int): Text = Format.sparkBlocks.at((level - 1).z).or(t"").pad(stepWidth, Rtl)
      def heading(step: Long): Text = step.show.pad(stepWidth, Rtl)
      def sustained(pair: (Long, Long)): Text = t"  sustained ${pair(0)} @ ${pair(1)} op/s"

      def line(label: Text, cells: Text, summary: Text): Text =
        t"  ${label.pad(labelWidth)}$cells$summary"

      val nothing = t""
      val headings: List[Text] = steps.map(heading)
      Out.println(t"  ${t"N".pad(labelWidth)}${headings.join}")

      // Left on `stdlib`: with a native `each`, the nested `map` over the sparkline's
      // `Optional` cells inside this lambda crashes the compiler's `wildApprox` assertion.
      sequence.stdlib.foreach: spark =>
        val cells: Text =
          spark.cells.map: cell =>
            // The `Optional` element is bound to a typed local first, for the same reason.
            val value: Optional[(Int, Boolean)] = cell
            value.lay(blank): (level, _) => block(level)

          . join

        val summary: Text = spark.sustained.lay(nothing)(sustained(_))
        Out.println(line(spark.label, cells, summary))

      Out.println(t"")

    case Block.Histogram(title, total, frames) =>
      title.let: id => Out.println(t"${id.id}  ${id.name.text}")

      val samples: List[Long] = frames.map(_.samples)
      val max = samples.most.or(0L)

      def name(frame: Hotspots.Frame): Text =
        val method = StackTrace.Method(frame.className, frame.method)
        val cls = if method.cls.starts(t"Ξ") then method.cls.skip(1) else method.cls
        t"${method.prefix}.$cls#${frame.method}"

      val widths: List[Int] = frames.map(name(_).length)
      val width = widths.most.or(0)

      frames.each: frame =>
        val percent = Format.percent(Format.basisPoints(frame.samples.toDouble, total.toDouble))
        val bar = Format.bar(frame.samples, max)
        Out.println(t"  ${name(frame).pad(width, Rtl)} ${percent.pad(6, Rtl)}% $bar")

  // Hoisted out of the `each` lambda in `renderFailures`: a `t"…"` interpolation evaluated
  // inside a combinator lambda trips the compiler's `wildApprox` assertion.
  private def failureHeading(row: SummaryRow): Text =
    val location = t"${row.id.codepoint.source}:${row.id.codepoint.line}"
    t"${row.id.id}  ${row.id.name.text} @ $location"

  private def capture(expr: Text, value: Text): Text = t"  $expr = ${Format.abbreviate(value)}"

  private def formatFrame(frame: StackTrace.Frame): Text =
    val ln = frame.line.let(_.toString.tt).or(t"?")
    t"  at ${frame.displaySegment}.${frame.displayMethod} (${frame.file}:$ln)"

  private def renderFailures(document: Document, columns: Int)(using Stdio): Unit =
    val failureStatuses: Set[Status] =
      Set(Status.Fail, Status.Throws, Status.CheckThrows, Status.Mixed)

    val failures: List[SummaryRow] =
      document.results.filter: row => failureStatuses.has(row.status)

    if !failures.nil then
      Out.println(t"")

      Scaffold[SummaryRow]
        ( escritoire.Column(e"Hash"): row =>
            e"${row.id.id}",
          escritoire.Column(e"Test"): row =>
            val depth = row.id.suite.let(_.id.depth).or(0)
            e"${t"  "*depth}${row.id.name}",
          escritoire.Column(e"Status"): row =>
            e"${Format.statusWord(row.status)}" )

      . tabulate(failures).grid(columns).render.each(Out.println(_))

      Out.println(t"")

      val details: Map[Test.Id, List[Verdict.Detail]] = document.failures.to[Map]

      failures.each: row =>
        Out.println(failureHeading(row))

        // Left on `stdlib`: this is a second traversal inside a lambda that already runs one,
        // and its body interpolates; a native `each` here trips `wildApprox`.
        details(row.id).or(Nil).stdlib.foreach: detail =>
          detail match
            case Verdict.Detail.Throws(err) =>
              Out.println:
                t"  threw ${err.component}.${err.className}: ${Format.abbreviate(err.message.text)}"

              err.crop(t"probably.Runner", t"run()").frames.keep(3).each: frame =>
                Out.println(formatFrame(frame))

            case Verdict.Detail.CheckThrows(err) =>
              val message = Format.abbreviate(err.message.text)
              Out.println(t"  check threw ${err.component}.${err.className}: $message")

              err.crop(t"probably.Verdict#", t"apply()").frames.keep(3).each: frame =>
                Out.println(formatFrame(frame))

            case Verdict.Detail.Compare(expected, observed, _) =>
              Out.println(t"  expected: ${Format.abbreviate(expected.sub(t"\n", t" "))}")
              Out.println(t"  observed: ${Format.abbreviate(observed.sub(t"\n", t" "))}")

            case Verdict.Detail.Message(text) =>
              Out.println(t"  ${Format.abbreviate(text)}")

            case Verdict.Detail.Captures(captures) =>
              captures.each: (expr, value) => Out.println(capture(expr, value))

        Out.println(t"")

  private def renderFatal(document: Document)(using Stdio): Unit =
    document.fatal.let: (error, active) =>
      val activeNames = active.to[List].map(_.name.text).join(t", ")
      val errorClass = Option(error.getClass.getName).map(_.nn.tt).getOrElse(t"")
      val msg = Option(error.getMessage).map(_.nn.tt).getOrElse(t"")

      if active.nil then Out.println(t"FATAL: $errorClass: $msg")
      else Out.println(t"FATAL in $activeNames: $errorClass: $msg")

      StackTrace(error).frames.keep(3).each: frame => Out.println(formatFrame(frame))
