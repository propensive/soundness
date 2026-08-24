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

import scala.math


import anticipation.*
import gossamer.*
import rudiments.*
import vacuous.*
import denominative.*
import symbolism.*
import denominative.dysasymptotics.linearSize

// The single structural pass over a report: builds the renderer-agnostic `Doc.Document`
// consumed by both output modes. All decisions about WHAT appears in a report are made
// here; the renderers decide only how it looks.
private[probably] object Documenting:
  import Doc.*
  import Report.Status

  // The verbosity of every report. Both renderers take it from the document, so introducing
  // a verbose mode later means sourcing this from a flag or an environment variable here,
  // and changing nothing else.
  private def verbosity: Verbosity = Verbosity.Summary

  def document(report: Report): Document =
    // Every entry, for the totals, which count a measurement as a pass; and the subset that
    // the results table renders, which excludes them (see `summaries`).
    val counted = summaries(report.lines, measurements = true)
    val results = summaries(report.lines, measurements = false)
    val counts = counted.stdlib.groupBy(_.status).view.mapValues(_.size).toMap - Status.Suite

    val passed: Int =
      List(Status.Pass, Status.Bench, Status.Stress, Status.Profile)
      . stdlib.map(counts.getOrElse(_, 0)).sum

    val aspirePassed: Int = counts.getOrElse(Status.AspirePass, 0)
    val aspireFailed: Int = counts.getOrElse(Status.AspireFail, 0)
    val total: Int = counts.values.sum
    val failed: Int = total - passed - aspirePassed - aspireFailed

    val groups =
      List(Entry.Kind.Check, Entry.Kind.Bench, Entry.Kind.Stress, Entry.Kind.Profile)
      . flatMap(suiteGroups(report.lines, _))

    val failures =
      report.details.toList.sortBy(_(0).timestamp).map:
        (pair: (Test.Id, scala.collection.mutable.ArrayBuffer[Verdict.Detail])) =>
          (pair(0), pair(1).to(List))
      . to(List)

    Document
      ( results,
        Totals(passed, failed, aspirePassed, aspireFailed),
        groups,
        failures,
        report.failure,
        verbosity )

  // One row per suite and per entry, in declaration order; a `Check` entry's runs are
  // aggregated across all its cells into a single status and duration statistics.
  //
  // A measurement entry has no count and no timing statistics — a benchmark's figures are
  // its own, and a stress test's are a curve — so its results-table row can only ever be a
  // name followed by four empty columns, while its real results are presented in full by
  // its blocks. With `measurements` unset, such entries are therefore omitted, as are the
  // suites which their omission leaves empty. The totals still count them, so the caller
  // asks for them included when counting and excluded when rendering.
  private def summaries(line: ReportLine, measurements: Boolean): List[SummaryRow] =
    line match
      case ReportLine.Suite(suite, tests) =>
        val rest: List[SummaryRow] =
          tests.list.stdlib.sortBy(_(0).timestamp).flatMap: (_, line) =>
            summaries(line, measurements).stdlib
          . to(List)

        if suite.absent || rest.nil && !measurements then rest
        else SummaryRow(Status.Suite, suite.option.get.id, 0, 0L, 0L, 0L) :: rest

      case ReportLine.Item(entry) => entry.kind match
        case Entry.Kind.Bench   => measured(Status.Bench, entry, measurements)
        case Entry.Kind.Stress  => measured(Status.Stress, entry, measurements)
        case Entry.Kind.Profile => measured(Status.Profile, entry, measurements)

        case Entry.Kind.Check =>
          val verdicts = entry.cells.stdlib.flatMap(_(1).runs.stdlib).flatMap(_.verdict.option)

          if verdicts.isEmpty then Nil else
            val durations = verdicts.map(_.duration)
            val avg = durations.sum/durations.length
            val status = verdictStatus(verdicts.to(List))

            List
              ( SummaryRow
                  (status, entry.id, verdicts.length, durations.min, durations.max, avg) )

  // A measurement entry's results-table row: present only when measurements are counted.
  private def measured(status: Status, entry: Entry, measurements: Boolean): List[SummaryRow] =
    if measurements then List(SummaryRow(status, entry.id, 0, 0L, 0L, 0L)) else Nil

  // The collective status of a set of verdicts: their common status, or `Mixed` when they
  // disagree.
  private def verdictStatus(verdicts: List[Verdict]): Status =
    if verdicts.all(_.typed[Verdict.Pass]) then Status.Pass
    else if verdicts.all(_.typed[Verdict.Fail]) then Status.Fail
    else if verdicts.all(_.typed[Verdict.Throws]) then Status.Throws
    else if verdicts.all(_.typed[Verdict.CheckThrows]) then Status.CheckThrows
    else if verdicts.all(_.typed[Verdict.AspirePass]) then Status.AspirePass
    else if verdicts.all(_.typed[Verdict.AspireFail]) then Status.AspireFail
    else Status.Mixed

  private def cellStatus(cell: Tally): Status =
    verdictStatus(cell.runs.stdlib.flatMap(_.verdict.option).to(List))

  // Measurement entries group by their immediate suite, one `Group` per suite and kind, in
  // declaration order; nested suites follow their parents.
  private def suiteGroups(line: ReportLine.Suite, kind: Entry.Kind): List[Group] =
    val children = (line.tests.list.stdlib.sortBy(_(0).timestamp)).to(List)

    val entries = children.flatMap: (_, child) =>
      child.absolve match
        case ReportLine.Item(entry) => if entry.kind == kind then List(entry) else Nil
        case _: ReportLine.Suite    => Nil

    val nested = children.flatMap: (_, child) =>
      child.absolve match
        case suite: ReportLine.Suite => suiteGroups(suite, kind)
        case _: ReportLine.Item      => Nil

    val here =
      if entries.nil then Nil else
        val (headline, detail) = blocks(kind, entries)

        if headline.nil && detail.nil then Nil
        else List(Group(line.suite, kind, headline, detail))

    here + nested

  // A kind's blocks, separated into those always rendered and those held back for verbose
  // output. Only stress groups currently distinguish the two.
  private def blocks(kind: Entry.Kind, entries: List[Entry]): (List[Block], List[Block]) =
    kind match
      case Entry.Kind.Bench   => (benchBlocks(entries), Nil)
      case Entry.Kind.Stress  => stressBlocks(entries)
      case Entry.Kind.Profile => (entries.map(histogram), Nil)

      // Only axial unit tests need their own blocks (a table or grid of per-cell statuses);
      // ordinary tests are fully described by the results table.
      case Entry.Kind.Check   => (entries.filter(!_.axes.nil).map(axialCheck), Nil)

  // The first (usually only) run of a cell: measurements record one run per cell, and a
  // duplicated declaration keeps its first measurement, as it always has.
  private def run(cell: Tally): Optional[Run] = cell.runs match
    case run :: _ => run
    case Nil      => Unset

  private def metric(run: Run, metric: Metric): Optional[Double] =
    run.metrics(metric)

  // A metric's value as a semantic datum, formatted by dimension.
  private def datum(metric: Metric, value: Double): Datum = metric.dimension match
    case Metric.Dimension.Time     => Datum.Time(value.toLong)
    case Metric.Dimension.Memory   => Datum.Memory(value.toLong)
    case Metric.Dimension.Rate     => if value == 0.0 then Datum.Blank else Datum.Rate(value.toLong)
    case Metric.Dimension.Count    => Datum.Num(value.toLong)
    case Metric.Dimension.Fraction => Datum.Percent(value)

  private def cellDatum(entry: Entry, cell: Tally): Datum =
    entry.headline.lay(Datum.Mark(cellStatus(cell))): headline =>
      run(cell).lay(Datum.Gap): run0 => metric(run0, headline).lay(Datum.Blank)(datum(headline, _))

  private def confidence(run: Run): Datum =
    val basisPoints = (metric(run, Metric.Confidence).or(0.0)*10000.0).toLong

    if basisPoints == 0L then Datum.Blank
    else Datum.Conf(metric(run, Metric.Percentile).or(0.0).toInt, basisPoints)

  private def sizing(run: Run): (Datum, Datum) =
    run.payload.option.collect { case Run.Payload.Sizing(size, rate) => (size, rate) } match
      case Some((size, rate)) =>
        (size.lay(Datum.Blank)(Datum.Str(_)), rate.lay(Datum.Blank)(Datum.Str(_)))

      case None =>
        (Datum.Blank, Datum.Blank)

  private def benchMetricColumns(sized: Boolean): List[Column] =
    val sizes =
      if sized then List(Column(t"Size", numeric = true), Column(t"Rate", numeric = true))
      else Nil: List[Column]

    List
      ( Column(t"n", numeric = true),
        Column(t"μ", numeric = true),
        Column(t"σ", numeric = true),
        Column(t"Confidence", numeric = true),
        Column(t"Throughput", numeric = true) ) + sizes

  private def rate(run: Run): Datum =
    val value = metric(run, Metric.Throughput).or(0.0).toLong
    if value == 0L then Datum.Blank else Datum.Rate(value)

  private def benchMetricCells(run: Run, sized: Boolean): List[Datum] =
    val sizes: List[Datum] = if sized then List(sizing(run)(0), sizing(run)(1)) else Nil

    List
      ( Datum.Num(metric(run, Metric.Iterations).or(0.0).toLong),
        Datum.Time(metric(run, Metric.Mean).or(0.0).toLong),
        Datum.Time(metric(run, Metric.Deviation).or(0.0).toLong),
        confidence(run),
        rate(run) ) + sizes

  private def benchBlocks(entries: List[Entry]): List[Block] =
    val plain = entries.filter(_.axes.nil)
    val axial = entries.filter(!_.axes.nil)

    val sized = entries.exists: entry =>
      entry.cells.stdlib.flatMap(_(1).runs.stdlib).exists: run0 =>
        run0.payload.option.exists:
          case Run.Payload.Sizing(_, _) => true
          case _                        => false

    val table =
      if plain.nil then Nil else
        val rows =
          plain.stdlib.flatMap: entry =>
            entry.cells.stdlib.take(1).flatMap: (_, cell) =>
              run(cell).option.map: run0 =>
                val lead = List(Datum.Hash(entry.id.id), Datum.Title(entry.id.name, 0))
                (metric(run0, Metric.Throughput).or(0.0), lead + benchMetricCells(run0, sized))

          . sortBy(-_(0)).map(x => (x(1)): List[Datum])
          . to(List)

        List(Block.Table
          ( Unset,
            List(Column(t"Hash"), Column(t"Test")) + benchMetricColumns(sized),
            rows ))

    table + axial.flatMap(axialBench(_, sized))

  // The baseline-relative datum of one run against the anchor's run, following the
  // anchor's `Baseline` settings: the compared statistic (min/mean/max of the timing
  // distribution), inverted to a rate for `Cadential`, as a ratio (`Geometric`, the
  // anchor's own row showing ★) or a signed difference (`Arithmetic`).
  private def relative(anchor: Anchor, anchorRun: Run, run0: Run): Datum =
    val key = anchor.baseline.compare match
      case Baseline.Compare.Min  => Metric.Least
      case Baseline.Compare.Mean => Metric.Mean
      case Baseline.Compare.Max  => Metric.Most

    metric(run0, key).lay(Datum.Blank): value =>
      metric(anchorRun, key).lay(Datum.Blank): anchorValue =>
        if value == 0.0 || anchorValue == 0.0 then Datum.Blank else
          val temporal = anchor.baseline.metric == Baseline.Metric.Temporal
          def side(value: Double): Double = if temporal then value else 1.0/value

          anchor.baseline.mode match
            case Baseline.Mode.Geometric =>
              Datum.Ratio(side(value)/side(anchorValue))

            case Baseline.Mode.Arithmetic =>
              val difference = side(value) - side(anchorValue)

              if difference == 0.0 then Datum.Ratio(1.0) else
                val magnitude =
                  if temporal then Datum.Time(math.abs(difference).toLong)
                  else Datum.Rate(math.abs(difference).toLong)

                Datum.Delta(magnitude, difference < 0.0)

  // An entry with one axis renders as a table of its cells; with two, as a crosstab of
  // headline data (followed, when anchored, by a grid of baseline-relative figures); with
  // more, as a flat listing of coordinates and headlines.
  private def axialBench(entry: Entry, sized: Boolean): List[Block] = entry.axes match
    case axis :: Nil =>
      val cells = entry.cells.to[Map]

      val anchored: Optional[(Anchor, Run)] =
        entry.anchor.let: anchor => cells(List(anchor.value)).let(run(_)).let(anchor -> _)

      val comparisonColumns: List[Column] = anchored.lay(Nil): (anchor, _) =>
        List(Column(t"×${anchor.value.text}", numeric = true))

      val rows =
        entry.values(axis).stdlib.flatMap: value =>
          cells(List(value)).option.flatMap: cell =>
            run(cell).option.map: run0 =>
              val comparison: List[Datum] = anchored.lay(Nil): (anchor, anchorRun) =>
                List(relative(anchor, anchorRun, run0))

              (Datum.Str(value.text) :: benchMetricCells(run0, sized) + comparison): List[Datum]
        . to(List)

      List(Block.Table
        ( entry.id,
          Column(axis.label) :: benchMetricColumns(sized) + comparisonColumns,
          rows ))

    case first :: second :: Nil =>
      crosstab(entry, first, second) :: relativesCrosstab(entry, first, second).option.to(List)

    case axes =>
      val rows = entry.cells.map: (address, cell) =>
        List(Datum.Str(address.map(_.text).join(t", ")), cellDatum(entry, cell))

      List(Block.Table
        ( entry.id,
          List(Column(axes.map(_.label).join(t", ")), Column(t"Headline", numeric = true)),
          rows ))

  // An axial unit test: one axis renders as a table of per-value statuses and timings; two
  // axes render as a grid of statuses with gaps at undefined combinations.
  private def axialCheck(entry: Entry): Block = entry.axes match
    case axis :: Nil =>
      val cells = entry.cells.to[Map]

      val rows =
        entry.values(axis).stdlib.flatMap: value =>
          cells(List(value)).option.map: cell =>
            val durations = cell.runs.stdlib.flatMap(_.verdict.option).map(_.duration)
            val avg = if durations.isEmpty then 0L else durations.sum/durations.length
            val time = if avg == 0L then Datum.Blank else Datum.Time(avg)

            List(Datum.Str(value.text), Datum.Mark(cellStatus(cell)), time)
        . to(List)

      Block.Table
        ( entry.id,
          List(Column(axis.label), Column(t"Status"), Column(t"Time", numeric = true)),
          rows )

    case first :: second :: Nil => crosstab(entry, first, second)

    case axes =>
      val rows = entry.cells.map: (address, cell) =>
        List(Datum.Str(address.map(_.text).join(t", ")), cellDatum(entry, cell))

      Block.Table
        ( entry.id,
          List(Column(axes.map(_.label).join(t", ")), Column(t"Status")),
          rows )

  // The biaxial grid: the first axis's values are rows, the second's are columns, and each
  // cell holds only the headline datum; absent combinations render as gaps.
  private def crosstab(entry: Entry, first: Axis.Spec, second: Axis.Spec): Block =
    val cells = entry.cells.to[Map]
    val columnValues = entry.values(second)

    val rows = entry.values(first).map: row =>
      val cellsRow: List[Datum] = columnValues.map: column =>
        cells(List(row, column)).lay(Datum.Gap)(cellDatum(entry, _))
      (Datum.Str(row.text) :: cellsRow): List[Datum]

    val columns = Column(first.label) :: columnValues.map: value =>
      Column(value.text, numeric = true)

    Block.Table(entry.id, columns, rows)

  // The baseline-relative companion of a biaxial grid: each cell compared to the anchored
  // value's cell within the same fiber (the same value of the other axis).
  private def relativesCrosstab(entry: Entry, first: Axis.Spec, second: Axis.Spec)
  :   Optional[Block] =

    entry.anchor.let: anchor =>
      val cells = entry.cells.to[Map]
      val onFirst = anchor.axis == first
      if !onFirst && anchor.axis != second then Unset else
        val columnValues = entry.values(second)

        val rows = entry.values(first).map: row =>
          val cellsRow: List[Datum] = columnValues.map: column =>
            val anchorAddress = if onFirst then List(anchor.value, column) else List(row, anchor.value)

            cells(List(row, column)).let(run(_)).lay(Datum.Gap): run0 =>
              cells(anchorAddress).let(run(_)).lay(Datum.Blank): anchorRun =>
                relative(anchor, anchorRun, run0)
          (Datum.Str(row.text) :: cellsRow): List[Datum]

        val columns = Column(t"×${anchor.value.text}") :: columnValues.map: value =>
          Column(value.text, numeric = true)

        Block.Table(Unset, columns, rows)

  // The point that stands for a whole scaling curve: the confirmed row of a capacity search
  // when there is one — finding it is the search's entire purpose — and otherwise the step
  // at which throughput peaked.
  private def peak(curve: Map[Long, Run]): Optional[(Long, Run)] =
    val points = curve.stdlib.toList

    points.find(_(1).sustained).orElse:
      points.maxByOption { point => metric(point(1), Metric.Throughput).or(0.0) }

    . optional

  private def throughput(run0: Run): Double = metric(run0, Metric.Throughput).or(0.0)

  // A stress group renders as a headline — the sparkline of every curve, then one row per
  // implementation at its best point, ranked — and a detail table of every step of every
  // curve, held back for verbose output.
  private def stressBlocks(entries: List[Entry]): (List[Block], List[Block]) =
    // Each stress entry's cells form its scaling curve: concurrency (the N axis) against
    // the strain measured there.
    val curves: List[(Entry, Map[Long, Run])] = entries.map: entry =>
      val index = entry.axes.where(_.label == t"N")

      val points = entry.cells.stdlib.flatMap: (address, cell) =>
        run(cell).option.flatMap: run0 =>
          index.lay(None): ordinal =>
            address.stdlib(ordinal.n0).numeric.option.map(_.toLong -> run0)

      entry -> points.toMap.to(Map)

    val steps: List[Long] =
      val all = curves.stdlib.flatMap(_(1).stdlib.keys)

      val shared =
        if curves.stdlib.length < 2 then all.distinct
        else all.groupBy(identity).filter(_(1).length > 1).keys.toList

      ((if shared.length > 1 then shared else all.distinct).sorted).to(List)

    val sparkline =
      if steps.size < 2 then Nil else
        val peakRate =
          curves.stdlib.flatMap(_(1).stdlib.values).map(throughput(_).toLong)
          . maxOption.getOrElse(0L).max(1L)

        val sequence = curves.map: (entry, curve) =>
          val sustained: Optional[(Long, Long)] =
            curve.seek(_(1).sustained).let: (n, run0) =>
              (n, throughput(run0).toLong)

          val limit: Long = sustained.lay(Long.MaxValue)(_(0))

          val cells: List[Optional[(Int, Boolean)]] = steps.map: step =>
            curve(step).let: run0 =>
              val level = ((throughput(run0).toLong*8L + peakRate - 1L)/peakRate).toInt.min(8).max(1)
              (level, step > limit)

          Spark(entry.id.name.text, cells, sustained)

        List(Block.Sparkline(steps, sequence))

    val latencies =
      entries.exists(_.cells.flatMap(_(1).runs).exists(_.metrics.defines(Metric.P50)))

    val slo =
      entries.exists(_.cells.flatMap(_(1).runs).exists(_.metrics.defines(Metric.Compliance)))

    def optionalTime(run0: Run, key: Metric): Datum =
      metric(run0, key).lay(Datum.Blank): value => Datum.Time(value.toLong)

    val sloColumns: List[Column] = if slo then List(Column(t"SLO", numeric = true)) else Nil

    def sloCells(run0: Run): List[Datum] =
      if slo
      then List(metric(run0, Metric.Compliance).lay(Datum.Blank)(Datum.Percent(_)))
      else Nil

    // The headline table: one row per implementation, at its best point, best first. A
    // stress test has no implementation axis of its own — each `stress` declaration is a
    // separate entry — so the implementations being compared are exactly the group's
    // entries, and the winner is the entry whose best throughput is highest. Each is shown
    // as a fraction of that throughput, which makes the winner's own ratio 1, rendered as ★.
    val peaks: List[(Entry, Long, Run)] =
      curves.stdlib.flatMap: (entry, curve) =>
        peak(curve).option.map { (n, run0) => (entry, n, run0) }
      . to(List)

    val best: Double =
      peaks.stdlib.map { point => throughput(point(2)) }.maxOption.getOrElse(0.0)

    // A single implementation has nothing to be ranked against, and a group which measured
    // no throughput at all cannot be ranked at all.
    val ranked = peaks.stdlib.length > 1 && best > 0.0

    val summary =
      if peaks.nil then Nil else
        val summaryColumns =
          List
            ( Column(t"Hash"),
              Column(t"Test"),
              Column(t"N", numeric = true),
              Column(t"Throughput", numeric = true) )
          + (if ranked then List(Column(t"×best", numeric = true)) else Nil: List[Column])
          + List(Column(t"Alloc·op¯¹", numeric = true))
          + (if latencies then List(Column(t"p99", numeric = true)) else Nil: List[Column])
          + sloColumns

        val summaryRows =
          peaks.stdlib.sortBy { point => -throughput(point(2)) }.map: point =>
            val (entry, n, run0) = point

            val lead =
              List
                ( Datum.Hash(entry.id.id),
                  Datum.Title(entry.id.name, 0),
                  Datum.Num(n),
                  rate(run0) )

            val ratio: List[Datum] = if ranked then List(Datum.Ratio(throughput(run0)/best)) else Nil
            val alloc = List(Datum.Memory(metric(run0, Metric.Allocation).or(0.0).toLong))
            val latency: List[Datum] = if latencies then List(optionalTime(run0, Metric.P99)) else Nil

            (lead + ratio + alloc + latency + sloCells(run0)): List[Datum]
          . to(List)

        List(Block.Table(Unset, summaryColumns, summaryRows))

    val latencyColumns =
      if latencies then
        List
          ( Column(t"p50", numeric = true),
            Column(t"p99", numeric = true),
            Column(t"p999", numeric = true) )
      else
        Nil

    val leadColumns =
      List
        ( Column(t"Hash"),
          Column(t"Test"),
          Column(t"N", numeric = true),
          Column(t"Ops", numeric = true),
          Column(t"Throughput", numeric = true),
          Column(t"Alloc·op¯¹", numeric = true) )

    val tailColumns =
      List
        ( Column(t"Peak", numeric = true),
          Column(t"Retained", numeric = true),
          Column(t"GC n", numeric = true),
          Column(t"GC t", numeric = true) )

    val columns = leadColumns + latencyColumns + sloColumns + tailColumns

    val rows =
      curves.stdlib.flatMap: (entry, curve) =>
        curve.stdlib.toList.sortBy(_(0)).map: (n, run0) =>
            val latencyCells =
              if latencies then
                List
                  ( optionalTime(run0, Metric.P50),
                    optionalTime(run0, Metric.P99),
                    optionalTime(run0, Metric.P999) )
              else
                Nil

            val lead =
              List
                ( Datum.Hash(entry.id.id),
                  Datum.Title(entry.id.name, 0),
                  Datum.Num(n),
                  Datum.Num(metric(run0, Metric.Operations).or(0.0).toLong),
                  rate(run0),
                  Datum.Memory(metric(run0, Metric.Allocation).or(0.0).toLong) )

            val tail =
              List
                ( Datum.Memory(metric(run0, Metric.PeakHeap).or(0.0).toLong),
                  Datum.Memory(metric(run0, Metric.Retained).or(0.0).toLong),
                  Datum.Num(metric(run0, Metric.GcCount).or(0.0).toLong),
                  Datum.Time(metric(run0, Metric.GcTime).or(0.0).toLong) )

            lead + latencyCells + sloCells(run0) + tail
      . to(List)

    (sparkline + summary, List(Block.Table(Unset, columns, rows)))

  private def histogram(entry: Entry): Block =
    val hotspots: Option[Hotspots] =
      entry.cells.prim.option.flatMap { (_, cell) => run(cell).option }.flatMap: run0 =>
        run0.payload.option.collect { case Run.Payload.Frames(hotspots) => hotspots }

    Block.Histogram
      ( entry.id,
        hotspots.map(_.total).getOrElse(0L),
        hotspots.map(_.frames).getOrElse(Nil) )
