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
import gossamer.*
import rudiments.*
import vacuous.*

// The single structural pass over a report: builds the renderer-agnostic `Doc.Document`
// consumed by both output modes. All decisions about WHAT appears in a report are made
// here; the renderers decide only how it looks.
private[probably] object Documenting:
  import Doc.*
  import Report.Status

  def document(report: Report): Document =
    val results = summaries(report.lines)
    val counts = results.groupBy(_.status).view.mapValues(_.size).to(Map) - Status.Suite

    val passed: Int =
      List(Status.Pass, Status.Bench, Status.Stress, Status.Profile)
      . map(counts.getOrElse(_, 0)).sum

    val aspirePassed: Int = counts.getOrElse(Status.AspirePass, 0)
    val aspireFailed: Int = counts.getOrElse(Status.AspireFail, 0)
    val total: Int = counts.values.sum
    val failed: Int = total - passed - aspirePassed - aspireFailed

    val groups =
      List(Entry.Kind.Check, Entry.Kind.Bench, Entry.Kind.Stress, Entry.Kind.Profile)
      . flatMap(suiteGroups(report.lines, _))

    val failures = report.details.to(List).sortBy(_(0).timestamp).map: (id, buffer) =>
      (id, buffer.to(List))

    Document
      ( results,
        Totals(passed, failed, aspirePassed, aspireFailed),
        groups,
        failures,
        report.failure )

  // One row per suite and per entry, in declaration order; a `Check` entry's runs are
  // aggregated across all its cells into a single status and duration statistics.
  private def summaries(line: ReportLine): List[SummaryRow] = line match
    case ReportLine.Suite(suite, tests) =>
      val rest = tests.list.sortBy(_(0).timestamp).flatMap: (_, line) => summaries(line)

      if suite.absent then rest
      else SummaryRow(Status.Suite, suite.option.get.id, 0, 0L, 0L, 0L) :: rest

    case ReportLine.Item(entry) => entry.kind match
      case Entry.Kind.Bench   => List(SummaryRow(Status.Bench, entry.id, 0, 0L, 0L, 0L))
      case Entry.Kind.Stress  => List(SummaryRow(Status.Stress, entry.id, 0, 0L, 0L, 0L))
      case Entry.Kind.Profile => List(SummaryRow(Status.Profile, entry.id, 0, 0L, 0L, 0L))

      case Entry.Kind.Check =>
        val verdicts = entry.cells.flatMap(_(1).runs).flatMap(_.verdict.option)

        if verdicts.isEmpty then Nil else
          val durations = verdicts.map(_.duration)
          val avg = durations.sum/durations.length
          val status = verdictStatus(verdicts)

          List(SummaryRow(status, entry.id, verdicts.length, durations.min, durations.max, avg))

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

  private def cellStatus(cell: Cell): Status =
    verdictStatus(cell.runs.flatMap(_.verdict.option))

  // Measurement entries group by their immediate suite, one `Group` per suite and kind, in
  // declaration order; nested suites follow their parents.
  private def suiteGroups(line: ReportLine.Suite, kind: Entry.Kind): List[Group] =
    val children = line.tests.list.sortBy(_(0).timestamp)

    val entries = children.flatMap: (_, child) =>
      child.absolve match
        case ReportLine.Item(entry) => if entry.kind == kind then List(entry) else Nil
        case _: ReportLine.Suite    => Nil

    val nested = children.flatMap: (_, child) =>
      child.absolve match
        case suite: ReportLine.Suite => suiteGroups(suite, kind)
        case _: ReportLine.Item      => Nil

    val here =
      if entries.isEmpty then Nil else
        val blockList = blocks(kind, entries)
        if blockList.isEmpty then Nil else List(Group(line.suite, kind, blockList))

    here ::: nested

  private def blocks(kind: Entry.Kind, entries: List[Entry]): List[Block] = kind match
    case Entry.Kind.Bench   => benchBlocks(entries)
    case Entry.Kind.Stress  => stressBlocks(entries)
    case Entry.Kind.Profile => entries.map(histogram)

    // Only axial unit tests need their own blocks (a table or grid of per-cell statuses);
    // ordinary tests are fully described by the results table.
    case Entry.Kind.Check   => entries.filter(_.axes.nonEmpty).map(axialCheck)

  // The first (usually only) run of a cell: measurements record one run per cell, and a
  // duplicated declaration keeps its first measurement, as it always has.
  private def run(cell: Cell): Optional[Run] = cell.runs match
    case run :: _ => run
    case Nil      => Unset

  private def metric(run: Run, metric: Metric): Optional[Double] =
    run.metrics.get(metric).getOrElse(Unset)

  // A metric's value as a semantic datum, formatted by dimension.
  private def datum(metric: Metric, value: Double): Datum = metric.dimension match
    case Metric.Dimension.Time     => Datum.Time(value.toLong)
    case Metric.Dimension.Memory   => Datum.Memory(value.toLong)
    case Metric.Dimension.Rate     => if value == 0.0 then Datum.Blank else Datum.Rate(value.toLong)
    case Metric.Dimension.Count    => Datum.Num(value.toLong)
    case Metric.Dimension.Fraction => Datum.Percent(value)

  private def cellDatum(entry: Entry, cell: Cell): Datum =
    entry.headline.lay(Datum.Mark(cellStatus(cell))): headline =>
      run(cell).lay(Datum.Gap): run0 =>
        metric(run0, headline).lay(Datum.Blank)(datum(headline, _))

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
      else Nil

    List
      ( Column(t"n", numeric = true),
        Column(t"μ", numeric = true),
        Column(t"σ", numeric = true),
        Column(t"Confidence", numeric = true),
        Column(t"Throughput", numeric = true) ) ::: sizes

  private def rate(run: Run): Datum =
    val value = metric(run, Metric.Throughput).or(0.0).toLong
    if value == 0L then Datum.Blank else Datum.Rate(value)

  private def benchMetricCells(run: Run, sized: Boolean): List[Datum] =
    val sizes = if sized then List(sizing(run)(0), sizing(run)(1)) else Nil

    List
      ( Datum.Num(metric(run, Metric.Iterations).or(0.0).toLong),
        Datum.Time(metric(run, Metric.Mean).or(0.0).toLong),
        Datum.Time(metric(run, Metric.Deviation).or(0.0).toLong),
        confidence(run),
        rate(run) ) ::: sizes

  private def benchBlocks(entries: List[Entry]): List[Block] =
    val (plain, axial) = entries.partition(_.axes.isEmpty)

    val sized = entries.exists: entry =>
      entry.cells.flatMap(_(1).runs).exists: run0 =>
        run0.payload.option.exists:
          case Run.Payload.Sizing(_, _) => true
          case _                        => false

    val table =
      if plain.isEmpty then Nil else
        val rows =
          plain.flatMap: entry =>
            entry.cells.take(1).flatMap: (_, cell) =>
              run(cell).option.map: run0 =>
                val lead = List(Datum.Hash(entry.id.id), Datum.Title(entry.id.name, 0))
                (metric(run0, Metric.Throughput).or(0.0), lead ::: benchMetricCells(run0, sized))

          . sortBy(-_(0)).map(_(1))

        List(Block.Table
          ( Unset,
            List(Column(t"Hash"), Column(t"Test")) ::: benchMetricColumns(sized),
            rows ))

    table ::: axial.map(axialBench(_, sized))

  // An entry with one axis renders as a table of its cells; with two, as a crosstab of
  // headline data; with more, as a flat listing of coordinates and headlines.
  private def axialBench(entry: Entry, sized: Boolean): Block = entry.axes match
    case axis :: Nil =>
      val rows = entry.values(axis).flatMap: value =>
        entry.cells.to(Map).at(List(value)).option.flatMap: cell =>
          run(cell).option.map: run0 =>
            Datum.Str(value.text) :: benchMetricCells(run0, sized)

      Block.Table(entry.id, Column(axis.label) :: benchMetricColumns(sized), rows)

    case first :: second :: Nil => crosstab(entry, first, second)

    case axes =>
      val rows = entry.cells.map: (address, cell) =>
        List(Datum.Str(address.map(_.text).join(t", ")), cellDatum(entry, cell))

      Block.Table
        ( entry.id,
          List(Column(axes.map(_.label).join(t", ")), Column(t"Headline", numeric = true)),
          rows )

  // An axial unit test: one axis renders as a table of per-value statuses and timings; two
  // axes render as a grid of statuses with gaps at undefined combinations.
  private def axialCheck(entry: Entry): Block = entry.axes match
    case axis :: Nil =>
      val cells = entry.cells.to(Map)

      val rows = entry.values(axis).flatMap: value =>
        cells.at(List(value)).option.map: cell =>
          val durations = cell.runs.flatMap(_.verdict.option).map(_.duration)
          val avg = if durations.isEmpty then 0L else durations.sum/durations.length
          val time = if avg == 0L then Datum.Blank else Datum.Time(avg)

          List(Datum.Str(value.text), Datum.Mark(cellStatus(cell)), time)

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
    val cells = entry.cells.to(Map)
    val columnValues = entry.values(second)

    val rows = entry.values(first).map: row =>
      Datum.Str(row.text) :: columnValues.map: column =>
        cells.at(List(row, column)).lay(Datum.Gap)(cellDatum(entry, _))

    val columns = Column(first.label) :: columnValues.map: value =>
      Column(value.text, numeric = true)

    Block.Table(entry.id, columns, rows)

  private def stressBlocks(entries: List[Entry]): List[Block] =
    // Each stress entry's cells form its scaling curve: concurrency (the N axis) against
    // the strain measured there.
    val curves: List[(Entry, Map[Long, Run])] = entries.map: entry =>
      val index = entry.axes.indexWhere(_.label == t"N")

      val points = entry.cells.flatMap: (address, cell) =>
        run(cell).option.flatMap: run0 =>
          if index < 0 then None else address(index).numeric.option.map(_.toLong -> run0)

      entry -> points.to(Map)

    val steps: List[Long] =
      val all = curves.flatMap(_(1).keys)

      val shared =
        if curves.length < 2 then all.distinct
        else all.groupBy(identity).filter(_(1).length > 1).keys.to(List)

      (if shared.length > 1 then shared else all.distinct).sorted

    val sparkline =
      if steps.length < 2 then Nil else
        val peak =
          curves.flatMap(_(1).values).map(metric(_, Metric.Throughput).or(0.0).toLong)
          . maxOption.getOrElse(0L).max(1L)

        val series = curves.map: (entry, curve) =>
          val sustained: Optional[(Long, Long)] = curve.find(_(1).sustained) match
            case Some((n, run0)) => (n, metric(run0, Metric.Throughput).or(0.0).toLong)
            case None            => Unset

          val limit: Long = sustained.lay(Long.MaxValue)(_(0))

          val cells: List[Optional[(Int, Boolean)]] = steps.map: step =>
            curve.at(step).let: run0 =>
              val throughput = metric(run0, Metric.Throughput).or(0.0).toLong
              val level = ((throughput*8L + peak - 1L)/peak).toInt.min(8).max(1)
              (level, step > limit)

          Spark(entry.id.name.text, cells, sustained)

        List(Block.Sparkline(steps, series))

    val latencies =
      entries.exists(_.cells.flatMap(_(1).runs).exists(_.metrics.contains(Metric.P50)))

    val slo =
      entries.exists(_.cells.flatMap(_(1).runs).exists(_.metrics.contains(Metric.Compliance)))

    val latencyColumns =
      if latencies then
        List
          ( Column(t"p50", numeric = true),
            Column(t"p99", numeric = true),
            Column(t"p999", numeric = true) )
      else
        Nil

    val sloColumns = if slo then List(Column(t"SLO", numeric = true)) else Nil

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

    val columns = leadColumns ::: latencyColumns ::: sloColumns ::: tailColumns

    def optionalTime(run: Run, key: Metric): Datum =
      metric(run, key).lay(Datum.Blank): value =>
        Datum.Time(value.toLong)

    val rows = curves.flatMap: (entry, curve) =>
      curve.to(List).sortBy(_(0)).map: (n, run0) =>
        val latencyCells =
          if latencies then
            List
              ( optionalTime(run0, Metric.P50),
                optionalTime(run0, Metric.P99),
                optionalTime(run0, Metric.P999) )
          else
            Nil

        val sloCells =
          if slo
          then List(metric(run0, Metric.Compliance).lay(Datum.Blank)(Datum.Percent(_)))
          else Nil

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

        lead ::: latencyCells ::: sloCells ::: tail

    sparkline ::: List(Block.Table(Unset, columns, rows))

  private def histogram(entry: Entry): Block =
    val hotspots: Option[Hotspots] =
      entry.cells.headOption.flatMap { (_, cell) => run(cell).option }.flatMap: run0 =>
        run0.payload.option.collect { case Run.Payload.Frames(hotspots) => hotspots }

    Block.Histogram
      ( entry.id,
        hotspots.map(_.total).getOrElse(0L),
        hotspots.map(_.frames).getOrElse(Nil) )
