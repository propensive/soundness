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
import dendrology.*
import denominative.*
import digression.*
import distillate.*
import escapade.*
import escritoire.*, columnAttenuation.ignoreAttenuation
import fulminate.*
import gossamer.*
import iridescence.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import turbulence.*
import vacuous.*

import Format.measurable
import tableStyles.defaultTableStyle

// The full-colour renderer: tables, ribbons, banner, sparklines, histograms and coverage,
// plus GitHub Actions annotations when running there. All content decisions live in
// `Documenting`; this object decides only presentation.
private[probably] object AnsiRenderer:
  import Doc.{Block, Datum, Document, Group, SummaryRow}
  import Report.Status

  def render(document: Document, coverage: Option[Coverage])
    ( using stdio: Stdio, environment: Environment, palette: TestPalette )
  :   Unit =

    val columns: Int = safely(Environment.columns.as[Int]).or(120)
    val githubActions = Ci.githubActions

    coverage.each(renderCoverage(_, columns))
    renderTotals(document, columns, top = true)
    document.groups.each(renderGroup(_, columns, githubActions))
    if githubActions then annotations(document)
    renderFailures(document, githubActions)
    renderFatal(document, githubActions)
    renderTotals(document, columns, top = false)

  private def time(n: Long)(using palette: TestPalette): Teletype =
    figure(Format.scaled(n), Format.timeUnits)

  private def memory(n: Long)(using palette: TestPalette): Teletype =
    figure(Format.scaled(n), Format.memoryUnits)

  private def figure(figure: Format.Figure, units: List[Text])(using palette: TestPalette)
  :   Teletype =

    if figure.unit >= 3 then figure.whole.teletype else
      val color = figure.unit match
        case 0 => palette.cold
        case 1 => palette.warm
        case _ => palette.hot

      val unit = units(figure.unit)
      e"${Fg(palette.foreground)}(${figure.whole}.${figure.fraction}) ${Fg(color)}($unit)"

  private def datum(value: Datum)(using palette: TestPalette): Teletype = value match
    case Datum.Blank              => e""
    case Datum.Gap                => e"${Fg(palette.subdued)}(–)"
    case Datum.Str(text)          => e"$text"
    case Datum.Hash(id)           => e"${Fg(palette.informative)}($id)"
    case Datum.Title(name, depth) => e"${t"  "*depth}$name"
    case Datum.Mark(status)       => status.symbol
    case Datum.Num(number)        => number.show.teletype
    case Datum.Time(nanos)        => time(nanos)
    case Datum.Memory(bytes)      => memory(bytes)

    case Datum.Rate(perSecond) =>
      val units = e"${Fg(palette.accented)}(op${Fg(palette.subdued)}(·)s¯¹)"
      e"${Fg(palette.foreground)}($perSecond) $units"

    case Datum.Percent(fraction) => e"${Fg(palette.foreground)}(${fraction*100})%"

    case Datum.Conf(percentile, basisPoints) =>
      val interval = e"${Fg(palette.foreground)}(${Format.percent(basisPoints)})"
      e"P$percentile ${Fg(palette.accented)}(±)$interval%"

    case Datum.Ratio(factor) =>
      if factor == 1.0 then e"★" else e"${Fg(palette.foreground)}($factor)"

    case Datum.Delta(inner, negative) =>
      val sign = if negative then e"${Fg(palette.accented)}(-)" else e"${Fg(palette.accented)}(+)"
      e"$sign${datum(inner)}"

  private def accent(level: Int): Color in Srgb =
    val stackPalette = summon[StackTrace.Palette]

    (level%5) + 1 match
      case 1 => stackPalette.accent1
      case 2 => stackPalette.accent2
      case 3 => stackPalette.accent3
      case 4 => stackPalette.accent4
      case _ => stackPalette.accent5

  private def renderTotals(document: Document, columns: Int, top: Boolean)
    ( using Stdio, TestPalette )
  :   Unit =

    val palette = summon[TestPalette]
    val totals = document.totals

    if totals.total == 0 then
      if !top then Out.println(e"$Italic(No tests were run.)")
    else
      val pass = totals.failed == 0

      if !top then
        Out.print(e"${escapes.Reset}")
        Out.println(e"$Bold($Underline(Test results))")

        val showStats = !document.results.all(_.count < 2)
        val timeTitle = if showStats then t"Avg" else t"Time"

        def testTitle(row: SummaryRow): Teletype =
          val depth = row.id.suite.let(_.id.depth).or(0)
          val name = row.id.name

          val title =
            if row.status == Status.Suite then e"${Fg(palette.foreground)}($Bold($name))"
            else e"$name"

          e"${t"  "*depth}$title"

        Scaffold[SummaryRow]
          ( Column(e"")(_.status.symbol),

            Column(e"$Bold(Hash)"): row =>
              e"${Fg(palette.informative)}(${row.id.id})",

            Column(e"$Bold(Test)")(testTitle),

            Column(e"$Bold(Count)", textAlign = TextAlignment.Right): row =>
              if row.count == 0 then e"" else e"${Fg(palette.informative)}(${row.count})",

            Column(e"$Bold(Min)", textAlign = TextAlignment.Right): row =>
              if row.count < 2 || row.min == 0L then e"" else time(row.min),

            Column(e"$Bold($timeTitle)", textAlign = TextAlignment.Right): row =>
              if row.avg == 0L then e"" else time(row.avg),

            Column(e"$Bold(Max)", textAlign = TextAlignment.Right): row =>
              if row.count < 2 || row.max == 0L then e"" else time(row.max) )

        . tabulate(document.results).grid(columns).render.each(Out.println(_))

      else
        Out.println(t"─"*72)

      val color = if pass then palette.pass else palette.fail

      val text1 =
        if !pass then t"┳┳━━━┓ ┏┳━━━┳┓   ┳┳   ┳┳    "
        else t"┳┳━━━┳┓  ┏┳━━━┳┓  ┏┳━━━┓  ┏┳━━━┓"

      val text2 =
        if !pass then t"┃┃     ┃┃   ┃┃   ┃┃   ┃┃    "
        else t"┃┃   ┃┃  ┃┃   ┃┃  ┃┃      ┃┃    "

      val text3 =
        if !pass then t"┃┣━━   ┃┣━━━┫┃   ┃┃   ┃┃    "
        else t"┃┣━━━┻┛  ┃┣━━━┫┃  ┗┻━━┳┓  ┗┻━━┳┓"

      val text4 =
        if !pass then t"┃┃     ┃┃   ┃┃   ┃┃   ┃┃    "
        else t"┃┃       ┃┃   ┃┃      ┃┃      ┃┃"

      val text5 =
        if !pass then t"┻┻     ┻┻   ┻┻   ┻┻   ┻┻━━━┛"
        else t"┻┻       ┻┻   ┻┻  ┗━━━┻┛  ┗━━━┻┛"

      val width = if pass then 38 else 34

      if !pass || !top then
        Out.println(e"$color(╭${t"─"*width}╮)")

        def banner(text: Text): Teletype =
          e"$color(│) $Bold(${Bg(color)}(${Fg(palette.black)}(  $text  ))) $color(│)"

        Out.println(banner(text1))
        Out.println(banner(text2))
        Out.println(banner(text3))
        Out.println(banner(text4))
        Out.println(banner(text5))
        Out.println(e"$color(╰${t"─"*width}╯)")

        given decimalizer: Decimalizer = Decimalizer(decimalPlaces = 1)
        val fg = Fg(palette.foreground)
        val total = totals.total
        val passText = e"$Bold($fg(${totals.passed})) passed (${100.0*totals.passed/total}%)"
        val failText = e"$Bold($fg(${totals.failed})) failed (${100.0*totals.failed/total}%)"

        val aspirePassText =
          e"$Bold($fg(${totals.aspirePassed})) aspire-passed (${100.0*totals.aspirePassed/total}%)"

        val aspireFailText =
          e"$Bold($fg(${totals.aspireFailed})) aspire-failed (${100.0*totals.aspireFailed/total}%)"

        val allText = e"$Bold(${Fg(palette.foreground)}($total)) total"

        if totals.aspirePassed + totals.aspireFailed == 0
        then Out.println(e" $passText, $failText, $allText")
        else Out.println(e" $passText, $failText, $aspirePassText, $aspireFailText, $allText")

        Out.println(t"─"*72)

      import Status.*

      val allStatuses =
        List(Pass, Bench, Stress, Profile, AspirePass, Throws, Fail, AspireFail, Mixed,
            CheckThrows)

      // Printed with index loops rather than `grouped(_).each`/`map` closures: a lambda whose
      // parameter is a `List` and whose body uses the `Stdio` capability leaks the list's reach
      // capability into the surrounding `(using Stdio)` scope under capture checking.
      val cells: IndexedSeq[Teletype] =
        allStatuses.map: status =>
          gossamer.pad[Teletype](e"  ${status.symbol} ${status.describe}")(20)

        . to(IndexedSeq)

      var cell = 0

      while cell < cells.length do
        Out.println(cells.slice(cell, cell + 4).join(e" "))
        cell += 4

      Out.println(t"─"*72)

  private def kindTitle(kind: Entry.Kind): Text = kind match
    case Entry.Kind.Check   => t"Tests"
    case Entry.Kind.Bench   => t"Benchmarks"
    case Entry.Kind.Stress  => t"Stress"
    case Entry.Kind.Profile => t"Profile"

  private def renderGroup(group: Group, columns: Int, githubActions: Boolean)
    ( using Stdio, TestPalette )
  :   Unit =

    val palette = summon[TestPalette]

    if githubActions then
      GithubActions.group(t"${kindTitle(group.kind)}: ${group.suite.let(_.name.text).or(t"")}")

    val ribbon =
      Ribbon
        ( palette.subdue(palette.detail, 0.3),
          palette.subdue(palette.detail, 0.6),
          palette.subdue(palette.detail, 0.9) )

    Out.println:
      ribbon.fill
        ( e"${group.suite.lay(t"")(_.id.id)}",
          kindTitle(group.kind).teletype,
          group.suite.let(_.name.teletype).or(e"") )

    group.blocks.each(renderBlock(_, columns))

    if githubActions then GithubActions.endGroup()

  private def renderBlock(block: Block, columns: Int)(using Stdio, TestPalette): Unit =
    val palette = summon[TestPalette]

    block match
      case Block.Table(title, tableColumns, rows) =>
        title.let: id =>
          Out.println(e"$Bold(${Fg(palette.informative)}(${id.id})) $Bold(${id.name})")

        val tableColumns2 = tableColumns.zipWithIndex.map: (column, index) =>
          val align = if column.numeric then TextAlignment.Right else TextAlignment.Left
          def cell(row: List[Datum]): Teletype = datum(row(index))
          escritoire.Column[List[Datum], Teletype, Teletype](e"$Bold(${column.title})", align)(cell)

        Scaffold[List[Datum]](tableColumns2*)
        . tabulate(rows).grid(columns).render.each(Out.println(_))

      case Block.Sparkline(steps, series) =>
        val labelWidth = series.map(_.label.length).max
        val stepWidth = steps.map(_.show.length).max + 2

        Out.println:
          val headings = steps.map(_.show.pad(stepWidth, Rtl)).join
          e"  ${Fg(palette.subdued)}(${t"N".pad(labelWidth)}$headings)"

        series.zipWithIndex.each: (spark, index) =>
          val color = accent(index)

          val cells: Teletype =
            spark.cells.map: cell =>
              val absent = e"${Fg(palette.subdued)}(${t"·".pad(stepWidth, Rtl)})"

              cell.lay(absent): (level, subdued) =>
                val text = Format.sparkBlocks(level - 1).pad(stepWidth, Rtl)
                if subdued then e"${Fg(palette.subdued)}($text)" else e"$color($text)"

            . join

          val summary: Teletype = spark.sustained.lay(e""): (n, throughput) =>
            val rate = e"${Fg(palette.foreground)}($throughput)"
            val units = e"${Fg(palette.accented)}(op${Fg(palette.subdued)}(·)s¯¹)"
            e"  sustained $Bold(${Fg(palette.foreground)}($n)) @ $rate $units"

          Out.println(e"  $color(${spark.label.pad(labelWidth)})$cells$summary")

        Out.println(e"")

      case Block.Histogram(title, total, frames) =>
        title.let: id =>
          Out.println(e"$Bold(${Fg(palette.foreground)}(${id.name}))")

        val max = frames.map(_.samples).maxOption.getOrElse(0L)
        val stackPalette = summon[StackTrace.Palette]

        // The digression convention: packages in first-appearance order take the accent
        // colours cyclically, exactly as coloured stack traces do.
        val packages: Map[Text, Color in Srgb] =
          frames.map: frame =>
            StackTrace.Method(frame.className, frame.method).prefix

          . distinct.zipWithIndex.map: (prefix, index) =>
              prefix -> accent(index)

          . to(Map)

        // The method column is leftmost and right-aligned against the bars, so the names
        // read into their histogram rows; the plain width is measured before styling.
        val width: Int =
          frames.map: frame =>
            val method = StackTrace.Method(frame.className, frame.method)
            val cls = if method.cls.starts(t"Ξ") then method.cls.skip(1) else method.cls
            method.prefix.length + 1 + cls.length + 1 + frame.method.length

          . maxOption.getOrElse(0)

        frames.each: frame =>
          val method = StackTrace.Method(frame.className, frame.method)
          val color = packages(method.prefix)
          val percent = Format.percent(Format.basisPoints(frame.samples.toDouble, total.toDouble))
          val obj = method.cls.starts(t"Ξ")
          val cls = if obj then method.cls.skip(1) else method.cls
          val separator = if obj then t"." else t"⌗"
          val length = method.prefix.length + 1 + cls.length + 1 + frame.method.length
          val margin = t" "*(width - length)
          val faded = stackPalette.subdue(color, 0.5)
          val qualified = e"$faded(${method.prefix}.$Bold($color($cls)))"
          val sep2 = e"${stackPalette.separator}($separator)"
          val callable = e"$sep2${stackPalette.method}(${frame.method})"
          val share = e"${Fg(palette.foreground)}(${percent.pad(6, Rtl)}%)"

          Out.println:
            e"  $margin$qualified$callable $share $color(${Format.bar(frame.samples, max)})"

        Out.println(e"")

  private def describeFailure(info: Optional[Verdict.Detail]): Text = info.lay(t"Test failed"):
    case Verdict.Detail.Throws(err) =>
      Format.truncate(t"Threw ${err.component}.${err.className}: ${err.message.text}")

    case Verdict.Detail.CheckThrows(err) =>
      Format.truncate(t"Check threw ${err.component}.${err.className}: ${err.message.text}")

    case Verdict.Detail.Compare(expected, observed, _) =>
      val observed2 = observed.sub(t"\n", t" ")
      Format.truncate(t"Expected: ${expected.sub(t"\n", t" ")}; observed: $observed2")

    case Verdict.Detail.Message(text) =>
      Format.truncate(text)

    case Verdict.Detail.Captures(_) =>
      t"Test failed"

  private def annotations(document: Document)(using Stdio): Unit =
    val details: Map[TestId, List[Verdict.Detail]] = document.failures.to(Map)

    document.results.each: row =>
      row.status match
        case Status.Fail | Status.Throws | Status.CheckThrows | Status.Mixed =>
          val firstDetail: Optional[Verdict.Detail] =
            details.at(row.id).let(_.headOption.getOrElse(Unset))

          GithubActions.error
            ( message = describeFailure(firstDetail),
              file    = row.id.codepoint.source,
              line    = row.id.codepoint.line,
              title   = row.id.name.text )

        case _ => ()

  private def renderFailures(document: Document, githubActions: Boolean)
    ( using Stdio, TestPalette )
  :   Unit =

    val palette = summon[TestPalette]

    def showLegend(): Unit =
      Out.println(t"─"*74)

      Out.println:
        StackTrace.legend.to(List).map: (symbol, description) =>
          e"$Bold(${Fg(palette.foreground)}(${symbol.pad(3, Rtl)}))  ${description.pad(20)}"

        . grouped(3).to(List).map(_.to(List).join).join(e"${t"\n"}")

      Out.println(t"─"*74)

    document.failures.each: (id, info) =>
      val ribbon =
        Ribbon(palette.fail, palette.subdue(palette.fail, 0.5), palette.subdue(palette.fail, 0.75))

      if githubActions then GithubActions.group(t"Failure: ${id.name.text} (${id.id})")

      Out.println(ribbon.fill(e"$Bold(${id.id})", id.codepoint.text.teletype, id.name.teletype))

      info.each: details =>
        Out.println(t"")

        details match
          case Verdict.Detail.Throws(err) =>
            Out.println(e"${Fg(palette.foreground)}(An exception was thrown while running test:)")
            Out.println(err.crop(t"probably.Runner", t"run()").teletype)
            showLegend()

          case Verdict.Detail.CheckThrows(err) =>
            val msg = e"An exception was thrown while checking the test predicate:"
            Out.println(e"${Fg(palette.foreground)}($msg)")
            Out.println(err.crop(t"probably.Verdict#", t"apply()").dropRight(1).teletype)
            showLegend()

          case Verdict.Detail.Compare(expected, observed, cmp) =>
            def indent(text: Text): Text =
              if text.contains(t"\n") then text.trim.cut(t"\n").join(t"\n│ ", t"\n│ ", t"\n")
              else text

            val expected2: Teletype = e"$Italic(${Fg(palette.foreground)}(${indent(expected)}))"
            val observed2: Teletype = e"$Italic(${Fg(palette.foreground)}(${indent(observed)}))"

            Out.println(e"${Fg(palette.foreground)}(Expected:) $expected2")
            Out.println(e"${Fg(palette.foreground)}(Observed:) $observed2")

            Out.println(cmp.teletype)

          case Verdict.Detail.Captures(map) =>
            Scaffold[(Text, Text), Teletype]
              ( escritoire.Column(e"Expression", textAlign = TextAlignment.Right)(_(0)),
                escritoire.Column(e"Value")(_(1)) )

            . tabulate(map.to(List)).grid(140).render.each(Out.println(_))

          case Verdict.Detail.Message(text) =>
            Out.println(text)

      Out.println()
      if githubActions then GithubActions.endGroup()

  private def renderFatal(document: Document, githubActions: Boolean)
    ( using Stdio, TestPalette )
  :   Unit =

    val palette = summon[TestPalette]

    document.fatal.let: (error, active) =>
      val explanation = active.to(List) match
        case Nil => e"No tests were active when a fatal error occurred."

        case _ =>
          val were = if active.size == 1 then e"was" else e"were"

          val tests =
            active.to(List).map: test => e"$Bold(${test.name})"
            . join(e"", e", ", e" and ", e"")

          e"A fatal error occurred while $tests $were running."

      if githubActions then
        val activeNames = active.to(List).map(_.name.text).join(t", ")
        val cause = Option(error.getMessage).map(_.nn.tt).getOrElse(t"")
        val errorClass = Option(error.getClass.getName).map(_.nn.tt).getOrElse(t"")

        val message =
          if active.nil
          then Format.truncate(t"Fatal error: $errorClass: $cause")
          else Format.truncate(t"Fatal error in $activeNames: $errorClass: $cause")

        GithubActions.error(message = message, title = t"Fatal error")
        GithubActions.group(t"Fatal error stack trace")

      Out.println()

      val fatalRibbon = Ribbon(palette.pass, palette.subdue(palette.pass, 0.5))
      Out.println(fatalRibbon.fill(e"$Bold(FATAL)", explanation))
      Out.println(StackTrace(error).teletype)

      if githubActions then GithubActions.endGroup()

  private def renderCoverage(coverage: Coverage, columns: Int)(using Stdio, TestPalette): Unit =
    val palette = summon[TestPalette]
    Out.println(e"$Bold($Underline(Test coverage))")

    case class CoverageData(path: Text, branches: Int, hits: Int, oldHits: Int):
      def hitsText: Teletype =
        val main = e"${if hits == 0 then palette.subdued else palette.detail}($hits)"

        if oldHits == 0 then main
        else e"${palette.detail}(${oldHits.show.subscripts}) $main"

    val data = coverage.spec.groupBy(_.path).to(List).map: (path, branches) =>
      val hitCount: Int =
        branches.to(List).map(_.id).map(coverage.hits.has).count(identity(_))

      val oldHitCount: Int =
        branches.to(List).map(_.id).map(coverage.oldHits.has).count(identity(_))

      CoverageData(path, branches.size, hitCount, oldHitCount)

    val maxHits = data.map(_.branches).maxOption

    import treeStyles.defaultTreeStyle

    def describe(surface: Surface): Teletype =
      if surface.juncture.treeName == t"DefDef" then e"• ${surface.juncture.method.teletype}"
      else e"• ${surface.juncture.shortCode}"

    def render(junctures: List[Surface]): LazyList[(Surface, Teletype)] =
      val diagram = TreeDiagram.by[Surface](_.children)(junctures*)
      diagram.nodes.zip(diagram.render(describe))

    val allHits = coverage.hits ++ coverage.oldHits

    val junctures2 =
      coverage.structure.values.flatten
      . to(List)
      . filter(!_.covered(allHits))
      . map(_.copy(children = Nil))

    Scaffold[(Surface, Teletype)]
      ( escritoire.Column(e""): row =>
          if row(0).juncture.branch then e"⎇" else e"",

        escritoire.Column(e""): row =>
          if coverage.hits.has(row(0).juncture.id) then e"${Bg(palette.detail)}(  )"
          else if coverage.oldHits.has(row(0).juncture.id) then e"${Bg(palette.detail)}(  )"
          else e"${Bg(palette.highlight)}(  )",

        escritoire.Column(e"Juncture")(_(1)),

        escritoire.Column(e"Line"): row =>
          val path = e"${Fg(palette.pass)}(${row(0).juncture.path})"
          val line = e"${Fg(palette.accented)}(${row(0).juncture.lineNo})"
          e"$path${Fg(palette.subdued)}(:)$line",

        escritoire.Column(e"Symbol")(_(0).juncture.symbolName) )

    . tabulate(render(junctures2))
    . grid(columns)(using tableStyles.horizontalTableStyle)
    . render
    . each(Out.println(_))

    Out.println(e"")

    Scaffold[CoverageData]
      ( escritoire.Column(e"Source file", textAlign = TextAlignment.Left): data =>
          data.path,
        escritoire.Column(e"Hits", textAlign = TextAlignment.Right)(_.hitsText),
        escritoire.Column(e"Size", textAlign = TextAlignment.Right)(_.branches),
        escritoire.Column(e"Coverage", textAlign = TextAlignment.Right): data =>
          e"${(100*(data.hits + data.oldHits)/data.branches.toDouble)}%",
        escritoire.Column(e""): data =>
          def width(n: Double): Text = if n == 0 then t"" else t"━"*(1 + (70*n).toInt)
          val covered: Text = width(maxHits.map(data.hits.toDouble/_).getOrElse(0))
          val oldCovered: Text = width(maxHits.map(data.oldHits.toDouble/_).getOrElse(0))

          val notCovered: Text =
            width(maxHits.map((data.branches.toDouble - data.hits - data.oldHits)/_).getOrElse(0))

          val bars = List
                      ( palette.accented  -> covered,
                        palette.detail    -> oldCovered,
                        palette.highlight -> notCovered )

          bars.filter(_(1).length > 0).map { (color, bar) => e"$color($bar)" }.join )

    . tabulate(data).grid(columns).render.each(Out.println(_))

    Out.println(e"")
