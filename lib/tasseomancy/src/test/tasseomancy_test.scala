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
package tasseomancy

import soundness.*

import strategies.throwUnsafely

// A minimal TrueType font, assembled the way phoenicia's own fixtures are: enough tables for the
// character map and the horizontal metrics, which is all a width measurement reads.
object TestFont:
  extension (left: Data)
    @targetName("concatData")
    def ++ (right: Data): Data = Array.frozen(left.readable ++ right.readable)

  def u16(values: Int*): Data =
    Array.from(values.flatMap { value => scala.Seq((value >> 8).toByte, value.toByte) })

  def u32(values: Long*): Data =
    Array.from:
      values.flatMap: value =>
        scala.Seq((value >> 24).toByte, (value >> 16).toByte, (value >> 8).toByte, value.toByte)

  def sfnt(tables: (Text, Data)*): Truetype =
    val count = tables.length
    val entrySelector = 31 - Integer.numberOfLeadingZeros(count)
    val searchRange = (1 << entrySelector)*16
    val header = u32(0x00010000L) ++ u16(count, searchRange, entrySelector, count*16 - searchRange)

    var offset = 12 + count*16
    val directory = scala.collection.immutable.List.newBuilder[Data]
    val body = scala.collection.immutable.List.newBuilder[Data]

    tables.each: (tag, table) =>
      val padding = if table.length%4 == 0 then 0 else 4 - table.length%4
      val tagBytes = Array.unsafeFrozen(tag.s.getBytes("US-ASCII").nn)
      directory += tagBytes ++ u32(0L, offset.toLong, table.length.toLong)
      body += table ++ Array.fill[Byte](padding)(0)
      offset += table.length + padding

    Truetype(header ++ directory.result().reduce(_ ++ _) ++ body.result().reduce(_ ++ _))

  val headTable: Data =
    u16(1, 0, 1, 0) ++ u32(0L, 0x5f0f3cf5L) ++ u16(0, 1000) ++ u32(0L, 0L, 0L, 0L)
    ++ u16(-50, -200, 1000, 800) ++ u16(0, 8) ++ u16(2, 0, 0)

  val hheaTable: Data =
    u16(1, 0) ++ u16(800, -200, 90) ++ u16(600) ++ u16(10, 10, 1000) ++ u16(1, 0, 0)
    ++ u16(0, 0, 0, 0) ++ u16(0, 3)

  // Glyph 0 (missing) advances 600, glyph 1 advances 500, glyph 2 advances 400, and every later
  // glyph shares the last advance.
  val hmtxTable: Data = u16(600, 10, 500, 20, 400, 30) ++ u16(25)
  val maxpTable: Data = u32(0x00010000L) ++ u16(4)

  // A–C map to glyphs 1–3 by delta.
  val cmapTable: Data =
    u16(0, 1) ++ u16(3, 1) ++ u32(12L)
    ++ u16(4, 46, 0) ++ u16(6, 4, 1, 2) ++ u16(0x43, 0x63, 0xffff) ++ u16(0)
    ++ u16(0x41, 0x61, 0xffff) ++ u16(-64, 0, 1) ++ u16(0, 4, 0) ++ u16(2, 3, 1)

  lazy val font: Truetype =
    sfnt(t"cmap" -> cmapTable, t"head" -> headTable, t"hhea" -> hheaTable, t"hmtx" -> hmtxTable,
        t"maxp" -> maxpTable)

object Tests extends Suite(m"Tasseomancy tests"):
  val decimal = Scale.Notation()

  def labels(scale: Scale, budget: Int): List[Text] =
    scale.gradations(budget).filter(_.major).map(_.label).to[List]

  def rendered[data, form, fit, style <: Chart.Style](chart: Chart[data, form, fit, style])
    ( using style, ChartPalette, FontMetric )
  :   Text =
    chart.svg.xml.show

  def describe(revision: Chart.Revision): Text = revision match
    case Chart.Revision.Redraw(_)      => t"redraw"
    case Chart.Revision.Replace(id, _) => t"replace ${id.text}"

  def occurrences(text: Text, needle: Text): Int =
    var count = 0
    var index = 0
    while index >= 0 do
      index = text.s.indexOf(needle.s, index)
      if index >= 0 then
        count += 1
        index += needle.length
    count

  val sales: List[Series[Text, Double]] =
    List
      ( Series(t"north")((t"jan", 3.0), (t"feb", 5.0), (t"mar", 4.0)),
        Series(t"south")((t"jan", 2.0), (t"feb", 1.0), (t"mar", 6.0)) )

  val growth: Series[Double, Double] = Series(t"growth")((0.0, 0.0), (10.0, 10.0))

  def run(): Unit =
    suite(m"Number formatting"):
      test(m"a whole number has no decimal point"):
        Scale.format(1000.0, 0)
      . assert(_ == t"1000")

      test(m"decimal places are fixed"):
        Scale.format(2.5, 1)
      . assert(_ == t"2.5")

      test(m"a small negative keeps its leading zero"):
        Scale.format(-0.05, 2)
      . assert(_ == t"-0.05")

      test(m"an interval below a second is in milliseconds"):
        Scale.interval(0.25)
      . assert(_ == t"250ms")

      test(m"an interval in microseconds trims trailing zeros"):
        Scale.interval(0.0000015)
      . assert(_ == t"1.5µs")

      test(m"an interval over a minute pairs minutes and seconds"):
        Scale.interval(90.0)
      . assert(_ == t"1m30s")

      test(m"an interval over an hour pads the minutes"):
        Scale.interval(3660.0)
      . assert(_ == t"1h01m")

      test(m"a clock time is hours, minutes and seconds"):
        Scale.clock(3661.0)
      . assert(_ == t"01:01:01")

    suite(m"Calibration"):
      test(m"a linear scale pads outward to whole steps"):
        val scale = Calibration.linear(0.0, 97.0, false, decimal, false)
        (scale.lower, scale.upper)
      . assert(_ == (0.0, 100.0))

      test(m"linear gradations fall on multiples of 1, 2 and 5"):
        labels(Calibration.linear(0.0, 97.0, false, decimal, false), 5)
      . assert(_ == List(t"0", t"20", t"40", t"60", t"80", t"100"))

      test(m"a logarithmic scale spans whole decades"):
        val scale = Calibration.logarithmic(3.0, 4200.0, false, decimal)
        (scale.transform, scale.lower, scale.upper)
      . assert(_ == (Scale.Transform.Logarithmic, 1.0, 10000.0))

      test(m"logarithmic gradations are powers of ten"):
        labels(Calibration.logarithmic(3.0, 4200.0, false, decimal), 5)
      . assert(_ == List(t"1", t"10", t"100", t"1000", t"10000"))

      test(m"a logarithmic scale over non-positive values falls back to linear"):
        Calibration.logarithmic(-1.0, 10.0, false, decimal).transform
      . assert(_ == Scale.Transform.Linear)

      test(m"adaptive calibration goes logarithmic at a thousandfold range"):
        Calibration[Double](Calibration.Policy.Adaptive).scale(1.0, 5000.0, false, decimal).transform
      . assert(_ == Scale.Transform.Logarithmic)

      test(m"adaptive calibration stays linear over a narrow range"):
        Calibration[Double](Calibration.Policy.Adaptive).scale(1.0, 500.0, false, decimal).transform
      . assert(_ == Scale.Transform.Linear)

      test(m"an anchored scale includes zero"):
        Calibration.linear(5.0, 9.0, true, decimal, false).lower
      . assert(_ == 0.0)

      test(m"a tight scale is exactly the data's extent"):
        val scale = Calibration.linear(5.0, 9.0, false, decimal, true)
        (scale.lower, scale.upper)
      . assert(_ == (5.0, 9.0))

      test(m"sexagesimal gradations land on clock steps"):
        val clock = Scale.Notation(Scale.Spacing.Sexagesimal, Scale.Labelling.Interval)
        val scale = Calibration.linear(0.0, 100.0, false, clock, false)
        labels(scale, 5)
      . assert(_ == List(t"0s", t"30s", t"1m", t"1m30s", t"2m"))

      test(m"the type-keyed default is linear"):
        growth.chart(Lines()).fit.ordinate.transform
      . assert(_ == Scale.Transform.Linear)

      test(m"an explicit calibration overrides one axis"):
        val chart = Series(t"a")((1.0, 10.0), (2.0, 1000.0)).chart(Lines(ordinate = calibrations.logarithmicCalibration))
        (chart.fit.abscissa.transform, chart.fit.ordinate.transform)
      . assert(_ == (Scale.Transform.Linear, Scale.Transform.Logarithmic))

      test(m"a scoped given overrides every axis of its type"):
        given Double is Calibration = calibrations.logarithmicCalibration
        Series(t"a")((1.0, 10.0), (2.0, 1000.0)).chart(Lines()).fit.abscissa.transform
      . assert(_ == Scale.Transform.Logarithmic)

      test(m"an imported calibration overrides every axis"):
        import calibrations.tightCalibration
        val fit = growth.chart(Lines()).fit
        (fit.abscissa.lower, fit.abscissa.upper, fit.ordinate.upper)
      . assert(_ == (0.0, 10.0, 10.0))

    suite(m"Fitting"):
      test(m"bands are the union of categories in first-appearance order"):
        val data = List(Series(t"a")((t"x", 1.0), (t"y", 2.0)), Series(t"b")((t"y", 3.0), (t"z", 4.0)))
        data.chart(Bars()).fit.bands.categories
      . assert(_ == Sequence(t"x", t"y", t"z"))

      test(m"a bar chart's ordinate is anchored at zero"):
        sales.chart(Bars()).fit.ordinate.lower
      . assert(_ == 0.0)

      test(m"a stacked chart's ordinate spans the largest total"):
        sales.chart(StackedBars()).fit.ordinate.upper
      . assert(_ == 10.0)

      test(m"Sturges' rule bins eight samples four ways"):
        Samples(t"s")(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0).chart(Histogram()).fit.edges.size
      . assert(_ == 5)

      test(m"the box summary of five values is the values"):
        Boxes.Summary.of(Sequence(5.0, 1.0, 4.0, 2.0, 3.0))
      . assert(_ == Boxes.Summary(1.0, 2.0, 3.0, 4.0, 5.0))

      test(m"quartiles interpolate between order statistics"):
        Boxes.Summary.of(Sequence(1.0, 2.0, 3.0, 4.0)).lowerQuartile
      . assert(_ == 1.75)

      test(m"a pie's total is the sum of its values"):
        Series(t"share")((t"a", 25.0), (t"b", 75.0)).chart(Pie()).fit.total
      . assert(_ == 100.0)

    suite(m"Drawing"):
      test(m"a chart's parts have stable identifiers"):
        val text = rendered(sales.chart(Bars()))
        List(t"backdrop", t"grid", t"abscissa", t"ordinate", t"series-0", t"series-1", t"legend").all: id =>
          text.contains(t"""id="$id"""")
      . assert(_ == true)

      test(m"grouped bars draw one rectangle per value, plus the legend's swatches"):
        occurrences(rendered(sales.chart(Bars())), t"<rect")
      . assert(_ == 1 + 6 + 2)

      test(m"a hidden legend draws no swatches"):
        given Chart.Standard = Chart.Standard(legend = Chart.Legend.Hidden)
        occurrences(rendered(sales.chart(Bars())), t"<rect")
      . assert(_ == 1 + 6)

      test(m"a line chart draws one polyline per series"):
        val text = rendered(List(growth, Series(t"decay")((0.0, 10.0), (10.0, 0.0))).chart(Lines()))
        occurrences(text, t"<polyline points=\"")
      . assert(_ >= 2)

      test(m"a scatter plot draws one marker per point"):
        given Chart.Standard = Chart.Standard(grid = false, legend = Chart.Legend.Hidden)
        occurrences(rendered(growth.chart(Scatter())), t"<circle")
      . assert(_ == 2)

      test(m"a pie draws a wedge and a percentage per category"):
        val text = rendered(Series(t"share")((t"a", 25.0), (t"b", 75.0)).chart(Pie()))
        (occurrences(text, t"<path"), text.contains(t"25%"), text.contains(t"75%"))
      . assert(_ == (2, true, true))

      test(m"a set of series plots the same as a list"):
        val fromSet = rendered(sales.to[Set].chart(Bars()))
        occurrences(fromSet, t"<rect")
      . assert(_ == 9)

      test(m"a sequence of series plots the same as a list"):
        occurrences(rendered(sales.to[Sequence].chart(Bars())), t"<rect")
      . assert(_ == 9)

      test(m"a box plot draws a box and whiskers per sample set"):
        val data = List(Samples(t"a")(1.0, 2.0, 3.0), Samples(t"b")(2.0, 4.0, 8.0))
        occurrences(rendered(data.chart(Boxes())), t"<rect")
      . assert(_ == 1 + 2 + 2)

      test(m"an error bar is drawn for an estimate"):
        given Chart.Standard = Chart.Standard(grid = false, legend = Chart.Legend.Hidden)
        val data = Series(t"a")((t"x", Estimate(5.0, 4.0, 6.0)))
        val text = rendered(data.chart(Bars())).s
        val start = text.indexOf("id=\"series-0\"")
        val end = text.indexOf("</g>", start)
        occurrences(text.substring(start, end).nn.tt, t"<polyline")
      . assert(_ == 3)

      test(m"a quantity's axis is titled with its dimension and unit"):
        val heights = Series(t"tide")((0.0, 1.2*Metre), (6.0, 4.6*Metre), (12.0, 1.1*Metre))
        rendered(heights.chart(Lines())).contains(t">distance / m<")
      . assert(_ == true)

      test(m"a style's title takes the unit from the quantity"):
        given Chart.Standard = Chart.Standard(ordinateTitle = t"tide height")
        val heights = Series(t"tide")((0.0, 1.2*Metre), (6.0, 4.6*Metre), (12.0, 1.1*Metre))
        rendered(heights.chart(Lines())).contains(t">tide height / m<")
      . assert(_ == true)

      test(m"a compound unit is rendered with its powers"):
        val speeds = Series(t"gust")((0.0, 3.0*Metre/Second), (1.0, 7.5*Metre/Second))
        rendered(speeds.chart(Lines())).contains(t">velocity / m·s¯¹<")
      . assert(_ == true)

      test(m"a duration axis is titled time, with units in its labels"):
        val latency = Series(t"p99")((0.0*Second, 0.012*Second), (30.0*Second, 0.019*Second))
        val text = rendered(latency.chart(Lines()))
        (text.contains(t">time<"), text.contains(t">12ms<"))
      . assert(_ == (true, true))

      test(m"axis titles are lettered"):
        given Chart.Standard = Chart.Standard(abscissaTitle = t"month", ordinateTitle = t"sales")
        val text = rendered(sales.chart(Bars()))
        (text.contains(t">month<"), text.contains(t">sales<"))
      . assert(_ == (true, true))

    suite(m"Font metrics"):
      test(m"the average metric is six tenths of an em per character"):
        summon[FontMetric].width(t"abcde").value
      . assert(_ == 3.0)

      test(m"a font metric sums the glyph advances"):
        FontMetric.of(TestFont.font).width(t"AB").value
      . assert(_ == 0.9)

      test(m"a measured legend is narrower for a narrow face"):
        val average = Framing.layout(Unset, Unset, List(t"ABC"))(using summon[Chart.Standard], summon[FontMetric])
        val measured = Framing.layout(Unset, Unset, List(t"ABC"))(using summon[Chart.Standard], FontMetric.of(TestFont.font))
        val width = measured.legend.let(_.width).or(0.0)
        (average.frame.width < measured.frame.width, (width - 32.4).abs < 0.000001)
      . assert(_ == (true, true))

    suite(m"Fonts"):
      given (Typeface of "Test") is Typesettable in Medium = Typesettable.embedded(TestFont.font)
      given (Typeface of "Menlo") is Typesettable in Web = Web.local()

      test(m"The metric of an embedded font is the font's own"):
        FontMetric.of(Web.font(Typeface["Test"].face)).width(t"AB").value
      . assert(_ == FontMetric.of(TestFont.font).width(t"AB").value)

      test(m"The metric of a font without a file is the average"):
        FontMetric.of(Web.font(Typeface["Menlo"].face)).width(t"AB").value
      . assert(_ == 1.2)

      test(m"A chart's SVG carries its font's @font-face"):
        given Chart.Standard = Chart.Standard(font = Web.font(Typeface["Menlo"].face))
        rendered(growth.chart(Lines())).s.contains("@font-face{font-family:\"Menlo\"")
      . assert(_ == true)

    suite(m"Revision"):
      test(m"unchanged data revises nothing"):
        growth.chart(Lines()).revise(growth)(1)
      . assert(_ == Nil)

      test(m"a point within the axes replaces only its series"):
        growth.chart(Lines()).revise(growth.add((5.0, 5.0)))(1).map(describe)
      . assert(_ == List(t"replace series-0"))

      test(m"a point beyond the axes redraws the chart"):
        growth.chart(Lines()).revise(growth.add((20.0, 20.0)))(1).map(describe)
      . assert(_ == List(t"redraw"))

      test(m"a revised chart keeps its fit when the data still fits"):
        val (next, _) = growth.chart(Lines()).revise(growth.add((5.0, 5.0)))
        (next.fit.abscissa.upper, next.fit.ordinate.upper)
      . assert(_ == (10.0, 10.0))

    suite(m"Styling"):
      test(m"a component's rendering is an override on the standard style"):
        given Chart.Standard = new Chart.Standard(legend = Chart.Legend.Hidden):
          override def bar(corner: Point, width: Double, height: Double, color: Color in Srgb, series: Int)
          :   List[Figure] =
            List(Circle(corner, (width/2.0).toFloat))

        val text = rendered(sales.chart(Bars()))
        (occurrences(text, t"<circle"), occurrences(text, t"<rect"))
      . assert(_ == (6, 1))

      test(m"an axis gains an arrowhead through its hook"):
        given Chart.Standard = new Chart.Standard():
          override def arrowhead(tip: Point, axis: Chart.Axis, color: Color in Srgb): List[Figure] =
            List(Polyline(List(tip, Point(tip.x - 6, tip.y - 3), Point(tip.x - 6, tip.y + 3)), closed = true))

        occurrences(rendered(growth.chart(Lines())), t"<polygon")
      . assert(_ == 2)

      test(m"a style shared by every kind still resolves for each"):
        given Chart.Standard = Chart.Standard(width = 320.0, height = 200.0)
        rendered(Series(t"share")((t"a", 1.0), (t"b", 3.0)).chart(Pie())).contains(t"viewBox=\"0 0 320.0 200.0\"")
      . assert(_ == true)

      test(m"an annotated point is labelled beside its marker"):
        val named = Series(t"cities")((1.0, Annotated(3.0, t"Paris")), (2.0, Annotated(5.0, t"Rome")))
        val text = rendered(named.chart(Scatter()))
        (text.contains(t">Paris<"), text.contains(t">Rome<"))
      . assert(_ == (true, true))

    suite(m"Names"):
      test(m"a series may be named by any showable value"):
        Series(2024)((t"jan", 1.0)).name
      . assert(_ == t"2024")

      test(m"samples may be named by an enum case"):
        Samples(Chart.Legend.Right)(1.0, 2.0).name
      . assert(_ == t"Right")

      test(m"a category may be made from any showable value"):
        Category(42).label
      . assert(_ == t"42")

    suite(m"Compatibility"):
      test(m"a categorical series cannot be a line chart"):
        demilitarize:
          Series(t"a")((t"x", 1.0)).chart(Lines())
        . exists(_.error)
      . assert(_ == true)

      test(m"several series cannot be a pie"):
        demilitarize:
          sales.chart(Pie())
        . exists(_.error)
      . assert(_ == true)

      test(m"samples cannot be bars"):
        demilitarize:
          Samples(t"s")(1.0, 2.0).chart(Bars())
        . exists(_.error)
      . assert(_ == true)
