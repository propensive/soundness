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
package escritoire

import soundness.*

import textMetrics.uniformMetric
import denominative.dysasymptotics.linearSize
import denominative.dysasymptotics.linearAccess

case class Person(name: Text, age: Int)

object Tests extends Suite(m"Escritoire tests"):
  val people: List[Person] = List(Person(t"Alice", 30), Person(t"Bob", 5))

  def render[row](scaffold: Scaffold[row, Text], data: List[row], width: Int)
     (using TableStyle, Attenuation^)
  :   List[Text] =
    scaffold.tabulate(data).grid(width).render.to[List]

  def run(): Unit =
    // ─── TextAlignment ──────────────────────────────────────────────────────

    test(m"Left alignment pads on the right"):
      TextAlignment.Left.pad(t"hi", 6, true)
    . assert(_ == t"hi    ")

    test(m"Right alignment pads on the left"):
      TextAlignment.Right.pad(t"hi", 6, true)
    . assert(_ == t"    hi")

    test(m"Center alignment splits padding evenly"):
      TextAlignment.Center.pad(t"hi", 6, true)
    . assert(_ == t"  hi  ")

    test(m"Center alignment puts the extra space on the right"):
      TextAlignment.Center.pad(t"hi", 7, true)
    . assert(_ == t"  hi   ")

    test(m"Justify spreads spaces between words on non-final lines"):
      TextAlignment.Justify.pad(t"a b c", 9, false)
    . assert(_ == t"a   b   c")

    test(m"Justify left-aligns the final line"):
      TextAlignment.Justify.pad(t"a b c", 9, true)
    . assert(_ == t"a b c    ")

    // ─── Columnation ────────────────────────────────────────────────────────

    test(m"Items fill as many columns as fit, reading across"):
      List(t"a", t"bb", t"c", t"dd", t"e").columnate(11)
    . assert(_ == List(t"a   bb  c", t"dd  e"))

    test(m"A narrow column may be shrunk to fit one more column"):
      List(t"aaaa", t"b", t"c", t"d").columnate(12)
    . assert(_ == List(t"aaaa  b   c", t"d"))

    test(m"Uniform columns are all as wide as the widest item"):
      List(t"aaaa", t"b", t"c", t"d").columnate(12, uniform = true)
    . assert(_ == List(t"aaaa  b", t"c     d"))

    test(m"Spare width widens shrunk columns back towards equality"):
      List(t"aaaa", t"b", t"c", t"d").columnate(16)
    . assert(_ == List(t"aaaa  b   c   d"))

    test(m"A downward layout reads down each column, dropping empty trailing columns"):
      List(t"a", t"b", t"c", t"d", t"e").columnate(7, downward = true)
    . assert(_ == List(t"a  c  e", t"b  d"))

    test(m"Everything fits on one row when the width allows"):
      List(t"a", t"b", t"c").columnate(80, gap = 1)
    . assert(_ == List(t"a b c"))

    test(m"An item wider than the width still occupies a single column"):
      List(t"abcdef", t"g").columnate(4)
    . assert(_ == List(t"abcdef", t"g"))

    test(m"Right alignment pads the last column too"):
      List(t"a", t"bb").columnate(10, align = TextAlignment.Right)
    . assert(_ == List(t" a  bb"))

    test(m"The documented example lays out as documented"):
      List(t"apple", t"fig", t"kiwi", t"lime", t"pear", t"plum").columnate(24)
    . assert(_ == List(t"apple  fig    kiwi  lime", t"pear   plum"))

    test(m"No items make no lines"):
      List[Text]().columnate(10)
    . assert(_ == Nil)

    // ─── Column combinators ─────────────────────────────────────────────────

    test(m"Column retitle changes the title"):
      Column[Person, Text, Text](t"Name")(_.name).retitle(t"Forename").title
    . assert(_ == t"Forename")

    test(m"Column contramap adapts the row type"):
      val nameColumn = Column[Person, Text, Text](t"Name")(_.name)
      nameColumn.contramap[(Person, Int)](_(0)).get((Person(t"Zoe", 9), 1))
    . assert(_ == t"Zoe")

    test(m"Int column is right-aligned by default"):
      Column[Person, Int, Text](t"Age")(_.age).textAlign
    . assert(_ == TextAlignment.Right)

    test(m"Text column is left-aligned by default"):
      Column[Person, Text, Text](t"Name")(_.name).textAlign
    . assert(_ == TextAlignment.Left)

    // ─── Basic rendering ────────────────────────────────────────────────────

    val scaffold =
      Scaffold[Person, Text]
        ( Column(t"Name")(_.name),
          Column(t"Age")(_.age) )

    test(m"Render a simple table with rounded borders"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.ignoreAttenuation
      render(scaffold, people, 40)
    . assert:
        _ == List
          ( t"╭───────┬─────╮",
            t"│ Name  │ Age │",
            t"├───────┼─────┤",
            t"│ Alice │  30 │",
            t"│ Bob   │   5 │",
            t"╰───────┴─────╯" )

    test(m"Render a simple table with the default thick border style"):
      import tableStyles.thickTableStyle
      import columnAttenuation.ignoreAttenuation
      render(scaffold, people, 40)
    . assert:
        _ == List
          ( t"┏━━━━━━━┯━━━━━┓",
            t"┃ Name  │ Age ┃",
            t"┠───────┼─────┨",
            t"┃ Alice │  30 ┃",
            t"┃ Bob   │   5 ┃",
            t"┗━━━━━━━┷━━━━━┛" )

    test(m"Horizontal style has rules but no vertical lines"):
      import tableStyles.horizontalTableStyle
      import columnAttenuation.ignoreAttenuation
      render(scaffold, people, 40)
    . assert:
        _ == List
          ( t"╶─────────────╴",
            t"  Name    Age  ",
            t"╶─────────────╴",
            t"  Alice    30  ",
            t"  Bob       5  ",
            t"╶─────────────╴" )

    test(m"Minimal style has only a title rule"):
      import tableStyles.minimalTableStyle
      import columnAttenuation.ignoreAttenuation
      render(scaffold, people, 40)
    . assert:
        _ == List
          ( t"  Name    Age  ",
            t"╶─────────────╴",
            t"  Alice    30  ",
            t"  Bob       5  " )

    test(m"Number column right-aligns its values"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.ignoreAttenuation
      render(scaffold, people, 40).at(Quat).or(t"")
    . assert(_ == t"│ Alice │  30 │")

    // ─── Paragraph wrapping ─────────────────────────────────────────────────

    val wrapping =
      Scaffold[Person, Text]
        ( Column(t"Phrase", sizing = columnar.Paragraph)(_ => t"the quick brown fox"),
          Column(t"Age")(_.age) )

    test(m"A paragraph column wraps text across several lines"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.ignoreAttenuation
      render(wrapping, List(Person(t"Alice", 30)), 18)
    . assert:
        _ == List
          ( t"╭──────────┬─────╮",
            t"│ Phrase   │ Age │",
            t"├──────────┼─────┤",
            t"│ the      │  30 │",
            t"│ quick    │     │",
            t"│ brown    │     │",
            t"│ fox      │     │",
            t"╰──────────┴─────╯" )

    test(m"A wrapped cell increases the height of its whole row"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.ignoreAttenuation
      render(wrapping, List(Person(t"Alice", 30)), 18).size
    . assert(_ == 8)

    // ─── Fixed-width truncation ─────────────────────────────────────────────

    val truncating =
      Scaffold[Person, Text]
        ( Column(t"Fixed", sizing = columnar.Fixed(6))(_ => t"abcdefghij") )

    test(m"A Fixed column truncates over-long cells with an ellipsis"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.ignoreAttenuation
      render(truncating, List(Person(t"Alice", 30)), 40).at(Quat).or(t"")
    . assert(_ == t"│ abcde… │")

    // ─── Derivation ─────────────────────────────────────────────────────────

    test(m"A case class table is derived with capitalized field-name titles"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.ignoreAttenuation
      summon[Person is Tabulable[Text]].tabulate(people).grid(40).render.to[List]
    . assert:
        _ == List
          ( t"╭───────┬─────╮",
            t"│ Name  │ Age │",
            t"├───────┼─────┤",
            t"│ Alice │  30 │",
            t"│ Bob   │   5 │",
            t"╰───────┴─────╯" )

    test(m"TableRelabelling overrides a derived column title"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.ignoreAttenuation
      given TableRelabelling[Person] = () => Map(t"name" -> t"Full Name")
      summon[Person is Tabulable[Text]].tabulate(people).grid(40).render.to[List].prim.or(t"")
    . assert(_ == t"╭───────────┬─────╮")

    test(m"A sequence of integers can be tabulated directly"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.ignoreAttenuation
      List(1, 22, 333).tabulation.grid(20).render.to[List]
    . assert:
        _ == List
          ( t"╭─────╮",
            t"│     │",
            t"├─────┤",
            t"│   1 │",
            t"│  22 │",
            t"│ 333 │",
            t"╰─────╯" )

    // ─── Display-width sizing, collapse and vertical alignment ─────────────

    test(m"wide characters size the column by display width"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.ignoreAttenuation
      import hieroglyph.textMetrics.wideCharacterWidthMetric
      val cjk = Scaffold[Person, Text](Column(t"N")(_ => t"日本"))
      cjk.tabulate(List(Person(t"Alice", 30))).grid(40).render.to[List]
    . assert:
        _ == List
          ( t"╭──────╮",
            t"│ N    │",
            t"├──────┤",
            t"│ 日本 │",
            t"╰──────╯" )

    test(m"a Collapsible column is dropped when space is scarce"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.ignoreAttenuation

      val collapsing =
        Scaffold[Person, Text]
          ( Column(t"Name")(_.name),
            Column(t"Note", sizing = columnar.Collapsible(0.5))(_ => t"annotation") )

      render(collapsing, people, 14)
    . assert:
        _ == List
          ( t"╭───────╮",
            t"│ Name  │",
            t"├───────┤",
            t"│ Alice │",
            t"│ Bob   │",
            t"╰───────╯" )

    test(m"a Collapsible column survives when there is room"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.ignoreAttenuation

      val collapsing =
        Scaffold[Person, Text]
          ( Column(t"Name")(_.name),
            Column(t"Note", sizing = columnar.Collapsible(0.5))(_ => t"annotation") )

      render(collapsing, people, 30)
    . assert:
        _ == List
          ( t"╭───────┬────────────╮",
            t"│ Name  │ Note       │",
            t"├───────┼────────────┤",
            t"│ Alice │ annotation │",
            t"│ Bob   │ annotation │",
            t"╰───────┴────────────╯" )

    test(m"Bottom vertical alignment pads short cells from above"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.ignoreAttenuation

      val aligned =
        Scaffold[Person, Text]
          ( Column(t"A")(_ => t"one two"),
            Column(t"B", verticalAlign = VerticalAlignment.Bottom)(_ => t"x") )

      render(aligned, List(Person(t"Alice", 30)), 13)
    . assert:
        _ == List
          ( t"╭───────┬───╮",
            t"│ A     │ B │",
            t"├───────┼───┤",
            t"│ one   │   │",
            t"│ two   │ x │",
            t"╰───────┴───╯" )

    // ─── Incremental layout ─────────────────────────────────────────────────

    // The table rendered row by row against a layout the rows were admitted to one at a time:
    // what a live view does, and what must match `grid.render` line for line.
    def incremental[row](scaffold: Scaffold[row, Text], data: List[row], width: Int)
       (using TableStyle, Attenuation^)
    :   List[Text] =
      val cells: List[Cells[Text]] = data.map(scaffold.cells(_))
      val layout = cells.fold(scaffold.layout(width)) { (layout, cells) => layout.extend(cells) }
      val body: List[Text] = cells.bind { (cells: Cells[Text]) => layout.lines(cells, Nil) }
      (layout.topRule.let(List(_)).or(Nil) + layout.titleLines + List(layout.titleRule) + body
          + layout.bottomRule.let(List(_)).or(Nil))

    test(m"the metrics of lines are the widest word and the widest line"):
      Columnar.metrics(Array(t"hello world", t"hi"))
    . assert(_ == Metrics(5, 11))

    test(m"a row within a layout's aggregates is accommodated, and extending with it is the identity"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.ignoreAttenuation
      val layout = scaffold.layout(40).extend(scaffold.cells(Person(t"Alice", 30)))
      val bob = scaffold.cells(Person(t"Bob", 5))
      (layout.accommodates(bob), layout.extend(bob) eq layout)
    . assert(_ == (true, true))

    test(m"a wider row is not accommodated and widens the aggregates"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.ignoreAttenuation
      val layout = scaffold.layout(40).extend(scaffold.cells(Person(t"Bob", 5)))
      val alice = scaffold.cells(Person(t"Alexandra", 30))
      val extended = layout.extend(alice)
      (layout.accommodates(alice), extended eq layout, extended.aggregates.readable(0).natural)
    . assert(_ == (false, false, 9))

    test(m"a Fixed column accommodates any content"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.ignoreAttenuation
      val layout = truncating.layout(40)
      layout.accommodates(truncating.cells(Person(t"Alice", 30)))
    . assert(_ == true)

    test(m"a row that widens a stretched column's claim but not its width leaves the layout stable"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.ignoreAttenuation
      // At 18 cells the phrase column is already pinned by the width, so a longer phrase
      // re-solves to the same survivors.
      val layout = wrapping.layout(18).extend(wrapping.cells(Person(t"Alice", 30)))
      val longer = Scaffold[Person, Text](Column(t"Phrase", sizing = columnar.Paragraph)(_ => t"the quick brown fox jumps"), Column(t"Age")(_.age)).cells(Person(t"Bob", 5))
      val extended = layout.extend(longer)
      (extended eq layout, extended.stable(layout))
    . assert(_ == (false, true))

    test(m"rendering row by row against an incremental layout matches the grid, in every style"):
      import columnAttenuation.ignoreAttenuation
      val styles: List[TableStyle] =
        List(tableStyles.thickTableStyle, tableStyles.horizontalTableStyle, tableStyles.midOnlyTableStyle,
            tableStyles.minimalTableStyle, tableStyles.thinRoundedTableStyle, tableStyles.verticalTableStyle)

      val collapsing =
        Scaffold[Person, Text]
          ( Column(t"Name")(_.name),
            Column(t"Note", sizing = columnar.Collapsible(0.5))(_ => t"annotation") )

      styles.all: style =>
        given TableStyle = style
        render(scaffold, people, 40) == incremental(scaffold, people, 40)
        && render(wrapping, people, 18) == incremental(wrapping, people, 18)
        && render(truncating, people, 40) == incremental(truncating, people, 40)
        && render(collapsing, people, 14) == incremental(collapsing, people, 14)
        && render(collapsing, people, 30) == incremental(collapsing, people, 30)
    . assert(_ == true)

    test(m"a row's height is its rendered line count, wrapped or not"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.ignoreAttenuation
      val alice = wrapping.cells(Person(t"Alice", 30))
      val narrow = wrapping.layout(18).extend(alice)
      val wide = wrapping.layout(40).extend(alice)
      val cut = truncating.layout(40).extend(truncating.cells(Person(t"Alice", 30)))
      val empty = Scaffold[Person, Text](Column(t"Name")(_.name), Column(t"Note")(_ => t"")).pipe: blank =>
        val cells = blank.cells(Person(t"Alice", 30))
        val layout = blank.layout(40).extend(cells)
        (layout.height(cells), layout.lines(cells, Nil).size)
      ( (narrow.height(alice), narrow.lines(alice, Nil).size),
        (wide.height(alice), wide.lines(alice, Nil).size),
        (cut.height(truncating.cells(Person(t"Alice", 30))), cut.lines(truncating.cells(Person(t"Alice", 30)), Nil).size),
        empty )
    . assert(_ == ((4, 4), (1, 1), (1, 1), (1, 1)))

    test(m"resizing a layout matches a fresh layout at the new width"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.ignoreAttenuation
      val cells = people.map(scaffold.cells(_))
      val layout = cells.fold(scaffold.layout(40)) { (layout, cells) => layout.extend(cells) }
      val fresh = cells.fold(scaffold.layout(12)) { (layout, cells) => layout.extend(cells) }
      layout.resize(12).survivors == fresh.survivors
    . assert(_ == true)

    test(m"a tabulation's layout has the widths its grid renders with"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.ignoreAttenuation
      val tabulation = scaffold.tabulate(people)
      tabulation.layout(40).widths.readable.to(List) == tabulation.grid(40).sections.prim.let(_.widths.readable.to(List)).or(Nil)
    . assert(_ == true)

    // ─── Attenuation ────────────────────────────────────────────────────────

    test(m"failAttenuation raises a Table.Error when the table is too wide"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.failAttenuation
      val wide = Scaffold[Person, Text](Column(t"Name", sizing = columnar.Fixed(20))(_.name))
      safely(wide.tabulate(people).grid(5).render.to[List]).absent
    . assert(_ == true)

    test(m"ignoreAttenuation renders without raising when the table is too wide"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.ignoreAttenuation
      val wide = Scaffold[Person, Text](Column(t"Name", sizing = columnar.Fixed(20))(_.name))
      safely(wide.tabulate(people).grid(5).render.to[List]).absent
    . assert(_ == false)
