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

case class Person(name: Text, age: Int)

object Tests extends Suite(m"Escritoire tests"):
  val people: List[Person] = List(Person(t"Alice", 30), Person(t"Bob", 5))

  def render[row](scaffold: Scaffold[row, Text], data: Seq[row], width: Int)
     (using TableStyle, Attenuation^)
  :   List[Text] =
    scaffold.tabulate(data).grid(width).render.to(List)

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
      import tableStyles.defaultTableStyle
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
      render(scaffold, people, 40)(3)
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
      render(wrapping, List(Person(t"Alice", 30)), 18).length
    . assert(_ == 8)

    // ─── Fixed-width truncation ─────────────────────────────────────────────

    val truncating =
      Scaffold[Person, Text]
        ( Column(t"Fixed", sizing = columnar.Fixed(6))(_ => t"abcdefghij") )

    test(m"A Fixed column truncates over-long cells with an ellipsis"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.ignoreAttenuation
      render(truncating, List(Person(t"Alice", 30)), 40)(3)
    . assert(_ == t"│ abcde… │")

    // ─── Derivation ─────────────────────────────────────────────────────────

    test(m"A case class table is derived with capitalized field-name titles"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.ignoreAttenuation
      summon[Person is Tabulable[Text]].tabulate(people).grid(40).render.to(List)
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
      summon[Person is Tabulable[Text]].tabulate(people).grid(40).render.to(List).head
    . assert(_ == t"╭───────────┬─────╮")

    test(m"A sequence of integers can be tabulated directly"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.ignoreAttenuation
      List(1, 22, 333).tabulation.grid(20).render.to(List)
    . assert:
        _ == List
          ( t"╭─────╮",
            t"│     │",
            t"├─────┤",
            t"│   1 │",
            t"│  22 │",
            t"│ 333 │",
            t"╰─────╯" )

    // ─── Attenuation ────────────────────────────────────────────────────────

    test(m"failAttenuation raises a TableError when the table is too wide"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.failAttenuation
      val wide = Scaffold[Person, Text](Column(t"Name", sizing = columnar.Fixed(20))(_.name))
      safely(wide.tabulate(people).grid(5).render.to(List)).absent
    . assert(_ == true)

    test(m"ignoreAttenuation renders without raising when the table is too wide"):
      import tableStyles.thinRoundedTableStyle
      import columnAttenuation.ignoreAttenuation
      val wide = Scaffold[Person, Text](Column(t"Name", sizing = columnar.Fixed(20))(_.name))
      safely(wide.tabulate(people).grid(5).render.to(List)).absent
    . assert(_ == false)
