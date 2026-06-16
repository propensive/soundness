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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package punctuation

import soundness.*

import strategies.throwUnsafely

import doms.html.whatwg
import classloaders.systemClassloader

object Tests extends Suite(m"Punctuation tests"):
  def run(): Unit =
    case class Testcase
      ( markdown:   Text,
        html:       Text,
        example:    Int,
        start_line: Int,
        end_line:   Int,
        section:    Text )

    cp"/punctuation/mdspec.json"
    . read[Json]
    . as[List[Testcase]]
    . groupBy(_.section)
    . filter(_(0) != "HTML blocks")
    . each: (section, cases) =>
        suite(section.communicate):
          cases.each: testcase =>
            safely(testcase.html.read[Html of whatwg.Flow]).let: html =>
              if !Set(308, 309, 475, 598, 605)(testcase.example) then
                test(m"Commonmark test case ${testcase.example}"):
                  Parser.parse(testcase.markdown).html.show
                . assert(_ == html.show)

    suite(m"Serializer round-trip"):
      def roundTrip(markdown: Text): Markdown of Layout =
        Parser.parse(Parser.parse(markdown).source)

      test(m"simple heading"):
        roundTrip(t"# Title\n").children.head
      . assert:
          case Layout.Heading(_, 1, Prose.Textual(t"Title")) => true
          case _                                             => false

      test(m"emphasis and strong"):
        roundTrip(t"Hello **bold** and *em* here.\n").children.head
      . assert:
          case Layout.Paragraph(_, prose*) =>
            prose.exists:
              case Prose.Strong(_*) => true
              case _                => false
            && prose.exists:
              case Prose.Emphasis(_*) => true
              case _                  => false

          case _ => false

      test(m"fenced code block preserves content"):
        val src = t"```scala\nval x = 1\n```\n"
        val first = Parser.parse(src).children.head
        val again = Parser.parse(Parser.parse(src).source).children.head

        (first, again).match
          case (Layout.CodeBlock(_, a, b), Layout.CodeBlock(_, c, d)) => (a, b) == (c, d)
          case _                                                     => false
      . assert(_ == true)

      test(m"link with title"):
        val src = t"See [docs](https://example.org \"Docs\") here.\n"
        Parser.parse(Parser.parse(src).source).children.head
      . assert:
          case Layout.Paragraph(_, _, Prose.Link(t"https://example.org", t"Docs", _*), _*) => true
          case _                                                                          => false

      test(m"blockquote nests paragraph"):
        val src = t"> hello\n"
        Parser.parse(Parser.parse(src).source).children.head
      . assert:
          case Layout.BlockQuote(_, Layout.Paragraph(_, Prose.Textual(t"hello"))) => true
          case _                                                                  => false

      test(m"bullet list with two items"):
        val src = t"- one\n- two\n"
        Parser.parse(Parser.parse(src).source).children.head
      . assert:
          case Layout.BulletList(_, true, items*) if items.size == 2 => true
          case _                                                     => false

      test(m"ordered list with two items"):
        val src = t"1. one\n2. two\n"
        Parser.parse(Parser.parse(src).source).children.head
      . assert:
          case Layout.OrderedList(_, 1, true, _, items*) if items.size == 2 => true
          case _                                                            => false

    suite(m"Terminal renderer"):
      import hyphenations.englishHyphenation
      import termcapDefinitions.xtermTrueColorTermcap

      test(m"heading is styled and followed by a rule"):
        val md = Parser.parse(t"# Hello\n")
        md.terminal(width = 20).plain
      . assert(_.s.contains("Hello"))

      test(m"link content carries an OSC 8 escape"):
        val md = Parser.parse(t"See [home](https://example.org/) here.\n")
        md.terminal(width = 60).render(xtermTrueColorTermcap).s
      . assert(_.contains("]8;;https://example.org/"))

      test(m"long word hyphenates at width 20"):
        val md = Parser.parse(t"supercalifragilisticexpialidocious is a word.\n")
        md.terminal(width = 20).plain.s
      . assert(_.contains("‐"))

      test(m"thematic break is a horizontal rule of the requested width"):
        val md = Parser.parse(t"---\n")
        md.terminal(width = 10).plain.s
      . assert(_.contains("──────────"))

      test(m"Printable reads width from Termcap"):
        given Termcap:
          def ansi = false
          def color = ColorDepth.NoColor
          override def width = 12

        val md = Parser.parse(t"---\n")
        summon[(Markdown of Layout) is Printable].print(md, summon[Termcap]).s
      . assert(_.contains("────────────"))
