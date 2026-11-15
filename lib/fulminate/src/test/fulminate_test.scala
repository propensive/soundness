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
package fulminate

import soundness.*
import denominative.dysasymptotics.linearSize

object Tests extends Suite("messages", m"Fulminate Tests"):
  def run(): Unit =
    suite(m"Existing behaviour"):
      test(m"Whitespace in strings embedded into text should be quoted"):
        m"This (${t" "}) should be quoted".text
      . assert(_ == t"This (“ ”) should be quoted")

      test(m"Static text renders unchanged"):
        m"hello".text
      . assert(_ == t"hello")

      test(m"Single substitution"):
        val name = t"world"
        m"hello $name".text
      . assert(_ == t"hello world")

      test(m"Multiple substitutions render in order"):
        val a = t"Romeo"
        val b = t"Juliet"
        m"a=$a, b=$b".text
      . assert(_ == t"a=Romeo, b=Juliet")

      test(m"Substitution at start"):
        val x = t"start"
        m"${x}-end".text
      . assert(_ == t"start-end")

      test(m"Static text has texts.size 1"):
        m"hello".texts.size
      . assert(_ == 1)

      test(m"Static text has messages.size 0"):
        m"hello".messages.size
      . assert(_ == 0)

      test(m"Two substitutions yield 3 text segments and 2 messages"):
        val msg = m"a ${1} b ${2} c"
        (msg.texts.size, msg.messages.size)
      . assert(_ == ((3, 2)))

      test(m"Append concatenates texts at the boundary"):
        (m"hello " + m"world").text
      . assert(_ == t"hello world")

      test(m"Append yields one continuous text segment when boundary merges"):
        (m"hello " + m"world").texts.size
      . assert(_ == 1)

      test(m"Embedded message renders italics at depth 1 in colorText"):
        val inner = m"world"
        val outer = m"hello $inner end"
        outer.colorText
      . assert(_ == t"hello \u001B[3mworld\u001B[0m end")

      test(m"Doubly-embedded renders bold-italics at depth 2 in colorText"):
        val deepest = m"deep"
        val mid = m"mid $deepest mid"
        val outer = m"start $mid end"
        outer.colorText.s.contains("\u001B[3m\u001B[1mdeep\u001B[0m")
      . assert(_ == true)

      test(m"Int substitution renders as decimal"):
        val n: Int = 42
        m"value=$n".text
      . assert(_ == t"value=42")

      test(m"Char substitution renders as the character"):
        val c: Char = 'X'
        m"char=$c".text
      . assert(_ == t"char=X")

      test(m"segments interleaves texts and messages"):
        val name = t"Puck"
        m"a $name b".segments.size
      . assert(_ == 3)

      test(m"newline escape decodes to a literal newline"):
        m"line1\nline2".texts.head.let(_.s.indexOf('\n'))
      . assert(_ == 5)

      test(m"backslash escape decodes to a literal backslash"):
        m"a\\b".texts.head
      . assert(_ == t"a\\b")

    suite(m"Bolt codes"):
      test(m"A code of two numbers is written with its project's letter"):
        Bolt('s', 87, 3).text
      . assert(_ == t"↯/s.87.3")

      test(m"A variant and a site follow the error"):
        (Bolt('s', 87, 3, 2).text, Bolt('s', 87, 3, 2, 5).text)
      . assert(_ == (t"↯/s.87.3.2", t"↯/s.87.3.2.5"))

      test(m"A code carrying another is a compound"):
        Bolt('s', 118, 1).carrying(Bolt('t', 1, 7)).text
      . assert(_ == t"↯/s.118.1/t.1.7")

      test(m"A compound carrying another puts it innermost"):
        Bolt('p', 2, 2).carrying(Bolt('s', 118, 1)).carrying(Bolt('t', 1, 7)).text
      . assert(_ == t"↯/p.2.2/s.118.1/t.1.7")

      test(m"The label is the code in brackets"):
        Bolt('s', 87, 3, 2).label
      . assert(_ == t"[↯/s.87.3.2]")

      test(m"A code is parsed from its text"):
        Bolt.parse(t"↯/s.118.1/t.1.7")
      . assert(_ == Bolt('s', 118, 1, cause = Bolt('t', 1, 7)))

      test(m"Text which is not a code is uncoded"):
        ( Bolt.parse(t"SN-077").coded,
          Bolt.parse(t"↯/s.87").coded,
          Bolt.parse(t"↯/s.1.2.3.4.5").coded )
      . assert(_ == (false, false, false))

      test(m"A code is read from the start of a message, with its brackets and the space"):
        val text = t"[↯/s.87.3.2] the port is taken"
        val (bolt, length) = Bolt.prefix(text)
        (bolt.text, text.s.substring(length).nn.tt)
      . assert(_ == (t"↯/s.87.3.2", t"the port is taken"))

      test(m"A message without a code has no prefix"):
        Bolt.prefix(t"the port is taken")
      . assert(_ == (Bolt.Uncoded, 0))

      test(m"An unnumbered error has no code"):
        Bolt.Uncoded.text
      . assert(_ == t"")

    suite(m"Backtick-delimited nesting"):
      test(m"single backtick pair produces an embedded message"):
        val msg = m"hello `world` today"
        (msg.texts, msg.messages.size, msg.messages.head.let(_.texts))
      . assert(_ == ((List(t"hello ", t" today"), 1, List(t"world"))))

      test(m"backticks render as nested italics in colorText"):
        m"hello `world` today".colorText
      . assert(_ == t"hello \u001B[3mworld\u001B[0m today")

      test(m"backticks at start of string"):
        m"`x` y".messages.head.let(_.texts)
      . assert(_ == List(t"x"))

      test(m"backticks at end of string"):
        m"y `x`".messages.head.let(_.texts)
      . assert(_ == List(t"x"))

      test(m"substitution inside backtick region attaches to inner message"):
        val name = t"y"
        val msg = m"`x $name`"
        (msg.texts, msg.messages.size, msg.messages.head.let(_.texts), msg.messages.head.let(_.messages.size))
      . assert(_ == ((List(t"", t""), 1, List(t"x ", t""), 1)))

      test(m"backticks across part boundaries"):
        val n = 1
        val msg = m"a `b $n c` d"
        (msg.texts, msg.messages.head.let(_.texts), msg.messages.head.let(_.messages.size))
      . assert(_ == ((List(t"a ", t" d"), List(t"b ", t" c"), 1)))

      test(m"two adjacent backtick pairs produce two inner messages"):
        m"`a` `b`".messages.size
      . assert(_ == 2)

      test(m"two adjacent backtick pairs preserve content"):
        m"`a` `b`".messages.map(_.texts.head)
      . assert(_ == List(t"a", t"b"))

      test(m"depth 2 via substitution into a backtick region"):
        val inner = m"b"
        val msg = m"`a $inner c`"
        msg.messages.head.let(_.messages.head).let(_.texts)
      . assert(_ == List(t"b"))

      test(m"depth 2 renders with bold-italic ANSI in colorText"):
        val inner = m"b"
        m"`a $inner c`".colorText.s.contains("\u001B[3m\u001B[1mb\u001B[0m")
      . assert(_ == true)

      test(m"unclosed backtick is a compile error"):
        demilitarize:
          m"hello `world"
        . map(_.message)
      . assert(_.exists(_.contains("unmatched backtick")))

      test(m"stray closing backtick is a compile error"):
        demilitarize:
          m"hello` world"
        . map(_.message)
      . assert(_.exists(_.contains("unmatched backtick")))

      test(m"unclosed nested backtick is a compile error"):
        demilitarize:
          m"`a `b` c"
        . map(_.message)
      . assert(_.exists(_.contains("unmatched backtick")))

      test(m"well-formed backticks compile without error"):
        demilitarize:
          m"hello `world`"
      . assert(_.nil)
