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
package fulminate

import gossamer.*
import larceny.*
import probably.*

object Tests extends Suite(m"Fulminate Tests"):
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
        val a = t"foo"
        val b = t"bar"
        m"a=$a, b=$b".text
      . assert(_ == t"a=foo, b=bar")

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
      . assert(_ == t"hello [3mworld[0m end")

      test(m"Doubly-embedded renders bold-italics at depth 2 in colorText"):
        val deepest = m"deep"
        val mid = m"mid $deepest mid"
        val outer = m"start $mid end"
        outer.colorText.s.contains("[3m[1mdeep[0m")
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
        val name = t"foo"
        m"a $name b".segments.size
      . assert(_ == 3)

      test(m"newline escape decodes to a literal newline"):
        m"line1\nline2".texts.head.s.indexOf('\n')
      . assert(_ == 5)

      test(m"backslash escape decodes to a literal backslash"):
        m"a\\b".texts.head
      . assert(_ == t"a\\b")

    suite(m"Backtick-delimited nesting"):
      test(m"single backtick pair produces an embedded message"):
        val msg = m"hello `world` today"
        (msg.texts, msg.messages.size, msg.messages.head.texts)
      . assert(_ == ((List(t"hello ", t" today"), 1, List(t"world"))))

      test(m"backticks render as nested italics in colorText"):
        m"hello `world` today".colorText
      . assert(_ == t"hello [3mworld[0m today")

      test(m"backticks at start of string"):
        m"`x` y".messages.head.texts
      . assert(_ == List(t"x"))

      test(m"backticks at end of string"):
        m"y `x`".messages.head.texts
      . assert(_ == List(t"x"))

      test(m"substitution inside backtick region attaches to inner message"):
        val name = t"y"
        val msg = m"`x $name`"
        (msg.texts, msg.messages.size, msg.messages.head.texts, msg.messages.head.messages.size)
      . assert(_ == ((List(t"", t""), 1, List(t"x ", t""), 1)))

      test(m"backticks across part boundaries"):
        val n = 1
        val msg = m"a `b $n c` d"
        (msg.texts, msg.messages.head.texts, msg.messages.head.messages.size)
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
        msg.messages.head.messages.head.texts
      . assert(_ == List(t"b"))

      test(m"depth 2 renders with bold-italic ANSI in colorText"):
        val inner = m"b"
        m"`a $inner c`".colorText.s.contains("[3m[1mb[0m")
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
      . assert(_.isEmpty)
