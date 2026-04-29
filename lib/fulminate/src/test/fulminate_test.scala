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

      test(m"Doubly-embedded message renders bold-italics at depth 2 in colorText"):
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
