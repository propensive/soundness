/*
    Escapade, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package escapade

import probably.*
import yossarian.*
import spectacular.*
import rudiments.*
import perforate.*
import gossamer.*

import escapes.*

object Tests extends Suite(t"Escapade tests"):
  def run(): Unit =
    suite(t"Rendering tests"):
      test(t"normal string"):
        e"hello world".render
      .assert(_ == t"hello world")
      
      test(t"simple string substitution"):
        e"hello ${"world"}".render
      .assert(_ == t"hello world")
      
      test(t"bold text"):
        e"$Bold{bold} text".render
      .assert(_ == t"\e[1mbold\e[22m text")
      
      test(t"italic text"):
        e"$Italic{italic} text".render
      .assert(_ == t"\e[3mitalic\e[23m text")
      
      test(t"24-bit colored text"):
        e"${iridescence.colors.Tan}[text]".render
      .assert(_ == t"\e[38;2;210;180;139mtext\e[39m")
      
      test(t"non-escape insertion should not parse brackets"):
        val notAnEscape = 42
        e"${notAnEscape}[text]".render
      .assert(_ == t"42[text]")

    suite(t"Escaping tests"):
      test(t"Check that an escaped tab is a tab"):
        e"|\t|".plain
      .assert(_.length == 3)
      
      test(t"Check that a unicode value is converted"):
        e"|\u0040|".plain
      .assert(_ == t"|@|")
      
      test(t"Check that a newline is converted correctly"):
        e"\n".plain
      .assert(_ == t"\n")

    suite(t"Screenbuffer tests"):
      import errorHandlers.throwUnsafely
      val pty = Pty(80, 1)
      val boldSample = pty.consume(e"This text is $Bold(bold).".render).buffer
      println(e"start$Bold(bold$Italic(bold-italic)unitalic)end".render)
      println(e"start$Bold(bold$Italic(bold-italic)unitalic)end".render.debug)
      val boldItalicSample = pty.consume(e"start$Bold(bold$Italic(bold-italic)unitalic)end".render).buffer
      
      test(t"Check that bold text is bold"):
        boldSample.find(t"bold").vouch(using Unsafe).styles
      .assert(_.forall(_.bold))
      
      test(t"Check that text before bold text is not bold"):
        boldSample.find(t"This text is ").vouch(using Unsafe).styles
      .assert(_.forall(!_.bold))
      
      test(t"Check that text after bold text is not bold"):
        boldSample.find(t".").vouch(using Unsafe).styles
      .assert(_.forall(!_.bold))

      test(t"Check that nested bold/italic is both"):
        boldItalicSample.find(t"bold-italic").vouch(using Unsafe).styles
      .assert(_.forall { style => style.bold && style.italic })
      
      test(t"Check that nested italic is removed but not bold"):
        println(boldItalicSample.find(t"unitalic").vouch(using Unsafe).styles.debug)
        boldItalicSample.find(t"unitalic").vouch(using Unsafe).styles
      .assert(_.forall { style => style.bold && !style.italic })
      
      test(t"Check that nested non-bold, non-italic text is neither"):
        boldItalicSample.find(t"end").vouch(using Unsafe).styles
      .assert(_.forall { style => !style.bold && !style.italic })