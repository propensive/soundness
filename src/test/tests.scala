/*
    Escapade, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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
import iridescence.*, colors.{Red, Yellow}
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
      test(t"an escaped tab is a tab"):
        e"|\t|".plain
      .assert(_.length == 3)
      
      test(t"a unicode value is converted"):
        e"|\u0040|".plain
      .assert(_ == t"|@|")
      
      test(t"a newline is converted correctly"):
        e"\n".plain
      .assert(_ == t"\n")

    suite(t"Screenbuffer tests"):
      import errorHandlers.throwUnsafely
      val pty = Pty(80, 1)
      val boldSample = pty.consume(e"This text is $Bold(bold).".render).buffer
      val boldItalicSample = pty.consume(e"start$Bold(bold$Italic(bold-italic)unitalic)end".render).buffer
      val boldItalicSample2 = pty.consume(e"start$Bold($Italic(bold-italic))end".render).buffer
      
      test(t"bold text is bold"):
        boldSample.find(t"bold").vouch(using Unsafe).styles
      .assert(_.forall(_.bold))
      
      test(t"text before bold text is not bold"):
        boldSample.find(t"This text is ").vouch(using Unsafe).styles
      .assert(_.forall(!_.bold))
      
      test(t"text after bold text is not bold"):
        boldSample.find(t".").vouch(using Unsafe).styles
      .assert(_.forall(!_.bold))

      test(t"nested bold/italic is both"):
        boldItalicSample.find(t"bold-italic").vouch(using Unsafe).styles
      .assert(_.forall { style => style.bold && style.italic })
      
      test(t"nested italic is removed but not bold"):
        boldItalicSample.find(t"unitalic").vouch(using Unsafe).styles
      .assert(_.forall { style => style.bold && !style.italic })
      
      test(t"nested non-bold, non-italic text is neither"):
        boldItalicSample.find(t"end").vouch(using Unsafe).styles
      .assert(_.forall { style => !style.bold && !style.italic })
      
      test(t"nested non-bold, non-italic text is neither"):
        boldItalicSample.find(t"end").vouch(using Unsafe).styles
      .assert(_.forall { style => !style.bold && !style.italic })
      
      test(t"nested bold/italic (without intermediate characters) is both"):
        boldItalicSample2.find(t"bold-italic").vouch(using Unsafe).styles
      .assert(_.forall { style => style.bold && style.italic })
      
      test(t"normal text following doubly-nested text (without intermediate characters) is normal"):
        boldItalicSample2.find(t"end").vouch(using Unsafe).styles
      .assert(_.forall(_ == Style()))

      test(t"double color change uses latter"):
        pty.consume(e"$Red($Yellow(yellow))".render).buffer.find(t"yellow").vouch(using Unsafe).styles
      .assert(_.forall(_.foreground == Yellow))
      
      test(t"double color change and removal of nested uses former"):
        pty.consume(e"$Red($Yellow(yellow)red)".render).buffer.find(t"red").vouch(using Unsafe).styles
      .assert(_.forall(_.foreground == Red))
      
      test(t"double color change and removal uses default"):
        pty.consume(e"$Red($Yellow(yellow)red)default".render).buffer.find(t"default").vouch(using Unsafe).styles
      .assert(_.forall(_.foreground == Rgb24(255, 255, 255)))