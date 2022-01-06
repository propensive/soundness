/*
    Escapade, version 0.1.0. Copyright 2021-22 Jon Pretty, Propensive OÜ.

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
import eucalyptus.*
import rudiments.*
import gossamer.*

import escapes.*

import unsafeExceptions.canThrowAny

given Log(Everything |-> Stdout)

object Tests extends Suite(t"Escapade tests"):
  def run(using Runner): Unit =
    test(t"normal string") {
      ansi"hello world".render
    }.check(_ == t"hello world")
    
    test(t"simple string substitution") {
      ansi"hello ${"world"}".render
    }.check(_ == t"hello world")
    
    test(t"bold text") {
      ansi"$Bold{bold} text".render
    }.check(_ == t"\e[1mbold\e[22m text")
    
    test(t"italic text") {
      ansi"$Italic{italic} text".render
    }.check(_ == t"\e[3mitalic\e[23m text")
    
    test(t"24-bit colored text") {
      ansi"${iridescence.colors.Tan}[text]".render
    }.check(_ == t"\e[38;2;210;180;139mtext\e[38;2;255;255;255m")
    
    test(t"non-escape insertion should not parse brackets") {
      val notAnEscape = 42
      ansi"${notAnEscape}[text]".render
    }.check(_ == t"42[text]")
