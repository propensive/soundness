/*
    Escapade, version 0.1.0. Copyright 2019-21 Jon Pretty, Propensive OÜ.

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

import escapes.*

import unsafeExceptions.canThrowAny

object Tests extends Suite("Escapade tests"):
  def run(using Runner): Unit =
    test("normal string") {
      ansi"hello world".render
    }.assert(_ == "hello world")
    
    test("simple string substitution") {
      ansi"hello ${"world"}".render
    }.assert(_ == "hello world")
    
    test("bold text") {
      ansi"$Bold{bold} text".explicit
    }.assert(_ == "^[1mbold^[22m text")
    
    test("italic text") {
      ansi"$Italic{italic} text".explicit
    }.assert(_ == "^[3mitalic^[23m text")
    
    test("24-bit colored text") {
      ansi"${iridescence.colors.Tan}[text]".explicit
    }.assert(_ == "^[38;2;210;180;139mtext^[39m")
    
    test("non-escape insertion should not parse brackets") {
      val notAnEscape = 42
      ansi"${notAnEscape}[text]".explicit
    }.assert(_ == "42[text]")
