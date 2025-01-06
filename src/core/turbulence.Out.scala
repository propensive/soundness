/*
    Turbulence, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package turbulence

import language.experimental.captureChecking

import anticipation.*
import rudiments.*

object Out:
  def write(bytes: Bytes)(using stdio: Stdio): Unit = stdio.write(bytes)

  def print[TextType: Printable as printable](text: Termcap ?=> TextType)(using stdio: Stdio)
          : Unit =
    stdio.print(printable.print(text(using stdio.termcap), stdio.termcap))

  def println()(using Stdio): Unit = print("\n".tt)

  def println[TextType: Printable](lines: Termcap ?=> TextType*)(using stdio: Stdio): Unit =
    lines.map(_(using stdio.termcap)).pipe: lines =>
      stdio.out.synchronized:
        lines.foreach: line =>
          print(line)
          println()
