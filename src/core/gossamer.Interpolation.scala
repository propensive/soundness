/*
    Gossamer, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package gossamer

import language.experimental.into
import language.experimental.pureFunctions

import scala.reflect.*

import rudiments.*
import fulminate.*
import anticipation.*
import spectacular.*
import contextual.*

import errorDiagnostics.empty

object Interpolation:
  case class Input(txt: Text)

  given [ValueType: Showable] => Insertion[Input, ValueType] = value => Input(value.show)
  given Insertion[Input, Nothing] = value => Input("".tt)

  object T extends Interpolator[Input, Text, Text]:
    def initial: Text = anticipation.Text("")

    def parse(state: Text, next: Text): Text =
      try anticipation.Text(state.s+TextEscapes.escape(next).s)
      catch case error: EscapeError => error match
        case EscapeError(message) => throw InterpolationError(message)

    def skip(state: Text): Text = state
    def insert(state: Text, input: Input): Text = anticipation.Text(state.s+input.txt.s)
    def complete(state: Text): Text = state

  object Text extends Interpolator[Input, Text, Text]:
    def initial: Text = anticipation.Text("")

    def parse(state: Text, next: Text): Text =
      try anticipation.Text(state.s+TextEscapes.escape(next).s)
      catch case error: EscapeError => error match
        case EscapeError(message) => throw InterpolationError(message)

    def skip(state: Text): Text = state
    def insert(state: Text, input: Input): Text = anticipation.Text(state.s+input.txt.s)

    def complete(state: Text): Text =
      val array = state.s.split("\\n\\s*\\n").nn.map(_.nn.replaceAll("\\s\\s*", " ").nn.trim.nn)
      anticipation.Text(String.join("\n", array*).nn)
