/*
    Escapade, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import language.experimental.pureFunctions

import anticipation.*
import contextual.*
import contingency.*
import denominative.*
import fulminate.*
import gossamer.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

import errorDiagnostics.empty

trait Ansi2:

  class TeletypeSubstitution[ValueType](teletype: ValueType => Teletype)
  extends Substitution[Ansi.Input, ValueType, "t"]:
    def embed(value: ValueType) = Ansi.Input.TextInput(teletype(value))

  inline given [ValueType] => Substitution[Ansi.Input, ValueType, "t"] as teletype =
    val teletype: ValueType => Teletype = value => compiletime.summonFrom:
      case given (ValueType is Teletypeable) => value.teletype
      case given (ValueType is Showable)    => Teletype(value.show)

    TeletypeSubstitution[ValueType](teletype)

object Ansi extends Ansi2:
  type Transform = TextStyle => TextStyle

  def strip(txt: Text): Text = txt.sub(t"""\e\\[?.*?[\\@-~]""", t"")

  given Stylize[Escape] = identity(_)

  given [ColorType: Chromatic as color] => Stylize[ColorType] =
    color => Stylize(_.copy(fg = color.asRgb24Int))

  given Stylize[Bg] = bgColor => Stylize(_.copy(bg = bgColor.color))
  given Stylize[Fg] = fgColor => Stylize(_.copy(fg = fgColor.color))

  given Stylize[Bold.type] = _ => Stylize(_.copy(bold = true))
  given Stylize[Italic.type] = _ => Stylize(_.copy(italic = true))
  given Stylize[Underline.type] = _ => Stylize(_.copy(underline = true))
  given Stylize[Strike.type] = _ => Stylize(_.copy(strike = true))
  given Stylize[Conceal.type] = _ => Stylize(_.copy(conceal = true))
  given Stylize[Reverse.type] = _ => Stylize(_.copy(reverse = true))

  enum Input:
    case TextInput(text: Teletype)
    case Markup(transform: Transform)
    case Escape(on: Text, off: Text)

  case class Frame(bracket: Char, start: Int, transform: Transform)

  case class State
     (text:       Text                         = t"",
      last:       Option[Transform]            = None,
      stack:      List[Frame]                  = Nil,
      spans:      TreeMap[CharSpan, Transform] = TreeMap(),
      insertions: TreeMap[Int, Text]           = TreeMap()):

    def add(span: CharSpan, transform: Transform): State =
      copy(spans = spans.updated(span, spans.get(span).fold(transform)(transform.andThen(_))))

    def add(pos: Int, esc: Escape): State =
      val insertions2 = insertions.get(pos).fold(t"\e"+esc.on)(_+t"\e"+esc.on)
      copy(insertions = insertions.updated(pos, insertions2))

  object Interpolator extends contextual.Interpolator[Input, State, Teletype]:
    private val complement = Map('[' -> ']', '(' -> ')', '{' -> '}', '<' -> '>', '«' -> '»')
    def initial: State = State()

    def parse(state: State, text: Text): State =
      state.last.fold(closures(state, text)): transform =>
        text.at(Prim) match
          case '\\' =>
            closures(state.copy(last = None), text.skip(1))
          case '[' | '(' | '<' | '«' | '{' =>
            val frame = Frame(complement(text.s.head), state.text.length, transform)
            closures(state.copy(stack = frame :: state.stack, last = None), text.skip(1))

          case _ =>
            val state2 = state.add(CharSpan(state.text.length, state.text.length), transform)
            closures(state2.copy(last = None), text)

    private def closures(state: State, text: Text): State =
      try state.stack.headOption.fold(state.copy(text = state.text+TextEscapes.escape(text))): frame =>
        safely(text.where(_ == frame.bracket)).let(_.n0) match
          case Unset =>
            state.copy(text = state.text+text)

          case idx: Int =>
            val text2 = state.text+text.keep(idx)
            val span2: CharSpan = CharSpan(frame.start, state.text.length + idx)
            val state2: State = state.add(span2, frame.transform)
            val state3: State = state2.copy(text = text2, last = None, stack = state.stack.tail)
            closures(state3, text.skip(idx + 1))

      catch case error: EscapeError => error match
        case EscapeError(message) => throw InterpolationError(message)

    def insert(state: State, value: Input): State = value match
      case Input.TextInput(text) =>
        val textSpans: TreeMap[CharSpan, Transform] = text.spans.map:
          case (span, transform) => (span.shift(state.text.length): CharSpan) -> transform

        val textInsertions: TreeMap[Int, Text] = text.insertions.map:
          case (pos, ins) => (pos + state.text.length) -> ins

        state.copy(text = state.text+text.plain, last = None, spans = state.spans ++ textSpans,
            insertions = state.insertions ++ textInsertions)

      case Input.Markup(transform) =>
        state.copy(last = Some(transform))

      case esc@Input.Escape(on, off) =>
        state.copy(last = None).add(state.text.length, esc)

    def skip(state: State): State = insert(state, Input.TextInput(Teletype.empty))

    def complete(state: State): Teletype =
      if !state.stack.isEmpty
      then throw InterpolationError(m"the closing brace does not match an opening brace")

      Teletype(state.text, state.spans, state.insertions)
