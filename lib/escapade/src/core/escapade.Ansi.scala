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
package escapade

import language.experimental.pureFunctions

import scala.collection.mutable as scm

import anticipation.*
import contextual.*
import contingency.*
import denominative.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

import errorDiagnostics.empty

trait Ansi2:
  class TeletypeSubstitution[value](teletype: value => Teletype)
  extends Substitution[Ansi.Input, value, "t"]:
    def embed(value: value) = Ansi.Input.TextInput(teletype(value))

  inline given teletype: [value] => Substitution[Ansi.Input, value, "t"] =
    val teletype: value => Teletype = value => compiletime.summonFrom:
      case given (`value` is Teletypeable) => value.teletype
      case given (`value` is Showable)     => Teletype(value.show)

    TeletypeSubstitution[value](teletype)

object Ansi extends Ansi2:
  type Transform = TextStyle => TextStyle

  def strip(txt: Text): Text = txt.sub(t"""\e\\[?.*?[\\@-~]""", t"")

  given escape: Stylize[Escape] = identity(_)

  given chromatic: [color: Chromatic as color] => Stylize[color] =
    color => Stylize(_.copy(fg = color.chroma))

  given bg: Stylize[Bg] = bgColor => Stylize(_.copy(bg = bgColor.color))
  given fg: Stylize[Fg] = fgColor => Stylize(_.copy(fg = fgColor.color))
  given bold: Stylize[Bold.type] = _ => Stylize(_.copy(bold = true))
  given italic: Stylize[Italic.type] = _ => Stylize(_.copy(italic = true))
  given underline: Stylize[Underline.type] = _ => Stylize(_.copy(underline = true))
  given strike: Stylize[Strike.type] = _ => Stylize(_.copy(strike = true))
  given conceal: Stylize[Conceal.type] = _ => Stylize(_.copy(conceal = true))
  given reverse: Stylize[Reverse.type] = _ => Stylize(_.copy(reverse = true))
  given faint: Stylize[Faint.type] = _ => Stylize(_.copy(faint = true))
  given doubleUnderline: Stylize[DoubleUnderline.type] =
    _ => Stylize(_.copy(doubleUnderline = true))
  given blinkSlow: Stylize[BlinkSlow.type] = _ => Stylize(_.copy(blinkSlow = true))
  given blinkFast: Stylize[BlinkFast.type] = _ => Stylize(_.copy(blinkFast = true))
  given overline: Stylize[Overline.type] = _ => Stylize(_.copy(overline = true))

  given hyperlink: Substitution[Input, Hyperlink, "esc"] = h => Input.Hyperlink(h.url)

  enum Input:
    case TextInput(text: Teletype)
    case Markup(transform: Transform)
    case Escape(on: Text, off: Text)
    case Hyperlink(url: Text)

  enum Pending:
    case Style(transform: Transform)
    case Link(url: Text)

  enum Frame(val bracket: Char):
    case Style(override val bracket: Char) extends Frame(bracket)
    case Link(override val bracket: Char)  extends Frame(bracket)

  class State:
    val plain: StringBuilder = StringBuilder()
    val styles: scm.ArrayBuffer[Long] = scm.ArrayBuffer.empty
    val hyperlinks: scm.HashMap[Int, Text] = scm.HashMap.empty
    val insertions: scm.TreeMap[Int, Text] = scm.TreeMap.empty
    var last: Optional[Pending] = Unset
    var stack: List[Frame] = Nil
    var styleStack: List[TextStyle] = Nil
    var currentStyle: TextStyle = TextStyle()
    var linkArmed: Boolean = false

    def appendChar(char: Char): Unit =
      plain.append(char)
      val base = currentStyle.styleWord
      val styled = if linkArmed then base | StyleWord.HyperlinkChange else base
      styles += styled
      linkArmed = false

    def appendChars(text: Text): Unit =
      var i = 0
      val s = text.s
      while i < s.length do
        appendChar(s.charAt(i))
        i += 1

    def appendTeletype(text: Teletype): Unit =
      val outer = currentStyle.styleWord
      val n = plain.length
      var i = 0
      val triggerLink = linkArmed
      while i < text.plain.length do
        plain.append(text.plain.s.charAt(i))
        var combined = StyleWord.combine(outer, text.styleAt(i))
        if i == 0 && triggerLink then combined = combined | StyleWord.HyperlinkChange
        styles += combined
        i += 1

      if triggerLink then linkArmed = false

      if text.hyperlinks.nonEmpty then
        text.hyperlinks.each { (k, v) => hyperlinks(n + k) = v }

      if text.insertions.nonEmpty then
        text.insertions.each { (k, v) => insertions(n + k) = v }

    def addInsertion(position: Int, content: Text): Unit =
      insertions.updateWith(position):
        case None             => Some(content)
        case Some(existing)   => Some(existing+content)

    def pushStyleFrame(bracket: Char, transform: Transform): Unit =
      stack = Frame.Style(bracket) :: stack
      styleStack = currentStyle :: styleStack
      currentStyle = transform(currentStyle)

    def pushLinkFrame(bracket: Char, url: Text): Unit =
      stack = Frame.Link(bracket) :: stack
      hyperlinks(plain.length) = url
      linkArmed = true

    def popFrame(): Unit =
      stack.head match
        case _: Frame.Style =>
          stack = stack.tail
          currentStyle = styleStack.head
          styleStack = styleStack.tail

        case _: Frame.Link =>
          stack = stack.tail
          linkArmed = true

    def applyOnce(transform: Transform): Unit =
      currentStyle = transform(currentStyle)


  object Interpolator extends contextual.Interpolator[Input, State, Teletype]:
    private val complement = Map('[' -> ']', '(' -> ')', '{' -> '}', '<' -> '>', '«' -> '»')

    def initial: State = State()

    def parse(state: State, text: Text): State =
      state.last match
        case Unset =>
          closures(state, text)

        case Pending.Style(transform) =>
          text.at(Prim) match
            case Bsl =>
              state.last = Unset
              closures(state, text.skip(1))

            case '[' | '(' | '<' | '«' | '{' =>
              state.pushStyleFrame(complement(text.at(Prim).vouch), transform)
              state.last = Unset
              closures(state, text.skip(1))

            case _ =>
              state.applyOnce(transform)
              state.last = Unset
              closures(state, text)

        case Pending.Link(url) =>
          text.at(Prim) match
            case Bsl =>
              state.last = Unset
              closures(state, text.skip(1))

            case '[' | '(' | '<' | '«' | '{' =>
              state.pushLinkFrame(complement(text.at(Prim).vouch), url)
              state.last = Unset
              closures(state, text.skip(1))

            case _ =>
              state.last = Unset
              closures(state, text)

    private def closures(state: State, text: Text): State =
      try state.stack match
        case Nil =>
          state.appendChars(TextEscapes.escape(text))
          state

        case frame :: _ =>
          safely(text.where(_ == frame.bracket)).let(_.n0) match
            case Unset =>
              state.appendChars(text)
              state

            case index: Int =>
              state.appendChars(text.keep(index))
              state.popFrame()
              state.last = Unset
              closures(state, text.skip(index + 1))

      catch case error: EscapeError => error match
        case EscapeError(message) => throw InterpolationError(message)

    def insert(state: State, value: Input): State = value match
      case Input.TextInput(text) =>
        state.appendTeletype(text)
        state.last = Unset
        state

      case Input.Markup(transform) =>
        state.last = Pending.Style(transform)
        state

      case Input.Escape(on, _) =>
        state.last = Unset
        state.addInsertion(state.plain.length, t"\e"+on)
        state

      case Input.Hyperlink(url) =>
        state.last = Pending.Link(url)
        state

    def skip(state: State): State = insert(state, Input.TextInput(Teletype.empty))

    def complete(state: State): Teletype =
      if state.stack.nonEmpty
      then throw InterpolationError(m"the closing brace does not match an opening brace")

      val tail = if state.linkArmed then StyleWord.HyperlinkChange else 0L
      state.styles += tail

      val plainText = state.plain.toString.tt
      val denseStyles = IArray.unsafeFromArray(state.styles.toArray)
      val (newStyles, newBoundaries) = Teletype.compressIfBeneficial(plainText, denseStyles)

      Teletype
        ( plainText,
          newStyles,
          state.hyperlinks.toMap,
          state.insertions.to(TreeMap),
          newBoundaries )
