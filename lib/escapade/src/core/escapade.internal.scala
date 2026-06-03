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

import scala.language.experimental.pureFunctions

import scala.quoted.*

import anticipation.*
import contextual.*
import denominative.*
import fulminate.*
import gossamer.*
import hypotenuse.*
import rudiments.*
import symbolism.*
import vacuous.*

object internal:
  // Mirrors what contextual.Interpolator.expand used to do for the e""
  // interpolator, without going through the trait. We summon Insertion
  // typeclasses at the macro level, run the State machine at compile time
  // to surface bracket-matching errors, and emit a runtime sequence of
  // parse/insert calls against the same State machine.
  def eMacro(context: Expr[StringContext], insertions: Expr[Seq[Any]])(using Quotes)
  :   Expr[Teletype] =

    import quotes.reflect.*

    val parts: List[String] =
      context.value.getOrElse:
        halt(m"the StringContext extension method parameter does not appear to be inline")

      . parts.to(List)

    val insertionExprs: List[Expr[Any]] = insertions.absolve match
      case Varargs(exprs) => exprs.to(List)

    // Determine each insertion's substitution label (e.g. "esc" for stylize
    // markup, "t" for text-input). Returns None for insertions that aren't
    // labelled Substitutions.
    def insertionLabel(head: Expr[Any]): Option[String] =
      head.absolve match
        case '{$value: tpe} =>
          Expr.summon[Insertion[Ansi.Input, tpe]].flatMap: tcExpr =>
            tcExpr.absolve match
              case '{$_ : Substitution[Ansi.Input, tpe, sub]} =>
                TypeRepr.of[sub] match
                  case ConstantType(StringConstant(s)) => Some(s)
                  case _                               => None

              case _ => None

    // A part ending with `\\` (raw source form: two consecutive backslash
    // chars) immediately before a Markup substitution escapes the markup —
    // the trailing `\\` is consumed and the markup is treated as a no-op.
    def escapesMarkup(partIdx: Int): Boolean =
      partIdx < insertionExprs.length
      && parts(partIdx).endsWith("\\\\")
      && insertionLabel(insertionExprs(partIdx)) == Some("esc")

    def adjustedPart(partIdx: Int): String =
      if escapesMarkup(partIdx) then parts(partIdx).dropRight(2) else parts(partIdx)

    val checkState = Ansi.Runtime.initial

    def rethrow[result](block: => result): result =
      try block catch case error: Ansi.AnsiError => halt(error.detail)

    val firstPart = adjustedPart(0)
    rethrow(Ansi.Runtime.parse(checkState, firstPart.tt))

    var runtimeExpr: Expr[Ansi.State] =
      '{Ansi.Runtime.parse(Ansi.Runtime.initial, ${Expr(firstPart)}.tt)}

    var i = 0

    while i < insertionExprs.length do
      val head = insertionExprs(i)
      val nextPart = adjustedPart(i + 1)
      val cancelled = escapesMarkup(i)

      head.absolve match
        case '{$value: tpe} =>
          val typeclassExpr: Expr[Insertion[Ansi.Input, tpe]] =
            Expr.summon[Insertion[Ansi.Input, tpe]].getOrElse:
              halt(m"can't substitute ${TypeRepr.of[tpe].show} into an e-interpolator")

          if cancelled then
            rethrow(Ansi.Runtime.skip(checkState))
            rethrow(Ansi.Runtime.parse(checkState, nextPart.tt))

            val current = runtimeExpr

            runtimeExpr =
              ' {
                  Ansi.Runtime.parse
                    ( Ansi.Runtime.skip($current),
                      ${Expr(nextPart)}.tt )
                }
          else
            typeclassExpr.absolve match
              case '{$_ : Substitution[Ansi.Input, tpe, sub]} =>
                val label: String = TypeRepr.of[sub] match
                  case ConstantType(StringConstant(s)) => s

                  case _ =>
                    halt(m"expected a literal string label for the substitution")

                // Mirror runtime's insert at compile time so the State machine
                // tracks brackets opened by the substitution and `complete`
                // can surface unclosed-bracket errors as a compile error.
                if label == "esc" then
                  rethrow(Ansi.Runtime.insert(checkState, Ansi.Input.Markup(identity)))
                else
                  rethrow(Ansi.Runtime.skip(checkState))

              case _ =>
                rethrow(Ansi.Runtime.skip(checkState))

            rethrow(Ansi.Runtime.parse(checkState, nextPart.tt))

            val current = runtimeExpr

            runtimeExpr =
              ' {
                  Ansi.Runtime.parse
                    ( Ansi.Runtime.insert($current, $typeclassExpr.embed($value)),
                      ${Expr(nextPart)}.tt )
                }

      i += 1

    rethrow(Ansi.Runtime.complete(checkState))

    '{Ansi.Runtime.complete($runtimeExpr)}


  opaque type CharSpan = Long

  object CharSpan:
    def apply(start: Int, end: Int): CharSpan = (start.toLong << 32) + (Int.MaxValue - end)

    given ordering: Ordering[CharSpan] = Ordering.Long.on[CharSpan](identity(_))

    val Nowhere: CharSpan = CharSpan(Int.MaxValue, Int.MaxValue)


  extension (span: CharSpan)
    def start: Int = (span >> 32).toInt

    def end: Int = Int.MaxValue - span.toInt
    def nil: Boolean = start == end

    def trimLeft(n: Int): CharSpan =
      if n >= end then CharSpan.Nowhere else if n <= start then CharSpan(start - n, end - n)
      else CharSpan(0, end - n)

    def takeLeft(n: Int): CharSpan =
      if n <= start then CharSpan.Nowhere else if n >= end then span else CharSpan(start, n)

    def shift(n: Int): CharSpan = CharSpan(start + n, end + n)


  opaque type AnsiStyle = B64

  object indexes:
    val SetBold = Prim
    val Bold = SetBold.next
    val SetItalic = Ter
    val Italic = SetItalic.next
    val SetUnderline = Quin
    val Underline = SetUnderline.next
    val SetStrike = Sept
    val Strike = SetStrike.next
    val SetConcealed = 8.z
    val Concealed = SetConcealed.next
    val SetForeground = Concealed.next
    val Foreground: Interval = SetForeground.subsequent(24)
    val SetBackground = Foreground.next
    val Background: Interval = SetBackground.subsequent(24)

    val Changes: B64 =
      B64.set(SetBold) | B64.set(SetItalic) | B64.set(SetUnderline) | B64.set(Strike)
      | B64.set(Concealed) | B64.set(SetForeground) | B64.set(SetBackground)

    val FlagChanges: B64 =
      B64.set(SetBold) | B64.set(SetItalic) | B64.set(SetUnderline) | B64.set(Strike)
      | B64.set(Concealed)

    val Mask: B64 =
      B64.set(Bold) | B64.set(Italic) | B64.set(Underline) | B64.set(Strike) | B64.set(Concealed)

    val FgMask: B64 = B64.set(Foreground)
    val BgMask: B64 = B64.set(Background)

  import indexes.*


  extension (style: AnsiStyle)
    def setBold: Boolean = (style: B64).bit(SetBold)

    def bold: Boolean = style.bit(Bold)
    def setItalic: Boolean = style.bit(SetItalic)
    def italic: Boolean = style.bit(Italic)
    def setUnderline: Boolean = style.bit(SetUnderline)
    def underline: Boolean = style.bit(Underline)
    def setStrike: Boolean = style.bit(SetStrike)
    def strike: Boolean = style.bit(Strike)
    def setConcealed: Boolean = style.bit(SetConcealed)
    def concealed: Boolean = style.bit(Concealed)
    def setForeground: Boolean = style.bit(SetForeground)
    def foreground: Chroma = Chroma(style(Foreground).s64.long.toInt)
    def setBackground: Boolean = style.bit(SetBackground)
    def background: Chroma = Chroma(style(Background).s64.long.toInt)
    def changed: Boolean = (style & Changes) != B64(0)

    def update(changes: AnsiStyle): AnsiStyle =
      val flagsChanged: B64 = changes & FlagChanges
      val changed: B64 = flagsChanged << 1
      val unchanged: B64 = ~changed
      val newFlags: B64 = (changes & changed) | (style & unchanged)
      val newFlags2 = if setForeground then (newFlags & ~FgMask) | (changes & FgMask) else newFlags

      if setBackground then (newFlags2 & ~BgMask) | (changes & BgMask) else newFlags2

case class Teletype2(plain: Text, ansi: IArray[escapade.internal.AnsiStyle]):
  import escapade.internal.AnsiStyle

  @targetName("concat")
  def + (that: Teletype2): Teletype2 = Teletype2(plain+that.plain, ansi ++ that.ansi)

  def render(using escapes: TerminalEscapes): Text =
    Text.build:
      def recur(current: AnsiStyle, index: Ordinal): Unit =
        if index.n0 < plain.length then
          val style = ansi(index.n0)

          if style.changed then
            if style.setBold then append(escapes.bold(style.bold))
            if style.setItalic then append(escapes.italic(style.italic))
            if style.setUnderline then append(escapes.underline(style.underline))
            if style.setConcealed then append(escapes.conceal(style.concealed))
            if style.setStrike then append(escapes.strike(style.strike))
            if style.setForeground then append(escapes.foreground(style.foreground))
            if style.setBackground then append(escapes.background(style.background))

            val current2 = current.update(style)

            append(plain.at(index).vouch)
            recur(current2, index + 1)
          else
            recur(current, index + 1)
