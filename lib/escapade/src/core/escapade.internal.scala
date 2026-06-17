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

import scala.quoted.*
import scala.reflect.*

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

      . parts.toList

    val insertionExprs: List[Expr[Any]] = insertions.absolve match
      case Varargs(exprs) => exprs.toList

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
      partIdx < insertionExprs.length &&
        parts(partIdx).endsWith("\\\\") &&
        insertionLabel(insertionExprs(partIdx)) == Some("esc")

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
      B64.set(SetBold) | B64.set(SetItalic) | B64.set(SetUnderline) | B64.set(Strike) |
        B64.set(Concealed) | B64.set(SetForeground) | B64.set(SetBackground)

    val FlagChanges: B64 =
      B64.set(SetBold) | B64.set(SetItalic) | B64.set(SetUnderline) | B64.set(Strike) |
        B64.set(Concealed)

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

  opaque type StyleWord = Long

  object StyleWord:
    final val FgMask:           Long = 0x0000000000ffffffL
    final val BgMask:           Long = 0x0000ffffff000000L
    final val FgSet:            Long = 1L << 48
    final val BgSet:            Long = 1L << 49
    final val Bold:             Long = 1L << 50
    final val Faint:            Long = 1L << 51
    final val Italic:           Long = 1L << 52
    final val Underline:        Long = 1L << 53
    final val DoubleUnderline:  Long = 1L << 54
    final val BlinkSlow:        Long = 1L << 55
    final val BlinkFast:        Long = 1L << 56
    final val Reverse:          Long = 1L << 57
    final val Conceal:          Long = 1L << 58
    final val Strike:           Long = 1L << 59
    final val Overline:         Long = 1L << 60
    final val HyperlinkChange:  Long = 1L << 61

    final val FlagsMask: Long =
      FgSet | BgSet | Bold | Faint | Italic | Underline | DoubleUnderline | BlinkSlow | BlinkFast |
        Reverse | Conceal | Strike | Overline | HyperlinkChange

    val Default: StyleWord = 0L

    // A `StyleWord` is a `Long`, so an `Array[StyleWord]` is an unboxed `long[]`; the
    // opaque type hides that from callers, so expose the `ClassTag` (cells stored in a
    // grid need it to allocate without boxing).
    given classTag: ClassTag[StyleWord] = summon[ClassTag[Long]].asInstanceOf[ClassTag[StyleWord]]

    inline def apply(raw: Long): StyleWord = raw

    def combine(outer: Long, inner: Long): Long =
      val flagsOnly = ~(FgMask | BgMask | FgSet | BgSet)
      val combinedFlags = (outer | inner) & flagsOnly

      val fgBits =
        if (inner & FgSet) != 0 then inner & (FgMask | FgSet)
        else if (outer & FgSet) != 0 then outer & (FgMask | FgSet)
        else 0L

      val bgBits =
        if (inner & BgSet) != 0 then inner & (BgMask | BgSet)
        else if (outer & BgSet) != 0 then outer & (BgMask | BgSet)
        else 0L

      combinedFlags | fgBits | bgBits

    def emitDiff(buffer: StringBuilder, prev: Long, next: Long, depth: ColorDepth): Unit =
      val diff = prev^next
      var open = false

      def sep(): Unit =
        if !open then
          buffer.add(t"\e[")
          open = true
        else
          buffer.append(';')

      def emitColor(prefix: Int, rgb: Int): Unit =
        val r = (rgb >> 16)&255
        val g = (rgb >> 8)&255
        val b = rgb&255

        depth match
          case ColorDepth.TrueColor =>
            sep()
            buffer.append(prefix); buffer.append(';'); buffer.append('2')
            buffer.append(';'); buffer.append(r)
            buffer.append(';'); buffer.append(g)
            buffer.append(';'); buffer.append(b)

          case _ =>
            val n =
              if r == 0 && g == 0 && b == 0 then 16
              else if r == 255 && g == 255 && b == 255 then 231
              else if b == r && r == g then 232 + r*23/255
              else 16 + r*5/255*36 + g*5/255*6 + b*5/255

            sep()
            buffer.append(prefix); buffer.append(';'); buffer.append('5')
            buffer.append(';'); buffer.append(n)

      def toggle(bit: Long, flagDiff: Long, on: Int, off: Int): Unit =
        if (flagDiff & bit) != 0 then
          sep()
          buffer.append(if (next & bit) != 0 then on else off)

      if (diff & (FgMask | FgSet)) != 0 then
        if (next & FgSet) == 0 then { sep(); buffer.append(39) }
        else emitColor(38, (next & FgMask).toInt)

      if (diff & (BgMask | BgSet)) != 0 then
        if (next & BgSet) == 0 then { sep(); buffer.append(49) }
        else emitColor(48, ((next & BgMask) >>> 24).toInt)

      val flagDiff = diff & (FlagsMask & ~(FgSet | BgSet | HyperlinkChange))

      if flagDiff != 0 then
        toggle(Italic,          flagDiff, 3,  23)
        toggle(Bold,            flagDiff, 1,  22)
        toggle(Reverse,         flagDiff, 7,  27)
        toggle(Underline,       flagDiff, 4,  24)
        toggle(Conceal,         flagDiff, 8,  28)
        toggle(Strike,          flagDiff, 9,  29)
        toggle(Faint,           flagDiff, 2,  22)
        toggle(DoubleUnderline, flagDiff, 21, 24)
        toggle(BlinkSlow,       flagDiff, 5,  25)
        toggle(BlinkFast,       flagDiff, 6,  25)
        toggle(Overline,        flagDiff, 53, 55)

      if open then buffer.append('m')


    extension (style: StyleWord)
      inline def raw: Long = style

      inline def fgRgb: Int = (style & StyleWord.FgMask).toInt
      inline def bgRgb: Int = ((style & StyleWord.BgMask) >>> 24).toInt
      inline def hasFg: Boolean = (style & StyleWord.FgSet) != 0
      inline def hasBg: Boolean = (style & StyleWord.BgSet) != 0

      inline def isBold:            Boolean = (style & StyleWord.Bold)            != 0
      inline def isFaint:           Boolean = (style & StyleWord.Faint)           != 0
      inline def isItalic:          Boolean = (style & StyleWord.Italic)          != 0
      inline def isUnderline:       Boolean = (style & StyleWord.Underline)       != 0
      inline def isDoubleUnderline: Boolean = (style & StyleWord.DoubleUnderline) != 0
      inline def isBlinkSlow:       Boolean = (style & StyleWord.BlinkSlow)       != 0
      inline def isBlinkFast:       Boolean = (style & StyleWord.BlinkFast)       != 0
      inline def isReverse:         Boolean = (style & StyleWord.Reverse)         != 0
      inline def isConceal:         Boolean = (style & StyleWord.Conceal)         != 0
      inline def isStrike:          Boolean = (style & StyleWord.Strike)          != 0
      inline def isOverline:        Boolean = (style & StyleWord.Overline)        != 0
      inline def hasHyperlinkChange: Boolean = (style & StyleWord.HyperlinkChange) != 0

      inline def withFg(rgb: Int): StyleWord =
        (style & ~StyleWord.FgMask) | (rgb.toLong & 0xffffffL) | StyleWord.FgSet

      inline def withBg(rgb: Int): StyleWord =
        (style & ~StyleWord.BgMask) | ((rgb.toLong & 0xffffffL) << 24) | StyleWord.BgSet

      inline def clearFg: StyleWord = style & ~(StyleWord.FgMask | StyleWord.FgSet)
      inline def clearBg: StyleWord = style & ~(StyleWord.BgMask | StyleWord.BgSet)

      inline def withBit(bit: Long): StyleWord = style | bit
      inline def withoutBit(bit: Long): StyleWord = style & ~bit

      inline def applyTransform(mask: Long, bits: Long): StyleWord = (style & ~mask) | bits


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
