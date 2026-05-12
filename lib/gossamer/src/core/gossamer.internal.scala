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
package gossamer

import scala.quoted.*

import anticipation.*
import denominative.*
import fulminate.*
import gigantism.*
import hieroglyph.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

import errorDiagnostics.empty

object internal:
  // Both `t""` and `txt""` build a Text by escape-processing each static part
  // at compile time and converting each substitution via Showable at runtime.
  // Only the final treatment differs: txt collapses runs of whitespace and
  // double-newlines into single newlines for multi-line literals.
  private def textInterpolator
    ( context:    Expr[StringContext],
     insertions: Expr[Seq[Any]],
     normalize:  Boolean )
    ( using Quotes )
  :   Expr[Text] =

    import quotes.reflect.*

    val rawParts: List[String] =
      context.value.getOrElse:
        halt(m"the StringContext extension method parameter does not appear to be inline")

      . parts.toList

    val escapedParts: List[String] = rawParts.map: part =>
      try TextEscapes.escape(part.tt).s catch case error: EscapeError => error match
        case EscapeError(msg) => halt(msg)

    val insertionExprs: List[Expr[Any]] = insertions.absolve match
      case Varargs(exprs) => exprs.toList

    val showedInsertions: List[Expr[String]] = insertionExprs.map: expr =>
      expr.absolve match
        case '{$value: tpe} =>
          Expr.summon[(? >: tpe) is Showable] match
            case Some('{$showable: Showable}) =>
              '{$showable.text($value).s}

            case _ =>
              halt(m"a value of ${TypeRepr.of[tpe].show} is not Showable")

    var concatExpr: Expr[String] = Expr(escapedParts.head)
    var i = 0
    while i < showedInsertions.length do
      val insertion = showedInsertions(i)
      val nextPart = Expr(escapedParts(i + 1))
      concatExpr = '{$concatExpr + $insertion + $nextPart}
      i += 1

    if normalize then '{
      val array =
        $concatExpr.split("\\n\\s*\\n").nn.map(_.nn.replaceAll("\\s\\s*", " ").nn.trim.nn)

      anticipation.Text(String.join("\n", array*).nn)
    }
    else '{anticipation.Text($concatExpr)}


  def t(context: Expr[StringContext], insertions: Expr[Seq[Any]]): Macro[Text] =
    textInterpolator(context, insertions, normalize = false)


  def txt(context: Expr[StringContext], insertions: Expr[Seq[Any]]): Macro[Text] =
    textInterpolator(context, insertions, normalize = true)

  object opaques:
    opaque type Ascii = anticipation.Data
    opaque type Grapheme = String

    object Grapheme:
      def apply(string: String): Grapheme = string

      given showable: Grapheme is Showable = grapheme => grapheme.tt

      // Width of a grapheme is the maximum width of its constituent codepoints. For
      // combining-mark sequences (e.g. e + ́) this gives the base character's width;
      // for joiner-glued graphemes (RI flag pairs, ZWJ family emoji, Hangul jamo
      // syllables) this avoids the over-counting that summing per-codepoint widths
      // would produce — one cell on the terminal regardless of how many codepoints
      // make up the cluster.
      given measurable: (charM: Char is Measurable) => Grapheme is Measurable = grapheme =>
        val s: String = grapheme
        var max = 0
        var i = 0
        while i < s.length do
          val cp = Character.codePointAt(s, i)

          val w =
            if Character.charCount(cp) == 2
            then charM.width(s.charAt(i)) + charM.width(s.charAt(i + 1))
            else charM.width(s.charAt(i))
          if w > max then max = w
          i += Character.charCount(cp)
        max

      extension (grapheme: Grapheme)
        def text: Text = grapheme.tt
        def chars: Int = grapheme.length

    object Ascii:
      def apply(bytes: Data): Ascii = bytes

      given showable: Ascii is Showable =
        ascii => String(ascii.mutable(using Unsafe), "ASCII").nn.tt

      given concatenable: Ascii is Concatenable:
        type Operand = Ascii
        def concat(left: Ascii, right: Ascii): Ascii = textual.concat(left, right)

      extension (ascii: Ascii) def bytes: Data = ascii

      given textual: Ascii is Textual:
        type Operand = Byte
        type Show[value] = value is Showable

        val empty: Ascii = IArray.from[Byte](Nil)
        val classTag: ClassTag[Ascii] = summon[ClassTag[Ascii]]

        def apply(text: Text): Ascii = text.sysData
        def single(operand: Byte): Ascii = IArray(operand)
        def fromChar(char: Char): Byte = char.toByte
        def length(ascii: Ascii): Int = ascii.size
        def text(ascii: Ascii): Text = String(ascii.mutable(using Unsafe), "ASCII").nn.tt
        def at(ascii: Ascii, index: Ordinal): Byte = ascii(index.n0)
        def builder(size: Optional[Int]): Builder[Ascii] = AsciiBuilder(size)
        def size(ascii: Ascii): Int = ascii.length

        def map(ascii: Ascii)(lambda: Byte => Byte): Ascii = ascii.map(lambda)

        def concat(left: Ascii, right: Ascii): Ascii =
          IArray.build[Byte](left.length + right.length): array =>
            array.place(left, Prim)
            array.place(right, left.length.z)

        def indexOf(ascii: Ascii, sub: Text, start: Ordinal): Optional[Ordinal] =
          ascii.indexOfSlice(apply(sub)).puncture(-1).let(_.z)

        def show[value](value: value)(using show: Show[value]): Ascii =
          Ascii(show.text(value).sysData)

        def segment(ascii: Ascii, interval: Interval): Ascii =
          ascii.slice(interval.start.n0, interval.end.n0)

  def ascii(context: Expr[StringContext], parts: Expr[Seq[Ascii]]): Macro[Ascii] =
    val dynamicParts: List[Expr[Ascii]] = parts.absolve match
      case Varargs(parts) => parts.to(List)

    val staticParts: List[Expr[Ascii]] = context.value.get.parts.to(List).map: part =>
      val bytes: IArray[Expr[Byte]] = part.tt.chars.map: char =>
        if char >= 128 then halt(824, m"$char is not a valid ASCII character")
        Expr[Byte](char.toByte)

      '{Ascii(Data(${Varargs(bytes)}*))}

    def recur(first: List[Expr[Ascii]], second: List[Expr[Ascii]], expr: Expr[Ascii]): Expr[Ascii] =
      first match
        case head :: tail => recur(second, tail, '{$expr+$head})
        case Nil          => expr

    recur(staticParts.tail.to(List), dynamicParts, staticParts.head)
