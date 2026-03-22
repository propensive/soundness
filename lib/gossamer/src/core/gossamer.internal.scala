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
import contextual.*
import denominative.*
import fulminate.*
import gigantism.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

import errorDiagnostics.empty

object internal:
  private given realm: Realm = realm"gossamer"

  case class Input(txt: Text)

  given showable: [value: Showable] => Insertion[Input, value] = value => Input(value.show)
  given input: Insertion[Input, Nothing] = value => Input("".tt)

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

  object opaques:
    opaque type Ascii = anticipation.Data

    object Ascii:
      def apply(bytes: Data): Ascii = bytes

      given showable: Ascii is Showable =
        ascii => String(ascii.mutable(using Unsafe), "ASCII").nn.tt

      extension (ascii: Ascii) def bytes: Data = ascii

      given textual: Ascii is Textual:
        type Show[value] = value is Showable

        val empty: Ascii = IArray.from[Byte](Nil)
        val classTag: ClassTag[Ascii] = summon[ClassTag[Ascii]]

        def apply(text: Text): Ascii = text.sysData
        def apply(char: Char): Ascii = IArray(char.toByte)
        def length(ascii: Ascii): Int = ascii.size
        def text(ascii: Ascii): Text = String(ascii.mutable(using Unsafe), "ASCII").nn.tt
        def unsafeChar(ascii: Ascii, index: Ordinal): Char = ascii(index.n0).toChar
        def builder(size: Optional[Int]): Builder[Ascii] = AsciiBuilder(size)
        def size(ascii: Ascii): Int = ascii.length

        def map(ascii: Ascii)(lambda: Char => Char): Ascii = ascii.map: byte =>
          lambda(byte.toChar).toByte

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
        if char >= 128 then halt(m"$char is not a valid ASCII character")
        Expr[Byte](char.toByte)

      '{Ascii(Data(${Varargs(bytes)}*))}

    def recur(first: List[Expr[Ascii]], second: List[Expr[Ascii]], expr: Expr[Ascii]): Expr[Ascii] =
      first match
        case head :: tail => recur(second, tail, '{$expr+$head})
        case Nil          => expr

    recur(staticParts.tail.to(List), dynamicParts, staticParts.head)
