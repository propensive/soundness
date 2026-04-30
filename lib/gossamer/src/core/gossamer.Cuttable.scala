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

import language.experimental.into
import language.experimental.pureFunctions

import java.util.regex.*

import scala.reflect.*

import anticipation.*
import denominative.*
import kaleidoscope.*
import prepositional.*
import proscenium.*
import rudiments.*
import symbolism.*
import vacuous.*

object Cuttable:
  given textualText: [textual: {Textual, Countable}] => textual is Cuttable by Text =
    (text, delimiter, limit) =>
      val string = textual.text(text).s
      val dLength = delimiter.s.length

      @tailrec
      def recur(start: Ordinal, results: List[textual]): List[textual] =
        textual.indexOf(text, delimiter, start)
        . lay(text.segment(start till text.length.z) :: results):
            index => recur(index + dLength, text.segment(start till index) :: results)

      recur(Prim, Nil).reverse

  given textualRegex: [textual: {Textual, Countable}] => textual is Cuttable by Regex =
    (text, regex, limit) =>
      val string = textual.text(text).s
      val matcher = Pattern.compile(regex.pattern.s).nn.matcher(string).nn

      @tailrec
      def recur(start: Ordinal, results: List[textual]): List[textual] =
        if matcher.find(start.n0)
        then recur(matcher.end.z, text.segment(matcher.start.z thru matcher.end.z) :: results)
        else results

      recur(Prim, Nil).reverse

  given textText: Text is Cuttable by Text = (text, delimiter, limit) =>
    text.s.split(Pattern.quote(delimiter.s), limit).nn.map(_.nn.tt).to(List)

  given textRegex: Text is Cuttable by Regex = (text, regex, limit) =>
    text.s.split(regex.pattern.s, limit).nn.map(_.nn.tt).to(List)


  given textualText: [textual] => (cuttable: textual is Cuttable by Text)
  =>  textual is Cuttable by Char =

    (text, delimiter, limit) =>
      cuttable.cut(text, delimiter.toString.tt, limit)


trait Cuttable extends Typeclass, Operable:
  def cut(value: Self, delimiter: Operand, limit: Int): List[Self]
