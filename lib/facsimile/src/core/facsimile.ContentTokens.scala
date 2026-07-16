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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package facsimile

import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import vacuous.*

// Lexes a decoded content stream — or the concatenation of a `/Contents` array, which the
// specification requires to be treated as one stream — into instructions: operands followed
// by an operator. The COS lexer serves unchanged (§7.8.2 shares the lexical grammar), with
// reference lookahead disabled, since `R` here is an operator. Inline images, the one
// lexical special case, are folded into a single `BI` instruction of dictionary-plus-bytes.
private[facsimile] object ContentTokens:
  case class Instruction(operands: List[Cos], operator: Text)

  def read(data: Data): List[Instruction] raises PdfError =
    val lexer = CosLexer(Scan(data))
    val parser = CosParser(lexer, references = false)
    val instructions = List.newBuilder[Instruction]
    var done = false

    while !done do
      parser.instruction().let: (operands, operator) =>
        if operator.s == "BI" then instructions += inlineImage(lexer, parser)
        else instructions += Instruction(operands, operator)

      . or:
          done = true

    instructions.result()

  // `BI <key value ...> ID <bytes> EI`: the keys parse as ordinary tokens up to the `ID`
  // operator, the payload is consumed at the byte level, and the closing `EI` is checked.
  private def inlineImage(lexer: CosLexer, parser: CosParser): Instruction raises PdfError =
    val entries = parser.instruction().let: (operands, operator) =>
      if operator.s != "ID" then abort(PdfError(PdfError.Reason.MalformedOperator(t"BI")))

      operands.grouped(2).to(List).flatMap:
        case List(Cos.Name(key), value) => List(key -> value)
        case _                          => List()

      . to(Map)

    . or(abort(PdfError(PdfError.Reason.MalformedOperator(t"BI"))))

    val length = entries.at(t"L").or(entries.at(t"Length")).let(_.long).let(_.toInt)
    val data = lexer.imageData(length)

    val closed = parser.instruction().let(_(1).s == "EI").or(false)
    if !closed then abort(PdfError(PdfError.Reason.MalformedOperator(t"BI")))
    Instruction(List(Cos.Dictionary(entries), Cos.Chars(data)), t"BI")