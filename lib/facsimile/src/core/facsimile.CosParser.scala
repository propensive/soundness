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
┃    Soundness, version 0.64.0.                                                                    ┃
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

import proscenium.compat.*
import rudiments.*

import anticipation.*
import contingency.*
import gossamer.*
import vacuous.*

// A recursive-descent parser over `CosToken`s. It never resolves indirect references — that
// is `Pdf`'s job — and never reads stream payloads: a `stream` keyword yields a `Cos.Body`
// locator and parsing stops there. `references` is disabled for content streams, where `R`
// is illegal and could otherwise misread three numeric operands.
private[facsimile] class CosParser(lexer: CosLexer, references: Boolean = true):
  @scala.caps.unsafe.untrackedCaptures
  private var pushback: List[CosToken] = List()

  def offset: Long = lexer.offset

  private def advance()(using Tactic[Pdf.Error]): CosToken = pushback match
    case head :: tail =>
      pushback = tail
      head

    case _ =>
      lexer.next()

  private def replace(token: CosToken): Unit = pushback = token :: pushback

  def value()(using Tactic[Pdf.Error]): Cos = interpret(advance())

  // Parses `N G obj <content> endobj`, returning the header numbers — the caller checks them
  // against the cross-reference entry it followed — and a `Cos.Body` if the content is a
  // stream dictionary. The payload itself is never traversed: every object is located through
  // the cross-reference table, so parsing stops at the `stream` keyword.
  def indirect()(using Tactic[Pdf.Error]): (Int, Int, Cos) =
    val number = integral(t"an object number")
    val generation = integral(t"a generation number")
    expect(CosToken.Keyword(t"obj"), t"the keyword 'obj'")
    (number, generation, content())

  private def integral(expected: Text)(using Tactic[Pdf.Error]): Int =
    val position = offset

    advance() match
      case CosToken.Integral(value) =>
        value.toInt

      case _ =>
        abort(Pdf.Error(Pdf.Error.Reason.Unparseable(position, expected)))

  private def content()(using Tactic[Pdf.Error]): Cos =
    val content = value()

    advance() match
      case CosToken.Keyword(word) => word.s match
        case "endobj" =>
          content

        case "stream" => content match
          case Cos.Dictionary(entries) =>
            Cos.Body(entries, lexer.payloadStart())

          case _ =>
            abort(Pdf.Error(Pdf.Error.Reason.Unparseable(offset, t"a stream dictionary")))

        case other =>
          abort(Pdf.Error(Pdf.Error.Reason.Unparseable(offset, t"the keyword 'endobj'")))

      case _ =>
        abort(Pdf.Error(Pdf.Error.Reason.Unparseable(offset, t"the keyword 'endobj'")))

  // One content-stream instruction: operand values followed by an operator keyword, or
  // `Unset` at the end of the stream. Operands left dangling by a truncated stream are
  // dropped, matching viewer behaviour.
  private[facsimile] def instruction()(using Tactic[Pdf.Error]): Optional[(List[Cos], Text)] =
    val operands = scala.collection.immutable.List.newBuilder[Cos]

    def recur(): Optional[(List[Cos], Text)] = advance() match
      case CosToken.End =>
        Unset

      case CosToken.Keyword(word) => word.s match
        case "true" =>
          operands += Cos.Truth(true)
          recur()

        case "false" =>
          operands += Cos.Truth(false)
          recur()

        case "null" =>
          operands += Cos.Nil
          recur()

        case _ =>
          (List.of(operands.result()), word)

      case token =>
        operands += interpret(token)
        recur()

    recur()

  private def expect(token: CosToken, expected: Text)(using Tactic[Pdf.Error]): Unit =
    val position = offset
    if advance() != token then abort(Pdf.Error(Pdf.Error.Reason.Unparseable(position, expected)))

  private def interpret(token: CosToken)(using Tactic[Pdf.Error]): Cos = token match
    case CosToken.Integral(first) =>
      // `N G R` is an indirect reference: two-token lookahead distinguishes it from a run of
      // numbers, with mismatches pushed back rather than lost.
      if !references then Cos.Integral(first) else advance() match
        case second @ CosToken.Integral(generation) =>
          advance() match
            case CosToken.Keyword(word) if word.s == "R" =>
              Cos.Ref(first.toInt, generation.toInt)

            case third =>
              replace(third)
              replace(second)
              Cos.Integral(first)

        case second =>
          replace(second)
          Cos.Integral(first)

    case CosToken.Real(value)  => Cos.Real(value)
    case CosToken.Name(text)   => Cos.Name(text)
    case CosToken.Chars(bytes) => Cos.Chars(bytes)
    case CosToken.ArrayStart   => sequence()
    case CosToken.DictStart    => dictionary()

    case CosToken.Keyword(word) => word.s match
      case "true"  => Cos.Truth(true)
      case "false" => Cos.Truth(false)
      case "null"  => Cos.Nil
      case other   => abort(Pdf.Error(Pdf.Error.Reason.Unparseable(offset, t"an object")))

    case _ =>
      abort(Pdf.Error(Pdf.Error.Reason.Unparseable(offset, t"an object")))

  private def sequence()(using Tactic[Pdf.Error]): Cos =
    val elements = scala.collection.immutable.List.newBuilder[Cos]

    while
      advance() match
        case CosToken.ArrayEnd =>
          false

        case CosToken.End =>
          abort(Pdf.Error(Pdf.Error.Reason.Truncated))

        case token =>
          elements += interpret(token)
          true
    do ()

    Cos.Sequence(List.of(elements.result()))

  private def dictionary()(using Tactic[Pdf.Error]): Cos =
    val entries = scala.collection.immutable.Map.newBuilder[Text, Cos]

    while
      advance() match
        case CosToken.DictEnd =>
          false

        case CosToken.Name(key) =>
          entries += key -> value()
          true

        case CosToken.End =>
          abort(Pdf.Error(Pdf.Error.Reason.Truncated))

        case token =>
          abort(Pdf.Error(Pdf.Error.Reason.Unparseable(offset, t"a name key")))
    do ()

    Cos.Dictionary(Map.of(entries.result()))
