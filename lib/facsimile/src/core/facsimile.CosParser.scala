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

// A recursive-descent parser over `CosToken`s. It never resolves indirect references — that
// is `Pdf`'s job — and never reads stream payloads: a `stream` keyword yields a `Cos.Body`
// locator and parsing stops there. `references` is disabled for content streams, where `R`
// is illegal and could otherwise misread three numeric operands.
private[facsimile] class CosParser(lexer: CosLexer, references: Boolean = true):
  private var pushback: List[CosToken] = List()

  def offset: Long = lexer.offset

  private def advance(): CosToken raises PdfError = pushback match
    case head :: tail =>
      pushback = tail
      head

    case _ =>
      lexer.next()

  private def replace(token: CosToken): Unit = pushback = token :: pushback

  def value(): Cos raises PdfError = interpret(advance())

  // Parses `N G obj <content> endobj`, or a `Cos.Body` if the content is a stream dictionary.
  // The payload itself is skipped over by the caller (or, more usually, never traversed at
  // all, since every object is located through the cross-reference table).
  def indirect(number: Int, generation: Int): Cos raises PdfError =
    expect(CosToken.Integral(number.toLong), t"the object number $number")
    expect(CosToken.Integral(generation.toLong), t"the generation number $generation")
    expect(CosToken.Keyword(t"obj"), t"the keyword 'obj'")
    val content = value()

    advance() match
      case CosToken.Keyword(word) => word.s match
        case "endobj" =>
          content

        case "stream" => content match
          case Cos.Dictionary(entries) =>
            Cos.Body(entries, lexer.payloadStart())

          case _ =>
            abort(PdfError(PdfError.Reason.Unparseable(offset, t"a stream dictionary")))

        case other =>
          abort(PdfError(PdfError.Reason.Unparseable(offset, t"the keyword 'endobj'")))

      case _ =>
        abort(PdfError(PdfError.Reason.Unparseable(offset, t"the keyword 'endobj'")))

  private def expect(token: CosToken, expected: Text): Unit raises PdfError =
    val position = offset
    if advance() != token then abort(PdfError(PdfError.Reason.Unparseable(position, expected)))

  private def interpret(token: CosToken): Cos raises PdfError = token match
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
      case other   => abort(PdfError(PdfError.Reason.Unparseable(offset, t"an object")))

    case _ =>
      abort(PdfError(PdfError.Reason.Unparseable(offset, t"an object")))

  private def sequence(): Cos raises PdfError =
    val elements = List.newBuilder[Cos]

    while
      advance() match
        case CosToken.ArrayEnd =>
          false

        case CosToken.End =>
          abort(PdfError(PdfError.Reason.Truncated))

        case token =>
          elements += interpret(token)
          true
    do ()

    Cos.Sequence(elements.result())

  private def dictionary(): Cos raises PdfError =
    val entries = Map.newBuilder[Text, Cos]

    while
      advance() match
        case CosToken.DictEnd =>
          false

        case CosToken.Name(key) =>
          entries += key -> value()
          true

        case CosToken.End =>
          abort(PdfError(PdfError.Reason.Truncated))

        case token =>
          abort(PdfError(PdfError.Reason.Unparseable(offset, t"a name key")))
    do ()

    Cos.Dictionary(entries.result())
