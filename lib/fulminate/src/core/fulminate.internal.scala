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
package fulminate

import scala.language.experimental.into

import scala.compiletime.*
import scala.quoted.*

import anticipation.*
import gigantism.*
import murmuration.*

object internal:
  opaque type Diagnostics = Boolean

  object Diagnostics:
    val capture: Diagnostics = true
    val omit: Diagnostics = false

  extension (diagnostics: Diagnostics) def captureStack: Boolean = diagnostics

  transparent inline def mSubMessages[param](inline subs: param): List[Message] =
    inline subs.asMatchable match
      case tuple: Tuple =>
        Message[tuple.type](tuple, scala.collection.immutable.Nil)

      case other =>
        import unsafeExceptions.canThrowAny
        List(infer[(? >: other.type) is Communicable].message(other))

  def mMacro[param: Type](context: Expr[StringContext], subs: Expr[param]): Macro[Message] =
    import quotes.reflect.*

    val parts: List[String] = context.valueOrAbort.parts.to(List)

    def parseUnicode(part: String, current: Int): Char =
      if current + 4 > part.length
      then report.errorAndAbort("the unicode escape is incomplete")
      else
        try Integer.parseInt(part.substring(current, current + 4), 16).toChar
        catch case _: NumberFormatException =>
          val seq = part.substring(current, current + 4)
          report.errorAndAbort(s"invalid unicode escape: \\u$seq")

    def decodeEscape(char: Char): Char = char match
      case 'n'   => '\n'
      case 'r'   => '\r'
      case 'f'   => '\f'
      case 'b'   => '\b'
      case 't'   => '\t'
      case 'e'   => '\u001b'
      case '\\'  => '\\'
      case '"'   => '"'
      case '\''  => '\''
      case other => report.errorAndAbort(s"the character $other should not be escaped")

    def decode(segment: String): String =
      def loop(current: Int, accumulator: String): String =
        if current >= segment.length then accumulator else segment.charAt(current) match
          case '\\' =>
            if current + 1 >= segment.length
            then report.errorAndAbort("the final character of an m\"\" part cannot be an escape")
            else segment.charAt(current + 1) match
              case 'u'  => loop(current + 6, accumulator + parseUnicode(segment, current + 2))
              case char => loop(current + 2, accumulator + decodeEscape(char))

          case char =>
            loop(current + 1, accumulator + char)

      loop(0, "")

    val groups: List[String] =
      List.iterator(parts).mkString("\u0000").split("`", -1).nn.map(_.nn).iterator.to(List)

    if List.size(groups)%2 == 0
    then report.errorAndAbort("the m\"\" interpolator has an unmatched backtick")

    // Hoisted from the `map` below: a quote (with its implicit `ToExpr` search) inside a
    // combinator lambda in a macro risks the `wildApprox` crash.
    def liftText(text: String): Expr[Text] = '{${Expr(text)}.tt}

    def toMessage(items: List[String | Expr[Message]]): Expr[Message] =
      val texts: List[String] = items.sweep { case text: String => text }
      val msgs:  List[Expr[Message]] = items.sweep { case expr: Expr[Message] @unchecked => expr }
      // `Lifts.list` splices `List.from` inside the quote, not a conversion: the Factory
      // route mints a fresh capture in the *generated* code, which `Message`'s pure fields
      // then reject at every `m""` call site.
      val textsExpr: Expr[List[Text]] = Lifts.list(texts.map(liftText))

      '{Message($textsExpr, ${Lifts.list(msgs)})}


    def sequence(group: String, startIndex: Int, subListRef: Expr[List[Message]])
    :   (List[String | Expr[Message]], Int) =

      val segments = group.split("\u0000", -1).nn.map(_.nn).iterator.to(List)

      val count = List.size(segments)

      // Hoisted for the same `wildApprox` reason as `liftText` above.
      def substitution(index: Int): Expr[Message] = '{List.at($subListRef, ${Expr(index)})}

      val indexed: List[(String, Int)] = segments.zip(List.range(0, count))

      val items: List[String | Expr[Message]] =
        indexed.flatMap: (segment, index) =>
          val text = decode(segment)

          val batch: List[String | Expr[Message]] =
            if index < count - 1 then List(text, substitution(startIndex + index)) else List(text)

          batch

      (items, startIndex + count - 1)


    def assemble(subListRef: Expr[List[Message]]): Expr[Message] =
      val indexed: List[(String, Int)] = groups.zip(List.range(0, List.size(groups)))

      val (items, _) = indexed.fold((List[String | Expr[Message]](), 0)):
        case ((accumulator, index), (group, i)) =>
          val (groups, nextIndex) = sequence(group, index, subListRef)

          val addition: List[String | Expr[Message]] =
            if i % 2 == 0 then groups else List(toMessage(groups))

          (List.concat(accumulator, addition), nextIndex)

      toMessage(items)

    assemble('{mSubMessages[param]($subs)})
