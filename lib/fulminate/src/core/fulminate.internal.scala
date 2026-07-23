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

import proscenium.compat.*

import scala.language.experimental.into

import scala.compiletime.*
import scala.quoted.*

import anticipation.*
import gigantism.*

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

    val parts: List[String] = List.from(context.valueOrAbort.parts)

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

    val groups: List[String] = parts.mkString("\u0000").split("`", -1).nn.map(_.nn).iterator.to(List)

    if groups.size%2 == 0
    then report.errorAndAbort("the m\"\" interpolator has an unmatched backtick")

    def toMessage(items: List[String | Expr[Message]]): Expr[Message] =
      val texts: List[String] = items.collect { case text: String => text }
      val msgs:  List[Expr[Message]] = items.collect { case expr: Expr[Message] @unchecked => expr }
      val textsExpr: Expr[List[Text]] =
        '{List.of(${Expr.ofList(texts.stdlib.map { text => '{${Expr(text)}.tt} })})}

      '{Message($textsExpr, List.of(${Expr.ofList(msgs.stdlib)}))}


    def sequence(group: String, startIndex: Int, subListRef: Expr[List[Message]])
    :   (List[String | Expr[Message]], Int) =

      val segments = group.split("\u0000", -1).nn.map(_.nn).iterator.to(List)

      val items: List[String | Expr[Message]] = List.of:
        segments.stdlib.zipWithIndex.flatMap: (segment, index) =>
          val text = decode(segment)

          if index < segments.stdlib.size - 1
          then List(text, '{$subListRef(${Expr(startIndex + index)})}).stdlib
          else List(text).stdlib

      (items, startIndex + segments.stdlib.size - 1)


    def assemble(subListRef: Expr[List[Message]]): Expr[Message] =
      val (items, _) = groups.stdlib.zipWithIndex.foldLeft((List[String | Expr[Message]](), 0)):
        case ((accumulator, index), (group, i)) =>
          val (groups, nextIndex) = sequence(group, index, subListRef)
          val addition = if i % 2 == 0 then groups else List(toMessage(groups))

          (accumulator ::: addition, nextIndex)

      toMessage(items)

    assemble('{mSubMessages[param]($subs)})
