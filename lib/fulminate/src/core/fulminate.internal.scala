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
package fulminate

import language.experimental.into

import scala.compiletime.*
import scala.quoted.*

import anticipation.*
import gigantism.*
import proscenium.*

object internal:
  opaque type Diagnostics = Boolean

  object Diagnostics:
    val capture: Diagnostics = true
    val omit: Diagnostics = false

  extension (diagnostics: Diagnostics) def captureStack: Boolean = diagnostics

  def realm(context: Expr[StringContext]): Macro[Realm] =
    val name: String = context.valueOrAbort.parts.head
    if !name.matches("[a-z]+") then
      val msg = Message(List("the realm code should contain only lowercase letters".tt))
      halt(3, msg)(using summon, Realm("fu"))
    else '{Realm(${Expr(name)}.tt)}

  transparent inline def mSubMessages[param](inline subs: param): List[Message] =
    inline subs.asMatchable match
      case tuple: Tuple =>
        Message[tuple.type](tuple, Nil)

      case other =>
        import unsafeExceptions.canThrowAny
        List(infer[(? >: other.type) is Communicable].message(other))

  def mMacro[param: Type](context: Expr[StringContext], subs: Expr[param]): Macro[Message] =
    import quotes.reflect.*

    val parts: List[String] = context.valueOrAbort.parts.toList

    class Frame:
      var textsAccum: List[String] = Nil
      var messagesAccum: List[Expr[Message]] = Nil
      val buffer: StringBuilder = StringBuilder()

    var stack: List[Frame] = List(Frame())

    def topFrame: Frame = stack.head

    def finalizeBuffer(): Unit =
      topFrame.textsAccum = topFrame.textsAccum :+ topFrame.buffer.toString
      topFrame.buffer.setLength(0)

    def buildMessageExpr(frame: Frame): Expr[Message] =
      val textsExpr: Expr[List[Text]] =
        Expr.ofList(frame.textsAccum.map { s => '{ ${Expr(s)}.tt } })
      val messagesExpr: Expr[List[Message]] = Expr.ofList(frame.messagesAccum)
      '{ Message($textsExpr, $messagesExpr) }

    def openFrame(): Unit =
      finalizeBuffer()
      stack = Frame() :: stack

    def closeFrame(): Unit =
      finalizeBuffer()
      val closed = topFrame
      stack = stack.tail
      topFrame.messagesAccum = topFrame.messagesAccum :+ buildMessageExpr(closed)

    def appendSub(idx: Int, subListRef: Expr[List[Message]]): Unit =
      finalizeBuffer()
      topFrame.messagesAccum =
        topFrame.messagesAccum :+ '{ $subListRef(${Expr(idx)}) }

    def parseUnicode(part: String, cur: Int): Char =
      if cur + 4 > part.length
      then report.errorAndAbort("the unicode escape is incomplete")
      else
        try Integer.parseInt(part.substring(cur, cur + 4), 16).toChar
        catch case _: NumberFormatException =>
          report.errorAndAbort(s"invalid unicode escape: \\u${part.substring(cur, cur + 4)}")

    def walkPart(part: String): Unit =
      var cur = 0
      var esc = false
      while cur < part.length do
        val ch = part.charAt(cur)
        if ch == '`' && !esc then
          if cur + 1 < part.length && part.charAt(cur + 1) == '`' then
            topFrame.buffer.append('`')
            cur += 2
          else
            if stack.size == 1 then openFrame() else closeFrame()
            cur += 1
        else if ch == '\\' && !esc then
          esc = true
          cur += 1
        else if esc then
          val decoded: Char = ch match
            case 'n'  => '\n'
            case 'r'  => '\r'
            case 'f'  => '\f'
            case 'b'  => '\b'
            case 't'  => '\t'
            case 'e'  => '\u001B'
            case '\\' => '\\'
            case '"'  => '"'
            case '\'' => '\''
            case 'u'  =>
              val codePoint = parseUnicode(part, cur + 1)
              cur += 4
              codePoint
            case other =>
              report.errorAndAbort(s"the character $other should not be escaped")
          topFrame.buffer.append(decoded)
          esc = false
          cur += 1
        else
          topFrame.buffer.append(ch)
          cur += 1
      if esc then
        report.errorAndAbort("the final character of an m\"\" part cannot be an escape")

    val subListRef: Expr[List[Message]] => Expr[Message] = { subListExpr =>
      parts.iterator.zipWithIndex.foreach: (part, idx) =>
        walkPart(part)
        if idx < parts.size - 1 then appendSub(idx, subListExpr)

      if stack.size != 1 then
        report.errorAndAbort("the m\"\" interpolator has an unmatched backtick")

      finalizeBuffer()
      buildMessageExpr(topFrame)
    }

    '{
      val subList: List[Message] = mSubMessages[param]($subs)
      ${ subListRef('subList) }
    }
