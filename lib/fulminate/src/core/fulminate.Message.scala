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
package fulminate

import scala.annotation.targetName
import scala.compiletime.*

import anticipation.*
import symbolism.*

object Message:
  def apply(value: Text): Message = Message(List(value))

  given printable: Message is Printable = (message, termcap) => message.text

  // In `Message`'s own companion (not `Communicable`'s) so it is in implicit scope when resolving
  // `Message is Transcribable to carrier` — letting `Loggable.fanOut` log a bare `Message`.
  given communicable: Message is Communicable = identity(_)


  transparent inline def apply[tuple <: Tuple](inline messages: tuple, done: List[Message])
  :   List[Message] =

    inline erasedValue[tuple] match
      case _: (message *: tail) =>
        messages.absolve match
          case message *: tail =>
            val message2 = message.asInstanceOf[message]
            val communicable = infer[(? >: message) is Communicable]
            apply[tail](tail.asInstanceOf[tail], communicable.message(message2) :: done)

      case _ =>
        done.reverse


case class Message(texts: List[Text], messages: List[Message] = Nil):
  @targetName("append")
  infix def + (right: Message): Message =
    Message
      ( texts.init ++ ((texts.last+right.texts.head) :: right.texts.tail),
        messages ++ right.messages )

  def segments: List[Text | Message] =
    def recur(parts: List[Text], messages: List[Message]): List[Text | Message] = parts match
      case head :: tail => messages.head :: head :: recur(tail, messages.tail)
      case Nil          => Nil

    texts.head :: recur(texts.tail, messages)

  def fold[render](initial: render)(append: (render, Text, Int) => render): render =
    def recur(done: render, textTodo: List[Text], messagesTodo: List[Message], level: Int): render =
      messagesTodo match
        case Nil => append(done, textTodo.head, level)

        case sub :: messages =>
          val prefix = recur(append(done, textTodo.head, level), sub.texts, sub.messages, level + 1)
          recur(prefix, textTodo.tail, messages, level)

    recur(initial, texts, messages, 0)

  def text: Text = unwrap(fold[String]("") { (acc, next, level) => acc+next })
  override def toString(): String = text.s

  def colorText: Text = unwrap:
    val esc = 27.toChar

    fold[String](""): (acc, next, level) =>
      if next.s.isEmpty then acc else level match
        case 0 => acc+next
        case 1 => s"$acc$esc[3m$next$esc[0m"
        case _ => s"$acc$esc[3m$esc[1m$next$esc[0m"

  def unwrap(string: String): Text =
    val buffer: StringBuilder = StringBuilder()

    def recur(lines: List[String]): Text = lines match
      case Nil => buffer.toString.nn.tt

      case line :: tail =>
        if line.forall(_.isWhitespace) then buffer.append("\n") else
          if !buffer.isEmpty then buffer.append(" ")

          buffer.append:
            line.nn.replaceAll("^ *", "").nn.replaceAll(" *$", "").nn.replaceAll("\\s+", " ")

        recur(tail)

    recur(string.split("\n").nn.map(_.nn).iterator.to(List))
