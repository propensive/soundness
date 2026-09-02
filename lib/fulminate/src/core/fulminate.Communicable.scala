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

import scala.quoted.*

import anticipation.*

object Communicable:
  given text: Text is Communicable = text =>
    Message:
      if text.s.length == 0 || text.s(0) == ' ' || text.s.last == ' ' then ("“"+text+"”").tt
      else text

  given string: String is Communicable = text.contramap(_.tt)
  given char: Char is Communicable = char => Message(char.toString.tt)
  given int: Int is Communicable = int => Message(int.toString.tt)
  given long: Long is Communicable = long => Message(long.toString.tt)

  given textualizable: [text: Textualizable] => text is Communicable =
    value => Message(value.textual)

  given term: (quotes: Quotes) => quotes.reflect.Term is Communicable = term => Message(term.show)
  given expr: [expr] => Quotes => Expr[expr] is Communicable = tpe => Message(tpe.show)

  given specializable: Specializable is Communicable = value =>
    Message(value.getClass.getName.nn.split("\\.").nn.last.nn.dropRight(1).toLowerCase.nn.tt)

  given listMessage: List[Message] is Communicable =
    messages =>
      // The `List.concat` primitive rather than symbolism's `+`: plain list plumbing needs no
      // typeclass here.
      val bullets = List.fill(List.size(messages))("\n - ".tt)
      Message(List.concat(bullets, List("".tt)), messages)

// A `Communicable` is a `Transcribable to Message`: converting a value to a `Message` is exactly
// how a loggable event is transcribed onto the common carrier. This lets `Loggable.fanOut` resolve
// `event is Transcribable to carrier` from the event's own `Communicable` (carrier = `Message`),
// with no separate bridge given and no need to name `Message` in `anticipation`.
trait Communicable extends Transcribable:
  type Result = Message
  def message(value: Self): Message
  def record(value: Self): Message = message(value)

  override def contramap[self](lambda: self -> Self): self is Communicable =
    value => message(lambda(value))
