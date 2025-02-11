/*
    Fulminate, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package fulminate

import language.experimental.captureChecking

import scala.quoted.*

import anticipation.*

object Communicable:
  given Text is Communicable = Message(_)
  given String is Communicable = string => Message(string.tt)
  given Char is Communicable = char => Message(char.toString.tt)
  given Int is Communicable = int => Message(int.toString.tt)
  given Long is Communicable = long => Message(long.toString.tt)
  given Message is Communicable = identity(_)

  given [TypeType] => Quotes => Type[TypeType] is Communicable = tpe => Message(tpe.toString.tt)
  given (quotes: Quotes) => quotes.reflect.TypeRepr is Communicable = tpe => Message(tpe.show)
  given (quotes: Quotes) => quotes.reflect.Term is Communicable = term => Message(term.show)
  given [ExprType] => Quotes => Expr[ExprType] is Communicable = tpe => Message(tpe.show)

  given listMessage: List[Message] is Communicable =
    messages => Message(List.fill(messages.size)("\n - ".tt) ::: List("".tt), messages)

trait Communicable:
  type Self
  def message(value: Self): Message
