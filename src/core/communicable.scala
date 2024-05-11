/*
    Fulminate, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import anticipation.*

import language.experimental.captureChecking

object Communicable:
  given text[TextType <: Text]: (Communicable { type Self = TextType }) = Message(_)
  given string[StringType <: String]: (Communicable { type Self = StringType }) = string => Message(string.tt)
  given char[CharType <: Char]: (Communicable { type Self = CharType }) = char => Message(char.toString.tt)
  given int[IntType <: Int]: (Communicable { type Self = IntType }) = int => Message(int.toString.tt)
  given long[LongType <: Long]: (Communicable { type Self = LongType }) = long => Message(long.toString.tt)
  given message: (Communicable { type Self = Message }) = identity(_)

  given listMessage: (Communicable { type Self = List[Message] }) =
    messages => Message(List.fill(messages.size)("\n - ".tt) ::: List("".tt), messages)

trait Communicable:
  type Self
  def message(value: Self): Message

extension [ValueType](value: ValueType)(using communicable: Communicable { type Self = ValueType })
  def communicate: Message = communicable.message(value)
