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
  given text: Communicable[Text] = Message(_)
  given string: Communicable[String] = string => Message(string.tt)
  given char: Communicable[Char] = char => Message(char.toString.tt) // Escape this
  given int: Communicable[Int] = int => Message(int.toString.tt)
  given long: Communicable[Long] = long => Message(long.toString.tt)
  given message: Communicable[Message] = identity(_)

  given listMessage: Communicable[List[Message]] = messages =>
    Message(List.fill(messages.size)("\n - ".tt) ::: List("".tt), messages)

trait Communicable[-ValueType]:
  def message(value: ValueType): Message

extension [ValueType](value: ValueType)(using communicable: Communicable[ValueType])
  def communicate: Message = communicable.message(value)
