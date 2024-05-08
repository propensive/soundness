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
  given [TextType <: Text] => TextType is Communicable = Message(_)
  given [StringType <: String] => StringType is Communicable = string => Message(string.tt)
  given [CharType <: Char] => CharType is Communicable = char => Message(char.toString.tt)
  given [IntType <: Int] => IntType is Communicable = int => Message(int.toString.tt)
  given [LongType <: Long] => LongType is Communicable = long => Message(long.toString.tt)
  given Message is Communicable = identity(_)

  given List[Message] is Communicable as listMessage =
    messages => Message(List.fill(messages.size)("\n - ".tt) ::: List("".tt), messages)

trait Communicable:
  type Self
  def message(value: Self): Message

extension [ValueType: Communicable as communicable](value: ValueType)
  def communicate: Message = communicable.message(value)
