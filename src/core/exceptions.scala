/*
    Rudiments, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package rudiments

import scala.compiletime.*

import language.experimental.captureChecking

object Message:
  def apply(value: Text): Message = Message(List(value))

  transparent inline def makeMessages(inline subs: Tuple, done: List[Message]): List[Message] =
    inline subs match
      case message *: tail =>
        makeMessages(tail, compiletime.summonInline[AsMessage[message.type]].message(message) :: done)
      case _ =>
        done.reverse

case class Message(textParts: List[Text], subs: List[Message] = Nil):
  def fold[RenderType](initial: RenderType)(append: (RenderType, Text, Int) -> RenderType): RenderType =
    def recur(done: RenderType, textTodo: List[Text], subsTodo: List[Message], level: Int): RenderType =
      subsTodo match
        case Nil => append(done, textTodo.head, level)
        case sub :: subs => recur(recur(append(done, textTodo.head, level), sub.textParts, sub.subs, level + 1), textTodo.tail, subs, level)

    recur(initial, textParts, subs, 0)

  def text: Text = Text(fold[String]("") { (acc, next, level) => acc+next })

transparent abstract class Error(val message: Message, val cause: Maybe[Error] = Unset)
extends Exception():
  this: Error =>
  def fullClass: List[Text] = List(getClass.nn.getName.nn.split("\\.").nn.map(_.nn).map(Text(_))*)
  def className: Text = fullClass.last
  def component: Text = fullClass.head

  override def getMessage: String = component.s+": "+message.text
  override def getCause: Exception | Null = cause.option.getOrElse(null)
  
  def explanation: Maybe[Text] = Unset

object Mistake:
  def apply(error: Exception): Mistake =
    Mistake(s"rudiments: an ${error.getClass.getName} exception was thrown when this was not "+
        s"believed to be possible; the error was '${error.getMessage}'")

case class Mistake(message: String) extends java.lang.Error(message)

object AsMessage:
  given AsMessage[Text] = Message(_)
  given AsMessage[Char] = char => Message(Text(char.toString)) // Escape this
  given AsMessage[Int] = int => Message(Text(int.toString))
  given AsMessage[Message] = identity(_)

trait AsMessage[-ValueType]:
  def message(value: ValueType): Message

extension (inline context: StringContext)
  transparent inline def msg[ParamType](inline subs: ParamType = EmptyTuple): Message =
    inline subs.asMatchable match
      case tuple: Tuple =>
        Message(context.parts.map(Text(_)).to(List), Message.makeMessages(tuple, Nil))
      
      case other =>
        Message(context.parts.map(Text(_)).to(List), List(compiletime.summonInline[AsMessage[other.type]].message(other)))
