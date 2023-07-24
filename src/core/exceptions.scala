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

import anticipation.*

import scala.compiletime.*

import language.experimental.captureChecking

object Message:
  def apply(value: Text): Message = Message(List(value))

  transparent inline def makeMessages
      [TupleType <: Tuple]
      (inline subs: TupleType, done: List[Message])
      : List[Message] =
    inline erasedValue[TupleType] match
      case _: (messageType *: tailType) => (subs: @unchecked) match
        case message *: tail =>
          val message2 = message.asInstanceOf[messageType]
          val asMessage = summonInline[AsMessage[messageType]]
          makeMessages[tailType](tail.asInstanceOf[tailType], asMessage.message(message2) :: done)

      case _ =>
        done.reverse

case class Message(textParts: List[Text], subs: List[Message] = Nil):
  def fold[RenderType](initial: RenderType)(append: (RenderType, Text, Int) -> RenderType): RenderType =
    def recur(done: RenderType, textTodo: List[Text], subsTodo: List[Message], level: Int): RenderType =
      subsTodo match
        case Nil =>
          append(done, textTodo.head, level)
        
        case sub :: subs =>
          val prefix = recur(append(done, textTodo.head, level), sub.textParts, sub.subs, level + 1)
          recur(prefix, textTodo.tail, subs, level)

    recur(initial, textParts, subs, 0)

  def text: Text = unwrap(fold[String]("") { (acc, next, level) => acc+next })
  
  def richText: Text = unwrap:
    fold[String](""): (acc, next, level) =>
      if next.s.isEmpty then acc else level match
        case 0 => acc+next
        case 1 => acc+s"${27.toChar}[3m"+next+s"${27.toChar}[0m"
        case _ => acc+s"${27.toChar}[3m${27.toChar}[1m"+next+s"${27.toChar}[0m"
  
  def unwrap(string: String): Text =
    val buf: StringBuilder = StringBuilder()
    
    def recur(lines: List[String], break: Boolean): Text = lines match
      case Nil =>
        buf.toString.trim.nn.tt
      
      case line :: tail =>
        if line.forall(_.isWhitespace) then recur(tail, true) else
          buf.append(if !break then " " else "\n")
          buf.append(line.trim.nn.replaceAll("\\s+", " "))
          recur(tail, false)
    
    recur(string.split("\n").nn.map(_.nn).to(List), false)

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
    Mistake(msg"""
      an ${error.getClass.getName.nn.tt} exception was unexpectedly thrown;
      the error was: ${Option(error.getMessage).getOrElse("[null]").nn.tt}
    """)

case class Mistake(message: Message) extends java.lang.Error(message.text.s)

object AsMessage:
  given AsMessage[Text] = Message(_)
  given AsMessage[String] = string => Message(string.tt)
  given AsMessage[Char] = char => Message(char.toString.tt) // Escape this
  given AsMessage[Int] = int => Message(int.toString.tt)
  given AsMessage[Message] = identity(_)

trait AsMessage[-ValueType]:
  def message(value: ValueType): Message

extension (inline context: StringContext)
  transparent inline def msg[ParamType](inline subs: ParamType = EmptyTuple): Message =
    inline subs.asMatchable match
      case tuple: Tuple =>
        Message(context.parts.map(Text(_)).to(List), Message.makeMessages[tuple.type](tuple, Nil))
      
      case other =>
        Message(context.parts.map(Text(_)).to(List), List(summonInline[AsMessage[other.type]].message(other)))
