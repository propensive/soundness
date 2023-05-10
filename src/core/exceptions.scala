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

import language.experimental.captureChecking

case class ErrorMessage(textParts: List[Text], subs: List[Text]):
  def fold
      [ResultType]
      (initial: ResultType)
      (textFn: (ResultType, Text) -> ResultType, subFn: (ResultType, Text) -> ResultType)
      : ResultType =
    def recur(textParts: List[Text], subs: List[Text], current: ResultType): ResultType = subs match
      case Nil         => textFn(current, textParts.head)
      case sub :: subs => recur(textParts.tail, subs, subFn(textFn(current, textParts.head), sub))

    recur(textParts, subs, initial)
  
  def text: Text = Text(fold("")(_+_, _+_))

transparent abstract class Error(val message: ErrorMessage, val cause: Maybe[Error] = Unset)
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

case class StreamCutError(total: ByteSize)
extends Error(ErrorMessage(List(Text("the stream was cut prematurely after "), Text("")),
    List(total.text)))
