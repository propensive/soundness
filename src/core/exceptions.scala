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

transparent abstract class Error
    (val message: Message, private val cause: Error | Null = null, hideStack: Boolean = false)
extends Exception(message.text.s, cause, false, !hideStack):
  this: Error =>
  def fullClass: List[Text] = List(getClass.nn.getName.nn.split("\\.").nn.map(_.nn).map(Text(_))*)
  def className: Text = fullClass.last
  def component: Text = fullClass.head

  override def getMessage: String = component.s+": "+message.text
  override def getCause: Exception | Null = cause
  
object Mistake:
  def apply(error: Exception): Mistake =
    Mistake(msg"""
      an ${error.getClass.getName.nn.tt} exception was unexpectedly thrown;
      the error was: ${Option(error.getMessage).getOrElse("[null]").nn.tt}
    """)

case class Mistake(message: Message) extends java.lang.Error(message.text.s)

