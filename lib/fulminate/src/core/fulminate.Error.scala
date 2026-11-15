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

import scala.caps

import anticipation.*

// The domain of raisable values: any pure exception. `Error` is pure by
// inheritance, so every Soundness error is a `Hazard`; a JDK exception must be
// wrapped in an `Error` (e.g. by `Error(throwable)`) to be raised, since
// purity cannot be established for arbitrary `Throwable` subclasses.
type Hazard = Exception & caps.Pure

object Error:
  def apply(throwable: Throwable): Error = throwable match
    case error: Error         => error
    case throwable: Throwable => Unchecked(throwable)

  object Unchecked:
    def describe(message: Text): Message =
      if message == "".tt then m"unchecked error with no message"
      else m"unchecked error with message, $message"

    def apply(throwable: Throwable): Unchecked =
      val message: Text = throwable.getMessage() match
        case null    => "".tt
        case message => message.tt

      new Unchecked(message, throwable.getStackTrace)

  case class Unchecked private
    ( text:       Text,
      // [java-boundary] raw stack-trace array from Throwable.getStackTrace
      @scala.caps.unsafe.untrackedCaptures
      stackTrace: scala.Array[StackTraceElement | Null] | Null )
  extends Error(15, 1, 0)(Unchecked.describe(text))(using errorDiagnostics.emptyDiagnostics):
    setStackTrace(stackTrace)

// An error is identified by the number of its module, its own number within the module, and — if
// it distinguishes several cases — the number of its variant, which together are its bolt code
// (`Bolt`). An error given no numbers has no code.
transparent abstract class Error(val module: Int, val number: Int, val variant: Int)
  ( val message: Message, private val cause: Throwable | Null )
  ( using val diagnostics: Diagnostics )
// `caps.Pure`: an error may never hold a live capability. `throw` is the one
// channel capture checking cannot see (an exception caught outside a scope
// arrives with its captures erased), so purity here guarantees that no error —
// raised, recorded, aborted or thrown — can smuggle a capability out of the
// scope that confines it.
extends Exception(message.text.s, cause, false, diagnostics.captureStack), caps.Pure:
  this: Error =>

  def this(module: Int, number: Int, variant: Int)(message: Message)(using Diagnostics) =
    this(module, number, variant)(message, null)

  def this(message: Message, cause: Throwable | Null = null)(using Diagnostics) =
    this(0, 0, 0)(message, cause)

  def fullClass: List[Text] =
    (getClass.getName.nn.split("\\.").nn.iterator.map { part => Text(part.nn) }).to(List)

  def className: Text = List.last(fullClass)
  def component: Text = List.head(fullClass)

  // The error's bolt code: `↯/s.`, its module's number, and its own within the module, with its
  // variant if it has one. An error which is reported on account of another project's overrides
  // this to carry that error's code.
  def bolt: Bolt =
    if module == 0 then Bolt.Uncoded else Bolt(Bolt.Soundness, module, number, variant)

  def errorCode: Text = bolt.text
  def colourCode: Text = bolt.colorLabel

  def labelled: Message =
    if !bolt.coded then message else m"${bolt.label} $message"

  override def getMessage: String =
    if !bolt.coded then component.s+": "+message.text else bolt.label.s+" "+message.text

  override def getCause: Throwable | Null = cause
