/*
    Perforate, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package perforate

import rudiments.*
import digression.*
import fulminate.*

import language.experimental.captureChecking

@capability
trait ErrorHandler[-ErrorType <: Error]:
  def record(error: ErrorType): Unit
  def apply(): Unit

@capability
class Aggregator[ErrorType <: Error](flow: ControlFlow[ErrorType])(using CanThrow[AggregateError[ErrorType]])
extends ErrorHandler[ErrorType]:
  private var collected: List[ErrorType] = Nil
  def record(error: ErrorType): Unit = collected ::= error
  def apply(): Unit = if !collected.isEmpty then throw AggregateError[ErrorType](collected.reverse)

@capability
class Thrower[ErrorType <: Error: CanThrow](flow: ControlFlow[ErrorType]) extends ErrorHandler[ErrorType]:
  def record(error: ErrorType): Unit = throw error
  def apply(): Unit = ()

case class UnexpectedSuccessError()
extends Error(msg"the expression was expected to fail, but succeeded")

@capability
class Capturer[ErrorType <: Error](flow: ControlFlow[ErrorType])(using CanThrow[UnexpectedSuccessError])
extends ErrorHandler[ErrorType]:
  def record(error: ErrorType): Unit =
    boundary.break(error)(using flow.label.or(throw Mistake(msg"expected control flow value")))
  def apply(): Unit = throw UnexpectedSuccessError()

trait Recovery[-ErrorType <: Error, +ResultType]:
  def recover(error: ErrorType): ResultType

object Perforate:
  opaque type ControlFlow[BreakType] = Maybe[boundary.Label[BreakType]]

  object ControlFlow:
    def apply[BreakType](label: Maybe[boundary.Label[BreakType]]): ControlFlow[BreakType] = label
  
  extension [BreakType](flow: ControlFlow[BreakType]) def label: Maybe[boundary.Label[BreakType]] = flow

export Perforate.ControlFlow

def raise
    [ResultType, ErrorType <: Error]
    (error: ErrorType)
    (using handler: ErrorHandler[ErrorType], recovery: Recovery[error.type, ResultType])
    : ResultType =
  handler.record(error)
  recovery.recover(error)

def raise
    [ResultType, ErrorType <: Error]
    (error: ErrorType)(ersatz: => ResultType)
    (using handler: ErrorHandler[ErrorType])
    : ResultType =
  handler.record(error)
  ersatz

def validate
    [ErrorType <: Error]
    (using aggregate: CanThrow[AggregateError[ErrorType]])
    [ResultType]
    (fn: ErrorHandler[ErrorType] ?=> ResultType)
    : ResultType =
  
  boundary:
    val aggregator = Aggregator[ErrorType](ControlFlow(Unset))
    fn(using aggregator).tap(aggregator().waive)

def attempt
    [ErrorType <: Error]
    (using error: CanThrow[ErrorType])
    [ResultType]
    (fn: ErrorHandler[ErrorType] ?=> ResultType)
    : ResultType =
  
  boundary: label ?=>
    val thrower = Thrower[ErrorType](ControlFlow(Unset))
    fn(using thrower).tap(thrower().waive)

def capture
    [ErrorType <: Error]
    (using error: CanThrow[UnexpectedSuccessError])
    [ResultType]
    (fn: ErrorHandler[ErrorType] ?=> ResultType)
    : ErrorType =
  
  boundary: label ?=>
    val capturer = Capturer[ErrorType](ControlFlow(label))
    fn(using capturer)
    throw UnexpectedSuccessError()

package errorHandlers:
  given throwAnything: ErrorHandler[Error] = Thrower[Error](ControlFlow(Unset))(using unsafeExceptions.canThrowAny)
