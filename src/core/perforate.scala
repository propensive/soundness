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

import fulminate.*
import rudiments.*

import scala.quoted.*

import java.util.concurrent.atomic as juca

import language.experimental.captureChecking

@capability
trait Raises[-ErrorType <: Error]:
  def record(error: ErrorType): Unit
  def abort(error: ErrorType): Nothing
  private[perforate] def finish(): Unit = ()

trait ErrorHandler[SuccessType]:
  type Result
  type Return
  type Raiser <: Raises[?]

  def raiser(label: boundary.Label[Result]): Raiser
  def wrap(value: => SuccessType): Result
  def finish(value: Result): Return

@capability
class RaisesThrow[ErrorType <: Error, SuccessType]()(using CanThrow[ErrorType]) extends Raises[ErrorType]:
  def record(error: ErrorType): Unit = throw error
  def abort(error: ErrorType): Nothing = throw error

@capability
class RaisesCompileFailure[ErrorType <: Error, SuccessType]()(using Quotes) extends Raises[ErrorType]:
  def record(error: ErrorType): Unit = fail(error.message)
  def abort(error: ErrorType): Nothing = fail(error.message)

@capability
class RaisesAggregate
    [ErrorType <: Error, SuccessType]
    (label: boundary.Label[Either[AggregateError[ErrorType], SuccessType]])
extends Raises[ErrorType]:

  private val collected: juca.AtomicReference[List[ErrorType]] = juca.AtomicReference(Nil)
  
  def record(error: ErrorType): Unit = collected.getAndUpdate(error :: _.nn)
  
  def abort(error: ErrorType): Nothing =
    boundary.break(Left(AggregateError(error :: collected.get().nn)))(using label)
  
  private[perforate] override def finish(): Unit =
    if !collected.get().nn.isEmpty then boundary.break(Left(AggregateError(collected.get().nn)))(using label)

@capability
class RaisesErrorResult
    [ErrorType <: Error, SuccessType]
    (label: boundary.Label[Either[ErrorType, SuccessType]])
    (using Raises[UnexpectedSuccessError[SuccessType]])
extends Raises[ErrorType]:

  def record(error: ErrorType): Unit = boundary.break(Left(error))(using label)
  def abort(error: ErrorType): Nothing = boundary.break(Left(error))(using label)

@capability
class RaisesMaybe[ErrorType <: Error, SuccessType](label: boundary.Label[Maybe[SuccessType]])
extends Raises[ErrorType]:
  type Result = Maybe[SuccessType]
  type Return = Maybe[SuccessType]
  
  def record(error: ErrorType): Unit = boundary.break(Unset)(using label)
  def abort(error: ErrorType): Nothing = boundary.break(Unset)(using label)

class ThrowErrors[ErrorType <: Error, SuccessType]()(using CanThrow[ErrorType])
extends ErrorHandler[SuccessType]:
  type Result = Either[ErrorType, SuccessType]
  type Return = SuccessType
  type Raiser = RaisesThrow[ErrorType, SuccessType]
 
  def raiser(label: boundary.Label[Result]): Raiser = RaisesThrow()
  def wrap(block: => SuccessType): Result = Right(block)
  
  def finish(value: Result): Return = value match
    case Left(error)  => throw error
    case Right(value) => value

class FailCompilation[ErrorType <: Error, SuccessType]()(using Quotes) extends ErrorHandler[SuccessType]:
  
  type Result = Either[ErrorType, SuccessType]
  type Return = SuccessType
  type Raiser = RaisesCompileFailure[ErrorType, SuccessType]
 
  def raiser(label: boundary.Label[Result]): Raiser = RaisesCompileFailure()
  def wrap(block: => SuccessType): Result = Right(block)
  
  def finish(value: Result): Return = value match
    case Left(error)  => fail(error.message)
    case Right(value) => value

class Validate
    [ErrorType <: Error, SuccessType]
    (using Raises[AggregateError[ErrorType]])
extends ErrorHandler[SuccessType]:
  type Result = Either[AggregateError[ErrorType], SuccessType]
  type Return = SuccessType
  type Raiser = RaisesAggregate[ErrorType, SuccessType]
  
  def raiser(label: boundary.Label[Result]): Raiser = RaisesAggregate(label)
  def wrap(block: => SuccessType): Either[AggregateError[ErrorType], SuccessType] = Right(block)
  
  def finish(value: Result): Return = value match
    case Left(error)  => abort[SuccessType, AggregateError[ErrorType]](error)
    case Right(value) => value

class Capture
    [ErrorType <: Error, SuccessType]
    (using handler: Raises[UnexpectedSuccessError[SuccessType]])
extends ErrorHandler[SuccessType]:
  type Result = Either[ErrorType, SuccessType]
  type Return = ErrorType
  type Raiser = RaisesErrorResult[ErrorType, SuccessType]
  
  def raiser(label: boundary.Label[Result]): Raiser = RaisesErrorResult(label)
  def wrap(block: => SuccessType): Either[ErrorType, SuccessType] = Right(block)
  
  def finish(value: Either[ErrorType, SuccessType]): ErrorType = value match
    case Left(error)  => error
    case Right(value) => abort(UnexpectedSuccessError(value))

class Safely[ErrorType <: Error, SuccessType]() extends ErrorHandler[SuccessType]:
  type Result = Maybe[SuccessType]
  type Return = Maybe[SuccessType]
  type Raiser = RaisesMaybe[ErrorType, SuccessType]
  
  def raiser(label: boundary.Label[Result]): RaisesMaybe[ErrorType, SuccessType] = RaisesMaybe(label)
  def wrap(block: => SuccessType): Maybe[SuccessType] = block
  def finish(value: Maybe[SuccessType]): Maybe[SuccessType] = value

trait Recovery[-ErrorType <: Error, +SuccessType]:
  def recover(error: ErrorType): SuccessType

def raise
    [SuccessType, ErrorType <: Error]
    (error: ErrorType)
    (using handler: Raises[ErrorType], recovery: Recovery[ErrorType, SuccessType])
    : SuccessType =
  handler.record(error)
  recovery.recover(error)

def raise
    [SuccessType, ErrorType <: Error]
    (error: ErrorType)(ersatz: => SuccessType)
    (using handler: Raises[ErrorType])
    : SuccessType =
  handler.record(error)
  ersatz

def abort[SuccessType, ErrorType <: Error](error: ErrorType)(using handler: Raises[ErrorType]): Nothing =
  handler.abort(error)

def safely
    [ErrorType <: Error]
    (using DummyImplicit)
    [SuccessType]
    (block: CanThrow[Exception] ?=> RaisesMaybe[ErrorType, SuccessType] ?=> SuccessType)
    : Maybe[SuccessType] =
  
  try handle(Safely[ErrorType, SuccessType])(block(using unsafeExceptions.canThrowAny))
  catch case error: Exception => Unset

def throwErrors
    [ErrorType <: Error]
    (using CanThrow[ErrorType])
    [SuccessType]
    (block: RaisesThrow[ErrorType, SuccessType] ?=> SuccessType)
    : SuccessType =
  
  handle(ThrowErrors[ErrorType, SuccessType])(block)

def validate
    [ErrorType <: Error]
    (using raise: Raises[AggregateError[ErrorType]])
    [SuccessType]
    (block: RaisesAggregate[ErrorType, SuccessType] ?=> SuccessType)
    : SuccessType =

  handle(Validate[ErrorType, SuccessType]())(block)

def capture
    [ErrorType <: Error]
    (using DummyImplicit)
    [SuccessType]
    (block: RaisesErrorResult[ErrorType, SuccessType] ?=> SuccessType)
    (using raise: Raises[UnexpectedSuccessError[SuccessType]])
    : ErrorType =
  
  handle(Capture[ErrorType, SuccessType])(block)

def failCompilation
    [ErrorType <: Error]
    (using Quotes)
    [SuccessType]
    (block: RaisesCompileFailure[ErrorType, SuccessType] ?=> SuccessType)
    : SuccessType =

  handle(FailCompilation[ErrorType, SuccessType])(block)

def handle
    [SuccessType, HandlerType <: ErrorHandler[SuccessType]]
    (handler: HandlerType^)
    (block: handler.Raiser ?=> SuccessType)
    : handler.Return =
  handler.finish:
    boundary: label ?=>
      val raiser = handler.raiser(label)
      handler.wrap(block(using raiser)).tap(raiser.finish().waive)

def unsafely[ResultType](block: CanThrow[Exception] ?=> ResultType): ResultType =
  block(using unsafeExceptions.canThrowAny)

case class AggregateError[+ErrorType <: Error](errors: List[ErrorType])
extends Error(AsMessage.listMessage.message(errors.map(_.message)))

case class UnexpectedSuccessError[ResultType](result: ResultType)
extends Error(msg"the expression was expected to fail, but succeeded")

package errorHandlers:
  given throwUnsafely[SuccessType]: RaisesThrow[Error, SuccessType] =
    RaisesThrow()(using unsafeExceptions.canThrowAny)

infix type raises[SuccessType, ErrorType <: Error] = Raises[ErrorType] ?=> SuccessType
