/*
    Contingency, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package contingency

import fulminate.*
import vacuous.*
import rudiments.*

import scala.quoted.*
import scala.compiletime.*

import java.util.concurrent.atomic as juca

import language.experimental.captureChecking

infix type fixes[+ErrorType1 <: Error, -ErrorType2 <: Error] = Fix[ErrorType2, ErrorType1]

@FunctionalInterface
trait Fix[-ErrorType1 <: Error, +ErrorType2 <: Error]:
  def fix(error: ErrorType1): ErrorType2

object Raises:
  given fixStrategy[ErrorType1 <: Error, ErrorType2 <: Error]
      (using fix: Fix[ErrorType1, ErrorType2], strategy: Raises[ErrorType2])
        : Raises[ErrorType1] =
    strategy.contramap(fix.fix(_))

@capability
trait Raises[-ErrorType <: Error] extends Pure:

  private inline def raises: this.type = this

  def record(error: ErrorType): Unit
  def abort(error: ErrorType): Nothing

  def contramap[ErrorType2 <: Error](lambda: ErrorType2 -> ErrorType): Raises[ErrorType2] =
    new Raises[ErrorType2]:
      def record(error: ErrorType2): Unit = raises.record(lambda(error))
      def abort(error: ErrorType2): Nothing = raises.abort(lambda(error))

@capability
class ThrowStrategy
    [ErrorType <: Error, SuccessType]()(using @annotation.constructorOnly error: CanThrow[ErrorType])
extends Raises[ErrorType]:
  def record(error: ErrorType): Unit = throw error
  def abort(error: ErrorType): Nothing = throw error

@capability
class FailStrategy[ErrorType <: Error, SuccessType]()(using Quotes, Realm) extends Raises[ErrorType]:
  def record(error: ErrorType): Unit = fail(error.message)
  def abort(error: ErrorType): Nothing = fail(error.message)

@capability
class AggregateStrategy
    [ErrorType <: Error, SuccessType]
    (label: boundary.Label[Either[AggregateError[ErrorType], SuccessType]])
extends Raises[ErrorType]:

  private val collected: juca.AtomicReference[List[ErrorType]] = juca.AtomicReference(Nil)
  
  def record(error: ErrorType): Unit = collected.getAndUpdate(error :: _.nn)
  
  def abort(error: ErrorType): Nothing =
    boundary.break(Left(AggregateError(error :: collected.get().nn)))(using label)
  
  def finish(): Unit =
    if !collected.get().nn.isEmpty then boundary.break(Left(AggregateError(collected.get().nn)))(using label)

@capability
class EitherStrategy[ErrorType <: Error, SuccessType](label: boundary.Label[Either[ErrorType, SuccessType]])
    (using @annotation.constructorOnly unexpectedSuccess: Raises[UnexpectedSuccessError[SuccessType]])
extends Raises[ErrorType]:

  def record(error: ErrorType): Unit = boundary.break(Left(error))(using label)
  def abort(error: ErrorType): Nothing = boundary.break(Left(error))(using label)

@capability
class OptionalStrategy[ErrorType <: Error, SuccessType](label: boundary.Label[Optional[SuccessType]])
extends Raises[ErrorType]:
  type Result = Optional[SuccessType]
  type Return = Optional[SuccessType]
  
  def record(error: ErrorType): Unit = boundary.break(Unset)(using label)
  def abort(error: ErrorType): Nothing = boundary.break(Unset)(using label)

@capability
class AttemptStrategy[ErrorType <: Error, SuccessType](label: boundary.Label[Attempt[SuccessType, ErrorType]])
extends Raises[ErrorType]:
  type Result = Attempt[SuccessType, ErrorType]
  type Return = Attempt[SuccessType, ErrorType]
  
  def record(error: ErrorType): Unit = boundary.break(Attempt.Failure(error))(using label)
  def abort(error: ErrorType): Nothing = boundary.break(Attempt.Failure(error))(using label)

trait Recovery[-ErrorType <: Error, +SuccessType]:
  def recover(error: ErrorType): SuccessType

def raise[SuccessType, ErrorType <: Error](error: ErrorType)
    (using handler: Raises[ErrorType], recovery: Recovery[ErrorType, SuccessType])
      : SuccessType =
  handler.record(error)
  recovery.recover(error)

def raise[SuccessType, ErrorType <: Error](error: ErrorType)(ersatz: => SuccessType)
    (using handler: Raises[ErrorType])
      : SuccessType =
  handler.record(error)
  ersatz

def abort[SuccessType, ErrorType <: Error](error: ErrorType)(using handler: Raises[ErrorType]): Nothing =
  handler.abort(error)

def safely[ErrorType <: Error](using DummyImplicit)[SuccessType]
    (block: OptionalStrategy[ErrorType, SuccessType] ?=> CanThrow[Exception] ?=> SuccessType)
      : Optional[SuccessType] =

  try boundary: label ?=>
    block(using OptionalStrategy(label))
  catch case error: Exception => Unset

def unsafely[ErrorType <: Error](using DummyImplicit)[SuccessType]
    (block: ThrowStrategy[ErrorType, SuccessType] ?=> CanThrow[Exception] ?=> SuccessType)
      : SuccessType =

  boundary: label ?=>
    import unsafeExceptions.canThrowAny
    block(using ThrowStrategy())

def throwErrors[ErrorType <: Error](using CanThrow[ErrorType])[SuccessType]
    (block: ThrowStrategy[ErrorType, SuccessType] ?=> SuccessType)
      : SuccessType =

  block(using ThrowStrategy())

def validate[ErrorType <: Error](using raise: Raises[AggregateError[ErrorType]])[SuccessType]
    (block: AggregateStrategy[ErrorType, SuccessType] ?=> SuccessType)
      : SuccessType =

  val value: Either[AggregateError[ErrorType], SuccessType] =
    boundary: label ?=>
      val raiser = AggregateStrategy(label)
      Right(block(using raiser)).also(raiser.finish())
  
  value match
    case Left(error)  => abort[SuccessType, AggregateError[ErrorType]](error)
    case Right(value) => value

def capture[ErrorType <: Error](using DummyImplicit)[SuccessType]
    (block: EitherStrategy[ErrorType, SuccessType] ?=> SuccessType)
    (using raise: Raises[UnexpectedSuccessError[SuccessType]])
      : ErrorType =
  val value: Either[ErrorType, SuccessType] = boundary: label ?=>
    Right(block(using EitherStrategy(label)))
  
  value match
    case Left(error)  => error
    case Right(value) => abort(UnexpectedSuccessError(value))

def attempt[ErrorType <: Error](using DummyImplicit)[SuccessType]
    (block: AttemptStrategy[ErrorType, SuccessType] ?=> SuccessType)
      : Attempt[SuccessType, ErrorType] =

  boundary: label ?=>
    Attempt.Success(block(using AttemptStrategy[ErrorType, SuccessType](label)))

def failCompilation[ErrorType <: Error](using Quotes, Realm)[SuccessType]
    (block: FailStrategy[ErrorType, SuccessType] ?=> SuccessType)
      : SuccessType =

  given FailStrategy[ErrorType, SuccessType]()
  block

case class AggregateError[+ErrorType <: Error](errors: List[ErrorType])
extends Error(Communicable.listMessage.message(errors.map(_.message)))

case class UnexpectedSuccessError[ResultType](result: ResultType)
extends Error(msg"the expression was expected to fail, but succeeded")

package errorHandlers:
  given throwUnsafely[SuccessType]: ThrowStrategy[Error, SuccessType] =
    ThrowStrategy()(using unsafeExceptions.canThrowAny)

  given throwSafely[ErrorType <: Error: CanThrow, SuccessType]: ThrowStrategy[ErrorType, SuccessType] =
    ThrowStrategy()

infix type raises[SuccessType, ErrorType <: Error] = Raises[ErrorType] ?=> SuccessType

enum Attempt[+SuccessType, +ErrorType <: Error]:
  case Success(value: SuccessType)
  case Failure(value: ErrorType)

  def handle(block: PartialFunction[ErrorType, Error]): Attempt[SuccessType, Error] = this match
    case Success(value) => Success(value)
    case Failure(value) => Failure(if block.isDefinedAt(value) then block(value) else value)

  transparent inline def get: SuccessType raises ErrorType = this match
    case Success(value) => value
    case Failure(error) => abort(error)

  def recover[SuccessType2 >: SuccessType](block: PartialFunction[ErrorType, SuccessType2]): SuccessType2 = this match
    case Success(value) => value
    case Failure(error) => block(error)
