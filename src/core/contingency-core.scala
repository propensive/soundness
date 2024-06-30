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

import language.experimental.pureFunctions

import scala.quoted.*
import scala.compiletime.*

import fulminate.*
import vacuous.*
import rudiments.*
import symbolism.*
import anticipation.*

given realm: Realm = realm"contingency"

def raise[SuccessType, ErrorType <: Error](error: ErrorType)
    (using handler: Errant[ErrorType], recovery: Recovery[ErrorType, SuccessType])
        : SuccessType =
  handler.record(error)
  recovery.recover(error)

def raise[SuccessType, ErrorType <: Error](error: ErrorType)(ersatz: => SuccessType)
    (using handler: Errant[ErrorType])
        : SuccessType =
  handler.record(error)
  ersatz

def abort[SuccessType, ErrorType <: Error](error: ErrorType)(using handler: Errant[ErrorType])
        : Nothing =
  handler.abort(error)

def safely[ErrorType <: Error](using DummyImplicit)[SuccessType]
    (block: OptionalStrategy[ErrorType, SuccessType] ?=> CanThrow[Exception] ?=> SuccessType)
        : Optional[SuccessType] =

  try boundary: label ?=>
    block(using OptionalStrategy(label))
  catch case error: Exception => Unset

def unsafely[ErrorType <: Error](using DummyImplicit)[SuccessType]
    (block: Unsafe ?=> ThrowStrategy[ErrorType, SuccessType] ?=> CanThrow[Exception] ?=>
              SuccessType)
        : SuccessType =

  boundary: label ?=>
    import unsafeExceptions.canThrowAny
    block(using Unsafe)(using ThrowStrategy())

def throwErrors[ErrorType <: Error](using CanThrow[ErrorType])[SuccessType]
    (block: ThrowStrategy[ErrorType, SuccessType] ?=> SuccessType)
        : SuccessType =

  block(using ThrowStrategy())

def validate[ErrorType <: Error](using raise: Errant[AggregateError[ErrorType]])[SuccessType]
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
    (using raise: Errant[ExpectationError[SuccessType]])
        : ErrorType =
  val value: Either[ErrorType, SuccessType] = boundary: label ?=>
    Right(block(using EitherStrategy(label)))

  value match
    case Left(error)  => error
    case Right(value) => abort(ExpectationError(value))

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

infix type raises[SuccessType, ErrorType <: Error] = Errant[ErrorType] ?=> SuccessType

extension [ErrorType <: Error, ResultType](inline context: Tended[ErrorType, ResultType])
  transparent inline def remedy(inline lambda: PartialFunction[ErrorType, ResultType]): Any =
    ${Contingency.remedy('context, 'lambda)}

  inline def mitigate[ErrorType2 <: Error](inline lambda: PartialFunction[ErrorType, ErrorType2])
          : Any =
    ${Contingency.mitigate('context, 'lambda)}

inline def tend[ResultType, ErrorType <: Error](inline block: Errant[ErrorType] ?=> ResultType)
        : Tended[ErrorType, ResultType] =
  Tended[ErrorType, ResultType](block(using _))

package errorHandlers:
  given throwUnsafely[SuccessType]: ThrowStrategy[Error, SuccessType] =
    ThrowStrategy()(using unsafeExceptions.canThrowAny)

  given throwSafely[ErrorType <: Error: CanThrow, SuccessType]
          : ThrowStrategy[ErrorType, SuccessType] =
    ThrowStrategy()

infix type mitigates [ErrorType <: Error, ErrorType2 <: Error] =
  ErrorType2 is Mitigable into ErrorType
