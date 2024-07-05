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

package errorHandlers:
  given throwUnsafely[SuccessType]: ThrowTactic[Exception, SuccessType] =
    ThrowTactic()(using unsafeExceptions.canThrowAny)

  given throwSafely[ErrorType <: Exception: CanThrow, SuccessType]
          : ThrowTactic[ErrorType, SuccessType] =
    ThrowTactic()

given realm: Realm = realm"contingency"

def raise[SuccessType, ErrorType <: Exception](error: ErrorType)
    (using handler: Errant[ErrorType], recovery: Recovery[ErrorType, SuccessType])
        : SuccessType =
  handler.record(error)
  recovery.recover(error)

def raise[SuccessType, ErrorType <: Exception](error: ErrorType)(ersatz: => SuccessType)
    (using handler: Errant[ErrorType])
        : SuccessType =
  handler.record(error)
  ersatz

def abort[SuccessType, ErrorType <: Exception](error: ErrorType)(using handler: Errant[ErrorType])
        : Nothing =
  handler.abort(error)

def safely[ErrorType <: Exception](using DummyImplicit)[SuccessType]
    (block: OptionalTactic[ErrorType, SuccessType] ?=> CanThrow[Exception] ?=> SuccessType)
        : Optional[SuccessType] =

  try boundary: label ?=>
    block(using OptionalTactic(label))
  catch case error: Exception => Unset

def unsafely[ErrorType <: Exception](using DummyImplicit)[SuccessType]
    (block: Unsafe ?=> ThrowTactic[ErrorType, SuccessType] ?=> CanThrow[Exception] ?=>
              SuccessType)
        : SuccessType =

  boundary: label ?=>
    import unsafeExceptions.canThrowAny
    block(using Unsafe)(using ThrowTactic())

def throwErrors[ErrorType <: Exception](using CanThrow[ErrorType])[SuccessType]
    (block: ThrowTactic[ErrorType, SuccessType] ?=> SuccessType)
        : SuccessType =

  block(using ThrowTactic())

def validate[ErrorType <: Exception](using raise: Errant[AggregateError[ErrorType]])[SuccessType]
    (block: AggregateTactic[ErrorType, SuccessType] ?=> SuccessType)
        : SuccessType =

  val value: Either[AggregateError[ErrorType], SuccessType] =
    boundary: label ?=>
      val tactic = AggregateTactic(label)
      Right(block(using tactic)).also(tactic.finish())

  value match
    case Left(error)  => abort[SuccessType, AggregateError[ErrorType]](error)
    case Right(value) => value

def capture[ErrorType <: Exception](using DummyImplicit)[SuccessType]
    (block: EitherTactic[ErrorType, SuccessType] ?=> SuccessType)
    (using Errant[ExpectationError[SuccessType]])
        : ErrorType =
  val value: Either[ErrorType, SuccessType] = boundary: label ?=>
    Right(block(using EitherTactic(label)))

  value match
    case Left(error)  => error
    case Right(value) => abort(ExpectationError(value))

def attempt[ErrorType <: Exception](using DummyImplicit)[SuccessType]
    (block: AttemptTactic[ErrorType, SuccessType] ?=> SuccessType)
        : Attempt[SuccessType, ErrorType] =

  boundary: label ?=>
    Attempt.Success(block(using AttemptTactic[ErrorType, SuccessType](label)))

def abandonment[ErrorType <: Error](using Quotes, Realm)[SuccessType]
    (block: AbandonTactic[ErrorType, SuccessType] ?=> SuccessType)
        : SuccessType =

  given AbandonTactic[ErrorType, SuccessType]()
  block

infix type raises [SuccessType, ErrorType <: Exception] = Errant[ErrorType] ?=> SuccessType

infix type mitigates [ErrorType <: Exception, ErrorType2 <: Exception] =
  ErrorType2 is Mitigable into ErrorType

transparent inline def quell(inline block: PartialFunction[Exception, Exception]): Any =
  ${Contingency.quell('block)}

extension [LambdaType[_]](inline quell: Quell[LambdaType])
  inline def within[ResultType](inline lambda: LambdaType[ResultType]): Any =
    ${Contingency.quellWithin[LambdaType, ResultType]('quell, 'lambda)}

transparent inline def quash[ResultType](inline block: PartialFunction[Exception, ResultType]): Any =
  ${Contingency.quash[ResultType]('block)}

extension [ResultType, LambdaType[_]](inline quash: Quash[ResultType, LambdaType])
  inline def within[ResultType2 >: ResultType](inline lambda: LambdaType[ResultType2])
          : ResultType2 =
    ${Contingency.quashWithin[LambdaType, ResultType2]('quash, 'lambda)}
