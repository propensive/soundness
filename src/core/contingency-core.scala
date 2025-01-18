/*
    Contingency, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import scala.compiletime.*
import scala.quoted.*

import anticipation.*
import fulminate.*
import prepositional.*
import rudiments.*
import vacuous.*

package strategies:
  given throwUnsafely[SuccessType]: ThrowTactic[Exception, SuccessType] =
    ThrowTactic()(using unsafeExceptions.canThrowAny)

  given [ErrorType <: Exception: CanThrow, SuccessType]
      => ThrowTactic[ErrorType, SuccessType] as throwSafely =
    ThrowTactic()

  given [ErrorType <: Exception: Tactic, ErrorType2 <: Exception: Mitigable into ErrorType]
      => Tactic[ErrorType2] as mitigation =
    ErrorType.contramap(ErrorType2.mitigate(_))

  given [ErrorType <: Exception: Fatal] => Tactic[ErrorType] as fatalErrors:
    given Diagnostics as diagnostics = errorDiagnostics.stackTraces
    def record(error: Diagnostics ?=> ErrorType): Unit = ErrorType.status(error).terminate()
    def abort(error: Diagnostics ?=> ErrorType): Nothing = ErrorType.status(error).terminate()

  given [ErrorType <: Exception](using erased ErrorType is Unchecked)
      => Tactic[ErrorType] as uncheckedErrors:
    given Diagnostics as diagnostics = errorDiagnostics.stackTraces
    given CanThrow[Exception] = unsafeExceptions.canThrowAny
    def record(error: Diagnostics ?=> ErrorType): Unit = throw error
    def abort(error: Diagnostics ?=> ErrorType): Nothing = throw error

given realm: Realm = realm"contingency"

def raise[SuccessType, ErrorType <: Exception: Recoverable into SuccessType]
   (error: Diagnostics ?=> ErrorType)
   (using tactic: Tactic[ErrorType])
        : SuccessType =
  tactic.record(error)
  ErrorType.recover(error(using tactic.diagnostics))

def raise[SuccessType, ErrorType <: Exception: Tactic]
   (error: Diagnostics ?=> ErrorType, ersatz: => SuccessType)
        : SuccessType =
  ErrorType.record(error)
  ersatz

def abort[SuccessType, ErrorType <: Exception: Tactic](error: Diagnostics ?=> ErrorType): Nothing =
  ErrorType.abort(error)

def safely[ErrorType <: Exception](using DummyImplicit)[SuccessType]
   (block: (Diagnostics, OptionalTactic[ErrorType, SuccessType]) ?=> CanThrow[Exception] ?=>
                SuccessType)
        : Optional[SuccessType] =

  try boundary: label ?=>
    block(using Diagnostics.omit, OptionalTactic(label))
  catch case error: Exception => Unset

def unsafely[ErrorType <: Exception](using DummyImplicit)[SuccessType]
   (block: Unsafe ?=> ThrowTactic[ErrorType, SuccessType] ?=> CanThrow[Exception] ?=> SuccessType)
        : SuccessType =

  boundary: label ?=>
    import unsafeExceptions.canThrowAny
    block(using Unsafe)(using ThrowTactic())

def throwErrors[ErrorType <: Exception](using CanThrow[ErrorType])[SuccessType]
   (block: ThrowTactic[ErrorType, SuccessType] ?=> SuccessType)
        : SuccessType =

  block(using ThrowTactic())

def capture[ErrorType <: Exception](using DummyImplicit)[SuccessType]
   (block: EitherTactic[ErrorType, SuccessType] ?=> SuccessType)
   (using Tactic[ExpectationError[SuccessType]], Diagnostics)
        : ErrorType =
  val value: Either[ErrorType, SuccessType] = boundary: label ?=>
    Right(block(using EitherTactic(label)))

  value match
    case Left(error)  => error
    case Right(value) => abort(ExpectationError(value))

def attempt[ErrorType <: Exception](using DummyImplicit)[SuccessType]
   (block: AttemptTactic[ErrorType, SuccessType] ?=> SuccessType)
   (using Diagnostics)
        : Attempt[SuccessType, ErrorType] =

  boundary: label ?=>
    Attempt.Success(block(using AttemptTactic(label)))

def amalgamate[ErrorType <: Exception](using DummyImplicit)[SuccessType]
   (block: AmalgamateTactic[ErrorType, SuccessType] ?=> SuccessType)
   (using Diagnostics)
        : SuccessType | ErrorType =
  boundary: label ?=>
    block(using AmalgamateTactic(label))

def haltingly[ErrorType <: Error](using Quotes, Realm)[SuccessType]
   (block: Diagnostics ?=> HaltTactic[ErrorType, SuccessType] ?=> SuccessType)
        : SuccessType =

  given HaltTactic[ErrorType, SuccessType]()
  given Diagnostics = Diagnostics.omit
  block

infix type raises [SuccessType, ErrorType <: Exception] = Tactic[ErrorType] ?=> SuccessType

infix type mitigates [ErrorType <: Exception, ErrorType2 <: Exception] =
  ErrorType2 is Mitigable into ErrorType

infix type tracks [ResultType, FocusType] = Foci[FocusType] ?=> ResultType

inline def focus[FocusType, ResultType](using inline track: Foci[FocusType])
   (transform: (prior: Optional[FocusType]) ?=> FocusType)
   (block: => ResultType)
        : ResultType =
  val length = track.length
  block.also:
    track.supplement(track.length - length, transform(using _))

transparent inline def tend(inline block: Exception ~> Exception): Any =
  ${Contingency.tend('block)}

extension [LambdaType[_]](inline tend: Tend[LambdaType])
  inline def within[ResultType](inline lambda: LambdaType[ResultType]): ResultType =
    ${Contingency.tendWithin[LambdaType, ResultType]('tend, 'lambda)}

transparent inline def mend[ResultType](inline block: Exception ~> ResultType): Any =
  ${Contingency.mend[ResultType]('block)}

extension [ResultType, LambdaType[_]](inline mend: Mend[ResultType, LambdaType])
  inline def within[ResultType2 >: ResultType](inline lambda: LambdaType[ResultType2])
          : ResultType2 =
    ${Contingency.mendWithin[LambdaType, ResultType2]('mend, 'lambda)}

transparent inline def track[FocusType](using DummyImplicit)[AccrualType <: Exception, ResultType]
   (accrual: AccrualType)
   (inline block: (focus:   Optional[FocusType],
                   accrual: AccrualType) ?=> Exception ~> AccrualType)
        : Any =
  ${Contingency.track[AccrualType, FocusType]('accrual, 'block)}

transparent inline def accrue[AccrualType <: Exception](accrual: AccrualType)[ResultType]
   (inline block: (accrual: AccrualType) ?=> Exception ~> AccrualType)
        : Any =
  ${Contingency.accrue[AccrualType]('accrual, 'block)}

extension [AccrualType <: Exception,  LambdaType[_]](inline accrue: Accrue[AccrualType, LambdaType])
  inline def within[ResultType](inline lambda: LambdaType[ResultType])
     (using tactic: Tactic[AccrualType], diagnostics: Diagnostics)
          : ResultType =
    ${Contingency.accrueWithin[AccrualType, LambdaType, ResultType]('accrue, 'lambda, 'tactic,
        'diagnostics)}

extension [AccrualType <: Exception,  LambdaType[_], FocusType]
   (inline track: Tracking[AccrualType, LambdaType, FocusType])
  inline def within[ResultType](inline lambda: Foci[FocusType] ?=> LambdaType[ResultType])
     (using tactic: Tactic[AccrualType], diagnostics: Diagnostics)
          : ResultType =
    ${Contingency.trackWithin[AccrualType, LambdaType, ResultType, FocusType]('track, 'lambda,
        'tactic, 'diagnostics)}

extension [ValueType](optional: Optional[ValueType])
  def lest[SuccessType, ErrorType <: Exception: Tactic](error: Diagnostics ?=> ErrorType)
          : ValueType =
    optional.or(abort(error))

  def dare[ErrorType <: Exception](using DummyImplicit)[SuccessType]
     (block: (Diagnostics, OptionalTactic[ErrorType, SuccessType]) ?=> CanThrow[Exception] ?=>
                  ValueType => SuccessType)
        : Optional[SuccessType] =

    try boundary: label ?=>
      optional.let(block(using Diagnostics.omit, OptionalTactic(label)))
    catch case error: Exception => Unset
