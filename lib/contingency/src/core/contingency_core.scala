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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package contingency

import language.experimental.pureFunctions

import scala.quoted.*

import anticipation.*
import fulminate.*
import prepositional.*
import proscenium.*
import vacuous.*

package strategies:
  given throwUnsafely: [success] => ThrowTactic[Exception, success] =
    ThrowTactic()(using unsafeExceptions.canThrowAny)

  given throwSafely: [error <: Exception: CanThrow, success] => ThrowTactic[error, success] =
    ThrowTactic()


  given mitigation: [error <: Exception: Tactic, error2 <: Exception: Mitigable to error]
  =>  Tactic[error2] =

    error.contramap(error2.mitigate(_))


  given fatalErrors: [exception <: Exception: Fatal] => Tactic[exception]:
    given diagnostics: Diagnostics = errorDiagnostics.stackTraces

    def record(error: Diagnostics ?=> exception): Unit = exception.status(error).terminate()
    def abort(error: Diagnostics ?=> exception): Nothing = exception.status(error).terminate()
    def certify(): Unit = ()

  given uncheckedErrors: [error <: Exception] => (erased error is Unchecked) => Tactic[error]:
    given diagnostics: Diagnostics = errorDiagnostics.stackTraces
    given canThrow: CanThrow[Exception] = unsafeExceptions.canThrowAny

    def record(error: Diagnostics ?=> error): Unit = throw error
    def abort(error: Diagnostics ?=> error): Nothing = throw error
    def certify(): Unit = ()

private given realm: Realm = realm"contingency"

def certify[error <: Exception: Tactic](): Unit = error.certify()


def raise[success, exception <: Exception: Recoverable to success]
  ( error: Diagnostics ?=> exception )
  ( using tactic: Tactic[exception] )
:   success =

  tactic.record(error)
  exception.recover(error(using tactic.diagnostics))


def abort[success, exception <: Exception: Tactic](error: Diagnostics ?=> exception): Nothing =
  exception.abort(error)


def safely[error <: Exception](using erased Void)[success]
  ( block: (Diagnostics, OptionalTactic[error, success]) ?=> CanThrow[Exception] ?=> success )
:   Optional[success] =

  try boundary: label ?=>
    block(using Diagnostics.omit, OptionalTactic(label))
  catch case error: Exception => Unset


def unsafely[error <: Exception](using erased Void)[success]
  ( block: (erased Unsafe) ?=> ThrowTactic[error, success] ?=> CanThrow[Exception] ?=> success )
:   success =

  boundary: label ?=>
    import unsafeExceptions.canThrowAny
    block(using Unsafe)(using ThrowTactic())


def throwErrors[error <: Exception](using CanThrow[error])[success]
  ( block: ThrowTactic[error, success] ?=> success )
:   success =

  block(using ThrowTactic())


def capture[error <: Exception: ClassTag](using erased Void)[success]
  ( block: Tactic[error] ?=> success )
  ( using Tactic[ExpectationError[success]], Diagnostics )
:   error =

  try
    val value: Either[error, success] =
      boundary: label ?=> Right(block(using EitherTactic(label)))

    value match
      case Left(error)  => error
      case Right(value) => abort(ExpectationError(value))

  catch
    case exception: `error`   => exception
    case exception: Throwable => throw exception


def attempt[error <: Exception](using erased Void)[success]
  ( block: AttemptTactic[error, success] ?=> success )
  ( using Diagnostics )
:   Attempt[success, error] =

  boundary: label ?=>
    Attempt.Success(block(using AttemptTactic(label)))


def amalgamate[error <: Exception](using erased Void)[success]
  ( block: AmalgamateTactic[error, success] ?=> success )
  ( using Diagnostics )
:   success | error =

  boundary: label ?=>
    block(using AmalgamateTactic(label))


def abortive[error <: Error](using Quotes, Realm)[success]
  ( block: Diagnostics ?=> HaltTactic[error, success] ?=> success )
:   success =

  given haltTactic: HaltTactic[error, success]()
  given diagnostics: Diagnostics = Diagnostics.omit
  block


infix type raises [success, error <: Exception] = Tactic[error] ?=> success

infix type raising[success, errors] = errors match
  case EmptyTuple.type => success
  case left *: right   => Tactic[left] ?=> raising[success, right]
  case _               => Tactic[errors] ?=> success

infix type mitigates [error <: Exception, error2 <: Exception] =
  error2 is Mitigable to error

infix type tracks [result, focus] = Foci[focus] ?=> result


inline def focus[focus, result](using foci: Foci[focus])
  ( transform: (prior: Optional[focus]) ?=> focus )
  ( block: => result )
:   result =

  val length = foci.length
  try block finally foci.supplement(foci.length - length, transform(using _))


transparent inline def mitigate(inline block: Exception ~> Exception): Mitigation[?] =
  ${contingency.internal.mitigate('block)}

extension [lambda[_]](inline mitigation: Mitigation[lambda])
  inline def within[result](inline lambda: lambda[result]): result =
    ${contingency.internal.mitigateWithin[lambda, result]('mitigation, 'lambda)}

transparent inline def recover[result](inline block: Exception ~> result): Recovery[result, ?] =
  ${contingency.internal.recover[result]('block)}

extension [result, lambda[_]](inline recovery: Recovery[result, lambda])
  inline def within[result2 >: result](inline lambda: lambda[result2]): result2 =
    ${contingency.internal.recoverWithin[lambda, result2]('recovery, 'lambda)}


transparent inline def track[focus](using erased Void)[accrual <: Exception](accrual: accrual)
  ( inline block: (focus: Optional[focus], accrual: accrual) ?=> Exception ~> accrual )
:   Tracking[accrual, ?, focus] =

  ${contingency.internal.track[accrual, focus]('accrual, 'block)}


transparent inline def validate[focus](using erased Void)[accrual](accrual: accrual)
  ( inline block: (focus: Optional[focus], accrual: accrual) ?=> Exception ~> accrual )
:   Any =

  ${contingency.internal.validate[accrual, focus]('accrual, 'block)}


transparent inline def accrue[accrual <: Exception](accrual: accrual)[result]
  ( inline block: (accrual: accrual) ?=> Exception ~> accrual )
:   Any =

  ${contingency.internal.accrue[accrual]('accrual, 'block)}


extension [accrual <: Exception,  lambda[_]](inline accrue: Accrue[accrual, lambda])
  inline def within[result](inline lambda: lambda[result])
    ( using tactic: Tactic[accrual], diagnostics: Diagnostics )
  :   result =

    ${contingency.internal.accrueWithin[accrual, lambda, result]('accrue, 'lambda, 'tactic, 'diagnostics)}


extension [accrual <: Exception,  lambda[_], focus](inline track: Tracking[accrual, lambda, focus])
  inline def within[result](inline lambda: Foci[focus] ?=> lambda[result])
    ( using tactic: Tactic[accrual], diagnostics: Diagnostics )
  :   result =

    $ {
        contingency.internal.trackWithin[accrual, lambda, result, focus]
          ( 'track, 'lambda, 'tactic, 'diagnostics )
      }


extension [accrual <: Exception,  lambda[_], focus]
  ( inline validate: Validate[accrual, lambda, focus] )
  inline def within(inline lambda: Foci[focus] ?=> lambda[Any])(using diagnostics: Diagnostics)
  :   accrual =

    ${contingency.internal.validateWithin[accrual, lambda, focus]('validate, 'lambda, 'diagnostics)}


extension [element](sequence: Iterable[element])
  transparent inline def survive[result](using erased Void)[error <: Exception]
    ( lambda
      : (OptionalTactic[error, result], Diagnostics, CanThrow[Exception])
          ?=> element => result )
  :   Iterable[result] =

    sequence.map { element => safely(lambda(element)) }.compact


extension [value](optional: Optional[value])
  def lest[success, error <: Exception: Tactic](error: Diagnostics ?=> error): value =
    optional.or(abort(error))


  def dare[error <: Exception](using erased Void)[success]
    ( block: (Diagnostics, OptionalTactic[error, success]) ?=> CanThrow[Exception] ?=>
                value => success )
  :   Optional[success] =

    try boundary: label ?=>
      optional.let(block(using Diagnostics.omit, OptionalTactic(label)))
    catch case error: Exception => Unset
