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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import scala.language.experimental.pureFunctions

import scala.language.unsafeNulls
import scala.quoted.*
import scala.util.boundary

import denominative.*
import fulminate.*
import prepositional.*
import vacuous.*

package strategies:
  given throwUnsafely: [success] => (ThrowTactic[Hazard, success]^) =
    ThrowTactic()

  given throwSafely: [error <: Hazard: CanThrow, success] => (ThrowTactic[error, success]^) =
    ThrowTactic()


  given mitigation: [error <: Hazard, error2 <: Hazard: Mitigable to error]
  =>  (tactic: Tactic[error]^)
  =>  ( Tactic[error2]^ ) =

    tactic.contramap(error2.mitigate(_))


  // Like `ThrowTactic`, these ambient strategies are `caps.Unscoped`: they capture no scoped
  // capability (they throw or terminate in place), so a use-site instantiation may flow into the
  // `raises` existential of a non-inline method result.
  given fatalErrors: [exception <: Hazard: Fatal] => (FatalTactic[exception]^) = FatalTactic()

  given uncheckedErrors: [error <: Hazard] => (erased unchecked: error is Unchecked)
  =>  ( UncheckedTactic[error]^ ) =

    UncheckedTactic()

def certify[error <: Hazard](using tactic: Tactic[error]^)(): Unit = tactic.certify()


def raise[exception <: Hazard]
  ( error: Diagnostics ?=> exception )
  ( using emitter: Emit[exception]^ )
:   Unit =

  emitter.record(error)


def abort[success, exception <: Hazard](error: Diagnostics ?=> exception)
  ( using tactic: Tactic[exception]^ )
:   Nothing =

  tactic.abort(error)


// The block's `Diagnostics`, `OptionalTactic` and `CanThrow` are provided in a single context-
// function layer rather than two (`… ?=> CanThrow ?=> success`). Under capture checking a second
// layer would be a `CanThrow ?=> success` arrow that closes over the scoped `OptionalTactic`, and
// that arrow's capture set cannot be reconciled with the block's expected type (the tactic is
// created inside `safely`, after the block value). Collapsing the layers makes the block's result
// the plain `success` value, which retains nothing.
def safely[error <: Hazard](using erased void: Void)[success]
  ( block: (Diagnostics, OptionalTactic[error, success]^, CanThrow[Exception]) ?=> success )
:   Optional[success] =

  try boundary: label ?=>
    block(using Diagnostics.omit, OptionalTactic(label), summon[CanThrow[Exception]])
  catch case error: Exception => Unset


def unsafely[error <: Hazard](using erased void: Void)[success]
  ( block: (Unsafe, ThrowTactic[error, success]^, CanThrow[Exception]) ?=> success )
:   success =

  // `Unsafe`, the injected `ThrowTactic` and the block's `CanThrow` are provided in a SINGLE
  // context-function layer (see `safely`: a separate inner arrow would close over the scoped
  // tactic bound by the outer layer, and under capture checking that arrow's capture cannot be
  // reconciled with the block's expected type — a curried `Unsafe ?=> … ?=> success` block was
  // rejected wherever the argument was a `raises`-typed expression). The `CanThrow[Exception]`
  // comes from the enclosing `try`, not the universal `canThrowAny`, so it leaves no capture in
  // `unsafely`'s type; the `catch` rethrows, preserving the original fire-and-forget propagation
  // semantics.
  boundary: label ?=>
    try block(using Unsafe, ThrowTactic(), summon[CanThrow[Exception]])
    catch case error: Exception =>
      import unsafeExceptions.canThrowAny
      throw error


def throwErrors[error <: Hazard](using CanThrow[error])[success]
  ( block: ThrowTactic[error, success]^ ?=> success )
:   success =

  block(using ThrowTactic())


def capture[error <: Hazard: ClassTag](using erased void: Void)[success]
  ( block: Tactic[error]^ ?=> success )
  ( using Tactic[ExpectationError[success]]^, Diagnostics )
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


def attempt[error <: Hazard](using erased void: Void)[success]
  ( block: AttemptTactic[error, success]^ ?=> success )
  ( using Diagnostics )
:   Attempt[success, error] =

  boundary: label ?=>
    Attempt.Success(block(using AttemptTactic(label)))


def amalgamate[error <: Hazard](using erased void: Void)[success]
  ( block: AmalgamateTactic[error, success]^ ?=> success )
  ( using Diagnostics )
:   success | error =

  boundary: label ?=>
    block(using AmalgamateTactic(label))


def abortive[error <: Error](using Quotes)[success]
  ( block: Diagnostics ?=> HaltTactic[error, success]^ ?=> success )
:   success =

  given haltTactic: HaltTactic[error, success]()
  given diagnostics: Diagnostics = Diagnostics.omit
  block


infix type raises [success, error <: Hazard] = Tactic[error]^ ?=> success

// `raising` cannot replace `raises` until a Scala 3 compiler bug is fixed: when this match type
// appears in a method's return type and reduces to a context function, PostTyper attaches
// `@ContextResultCount` (see `ContextFunctionResults.annotateContextResults`), but TASTy stores
// the unreduced `AppliedType`. At a cross-module use site, erasure runs
// `ContextFunctionResults.integrateContextResults` (lines ~70-78) whose `tp.dealias match` has
// no default case. `dealias` does not reduce match types during erasure because
// `TypeApplications.appliedTo` short-circuits with `if (args.nil || ctx.erasedTypes) self`,
// so the unreduced `AppliedType` falls through every case and crashes with a `MatchError`.
// Fix candidates: give the match a default case, or use `tryNormalize`/`superTypeNormalized`
// in place of `dealias`.
infix type raising[success, errors] = errors match
  case EmptyTuple.type => success
  case left *: right   => Tactic[left] ?=> raising[success, right]
  case _               => Tactic[errors] ?=> success

infix type mitigates [error <: Hazard, error2 <: Hazard] =
  error2 is Mitigable to error

infix type tracks [result, focus] = Foci[focus] ?=> result


inline def focus[focus, result](using foci: Foci[focus])
  ( transform: (Optional[focus] aka "prior") ?=> focus )
  ( block: => result )
:   result =

  val length = foci.length

  try block
  finally foci.supplement(foci.length - length, prior => transform(using prior.aka["prior"]))


inline def accrual[value](using value: value aka "accrual"): value = value()


transparent inline def track[focus](using erased void: Void)[accrual <: Hazard](accrual: accrual)
  ( inline block: (Optional[focus] aka "prior", accrual aka "accrual") ?=> Exception ~> accrual )
:   Tracking[accrual, ?, focus] =

  ${contingency.internal.track[accrual, focus]('accrual, 'block)}


transparent inline def validate[focus](using erased void: Void)[accrual](accrual: accrual)
  ( inline block: (Optional[focus] aka "prior", accrual aka "accrual") ?=> Exception ~> accrual )
:   Any =

  ${contingency.internal.validate[accrual, focus]('accrual, 'block)}


extension [element](sequence: Iterable[element])
  transparent inline def survive[result](using erased void: Void)[error <: Hazard]
    ( lambda
    : (OptionalTactic[error, result], Diagnostics, CanThrow[Exception]) ?=> element => result )
  :   Iterable[result] =

    sequence.map { element => safely(lambda(element)) }.compact


extension [value](optional: Optional[value])
  def lest[success, error <: Hazard](error: Diagnostics ?=> error)(using Tactic[error]^): value =
    optional.or(abort(error))


  def dare[error <: Hazard](using erased void: Void)[success]
    ( block
    : (Diagnostics, OptionalTactic[error, success]^, CanThrow[Exception]) ?=> value => success )
  :   Optional[success] =

    try boundary: label ?=>
      optional.let(block(using Diagnostics.omit, OptionalTactic(label), summon[CanThrow[Exception]]))
    catch case error: Exception => Unset


def defer[result, error <: Hazard](body: Tactic[error]^ ?=> result)
:   Deferred[result, error]^{body} =

  Deferred(body)


transparent inline def recover(inline handler: PartialFunction[Exception, Any]): Recovery[?] =
  ${contingency.internal.recoverBuild('handler)}


transparent inline def mitigate(inline handler: PartialFunction[Exception, Any]): Mitigation[?] =
  ${contingency.internal.mitigateBuild('handler)}


transparent inline def accrue[accrual <: Hazard](initial: accrual)
  ( combine: (accrual, Exception) => accrual )
  ( inline handler: PartialFunction[Exception, Any] )
:   Accrual[accrual, ?] =

  ${contingency.internal.accrueBuild[accrual]('initial, 'combine, 'handler)}


transparent inline def handle(inline handler: PartialFunction[Exception, Unit]): Handler[?] =
  ${contingency.internal.handleBuild('handler)}
