// contingency.core analogue — compiled WITH capture checking (as the real module is).
package repcontingency

import language.experimental.captureChecking
import language.experimental.erasedDefinitions
import scala.util.boundary
import scala.caps

trait Emit[-error <: Exception] extends caps.ExclusiveCapability

trait Tactic[-error <: Exception] extends Emit[error]:
  def abort(error: error): Nothing

class EitherTactic[error <: Exception, success]
  (label: boundary.Label[Either[error, success]])
extends Tactic[error]:
  def abort(error: error): Nothing = boundary.break(Left(error))(using label)

infix type raises[success, error <: Exception] = Tactic[error]^ ?=> success

def capture[error <: Exception](using erased void: DummyImplicit)[success]
  (block: Tactic[error]^ ?=> success)
:   error =

  val value: Either[error, success] =
    boundary: label ?=>
      Right(block(using EitherTactic(label)))

  value match
    case Left(error) => error
    case Right(_)    => throw new RuntimeException("unexpected success")
