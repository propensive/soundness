// P13: a Pure error hierarchy. `Error extends caps.Pure` makes every subtype's
// values pure, so an exception can never carry a live capability out of a scope
// through `throw`/`raise` — the one channel capture checking cannot see.
// Verifies: (a) a Pure error subtype with pure fields compiles and is raisable
// through a purity-bounded entry point; (b) generic payloads of pure types work.
import language.experimental.captureChecking

transparent abstract class Error(val message: String) extends Exception(message), caps.Pure

class ParseError(line: Int, char: Char) extends Error(s"parse error at $line")
class AggregateError[error <: Error](val errors: List[error])
extends Error(s"${errors.length} errors")

trait Tactic[-error <: Exception]:
  def abort(error: error): Nothing

def raise[error <: Exception & caps.Pure](error: error)(using tactic: Tactic[error]): Nothing =
  tactic.abort(error)

def use(using Tactic[ParseError], Tactic[AggregateError[ParseError]]): Int =
  if math.abs(-1) > 0 then raise(ParseError(1, 'x'))
  else raise(AggregateError(List(ParseError(2, 'y'))))
