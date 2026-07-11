// P13 (neg twin 2): a purity-bounded raise rejects a capability-carrying
// exception type even when the hierarchy is not Pure — the entry-point
// requirement stands on its own.
//EXPECT: (does not conform|bounds|Pure)
import language.experimental.captureChecking

class Stream extends caps.ExclusiveCapability, caps.Stateful:
  private var n: Int = 0
  update def refill(): Int = { n += 1; n }

class LeakyException(val stream: Stream^) extends Exception("leak")

trait Tactic[-error <: Exception]:
  def abort(error: error): Nothing

def raise[error <: Exception & caps.Pure](error: error)(using tactic: Tactic[error]): Nothing =
  tactic.abort(error)

def use(stream: Stream^)(using Tactic[Exception & caps.Pure]): Int =
  raise(LeakyException(stream))
