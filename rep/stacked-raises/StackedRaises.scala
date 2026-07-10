import language.experimental.captureChecking
import scala.caps

trait Emit[-e <: Exception] extends caps.ExclusiveCapability
trait Tactic[-e <: Exception] extends Emit[e]

infix type raises[s, e <: Exception] = Tactic[e]^ ?=> s
infix type logs[s, e <: Exception] = Emit[e]^ ?=> s

class EventA extends Exception
class ErrorB extends Exception

def emit(e: EventA)(using em: Emit[EventA]^): Unit = ()
def raise(e: ErrorB)(using t: Tactic[ErrorB]^): Unit = ()

// literal arrows
def literal(): (Emit[EventA]^) ?=> (Tactic[ErrorB]^) ?=> Int =
  emit(EventA())
  raise(ErrorB())
  42

// via aliases
def aliased(): Int raises ErrorB logs EventA =
  emit(EventA())
  raise(ErrorB())
  42
