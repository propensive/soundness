import language.experimental.captureChecking
import scala.caps

trait Emit[-e <: Exception] extends caps.ExclusiveCapability
trait Tactic[-e <: Exception] extends Emit[e]

class EventA extends Exception
class ErrorB extends Exception

def emit(e: EventA)(using em: Emit[EventA]^): Unit = ()

// single-layer context result whose body captures the METHOD's own parameter
def g(em: Emit[EventA]^): (Tactic[ErrorB]^) ?=> Int =
  emit(EventA())(using em)
  42
