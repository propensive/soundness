// P21: durable capabilities. A task's body is retained by a worker on another thread — itself a
// shared capability classified `Durable` — so what the body captures must be classified
// `Durable` too: a monitor, a connection, a logger, a log sink. An ambient sink given as a
// *value* is a static capability, visible from every level, so it passes the level check from
// inside a synthesised thunk with no fork classifier (the `DurableUnscoped` meet of `Durable`
// and proscala's `SharedUnscoped` is gone since #2328). The ambient `ThrowTactic` is pure (p17).
import language.experimental.captureChecking
import language.experimental.separationChecking

trait Durable extends caps.SharedCapability, caps.Classifier

trait Sink extends Durable:
  def log(message: String): Unit

trait Monitor extends Durable:
  def cancelled: Boolean

trait Tactic extends caps.Control:
  def abort(message: String): Nothing

class ThrowTactic extends Tactic:
  def abort(message: String): Nothing = throw Exception(message)

def task(body: () ->{caps.any.only[Durable]} Unit): Unit = body()

def use(monitor: Monitor^, sink: Sink^): Unit =
  task { () => if monitor.cancelled then () }     // durable: fine
  task { () => sink.log("x") }                     // durable and static: fine
  task { () => sink.log("y"); monitor.cancelled }  // both: fine

// The ambient sink, a value, passes the level check from inside a synthesised thunk.
given ambient: Sink = new Sink { def log(message: String): Unit = () }
def lazily(sink: => Sink^): Unit = sink.log("z")
def ambientUse(): Unit = lazily(summon[Sink])

// And the pure ambient ThrowTactic does too.
given throwing: (ThrowTactic^{}) = caps.unsafe.unsafeAssumePure(ThrowTactic())
def lazyTactic(tactic: => ThrowTactic^): Unit = ()
def ambientTactic(): Unit = lazyTactic(summon[ThrowTactic])
