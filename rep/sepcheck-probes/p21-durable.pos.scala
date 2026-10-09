// P21: durable capabilities. A task's body is retained by a worker on another thread, so it may
// capture only what is safe across a thread boundary: a `Durable` capability (a monitor, a
// connection, a logger), `DurableUnscoped` ones (a log sink, network access) included: those are
// the declared meet of `Durable` and `SharedUnscoped`, which proscala `classifiermeet` (from
// 3.9.1-dev-p20) accepts. Level exemption still comes from deriving `SharedUnscoped`, so the
// ambient sink and the ambient `ThrowTactic` pass the level check from inside a synthesised thunk.
import language.experimental.captureChecking
import language.experimental.separationChecking

trait Durable extends caps.SharedCapability, caps.Classifier
trait DurableUnscoped extends Durable, caps.SharedUnscoped, caps.Classifier

trait Sink extends DurableUnscoped:
  def log(message: String): Unit

trait Monitor extends Durable:
  def cancelled: Boolean

trait Tactic extends caps.SharedCapability:
  def abort(message: String): Nothing

class ThrowTactic extends Tactic, caps.SharedUnscoped:
  def abort(message: String): Nothing = throw Exception(message)

def task(body: () ->{caps.any.only[Durable]} Unit): Unit = body()

def use(monitor: Monitor^, sink: Sink^, tactic: Tactic^): Unit =
  task { () => if monitor.cancelled then () }     // durable: fine
  task { () => sink.log("x") }                     // durable and level-exempt: fine
  task { () => sink.log("y"); monitor.cancelled }  // both: fine

// The ambient sink still passes the level check from inside a synthesised thunk.
given ambient: Sink = new Sink { def log(message: String): Unit = () }
def lazily(sink: => Sink^): Unit = sink.log("z")
def ambientUse(): Unit = lazily(summon[Sink])

// And an ambient ThrowTactic is still level-exempt (not Durable).
given throwing: (ThrowTactic^) = ThrowTactic()
def ambientTactic(): Unit = (summon[ThrowTactic]).abort("ok")
