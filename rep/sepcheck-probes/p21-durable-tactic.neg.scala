// P21 (negative twin): the same body capturing a `Tactic`, which is shared but not durable, is
// rejected — the restriction that stops a task raising through a tactic whose stack is not its
// own.
//EXPECT: Reference `tactic` is not included in the allowed capture set
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
  task { () => tactic.abort("escaped") }           // a tactic: rejected

// The ambient sink still passes the level check from inside a synthesised thunk.
given ambient: Sink = new Sink { def log(message: String): Unit = () }
def lazily(sink: => Sink^): Unit = sink.log("z")
def ambientUse(): Unit = lazily(summon[Sink])

// And an ambient ThrowTactic is still level-exempt (not Durable).
given throwing: (ThrowTactic^) = ThrowTactic()
def ambientTactic(): Unit = (summon[ThrowTactic]).abort("ok")
