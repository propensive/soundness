// P21 (negative twin): the same body capturing a scoped `Tactic` — a `caps.Control` capability,
// which `Durable` is not — is rejected: the restriction that stops a task raising through a
// tactic whose stack is not its own.
//EXPECT: Reference `tactic` is not included in the allowed capture set
import language.experimental.captureChecking
import language.experimental.separationChecking

trait Durable extends caps.SharedCapability, caps.Classifier

trait Monitor extends Durable:
  def cancelled: Boolean

trait Tactic extends caps.Control:
  def abort(message: String): Nothing

def task(body: () ->{caps.any.only[Durable]} Unit): Unit = body()

def use(monitor: Monitor^, tactic: Tactic^): Unit =
  task { () => if monitor.cancelled then () }     // durable: fine
  task { () => tactic.abort("escaped") }           // a tactic: rejected
