// P20: a finer classifier than `SharedCapability` for what a long-lived body may retain. A
// daemon runs until cancelled, so its body must not capture a stack-confined tactic — yet a
// tactic is itself a `SharedCapability`, so `only[SharedCapability]` would admit it. The
// question: does a user classifier `Durable extends SharedCapability, Classifier` let a body
// typed `?->{caps.any.only[Durable]}` capture a `Durable` monitor (accepted) while rejecting a
// merely-shared tactic?
//EXPECT: tactic
import language.experimental.captureChecking
import language.experimental.separationChecking

trait Durable extends caps.SharedCapability, caps.Classifier

trait Monitor extends Durable:
  def cancelled: Boolean

trait Tactic extends caps.SharedCapability:
  def abort(message: String): Nothing

def daemon(body: (monitor: Monitor^) ?->{caps.any.only[Durable]} Unit)(using monitor: Monitor^): Unit =
  body(using monitor)

def use(using monitor: Monitor^, tactic: Tactic^): Unit =
  daemon { if summon[Monitor^].cancelled then () }   // captures only the monitor: fine
  daemon { tactic.abort("escaped") }                 // captures the tactic: must be rejected
