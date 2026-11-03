// P22 (negative): what exclusion rejects — a `Tactic` (Control), a bare `SharedCapability`
// (Control's ancestor, so it could hold one), an unclassified lambda, and an outer `CanThrow`.
//FLAGS: -language:experimental.saferExceptions
//EXPECT: Reference `tactic` is not included
//EXPECT: Reference `bare` is not included
//EXPECT: Reference `proc` is not included
//EXPECT: Reference `canThrow\$1` is not included
import language.experimental.captureChecking
import language.experimental.separationChecking
import scala.caps

trait Durable extends caps.SharedCapability, caps.Classifier
trait Tactic[-E] extends caps.Control:
  def abort(error: E): Nothing
trait Bare extends caps.SharedCapability:
  def ping(): Unit
class Boom extends Exception

def run(body: () ->{caps.any.except[caps.Control]} Unit): Unit = body()

def use(tactic: Tactic[String]^, bare: Bare^, proc: () => Unit): Unit =
  run(() => tactic.abort("escaped"))           // Control
  run(() => bare.ping())                       // bare SharedCapability
  run(() => proc())                            // unclassified
  try run(() => throw Boom())                  // the try's CanThrow is Control
  catch case e: Boom => ()
