// P22: classifier exclusion (Scala 3.10, scala/scala3#26383). `->{any.except[Control]}` is the
// type of a body that is merely RUN on another thread: it admits a capability known to be
// unrelated to `Control` — a `Durable` sibling, a static sink, an exclusive `Mutable` — and a
// lambda whose capture set is made of those. Compare p21: retained bodies cannot use it (neg).
import language.experimental.captureChecking
import language.experimental.separationChecking
import scala.caps

trait Durable extends caps.SharedCapability, caps.Classifier
trait Tactic[-E] extends caps.Control:
  def abort(error: E): Nothing
trait Monitor extends Durable:
  def cancelled: Boolean
trait Sink extends Durable:
  def log(s: String): Unit
object silent extends Sink:
  def log(s: String): Unit = ()
class Buffer extends caps.Mutable:
  update def write(): Unit = ()

def run(body: () ->{caps.any.except[caps.Control]} Unit): Unit = body()

def use(monitor: Monitor^, sink: Sink^, buffer: Buffer^): Unit =
  run(() => if monitor.cancelled then ())      // Durable
  run(() => sink.log("x"))                     // Durable
  run(() => silent.log("x"))                   // a static sink
  run(() => buffer.write())                    // exclusive: disjoint from Control
  val f: () ->{monitor} Unit = () => if monitor.cancelled then ()
  run(f)                                       // a capture set of Durable capabilities
