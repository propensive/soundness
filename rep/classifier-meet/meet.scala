//> using scala 3.9.1-dev-p19
//> using options -language:experimental.captureChecking -language:experimental.separationChecking
// A declared classifier meet: `DurableUnscoped` is a classifier deriving from two unrelated
// classifiers, `Durable` (shared) and `caps.SharedUnscoped` (shared and level-exempt). The
// least-classifier fold already picks it as the classifier of `Sink`; only the structural check
// in `Setup.checkClassifiedInheritance` rejects it ("inherits two unrelated classifier traits").
// With the meet accepted, a durable holder (`Loop`) may retain a sink, and a task body typed
// `only[Durable]` admits sinks and monitors and still rejects a tactic.
import language.experimental.captureChecking
import language.experimental.separationChecking

trait Durable extends caps.SharedCapability, caps.Classifier
trait DurableUnscoped extends Durable, caps.SharedUnscoped, caps.Classifier

trait Sink extends DurableUnscoped:
  def log(message: String): Unit

trait Monitor extends Durable:
  def cancelled: Boolean

class Loop(iteration: () ->{caps.any.only[Durable]} Unit) extends Durable:
  def run(): Unit = iteration()

def task(body: () ->{caps.any.only[Durable]} Unit): Unit = body()

def use(monitor: Monitor^, sink: Sink^): Unit =
  task { () => if monitor.cancelled then () }
  task { () => sink.log("x"); monitor.cancelled }
  Loop(() => { sink.log("tick"); if monitor.cancelled then () }).run()

// The ambient sink is level-exempt: summoned inside a synthesised thunk, it still checks.
given ambient: Sink = new Sink { def log(message: String): Unit = () }
def lazily(sink: => Sink^): Unit = sink.log("z")
def ambientUse(): Unit = lazily(summon[Sink])
