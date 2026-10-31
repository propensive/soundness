// P22 (negative twin): the reason retained bodies stay `only[Durable]`. Exclusion is not a
// classification: a `Durable` worker retaining a probate typed `Probate^{any.except[Control]}`
// holds a field that does not conform to its own classifier, and the field is rejected as
// unclassified (parasite's `Worker(…, probate: SharedProbate)` was the first to fail). The check
// runs for a class whose self type is capturing (`self: Worker^ =>`, as every Soundness
// capability class declares); a retained body is checked the same way.
//EXPECT: is unclassified
import language.experimental.captureChecking
import language.experimental.separationChecking
import scala.caps

trait Durable extends caps.SharedCapability, caps.Classifier
trait Probate:
  def cleanup(): Unit

class Worker(probate: Probate^{caps.any.except[caps.Control]}) extends Durable:
  self: Worker^ =>
  def go(): Unit = probate.cleanup()
