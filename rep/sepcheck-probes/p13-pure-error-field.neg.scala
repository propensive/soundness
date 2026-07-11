// P13 (neg twin 1): the load-bearing enforcement test. A Pure subclass may not
// hold a capability-typed field: if the compiler admitted this, `throw` would
// remain a capability-exfiltration channel and Error-purity would be mere
// documentation.
//EXPECT: (non-pure|impure|Pure|pure)
import language.experimental.captureChecking

class Stream extends caps.ExclusiveCapability, caps.Stateful:
  private var n: Int = 0
  update def refill(): Int = { n += 1; n }

transparent abstract class Error(val message: String) extends Exception(message), caps.Pure

class SneakyError(val stream: Stream^) extends Error("smuggling")
