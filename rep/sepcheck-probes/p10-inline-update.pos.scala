// P10: inline update methods mutating private state — the Cursor hot-path shape
// (`inline def advance()` bumping a private var, exclusive concrete-array fields written
// in update methods, exclusive extension receivers calling update members). REQUIRES the
// `inlineupdate` fork fix (AccessProxies propagates the Mutable flag to accessors for
// update methods and mutable-field setters); stock compilers fail with "access is in
// method Cur$$inline$pos_=, which is not an update method".
import language.experimental.captureChecking
import language.experimental.separationChecking

import caps.{ExclusiveCapability, Stateful}

class Cur extends ExclusiveCapability, Stateful:
  private var pos: Int = 0
  private var arr: Array[Long]^ = new Array[Long](4)
  inline update def advance(): Unit = pos += 1
  update def refill(): Int = { pos = 0; arr(0) = 1L; 1 }
  def value: Int = pos

  // the delegation shape: an inline wrapper whose call-forwarder accessor must also
  // count as an update method
  private update def bump0(): Unit = pos += 1
  inline update def bump(): Unit = bump0()

extension (c: Cur^)
  def stepped: Int = { c.advance(); c.value }
  inline def fast: Int = { c.bump(); c.value }

def drive(): Int =
  val c = Cur()
  c.refill()
  c.stepped + c.fast

def readonly(c: Cur): Int = c.value
