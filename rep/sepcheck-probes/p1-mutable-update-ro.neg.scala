// P1 negative: calling an update method through a bare (implicitly read-only) reference
// must be rejected. This is the rule that makes `Stream` (bare) mean read-only stream.
//EXPECT: Cannot call update method
//EXPECT: read-only
import language.experimental.captureChecking
import language.experimental.separationChecking

import caps.Mutable

class Ref extends Mutable:
  private var value: Int = 0
  def get: Int = value
  update def set(x: Int): Unit = value = x

def readonly(r: Ref): Unit = r.set(1)
