// P4 negative (design constraint): a `Mutable` class is Unscoped-classified, so it may
// not capture non-Unscoped capabilities — a Cursor-like class holding an arbitrary `load`
// thunk cannot extend Mutable. (The fix: ExclusiveCapability + Stateful, see the pos.)
//EXPECT: allowed capture set
import language.experimental.captureChecking
import language.experimental.separationChecking

import caps.Mutable

class Cursor[cap^](load: () ->{cap} Int) extends Mutable:
  private var total: Int = 0
  update def advance(): Int =
    val loaded = load()
    total += loaded
    loaded
