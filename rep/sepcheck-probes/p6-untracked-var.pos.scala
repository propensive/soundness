// P6 positive: the @untrackedCaptures escape hatch lets a non-Stateful class keep a
// mutable field (the Phase 1 Conduit strategy before honest Stateful classification).
import language.experimental.captureChecking
import language.experimental.separationChecking

import caps.unsafe.untrackedCaptures

class Plain:
  @untrackedCaptures private var count: Int = 0
  def bump(): Unit = count += 1
