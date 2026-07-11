// P11 negative (BLOCKER, fork-fix #12 candidate): a class with a `cap^` capture-set
// parameter (the Cursor shape) cannot have a fresh instance assigned into an exclusive
// field — "fresh in method reset is not visible from any in variable buf" — regardless of
// source shape: plain factory result, ascribed local, consume adapter, and DIRECT `new`
// all fail, and @untrackedCaptures does not relax it. The identical class WITHOUT the
// cap parameter admits all of these (see the p12x shapes referenced in DECISIONS.md).
// Also visible here: the dependent-param refinement (`{val load: () -> Int}`) riding
// along in the inferred type. Blocks the jacinta pooled-Parser conversion at exactly its
// three cursor-reset sites.
//EXPECT: cannot flow into capture set
import language.experimental.captureChecking
import language.experimental.separationChecking

import caps.{ExclusiveCapability, Stateful}

final class Buf[data, cap^](load: () ->{cap} Int) extends ExclusiveCapability, Stateful:
  private var n: Int = 0
  update def add(): Unit = n += 1

object Buf:
  def make[data](size: Int): Buf[data, {}]^ =
    val made: Buf[data, {}]^ = new Buf[data, {}](() => size)
    made

final class Holder extends ExclusiveCapability, Stateful:
  private var buf: Buf[String, {}]^ = Buf.make[String](4)

  update def reset(): Unit =
    val fresh = Buf.make[String](4)
    buf = fresh

  update def resetDirect(): Unit =
    val fresh = new Buf[String, {}](() => 4)
    buf = fresh
