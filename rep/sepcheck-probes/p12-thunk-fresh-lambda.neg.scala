// P12 (neg twin): a lambda whose body mints a fresh SCOPED capability cannot have
// the function type `() => Stream^` — the closure's per-call fresh is level-bound to
// the closure and is not visible from the binder's existential result capture. This
// is the level discipline for scoped capabilities, not a bug: an Unscoped (Mutable)
// class compiles in this position. The named-SAM shape (.pos twin) is the fix.
//EXPECT: cannot flow into capture set
//EXPECT: not visible from
import language.experimental.captureChecking

class Stream extends caps.ExclusiveCapability, caps.Stateful:
  private var n: Int = 0
  update def refill(): Int = { n += 1; n }

def freshStream(): Stream^ = new Stream

def request(flag: Boolean, body: () => Stream^): Int =
  def loop(fn: () => Stream^, remaining: Int): Int =
    if remaining <= 0 then 0 else
      val nextBody: () => Stream^ =
        if flag then fn else () => freshStream()
      loop(nextBody, remaining - 1)
  loop(body, 3)
