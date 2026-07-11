// P12: promising a fresh SCOPED stateful result per call.
// A lambda may not mint a fresh scoped capability as its result (see the .neg twin:
// the closure's fresh is not visible from the function type's result capture at the
// binder's level), but a METHOD may — its result is re-leveled at each call site.
// So a named SAM trait admits everything the function type `() => Stream^` rejects,
// while lambda construction sites still compile via SAM conversion.
// This is the `zephyrine.Spring` / `Postable.Streamer` shape.
import language.experimental.captureChecking

class Stream extends caps.ExclusiveCapability, caps.Stateful:
  private var n: Int = 0
  update def refill(): Int = { n += 1; n }

def freshStream(): Stream^ = new Stream

trait BodySource:
  def apply(): Stream^

object Probe:
  // SAM conversion from a lambda
  val sam: BodySource = () => freshStream()

  // anonymous class
  val anon: BodySource = new BodySource:
    def apply(): Stream^ = freshStream()

  // each call yields a usable exclusive stream, at the caller's level
  def use(flag: Boolean, body: BodySource): Int =
    def loop(fn: BodySource, remaining: Int): Int =
      if remaining <= 0 then 0 else
        val next: BodySource = if flag then fn else sam
        val s: Stream^ = next()
        s.refill() + loop(next, remaining - 1)
    loop(body, 3)
