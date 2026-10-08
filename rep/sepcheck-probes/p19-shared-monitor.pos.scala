// P19 (pos): the same program with `Monitor` a `SharedCapability`: the join is accepted, and the
// handle still captures the supervisor, so it cannot leave the block. A task handle captures the supervisor it was spawned under, and
// joining it passes the same supervisor as a using-argument — `async(…)` then `task.await()`
// under one `supervise` block, the shape behind every `unsafeAssumeSeparate(task.await())` in
// zephyrine's tests (and, with the loan's `using Monitor` in place of `await`'s, every sealed
// `Shell.tmux()(Tmux.completions(…))` in exoskeleton's). With `Monitor` an
// `ExclusiveCapability`, as today, the receiver and the argument overlap.

import language.experimental.captureChecking
import language.experimental.separationChecking

trait Monitor extends caps.SharedCapability:
  def cancelled: Boolean

trait Task[result]:
  def await()(using monitor: Monitor^): result

def async[result](evaluate: => result)(using monitor: Monitor^): Task[result]^{evaluate, monitor} =
  new Task[result]:
    def await()(using monitor: Monitor^): result = evaluate

def supervise[result](block: (monitor: Monitor^) ?=> result): result =
  val monitor = new Monitor { def cancelled = false }
  block(using monitor)

@main def run(): Unit =
  val n = supervise:
    val task = async(21*2)
    task.await()
  println(n)
