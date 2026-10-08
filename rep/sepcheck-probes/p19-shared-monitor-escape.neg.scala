// P19 (confinement twin): with `Monitor` a `SharedCapability` the join in
// p19-shared-monitor.pos is accepted — and the handle is still tracked, so it cannot leave the
// `supervise` block: shared classification removes the overlap check between two uses of one
// supervisor, not the capture that confines what was spawned under it.
//EXPECT: (cannot flow into|escapes|not visible|leak)
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
  val escaped: Task[Int] = supervise:
    async(21*2)
  println(escaped)
