/* Capture-checking regression cases for `ethereal.cli`. Uncomment a
   `def` to verify the leak is rejected. */
package ethereal

import language.experimental.captureChecking

import soundness.*

import backstops.silent
import executives.direct
import interpreters.posix
import threading.platform

object LeakTests:
  // The block in `cli` returns `executive.Return` (Exit for executives.direct),
  // a pure sum type, so the block can't *return* a leaking value directly.
  // Interesting leaks are side-channel: storing the service or a closure
  // that captures it in mutable state outside the cli scope.

  var leakedSvc: DaemonService[Text]^ = null.asInstanceOf[DaemonService[Text]^]
  var leakedFn: () => Unit = () => ()

  // 1. Assigning the service directly to an outer var. Rejected.
  // def leakDirect(): Unit = cli[Text]: (svc, _) ?=>
  //   leakedSvc = svc
  //   Exit.Ok

  // 2. Assigning a closure that captures the service to an outer var. Rejected.
  // def leakClosure(): Unit = cli[Text]: (svc, _) ?=>
  //   leakedFn = () => svc.broadcast(t"oops")
  //   Exit.Ok

  // 3. Smuggling via a case-class wrapper. Rejected.
  // case class Wrapper(action: () => Unit)
  // var leakedWrapper: Wrapper = Wrapper(() => ())
  // def leakWrapper(): Unit = cli[Text]: (svc, _) ?=>
  //   leakedWrapper = Wrapper(() => svc.broadcast(t"oops"))
  //   Exit.Ok
