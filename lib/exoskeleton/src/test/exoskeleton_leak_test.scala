/* Capture-checking regression cases for `exoskeleton.execute`. Uncomment
   a `def` to verify the leak is rejected. */
package exoskeleton

import language.experimental.captureChecking

import soundness.*

object LeakTests:
  // The block in `execute` returns `Exit` (a pure sum type), so the
  // immediate return can't carry a leaking value. The interesting leaks
  // are side-channel: storing the invocation or a closure that captures
  // it in mutable state outside execute's scope.

  var leakedInv: Invocation^ = null.asInstanceOf[Invocation^]

  // 1. Assigning the invocation directly to an outer var. Rejected.
  // def leakDirect(using Cli): Execution = execute:
  //   leakedInv = summon[Invocation^]
  //   Exit.Ok
  //
  // Note: closures that capture the invocation only via pure-typed field
  // accesses (e.g. `() => inv.proceed`) aren't always caught — the field
  // value is pure so the inferred closure capture may be empty. The
  // direct-assignment case above is the reliable detection.
