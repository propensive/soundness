/* Capture-checking regression cases for `interactive`. Each commented-out
   `def` is a leak that the compiler rejects today; uncomment to verify. */
package profanity

import language.experimental.captureChecking

import soundness.*

import strategies.throwUnsafely

object LeakTests:

  given BracketedPasteMode      = () => false
  given LuminosityDetection     = () => false
  given TerminalFocusDetection  = () => false
  given TerminalSizeDetection   = () => false

  // Capture checking catches these:

  // 1. Returning the terminal directly: rejected
  // def leakDirect(using Console, Monitor, Codicil): Terminal^ =
  //   interactive: terminal ?=>
  //     terminal

  // 2. Returning a closure that calls a method on terminal: rejected
  // def leakMethodClosure(using Console, Monitor, Codicil): () => Int =
  //   interactive: terminal ?=>
  //     () => terminal.knownColumns

  // 3. Returning a closure that uses the implicit Stdio derived from terminal:
  //    rejected (because Stdio^ leaks the capture)
  // def leakStdioClosure(using Console, Monitor, Codicil): () => Unit =
  //   interactive: terminal ?=>
  //     () => Out.println(t"after close")

  // 4. Returning a closure that goes through a `^{this}`-tagged field:
  //    rejected (after declaring `events: Spool[TerminalEvent]^{this}`).
  // def leakSpool(using Console, Monitor, Codicil): () => Unit =
  //   interactive: terminal ?=>
  //     () => terminal.events.stop()
