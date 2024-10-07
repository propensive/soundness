package tube.terminal

import soundness.*

import executives.direct
import unhandledErrors.stackTrace
import parameterInterpretation.posix

@main
def app(): Unit = application(Nil):
  Out.println(t"Hello world")
  Exit.Ok
