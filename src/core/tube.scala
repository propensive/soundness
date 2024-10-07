package tube.terminal

import soundness.*

import executives.direct
import unhandledErrors.stackTrace
import parameterInterpretation.posix
import stdioSources.virtualMachine.ansi

@main
def app(): Unit = application(Nil):
  Out.println(e"$Bold($Italic(Hello world))")
  Exit.Ok
