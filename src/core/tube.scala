package tube.terminal

import soundness.*

import executives.direct
import unhandledErrors.stackTrace
import parameterInterpretation.posix
import stdioSources.virtualMachine.ansi
import threadModels.platform

@main
def app(): Unit = cli:
  Out.println(e"$Bold($Italic(Hello world))")
  Out.println(arguments.inspect)
  Exit.Ok
