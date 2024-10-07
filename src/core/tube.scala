package tube.terminal

import soundness.*

import executives.completions
import unhandledErrors.stackTrace
import parameterInterpretation.posix
import threadModels.platform

@main
def app(): Unit = cli:
  Out.println(e"$Bold($Italic(Hello world))")
  Out.println(arguments.inspect)
  execute:
    Exit.Ok
