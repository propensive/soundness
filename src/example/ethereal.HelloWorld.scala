package example

import soundness.*

import executives.direct
import unhandledErrors.stackTrace
import parameterInterpretation.posix
import threadModels.virtual

@main
def run(args: Text*): Unit = cliService:
  Out.println(t"Hello world")
  ExitStatus.Ok


@main
def run2(): Unit = println("Testing")
