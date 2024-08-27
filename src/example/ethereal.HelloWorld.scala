package example

import soundness.*

import executives.direct
import unhandledErrors.stackTrace
import parameterInterpretation.posix
import threadModels.platform

@main
def run(): Unit = cliService:
  ExitStatus.Ok

@main
def run2(): Unit = println("Testing")
