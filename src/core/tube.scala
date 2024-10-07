package tube.terminal

import soundness.*

import executives.completions
import unhandledErrors.stackTrace
import parameterInterpretation.posix
import threadModels.platform

val About = Subcommand(t"about")

@main
def app(): Unit = cli:
  arguments match
    case About() :: _ => execute:
      Out.println(e"About this software")
      Exit.Ok
