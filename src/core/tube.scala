package tube.terminal

import soundness.*

import executives.completions
import unhandledErrors.stackTrace
import parameterInterpretation.posix
import threadModels.platform

val About = Subcommand(t"about")
val Install = Subcommand(t"install")

@main
def app(): Unit = cli:
  arguments match
    case About() :: _ => execute:
      Out.println(e"About this software")
      Exit.Ok

    case Install() :: _ => execute:
      TabCompletions.install()
      Exit.Ok

    case _ => execute:
      Out.println(e"$Bold(Unrecognized command!)")
      Exit.Fail(1)
