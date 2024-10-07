package tube.terminal

import soundness.*

import executives.completions
import unhandledErrors.stackTrace
import parameterInterpretation.posix
import threadModels.platform
import workingDirectories.daemonClient
import errorDiagnostics.stackTraces
import logging.silent
import strategies.throwUnsafely

val About = Subcommand(t"about", e"find out about the $Underline(tube) tool")
val Install = Subcommand(t"install", e"[re]install the tab-completions")

@main
def app(): Unit = cli:
  arguments match
    case About() :: _ => execute:
      Out.println(e"About this software")
      Exit.Ok

    case Install() :: _ => execute:
      Out.println(TabCompletions.install().communicate)
      Exit.Ok

    case _ => execute:
      Out.println(e"$Bold(Unrecognized command!)")
      Exit.Fail(1)
