package exoskeleton

import perforate.*
import spectacular.*
import parasite.*
import gossamer.*
import profanity.*
import rudiments.*
import turbulence.*

@main
def fury(): Unit =
  import parameterInterpretation.simple
  import errorHandlers.throwUnsafely
  Daemon.listen:
    parameters.headOption.foreach: arg =>
      arg.suggest(List(Suggestion(t"yes", t"Definitely so"), Suggestion(t"no", t"By no means")))
    
    parameters.lift(1).foreach: arg =>
      arg.suggest(List(Suggestion(t"1", t"one"), Suggestion(t"2", t"two"), Suggestion(t"3", t"three")))

    execute:
      Out.println(arguments.debug)
      supervise:
        terminal:
          println(tty.mode.debug)
          ExitStatus.Ok
