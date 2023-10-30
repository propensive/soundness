package exoskeleton

import perforate.*
import spectacular.*
import parasite.*
import profanity.*
import rudiments.*

@main
def fury(): Unit =
  import errorHandlers.throwUnsafely
  Daemon.listen:
    println(arguments.debug)
    execute:
      supervise:
        terminal:
          println(tty.mode.debug)
          ExitStatus.Ok
