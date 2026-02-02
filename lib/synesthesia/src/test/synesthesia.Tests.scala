package synesthesia

import soundness.*

import strategies.throwUnsafely
import autopsies.contrastExpectations
import servables.jsonIsServable
import charEncoders.utf8
import charDecoders.utf8
import textSanitizers.skip
import jsonPrinters.minimal

object Tests extends Suite(m"Synesthesia Tests"):
  def run(): Unit =
    test(m"Remote server"):
      import supervisors.global
      import codicils.cancel
      import internetAccess.enabled
      import Mcp.*
      val server = remote[McpApi](url"http://localhost:8080/")

      import supervisors.global
      import codicils.cancel
      import httpServers.stdlib
      import logging.silent
      import webserverErrorPages.stackTraces
      import classloaders.threadContext

      tcp"8080".serve:
        request.path match
          case % => t"Nothing here"
          case % /: t"favicon.ico" => t"Nothing here"
          case % /: t"favicon.png" => t"Nothing here"
          case % /: t"favicon.svg" => t"Nothing here"
          case % /: t"mcp"         => JsonRpc.server[McpServer]

      Thread.sleep(1000000)

    . assert()
