package synesthesia

import soundness.*

import strategies.throwUnsafely
import autopsies.contrastExpectations
import servables.jsonIsServable
import charEncoders.utf8
import charDecoders.utf8
import textSanitizers.skip
import jsonPrinters.minimal
import internetAccess.enabled
import supervisors.global
import codicils.cancel

object Tests extends Suite(m"Synesthesia Tests"):
  def run(): Unit =
    test(m"Remote server"):
      import internetAccess.enabled
      import supervisors.global
      import codicils.cancel
      import httpServers.stdlib
      import logging.silent
      import webserverErrorPages.stackTraces
      import classloaders.threadContext

      tcp"8080".serve:
        request.path match
          case % /: t"mcp"         =>
            try
              unsafely:
                TestMcpServer.serve
            catch case throwable: Throwable =>
              throwable.printStackTrace()
              ???
          case _                   => Http.Response(Http.NotFound)(t"Error 404: Not found")

      Thread.sleep(1000000)

    . assert()
