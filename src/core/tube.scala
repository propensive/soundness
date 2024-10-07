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
import charDecoders.utf8
import textSanitizers.skip

val About = Subcommand(t"about", e"find out about the $Underline(tube) tool")
val Install = Subcommand(t"install", e"[re]install the tab-completions")
val Trip = Subcommand(t"trip", e"plan a trip on the London Underground")

@main
def app(): Unit = cli:
  import internetAccess.enabled

  arguments match
    case About() :: _ => execute:
      Out.println(e"About this software")
      Exit.Ok

    case Install() :: _ => execute:
      Out.println(TabCompletions.install().communicate)
      Exit.Ok

    case Trip() :: _ => execute:
      Out.println(Data.stations)
      Exit.Ok

    case _ => execute:
      Out.println(e"$Bold(Unrecognized command!)")
      Exit.Fail(1)

object Data:
  def stations(using Online): Dsv =
    val sourceUrl = url"https://api.tfl.gov.uk/stationdata/tfl-stationdata-detailed.zip"
    Dsv.parse(ZipStream(sourceUrl.get()).extract(_ / n"Stations.csv").read[Text])