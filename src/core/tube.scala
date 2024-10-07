package tube.terminal

import soundness.*

import executives.completions
import unhandledErrors.stackTrace
import parameterInterpretation.posix
import threadModels.platform
import workingDirectories.daemonClient
import errorDiagnostics.stackTraces
import logging.silent
import charDecoders.utf8
import textSanitizers.skip
import dsvFormats.csvWithHeader
import tableStyles.minimal
import textMetrics.uniform
import columnAttenuation.ignore

erased given Naptan is Nominative under MustStart["HUB"] = ###

val About = Subcommand(t"about", e"find out about the $Underline(tube) tool")
val Install = Subcommand(t"install", e"[re]install the tab-completions")
val Trip = Subcommand(t"trip", e"plan a trip on the London Underground")

@main
def app(): Unit = cli:
  import internetAccess.enabled

  mend:
    case _: InitError => service.shutdown()
  .within:
    arguments match
      case About() :: _ => execute:
        Out.println(e"About this software")
        Exit.Ok

      case Install() :: _ => execute:
        safely(Out.println(TabCompletions.install().communicate))
        Exit.Ok

      case Trip() :: _ => execute:
        val stations = Data.stations
        Exit.Ok

      case _ => execute:
        Out.println(e"$Bold(Unrecognized command!)")
        Exit.Fail(1)

object Data:
  def stations(using Online): LazyList[StationRow] raises InitError =
    val sourceUrl = url"https://api.tfl.gov.uk/stationdata/tfl-stationdata-detailed.zip"

    tend:
      case _: HttpError => InitError(m"There was an HTTP error")
      case _: ZipError  => InitError(m"There was a problem with the ZIP file")
      case _: NameError => InitError(m"There was a naming issue")
      case _: DsvError  => InitError(m"The CSV file was not in the right format")
    .within:
      Dsv.parse(ZipStream(sourceUrl.get()).extract(_ / n"Stations.csv")).rows.map(_.as[StationRow]).strict

case class InitError(detail: Message)(using Diagnostics) extends Error(detail)
case class StationRow(id: Name[Naptan], name: Text)

erased trait Naptan