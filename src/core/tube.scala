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
import printableTypes.message
import pathNavigation.posix

erased given Naptan is Nominative under MustMatch["(|HUB[A-Z0-9]{3}|9[14]0[A-Z]+)"] = ###

val About = Subcommand(t"about", e"find out about the $Underline(tube) tool")
val Install = Subcommand(t"install", e"[re]install the tab-completions")
val Trip = Subcommand(t"trip", e"plan a trip on the London Underground")

@main
def app(): Unit = cli:
  import internetAccess.enabled

  mend:
    case error: InitError => execute:
      Out.println(t"An error occurred during initialization:")
      Out.println(error.message)
      service.shutdown()
      Exit.Fail(2)
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
  private val cache: Cache[Bijection[Name[Naptan], StationRow]] = Cache()

  def stations(using Online): Bijection[Name[Naptan], StationRow] raises InitError =
    val sourceUrl = url"https://api.tfl.gov.uk/stationdata/tfl-stationdata-detailed.zip"

    tend:
      case HttpError(url, _, status) => InitError(m"There was an HTTP $status error accessing $url")
      case _: ZipError               => InitError(m"There was a problem with the ZIP file")
      case error: NameError          => InitError(error.message)
      case _: DsvError               => InitError(m"The CSV file was not in the right format")
      case error: ConcurrencyError   => InitError(error.message)
      case PathError(_, _)           => InitError(m"The XDG cache home is not a valid path")
    .within:
      cache.establish:
        val file: Path on Posix = Xdg.cacheHome[Path on Posix] / n"tube.csv"
        val csv = ZipStream(sourceUrl.get()).extract(_ / n"Stations.csv")
        Dsv.parse(csv).rows.map(_.as[StationRow]).indexBy(_.id).bijection

case class InitError(detail: Message)(using Diagnostics) extends Error(detail)
case class StationRow(id: Name[Naptan], name: Text)

erased trait Naptan