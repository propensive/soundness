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
import environments.daemonClient
import homeDirectories.default

erased given Naptan is Nominative under MustMatch["(|HUB[A-Z0-9]{3}|9[14]0[A-Z]+)"] = ###
given StationRow is Suggestible = row => Suggestion(row.ref, row.name)
given StationRow is Showable = _.name
given StationRow is Embeddable in HttpUrl by UrlFragment = row => UrlFragment(row.id.text)

val About = Subcommand(t"about", e"find out about the $Underline(tube) tool")
val Install = Subcommand(t"install", e"[re]install the tab-completions")
val Trip = Subcommand(t"trip", e"plan a trip on the London Underground")
val Start = Flag(t"start", false, List('s'), t"The start of your journey")
val Destination = Flag(t"destination", false, List('d'), t"The end of your journey")
val Departure = Flag(t"departure", false, List('D'), t"The departure time in HHMM format")

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

      case Trip() :: _ =>
        val stations = Data.stations
        val start0: Optional[StationRow] = Start.select(stations.values)
        val destination0: Optional[StationRow] = Destination.select(start0.lay(stations)(stations - _.id).values)
        val departure0: Optional[Text] = Departure[Text]()

        execute:
          mend:
            case error: UserError => Out.println(error.message) yet Exit.Fail(1)
          .within:
            val start = start0.or(abort(UserError(m"The $Start parameter has not been specified")))
            val destination = destination0.or(abort(UserError(m"The $Destination parameter has not been specified")))

            val departure = departure0 match
              case time@r"${As[Int](hh)}([0-2][0-9])[0-5][0-9]" if 0 <= hh < 24 => time
              case _                                                            => t"0800"

            Out.println(e"Searching for a journey from $Italic($start) to $Italic($destination)")
            Exit.Ok

      case _ => execute:
        Out.println(e"$Bold(Unrecognized command!)")
        Exit.Fail(1)

object Data:
  private val cache: Cache[Bijection[Name[Naptan], StationRow]] = Cache()

  def stations(using Online, Environment): Bijection[Name[Naptan], StationRow] raises InitError =
    val sourceUrl = url"https://api.tfl.gov.uk/stationdata/tfl-stationdata-detailed.zip"

    tend:
      case HttpError(url, _, status) => InitError(m"There was an HTTP $status error accessing $url")
      case _: ZipError               => InitError(m"There was a problem with the ZIP file")
      case error: NameError          => InitError(error.message)
      case _: DsvError               => InitError(m"The CSV file was not in the right format")
      case error: ConcurrencyError   => InitError(error.message)
      case PathError(_, _)           => InitError(m"The XDG cache home is not a valid path")
      case error: IoError            => InitError(error.message)
      case _: StreamError            => InitError(m"An error occurred when reading the cache file from disk")
    .within:
      cache.establish:
        import filesystemOptions.readAccess.enabled
        import filesystemOptions.writeAccess.disabled
        import filesystemOptions.dereferenceSymlinks.enabled
        import filesystemOptions.createNonexistent.enabled
        import filesystemOptions.createNonexistentParents.enabled
        val file: Path on Posix = Xdg.cacheHome[Path on Posix] / n"tube.csv"

        val csv = if file.exists() then file.open(_.stream[Bytes].strict) else
          ZipStream(sourceUrl.get()).extract(_ / n"Stations.csv").stream[Bytes].tap: stream =>
            import filesystemOptions.writeAccess.enabled
            file.open(stream.writeTo(_))

        Dsv.parse(csv).rows.map(_.as[StationRow]).indexBy(_.id).bijection

  def plan(start: StationRow, destination: StationRow, time: Text)(using Online): Plan raises UserError =
    val sourceUrl = url"https://api.tfl.gov.uk/Journey/JourneyResults/$start/to/$destination/?time=$time"
    tend:
      case HttpError(url, _, status)       => UserError(m"Attempt to access $url returned $status.")
      case JsonParseError(line, _, reason) => UserError(m"Could not parse JSON response: $reason at line $line")
      case JsonError(reason)               => UserError(m"Unexpected JSON response from TfL: $reason")
    .within:
      Json.parse(sourceUrl.get(RequestHeader.Accept(media"application/json"))).as[Plan]

case class InitError(detail: Message)(using Diagnostics) extends Error(detail)
case class UserError(messages: Message*)(using Diagnostics) extends Error(messages.join(m"\n"))

case class StationRow(id: Name[Naptan], name: Text):
  def ref: Text = name.lower.cut(t" ").kebab

case class Plan()

erased trait Naptan
