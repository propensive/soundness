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
import enumIdentification.kebabCase

erased given Naptan is Nominative under MustMatch["(|HUB[A-Z0-9]{3}|9[14]0[A-Z]+)"] = ###
given StationRow is Suggestible = row => Suggestion(row.ref, row.name)
given StationRow is Showable = _.name
given StationRow is Embeddable in HttpUrl by UrlFragment = row => UrlFragment(row.id.text)

type HoursAndMinutes = Count[(Hours[1], Minutes[1])]

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
            Output.render(Data.plan(start, destination, departure), start, destination)
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
    given Optional[JsonPath] is Communicable = _.lay(t"<unknown>")(_.show).communicate

    track[JsonPath](UserError()):
      case HttpError(url, _, status)       => accrual + m"Attempt to access $url returned $status."
      case JsonParseError(line, _, reason) => accrual + m"Could not parse JSON response: $reason at line $line"
      case JsonError(reason)               => accrual + m"Unexpected JSON response from TfL: $reason at $focus when accessing $sourceUrl"
      case error: VariantError             => accrual + m"${error.message} at $focus from $sourceUrl"
    .within:
      Json.parse(sourceUrl.get(RequestHeader.Accept(media"application/json"))).as[Plan]

object Output:
  val tl = u"box drawings light arc down and right"
  val bl = u"box drawings light arc up and right"
  val br = u"box drawings light arc up and left"
  val tr = u"box drawings light arc down and left"
  val hl = u"box drawings light horizontal"
  val vl = u"box drawings light vertical"
  val dt = u"black small square"
  val st = u"black square for stop"
  val ln = e"$Reverse(  )"

  def render(plan: Plan, start: StationRow, destination: StationRow)(using Stdio): Unit =
    plan.journeys.each: journey =>
      Out.println(e"$Underline(Option ${ordinal.n1}), ${journey.duration}")
      val startTitle = e"$Reverse( $Bold(${start.name.upper}) )"
      val destinationTitle = e"$Reverse( $Bold(${destination.name.upper}) )"
      def indent(ordinal: Ordinal, extra: Int): Text = t" "*(ordinal.n0*5 + 10 + extra)

      Out.println(e"${indent(Prim, 9)}${startTitle.center(40)}\n")

      def renderLeg(leg: Leg, legNo: Ordinal): Unit =
        leg.path.stopPoints.dropRight(1).each: stop =>
          Out.println(e"${indent(legNo, 28)}$ln")
          Out.println(e"${indent(legNo, 0)}${stop.shortName.fit(25, Bidi.Rtl)}  $st$ln")
          Out.println(e"${indent(legNo, 28)}$ln")

      journey.legs.prim.let(renderLeg(_, Prim))
      journey.legs.slide(2).each: pair =>
        pair(0).path.stopPoints.lastOption.foreach: stop =>
          Out.println(e"${indent(ordinal, 26)}  $ln")
          Out.println(e"${indent(ordinal, 26)}$tl$hl$ln$hl$hl$hl$ln$hl$tr")
          Out.println(e"${indent(ordinal, 0)}${stop.shortName.fit(25, Bidi.Rtl)} $vl $ln$dt$dt$dt$ln $vl")
          Out.println(e"${indent(ordinal, 26)}$bl$hl$ln$hl$hl$hl$ln$hl$br")
          Out.println(e"${indent(ordinal, 26)}       $ln")

        renderLeg(pair(1), ordinal + 1)

      Out.println(e"\n${indent(Ordinal.natural(journey.legs.length), 9)}${destinationTitle.center(40)}\n")


case class InitError(detail: Message)(using Diagnostics) extends Error(detail)
case class UserError(messages: Message*)(using Diagnostics) extends Error(messages.reverse.join(m"\n")):
  infix def + (message: Message): UserError = UserError(message +: messages*)

case class StationRow(id: Name[Naptan], name: Text):
  def ref: Text = name.lower.cut(t" ").kebab

case class Plan(journeys: List[Journey])
case class Journey(duration: HoursAndMinutes, legs: List[Leg])
case class Leg(duration: HoursAndMinutes, path: LegPath, instruction: Instruction, routeOptions: List[RouteOption])
case class LegPath(stopPoints: List[Stop])
case class Instruction(detailed: Text)
case class RouteOption(lineIdentifier: Optional[LineIdentifier])
case class LineIdentifier(id: TubeLine)

case class Stop(name: Text):
  def shortName: Text = name.sub(t" Underground Station", t"")

enum TubeLine:
  case Bakerloo, Central, Circle, District, HammersmithCity, Jubilee, Metropolitan, Northern, Piccadilly, Victoria, WaterlooCity, Elizabeth, Dlr, LondonOverground

erased trait Naptan
