                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package geodesy

import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import kaleidoscope.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*
import fulminate.*

object Geolocation:
  import Geolocation.Error.Reason.*

  private given decimalizer: Decimalizer = Decimalizer(decimalPlaces = 6)

  private def parseParams(text: Text): List[(Text, Text)] raises Geolocation.Error =
    text.cut(t";").map: parameter =>
      parameter.cut(t"=") match
        case List(key, value) => (key, value)
        case Nil | List(_)    => abort(Geolocation.Error(MissingEquals))
        case _                => abort(Geolocation.Error(MultipleEquals))

  given decoder: (tactic: Tactic[Geolocation.Error])
  =>  ((Geolocation is Decodable in Text)^{tactic}) =
    case r"geo:$latitude(-?[0-9]+(\.[0-9]+)?),$longitude(-?[0-9]+(\.[0-9]+)?)$more(.*)" =>
      val location =
        unsafely(Location(latitude.as[Double].deg, longitude.as[Double].deg))

      more match
        case t""           => Geolocation(location)

        case r",$more(.*)" => more match
          case r"$altitude0(-?[0-9]+(\.[0-9]+)?)$more(.*)" =>
            val altitude = unsafely(altitude0.as[Double])

            more match
              case t"" =>
                Geolocation(location, altitude)

              case r";.*" =>
                val (crs, params0) = parseParams(more) match
                  case (t"crs", crs) :: params => (crs, params)
                  case params                  => (Unset, params)

                val (uncertainty, params) = params0 match
                  case (t"u", u) :: params =>
                    val uncertainty = safely(u.as[Double]).or:
                      raise(Geolocation.Error(BadUncertainty))
                      Unset

                    (uncertainty, params)

                  case params =>
                    (Unset, params)

                Geolocation(location, altitude, crs, uncertainty, Map.from(params.stdlib))

              case other =>
                raise(Geolocation.Error(ExpectedSemicolon))
                Geolocation(location, altitude)

          case other =>
            raise(Geolocation.Error(UnexpectedSuffix))
            Geolocation(location)

        case other =>
          raise(Geolocation.Error(UnexpectedSuffix))
          Geolocation(location)

    case r"geo:.*" =>
      raise(Geolocation.Error(ExpectedCoordinates))
      Geolocation(Location(0.deg, 0.deg))

    case value =>
      raise(Geolocation.Error(BadScheme))
      Geolocation(Location(0.deg, 0.deg))

  given encodable: Geolocation is Encodable in Text = geolocation =>
    import geolocation.{location, altitude, uncertainty}

    val alt = altitude.lay(t""): a => t",$a"
    t"geo:${location.encode}$alt${uncertainty.lay(t"") { u => t";u=$u" }}"

  // Geolocation.Error → Geolocation.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case MissingEquals       extends Reason(1)
      case MultipleEquals      extends Reason(2)
      case BadScheme           extends Reason(3)
      case ExpectedSemicolon   extends Reason(4)
      case UnexpectedSuffix    extends Reason(5)
      case ExpectedCoordinates extends Reason(6)
      case BadUncertainty      extends Reason(7)

    given communicable: Reason is Communicable =
      case Reason.MissingEquals       => m"the parameter does not contain an `=`"
      case Reason.MultipleEquals      => m"the parameter contains more than one `=`"
      case Reason.BadScheme           => m"the value does not begin with the `geo:` URI scheme"
      case Reason.ExpectedSemicolon   => m"a `;` was expected after the altitude value"
      case Reason.UnexpectedSuffix    => m"a `,` or `;` was expected"
      case Reason.ExpectedCoordinates => m"latitude and longitude coordinates were expected"
      case Reason.BadUncertainty      => m"the `uncertainty` parameter vas not a valid number"

  case class Error(reason: Geolocation.Error.Reason)(using Diagnostics)
  extends fulminate.Error(420, reason.number)
    ( m"the geo URI is not in the correct format because $reason" )

case class Geolocation
  ( location:    Location,
    altitude:    Optional[Double] = Unset,
    crs:         Optional[Text]   = Unset,
    uncertainty: Optional[Double] = Unset,
    parameters:  Map[Text, Text]  = Map() )
