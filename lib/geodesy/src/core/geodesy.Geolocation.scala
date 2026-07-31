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

object Geolocation:
  import GeolocationError.Reason.*

  private given decimalizer: Decimalizer = Decimalizer(decimalPlaces = 6)

  private def parseParams(text: Text): List[(Text, Text)] raises GeolocationError =
    text.cut(t";").map: parameter =>
      parameter.cut(t"=") match
        case List(key, value) => (key, value)
        case Nil | List(_)    => abort(GeolocationError(MissingEquals))
        case _                => abort(GeolocationError(MultipleEquals))

  given decoder: (tactic: Tactic[GeolocationError])
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
                      raise(GeolocationError(BadUncertainty))
                      Unset

                    (uncertainty, params)

                  case params =>
                    (Unset, params)

                Geolocation(location, altitude, crs, uncertainty, Map.from(params.stdlib))

              case other =>
                raise(GeolocationError(ExpectedSemicolon))
                Geolocation(location, altitude)

          case other =>
            raise(GeolocationError(UnexpectedSuffix))
            Geolocation(location)

        case other =>
          raise(GeolocationError(UnexpectedSuffix))
          Geolocation(location)

    case r"geo:.*" =>
      raise(GeolocationError(ExpectedCoordinates))
      Geolocation(Location(0.deg, 0.deg))

    case value =>
      raise(GeolocationError(BadScheme))
      Geolocation(Location(0.deg, 0.deg))

  given encodable: Geolocation is Encodable in Text = geolocation =>
    import geolocation.{location, altitude, uncertainty}

    val alt = altitude.lay(t""): a => t",$a"
    t"geo:${location.encode}$alt${uncertainty.lay(t"") { u => t";u=$u" }}"

case class Geolocation
  ( location:    Location,
    altitude:    Optional[Double] = Unset,
    crs:         Optional[Text]   = Unset,
    uncertainty: Optional[Double] = Unset,
    parameters:  Map[Text, Text]  = Map() )
