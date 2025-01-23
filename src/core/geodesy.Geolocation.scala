/*
    Geodesy, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package geodesy

import anticipation.*
import contingency.*
import gossamer.*
import kaleidoscope.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*

object Geolocation:
  import GeolocationError.Reason.*
  private given Decimalizer = Decimalizer(decimalPlaces = 6)

  private def parseParams(text: Text): List[(Text, Text)] raises GeolocationError =
    text.cut(t";").map: parameter =>
      parameter.cut(t"=") match
        case List(key, value) => (key, value)
        case Nil | List(_)    => raise(GeolocationError(MissingEquals)) yet Unset
        case _                => raise(GeolocationError(MultipleEquals)) yet Unset

    . compact

  given decoder: Tactic[GeolocationError] => Decoder[Geolocation] =
    case r"geo:$latitude(-?[0-9]+(\.[0-9]+)?),$longitude(-?[0-9]+(\.[0-9]+)?)$more(.*)" =>
      val location =
        unsafely(Location(latitude.decode[Double].deg, longitude.decode[Double].deg))

      more match
        case t""           => Geolocation(location)
        case r",$more(.*)" => more match
          case r"$altitude0(-?[0-9]+(\.[0-9]+)?)$more(.*)" =>
            val altitude = unsafely(altitude0.decode[Double])
            more match
              case t"" =>
                Geolocation(location, altitude)

              case r";.*" =>
                val (crs, params0) = parseParams(more) match
                  case (t"crs", crs) :: params => (crs, params)
                  case params                  => (Unset, params)

                val (uncertainty, params) = params0 match
                  case (t"u", u) :: params =>
                    val uncertainty = safely(u.decode[Double]).or:
                      raise(GeolocationError(BadUncertainty))
                      Unset

                    (uncertainty, params)

                  case params =>
                    (Unset, params)

                Geolocation(location, altitude, crs, uncertainty, params.to(Map))

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
    t"geo:$location${altitude.lay(t"") { a => t",$a" }}${uncertainty.lay(t"") { u => t";u=$u" }}"

case class Geolocation
   (location:    Location,
    altitude:    Optional[Double] = Unset,
    crs:         Optional[Text]   = Unset,
    uncertainty: Optional[Double] = Unset,
    parameters:  Map[Text, Text]  = Map())
