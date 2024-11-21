package geodesy

import vacuous.*
import anticipation.*
import spectacular.*
import rudiments.*
import gossamer.*
import contingency.*
import kaleidoscope.*
import prepositional.*

object Geolocation:
  import GeolocationError.Reason.*
  private given Decimalizer = Decimalizer(decimalPlaces = 6)

  private def parseParams(text: Text): List[(Text, Text)] raises GeolocationError =
    text.cut(t";").map: parameter =>
      parameter.cut(t"=") match
        case List(key, value) => (key, value)
        case Nil | List(_)    => raise(GeolocationError(MissingEquals)) yet Unset
        case _                => raise(GeolocationError(MultipleEquals)) yet Unset
    .compact

  given (using Tactic[GeolocationError]) => Decoder[Geolocation] as decoder =
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

  given Geolocation is Encodable in Text as encodable = geolocation =>
    import geolocation.{location, altitude, uncertainty}
    t"geo:$location${altitude.lay(t"") { a => t",$a" }}${uncertainty.lay(t"") { u => t";u=$u" }}"

case class Geolocation
   (location:    Location,
    altitude:    Optional[Double] = Unset,
    crs:         Optional[Text]   = Unset,
    uncertainty: Optional[Double] = Unset,
    parameters:  Map[Text, Text]  = Map())
