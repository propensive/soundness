package geodesy

import prepositional.*
import rudiments.*
import symbolism.*

export Geodesy.{Location, Radians, Degrees}

extension (double: Double)
  def rad: Radians = Radians(double)
  def deg: Degrees = Degrees(double)

export CardinalWind.*, IntercardinalWind.*, HalfWind.*

package compassBearings:
  given Bearing[Compass[4]] as fourPointCompass = Compass[4](_)
  given Bearing[Compass[8]] as eightPointCompass = Compass[8](_)
  given Bearing[Compass[16]] as sixteenPointCompass = Compass[16](_)
  given Bearing[Degrees] as degreesFromNorth = _.degrees
  given Bearing[Radians] as radiansFromNorth = identity(_)
  given Bearing[Degrees] as degreesFromEast = radians => (radians - Radians(math.Pi/2)).degrees
  given Bearing[Radians] as radiansFromEast = radians => radians - Radians(math.Pi/2)
