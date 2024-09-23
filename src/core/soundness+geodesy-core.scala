package soundness

export geodesy.{Bearing, CardinalWind, Compass, IntercardinalWind, HalfWind, Locatable, rad, deg,
    North, East, South, West, Northeast, Southeast, Northwest, Southwest, NorthNortheast,
    EastNortheast, SouthSoutheast, EastSoutheast, NorthNorthwest, WestNorthwest, SouthSouthwest,
    WestSouthwest, Location, Geolocation, Degrees, Radians}

package compassBearings:
  export geodesy.compassBearings.{fourPointCompass, eightPointCompass, sixteenPointCompass,
    degreesFromNorth, radiansFromNorth, degreesFromEast, radiansFromEast}