package quantify

trait Inch[Power <: Int & Singleton] extends Units[Power, Length]
object Inch extends Quantity[Inch[1]](1):
  given Coefficient[Inch[1], Metre[1]](39.3701)

trait Foot[Power <: Int & Singleton] extends Units[Power, Length]
object Foot extends Quantity[Foot[1]](1):
  given Coefficient[Foot[1], Metre[1]](3.28084)

trait Yard[Power <: Int & Singleton] extends Units[Power, Length]
object Yard extends Quantity[Yard[1]](1):
  given Coefficient[Yard[1], Metre[1]](1.09361)

trait Mile[Power <: Int & Singleton] extends Units[Power, Length]
object Mile extends Quantity[Mile[1]](1):
  given Coefficient[Mile[1], Metre[1]](0.000621371)

trait Lightyear[Power <: Int & Singleton] extends Units[Power, Length]
object Lightyear extends Quantity[Lightyear[1]](1):
  given Coefficient[Lightyear[1], Metre[1]](1.057e-16)

