package quantify

type Distance = Quantity[Metre[1]]
type Duration = Quantity[Second[1]]
type Speed = Quantity[Metre[1] & Second[-1]]
type Acceleration = Quantity[Metre[1] & Second[-2]]
type Bodymass = Quantity[Kilogram[1]]
type Force = Quantity[Kilogram[1] & Metre[1] & Second[-2]]

@main def run(): Unit =
  val distance: Distance = 100*Metre
  val time: Duration = 9.58*Second
  val v0: Speed = 0*Metre/Second
  val v1: Speed = distance/time
  val acc: Acceleration = (distance - v0*time)/(time*time)*2
  val mass: Bodymass = 94*Kilogram

  val force: Force = mass*acc

  println(force)


  val width: Distance = 10*Metre
  val height: Quantity[Inch[1]] = 10*Inch

  val size = width*height
  println(size)

  println(size/(1.0*Metre)*Metre)

  val c = 299792458*Metre/Second
  val c2 = c*c

  val abs = acc/c
  val ħ = 1.054571817e-34*Joule*Second

  println(abs)