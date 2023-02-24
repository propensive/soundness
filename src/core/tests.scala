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