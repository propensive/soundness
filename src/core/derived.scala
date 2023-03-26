package quantify

val Hertz = Quantity[Second[-1]](1.0)
val Newton = Metre*Kilo(Gram)/(Second*Second)
val Pascal = Newton/(Metre*Metre)
val Joule = Newton*Metre
val Watt = Joule/Second
val Coulomb = Second*Ampere
val Volt = Watt/Ampere
val Farad = Coulomb/Volt
val Ohm = Volt/Ampere
val Siemens = Ampere/Volt
val Weber = Volt*Second
val Tesla = Weber/(Metre*Metre)
val Henry = Weber/Ampere
val Lux = Candela/(Metre*Metre)
val Becquerel = Quantity[Second[-1]](1.0)
val Gray = Joule/Kilo(Gram)
val Sievert = Joule/Kilo(Gram)
val Katal = Mole/Second