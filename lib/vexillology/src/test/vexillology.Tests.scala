package vexillology

import soundness.*

object Tests extends Suite(t"Vexillology tests"):
  def run(): Unit =

    enum Color:
      case Red, Orange, Yellow, Green, Blue, Indigo, Violet

    import Color.*

    test(t"Empty flags"):
      val flags = Flags[Color]()
      flags.set

    . assert(_.isEmpty)

    test(t"Value is initially unset"):
      val flags = Flags[Color]()
      flags(Orange)
    . assert(_ == false)

    test(t"Value can be set"):
      var flags = Flags[Color]()
      flags = flags(Orange) = true
      flags(Orange)
    . assert(_ == true)

    test(t"Setting one value does not set others"):
      var flags = Flags[Color]()
      flags = flags(Orange) = true
      flags(Red) || flags(Yellow) || flags(Green) || flags(Blue) || flags(Indigo) || flags(Violet)
    . assert(_ == false)

    test(t"Single value"):
      var flags = Flags[Color]()
      flags = flags(Orange) = true
      flags.set
    . assert(_ == Set(Orange))

    test(t"Multiple values"):
      var flags = Flags[Color]()
      flags = flags(Orange) = true
      flags = flags(Green) = true
      flags = flags(Violet) = true
      flags.set
    . assert(_ == Set(Orange, Green, Violet))

    test(t"Unset a value"):
      var flags = Flags[Color]()
      flags = flags(Orange) = true
      flags = flags(Green) = true
      flags = flags(Violet) = true
      flags = flags(Green) = false
      flags.set
    . assert(_ == Set(Orange, Violet))
