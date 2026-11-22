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
package vexillology

import soundness.*

object Tests extends Suite("flags", m"Vexillology tests"):
  def run(): Unit =

    enum Color:
      case Red, Orange, Yellow, Green, Blue, Indigo, Violet

    import Color.*

    test(m"Empty flags"):
      val flags = Flags[Color]()
      flags.set

    . assert(_.nil)

    test(m"Value is initially unset"):
      val flags = Flags[Color]()
      flags(Orange)
    . assert(_ == false)

    test(m"Value can be set"):
      var flags = Flags[Color]()
      flags = flags(Orange) = true
      flags(Orange)
    . assert(_ == true)

    test(m"Setting one value does not set others"):
      var flags = Flags[Color]()
      flags = flags(Orange) = true
      flags(Red) || flags(Yellow) || flags(Green) || flags(Blue) || flags(Indigo) || flags(Violet)
    . assert(_ == false)

    test(m"Single value"):
      var flags = Flags[Color]()
      flags = flags(Orange) = true
      flags.set
    . assert(_ == Set(Orange))

    test(m"Multiple values"):
      var flags = Flags[Color]()
      flags = flags(Orange) = true
      flags = flags(Green) = true
      flags = flags(Violet) = true
      flags.set
    . assert(_ == Set(Orange, Green, Violet))

    test(m"Unset a value"):
      var flags = Flags[Color]()
      flags = flags(Orange) = true
      flags = flags(Green) = true
      flags = flags(Violet) = true
      flags = flags(Green) = false
      flags.set
    . assert(_ == Set(Orange, Violet))

    test(m"The underlying bits have one bit per set case"):
      var flags = Flags[Color]()
      flags = flags(Orange) = true
      flags = flags(Green) = true
      flags.bits
    . assert(_ == B64(10L))

    suite(m"Bitset"):
      test(m"an empty Bitset has no members"):
        Bitset.empty.size
      . assert(_ == 0)

      test(m"a Bitset has the members it was made with"):
        val bits = Bitset(1.z, 5.z, 64.z)
        (bits.has(1.z), bits.has(5.z), bits.has(64.z), bits.has(2.z))
      . assert(_ == (true, true, true, false))

      test(m"adding a member gives a new Bitset"):
        val bits = Bitset(1.z)
        val more = bits + 300.z
        (bits.has(300.z), more.has(300.z), more.has(1.z))
      . assert(_ == (false, true, true))

      test(m"removing a member gives a new Bitset"):
        val bits = Bitset(1.z, 2.z)
        val fewer = bits - 1.z
        (bits.has(1.z), fewer.has(1.z), fewer.has(2.z))
      . assert(_ == (true, false, true))

      test(m"the union of two Bitsets is their sum"):
        Bitset(1.z, 2.z) + Bitset(2.z, 70.z)
      . assert(_ == Bitset(1.z, 2.z, 70.z))

      test(m"intersect keeps the members of both"):
        Bitset(1.z, 2.z, 70.z).intersect(Bitset(2.z, 70.z, 71.z))
      . assert(_ == Bitset(2.z, 70.z))

      test(m"except removes the other's members"):
        Bitset(1.z, 2.z, 70.z).except(Bitset(2.z, 71.z))
      . assert(_ == Bitset(1.z, 70.z))

      test(m"size counts the members"):
        Bitset(3.z, 3.z, 64.z, 1000.z).size
      . assert(_ == 3)

      test(m"a Bitset is traversed in ascending order"):
        var seen: List[Int] = Nil
        Bitset(200.z, 3.z, 64.z, 0.z).each { ordinal => seen = ordinal.n0 :: seen }
        seen.reverse
      . assert(_ == List(0, 3, 64, 200))

      test(m"Bitsets with the same members are equal however they were made"):
        (Bitset(1.z, 900.z) - 900.z) == Bitset(1.z)
      . assert(_ == true)

      test(m"a Bitset is built in a block"):
        Bitset.build: bits =>
          (0 until 10).foreach: index =>
            if index%3 == 0 then bits.add(index.z)
      . assert(_ == Bitset(0.z, 3.z, 6.z, 9.z))

      test(m"a builder can be asked what it has, and can remove"):
        Bitset.build: bits =>
          bits.add(4.z)
          bits.add(5.z)
          if bits.has(4.z) then bits.remove(5.z)
      . assert(_ == Bitset(4.z))

      test(m"the greatest member of a Bitset is its last"):
        Bitset(3.z, 64.z).last
      . assert(_ == 64.z)

      test(m"an empty Bitset has no last member"):
        Bitset.empty.last
      . assert(_.absent)

      test(m"an empty Bitset is nil"):
        Bitset.empty.nil
      . assert(_ == true)
