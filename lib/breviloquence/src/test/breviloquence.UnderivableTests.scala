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
package breviloquence

import soundness.*

import strategies.throwUnsafely

// A unit of its own, referring to nothing defined in a sibling: `demilitarize` recompiles the
// whole file on its own against the classpath, and a name it cannot see there is an error
// outside the block, after which the compiler inlines nothing more.
class Poison
case class Vial(label: Text, contents: Poison)
case class Cabinet(vials: List[Vial])

// A derivation resolves a field's instance once for each traversal it makes of the fields, so
// the one fault may be reported more than once: the tests compare the distinct messages.
object UnderivableTests extends Suite("cbor-underivable", m"Breviloquence underivable codec tests"):
  def run(): Unit =
    val reason = t"`Poison` is not a case class, an enum or a sealed trait"

    val decoder = t"`Decodable in Cbor`"
    val encoder = t"`Encodable in Cbor`"
    val decodable = t"a `Decodable in Text` instance would also serve"
    val encodable = t"an `Encodable in Text` instance would also serve"

    def wanted(instance: Text, hint: Text): Text =
      t"wisteria: the field `contents` of `Vial` needs a $instance instance for `Poison`, but "
      + t"none could be found or derived: $reason; $hint"

    test(m"a field with no decoder is named, with its type"):
      demilitarize:
        summon[Vial is Decodable in Cbor]
      . map(_.message).distinct
    . assert(_ == List(wanted(decoder, decodable)))

    test(m"a field with no encoder is named, with its type"):
      demilitarize:
        summon[Vial is Encodable in Cbor]
      . map(_.message).distinct
    . assert(_ == List(wanted(encoder, encodable)))

    test(m"a field of a nested record with no decoder is named in that record"):
      demilitarize:
        summon[Cabinet is Decodable in Cbor]
      . map(_.message).distinct
    . assert(_ == List(wanted(decoder, decodable)))

    test(m"a type with no decoder, wanted directly, names no field"):
      demilitarize:
        summon[Poison is Decodable in Cbor]
      . map(_.message).distinct
    . assert:
        _ == List(t"wisteria: no $decoder instance for `Poison` could be found or derived: "+
            t"$reason; $decodable")
