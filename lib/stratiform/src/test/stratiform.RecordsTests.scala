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
package stratiform

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import probably.*
import turbulence.*
import vacuous.*

import strategies.throwUnsafely
import charEncoders.utf8Encoder

object RecordsTests extends Suite(m"Stratiform Records tests"):
  def run(): Unit =
    suite(m"TelBlueprint field access"):
      test(m"required String field is accessed as Text"):
        val record = ContactRecords.record("name Alice\nage 30\n".read[Tel])
        record.name
      . assert(_ == "Alice")

      test(m"second required field is accessed as Text"):
        val record = ContactRecords.record("name Alice\nage 30\n".read[Tel])
        record.age
      . assert(_ == "30")

      test(m"optional field is absent when missing"):
        val record = ContactRecords.record("name Alice\nage 30\n".read[Tel])
        record.email
      . assert(_ == Unset)

      test(m"optional field is present when supplied"):
        val record = ContactRecords.record
                      ("name Alice\nemail alice@example.com\nage 30\n".read[Tel])
        record.email
      . assert(_ == ("alice@example.com": Optional[Text]))

      test(m"records derived from the same schema can be queried independently"):
        val a = ContactRecords.record("name Alice\nage 30\n".read[Tel])
        val b = ContactRecords.record("name Bob\nage 40\n".read[Tel])
        (a.name, b.name)
      . assert(_ == ("Alice", "Bob"))

    suite(m"TelBlueprint flag fields"):
      test(m"present flag reads as true"):
        val record = FeatureRecords.record("enabled\n".read[Tel])
        record.enabled
      . assert(_ == true)

      test(m"absent flag reads as false"):
        val record = FeatureRecords.record("".read[Tel])
        record.enabled
      . assert(_ == false)
