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
import murmuration.*
import denominative.*
import probably.*
import turbulence.*
import vacuous.*

import strategies.throwUnsafely
import charEncoders.utf8Encoder
import denominative.dysasymptotics.linearSize

object RecordsTests extends Suite(m"Stratiform Records tests"):
  def run(): Unit =
    suite(m"TelBlueprint field access"):
      test(m"required String field is accessed as Text"):
        val record = ContactRecords.record(t"name Alice\nage 30\n".read[Tel])
        record.name
      . assert(_ == t"Alice")

      test(m"second required field is accessed as Text"):
        val record = ContactRecords.record(t"name Alice\nage 30\n".read[Tel])
        record.age
      . assert(_ == t"30")

      test(m"optional field is absent when missing"):
        val record = ContactRecords.record(t"name Alice\nage 30\n".read[Tel])
        record.email
      . assert(_ == Unset)

      test(m"optional field is present when supplied"):
        val record = ContactRecords.record
                      (t"name Alice\nemail alice@example.com\nage 30\n".read[Tel])
        record.email
      . assert(_ == (t"alice@example.com": Optional[Text]))

      test(m"records derived from the same schema can be queried independently"):
        val a = ContactRecords.record(t"name Alice\nage 30\n".read[Tel])
        val b = ContactRecords.record(t"name Bob\nage 40\n".read[Tel])
        (a.name, b.name)
      . assert(_ == (t"Alice", t"Bob"))

    suite(m"TelBlueprint flag fields"):
      test(m"present flag reads as true"):
        val record = FeatureRecords.record(t"enabled\n".read[Tel])
        record.enabled
      . assert(_ == true)

      test(m"absent flag reads as false"):
        val record = FeatureRecords.record(t"".read[Tel])
        record.enabled
      . assert(_ == false)

    suite(m"TelBlueprint layers"):
      test(m"a member a layer introduces reads as optional, absent without the layer"):
        LayeredRecords.record(t"name Alice\n".read[Tel]).email
      . assert(_ == Unset)

      test(m"and present with it"):
        LayeredRecords.record(t"name Alice\nemail alice@example.com\n".read[Tel]).email
      . assert(_ == (t"alice@example.com": Optional[Text]))

      test(m"the layers a document carries are named"):
        LayeredRecords.layersOf(t"name Alice\nemail alice@example.com\n".read[Tel])
      . assert(_ == List(t"with-email"))

      test(m"the acceptance requires the base and offers the layer"):
        val lineage = SchemaSignature.Lineage(LayeredSchemaFixture.tels)
        val acceptance = LayeredRecords.acceptance(lineage)
        acceptance.alternatives.map { alternative => (alternative.schema.count, alternative.components.size, alternative.selfContained) }
      . assert(_ == List((1, 1, false), (1, 0, true)))

      test(m"a document served with the layer is received with its member"):
        val lineage = SchemaSignature.Lineage(LayeredSchemaFixture.tels)
        val acceptance = LayeredRecords.acceptance(lineage)
        val composition = lineage.base :: lineage.layers.map(_.hash)
        val held = Tel.Type.assign(t"name Alice\nemail alice@example.com\n".read[Tel], lineage.compose(composition))

        Tel.Acceptance.serve(acceptance, lineage, composition, held).let: served =>
          LayeredRecords.receive(acceptance, SchemaSignature.Library(List(lineage)), served.document).let: tel =>
            LayeredRecords.record(tel).email
      . assert(_ == (t"alice@example.com": Optional[Text]))

