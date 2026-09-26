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
import larceny.*
import polyvinyl.*
import prepositional.*
import murmuration.*
import denominative.*
import probably.*
import turbulence.*
import vacuous.*

import errorDiagnostics.stackTracesDiagnostics
import strategies.throwUnsafely
import charEncoders.utf8Encoder
import denominative.dysasymptotics.linearSize

object RecordsTests extends Suite(m"Stratiform Records tests"):
  def run(): Unit =
    suite(m"Tel.Provider field access"):
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

    suite(m"Tel.Provider nested and repeatable fields"):
      val team: Tel =
        t"""name Core
           |tag alpha
           |tag beta
           |lead
           |  name Alice
           |  role lead
           |member
           |  name Bob
           |member
           |  name Carol
           |  role tester
           |""".s.stripMargin.tt.read[Tel]

      test(m"a repeatable scalar field reads as a list"):
        TeamRecords.record(team).tag
      . assert(_ == List(t"alpha", t"beta"))

      test(m"an absent repeatable field reads as an empty list"):
        TeamRecords.record(t"name Solo\nlead\n  name Alice\n".read[Tel]).tag
      . assert(_ == List())

      test(m"a reference to a record reads as a nested record"):
        TeamRecords.record(team).lead.name
      . assert(_ == t"Alice")

      test(m"an optional member of a nested record is read"):
        TeamRecords.record(team).lead.role
      . assert(_ == (t"lead": Optional[Text]))

      test(m"an optional nested record is Unset when absent"):
        TeamRecords.record(team).deputy.let(_.name)
      . assert(_ == Unset)

      test(m"a repeatable reference reads as a list of records"):
        TeamRecords.record(team).member.map(_.name)
      . assert(_ == List(t"Bob", t"Carol"))

      test(m"each repeated record reads its own optional members"):
        TeamRecords.record(team).member.map(_.role)
      . assert(_ == List(Unset, t"tester"))

      test(m"a tuple carries nested and repeated elements"):
        val tuple = TeamRecords.tuple(team)
        (tuple.name, tuple.tag, tuple.lead.name, tuple.member.map(_.name))
      . assert(_ == (t"Core", List(t"alpha", t"beta"), t"Alice", List(t"Bob", t"Carol")))

    suite(m"Tel.Provider names, structs, selects and validators"):
      given handle: ("handle" is Intensional in Tel.Provider from Tel to Handle) =
        Intensional { tel => Handle(tel.primaryAtom) }

      val profile: Tel =
        t"""first-name Ada
           |handle @ada
           |bio
           |  line one
           |  line two
           |note hello
           |""".s.stripMargin.tt.read[Tel]

      test(m"a kebab-case field is read through a backticked name"):
        ProfileRecords.record(profile).`first-name`
      . assert(_ == t"Ada")

      test(m"a custom validator reads through a given at the call site"):
        ProfileRecords.record(profile).handle
      . assert(_ == Handle(t"@ada"))

      test(m"an inline struct reads as a nested record"):
        ProfileRecords.record(profile).bio.let(_.line)
      . assert(_ == (List(t"one", t"two"): Optional[List[Text]]))

      test(m"a select's scalar variant reads when present"):
        ProfileRecords.record(profile).note
      . assert(_ == (t"hello": Optional[Text]))

      test(m"a select's flag variants read as booleans"):
        val record = ProfileRecords.record(t"first-name Bo\nhandle @bo\narchived\n".read[Tel])
        (record.active, record.archived, record.note)
      . assert(_ == (false, true, Unset))

      test(m"a missing required field fails when read"):
        capture[Tel.Error](ProfileRecords.record(t"handle @x\n".read[Tel]).`first-name`).reason
      . assert(_ == Tel.Error.Reason.Absent)

      test(m"a missing required nested record fails when read"):
        capture[Tel.Error](TeamRecords.record(t"name Solo\n".read[Tel]).lead).reason
      . assert(_ == Tel.Error.Reason.Absent)

    suite(m"Tel.Provider compiletime checks"):
      test(m"a field the schema does not declare does not compile"):
        demilitarize(ContactRecords.record(Tel.empty).nope)
      . assert(_.exists(_.reason == CompileError.Reason.NotAMember))

      test(m"a field cannot be read at the wrong type"):
        demilitarize:
          val name: Int = ContactRecords.record(Tel.empty).name
      . assert(_.exists(_.reason == CompileError.Reason.TypeMismatch))

      test(m"a tuple has the schema's names and types"):
        val tuple: (name: Text, email: Optional[Text], age: Text) =
          ContactRecords.tuple(t"name Alice\nage 30\n".read[Tel])

        tuple.age
      . assert(_ == t"30")

    suite(m"Tel.Provider flag fields"):
      test(m"present flag reads as true"):
        val record = FeatureRecords.record(t"enabled\n".read[Tel])
        record.enabled
      . assert(_ == true)

      test(m"absent flag reads as false"):
        val record = FeatureRecords.record(t"".read[Tel])
        record.enabled
      . assert(_ == false)

    suite(m"Tel.Provider layers"):
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

