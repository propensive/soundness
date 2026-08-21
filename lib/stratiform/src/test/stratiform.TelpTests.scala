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
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package stratiform

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import charEncoders.utf8Encoder
import denominative.asymptotics.linearSizeComplexity

// TELP (the TEL Path companion specification): grammar and identity (§3),
// resolution over the semantic model (§4), the all-digit shadowing rule
// (§5), same-keyword selector scope (§6), failure kinds (§7), and the §10
// worked examples verbatim.
object TelpTests extends Suite(m"Stratiform TELP tests"):

  // The §10 example schema and document.
  private val menagerie: Text =
    Text("""|tel 1.0
        |
        |name menagerie
        |
        |record Contact
        |  field name Identifier key
        |  field email String optional repeatable
        |
        |record Toy
        |  field label String key
        |
        |record Cat
        |  field name Identifier key
        |  field toy Toy optional repeatable
        |
        |record Dog
        |  field name Identifier key
        |
        |select Pet
        |  variant cat Cat
        |  variant dog Dog
        |
        |document
        |  field owner Contact
        |  field contact Contact optional repeatable
        |  select Pet optional repeatable
        |""".stripMargin)

  private val document: Text =
    Text("""|tel 1.0
        |
        |owner amy
        |contact bea
        |  email bea@example.com
        |  email bea@example.org
        |contact chu
        |
        |cat felix
        |  toy  ball of string
        |dog rex
        |cat tom
        |""".stripMargin)

  private lazy val schema: Tels =
    Tels.Validation.validate(Tels.Reconstructor.fromTel(menagerie.read[Tel]))

  private lazy val root: Tel.Element = Tel.Type.assign(document.read[Tel], schema)

  private def resolve(path: Text): Telp.Resolution =
    given Tels = schema
    Telp.parse(path).resolve(root)

  // The semantic text of the element a path resolves to (for a Value), or
  // its key field's value (for a keyed Node), or a shape marker.
  private def leaf(path: Text): Text = resolve(path).absolve match
    case Telp.Resolution.One(value: Tel.Element.Value) => value.text
    case Telp.Resolution.One(node: Tel.Element.Node)   => t"<node>"
    case Telp.Resolution.Occurrences(elements)         => t"<${elements.size} occurrences>"

  private def failure(path: Text): Telp.Error.Reason =
    given Tels = schema
    capture[Telp.Error](Telp.parse(path).resolve(root)).reason

  def run(): Unit =
    suite(m"Grammar and identity (§3)"):
      test(m"the first character selects the delimiter"):
        Telp.parse(t".contact.bea") == Telp.parse(t"/contact/bea")
      . assert(_ == true)

      test(m"a path is its component sequence"):
        Telp.parse(t"/owner/name").components
      . assert(_ == List(t"owner", t"name"))

      test(m"the delimiter alone is the root path"):
        (Telp.parse(t"/"), Telp.parse(t"."))
      . assert(_ == (Telp.Root, Telp.Root))

      test(m"components may contain spaces"):
        Telp.parse(t"/cat/felix/toy/ball of string").components.stdlib.last
      . assert(_ == t"ball of string")

      test(m"a doubled delimiter is a syntax error"):
        capture[Telp.Error](Telp.parse(t"/contact//name")).reason
      . assert(_ == Telp.Error.Reason.Syntax)

      test(m"a trailing delimiter is a syntax error"):
        capture[Telp.Error](Telp.parse(t"/contact/")).reason
      . assert(_ == Telp.Error.Reason.Syntax)

      test(m"a path must begin with a delimiter character"):
        capture[Telp.Error](Telp.parse(t"contact/bea")).reason
      . assert(_ == Telp.Error.Reason.Syntax)

      test(m"hyphen is not a delimiter"):
        capture[Telp.Error](Telp.parse(t"-contact-bea")).reason
      . assert(_ == Telp.Error.Reason.Syntax)

      test(m"a line feed in a path is a syntax error"):
        capture[Telp.Error](Telp.parse(t"/contact/a\nb")).reason
      . assert(_ == Telp.Error.Reason.Syntax)

      test(m"the empty string is a syntax error"):
        capture[Telp.Error](Telp.parse(t"")).reason
      . assert(_ == Telp.Error.Reason.Syntax)

    suite(m"Rendering"):
      test(m"the encoder prefers the slash delimiter"):
        Telp(List(t"contact", t"bea")).encode
      . assert(_ == t"/contact/bea")

      test(m"the root path renders as the delimiter alone"):
        Telp.Root.encode
      . assert(_ == t"/")

      test(m"a component containing a slash switches the delimiter"):
        // `/` and `.` both occur in `a.b/c`, so the encoder falls through
        // to the first free delimiter of §3's set, `!`.
        Telp(List(t"contact", t"a.b/c", t"email", t"0")).encode
      . assert(_ == t"!contact!a.b/c!email!0")

      test(m"parse then render round-trips"):
        Telp.parse(Telp(List(t"cat", t"felix", t"toy", t"0")).encode)
      . assert(_ == Telp(List(t"cat", t"felix", t"toy", t"0")))

    suite(m"Resolution (§4, §10 examples)"):
      test(m"the root path resolves to the context element"):
        resolve(t"/").absolve match
          case Telp.Resolution.One(node: Tel.Element.Node) => node == root
      . assert(_ == true)

      test(m"a non-repeatable member needs no selector"):
        leaf(t"/owner")
      . assert(_ == t"<node>")

      test(m"/owner/name is the Value amy"):
        leaf(t"/owner/name")
      . assert(_ == t"amy")

      test(m"a repeatable keyword at path end is its occurrence sequence"):
        leaf(t"/contact")
      . assert(_ == t"<2 occurrences>")

      test(m"an occurrence is selected by key value"):
        leaf(t"/contact/bea/name")
      . assert(_ == t"bea")

      test(m"an occurrence is selected by zero-based index"):
        leaf(t"/contact/1/name")
      . assert(_ == t"chu")

      test(m"an unkeyed repeatable member is index-only"):
        leaf(t"/contact/bea/email/1")
      . assert(_ == t"bea@example.org")

      test(m"key selection is scoped to same-keyword occurrences"):
        // `dog rex` between the two cats does not affect the cat sequence.
        leaf(t"/cat/tom/name")
      . assert(_ == t"tom")

      test(m"index selection counts same-keyword occurrences only"):
        leaf(t"/cat/1/name")
      . assert(_ == t"tom")

      test(m"/dog/0/name is the Value rex"):
        leaf(t"/dog/0/name")
      . assert(_ == t"rex")

      test(m"a Select name is not a keyword"):
        failure(t"/pet")
      . assert(_ == Telp.Error.Reason.UnknownKeyword)

      test(m"a nested keyed occurrence is selected by index"):
        leaf(t"/cat/felix/toy/0/label")
      . assert(_ == t"ball of string")

      test(m"a nested keyed occurrence is selected by key, spaces included"):
        leaf(t"/cat/felix/toy/ball of string/label")
      . assert(_ == t"ball of string")

    suite(m"Shadowing (§5)"):
      test(m"an all-digit component is always an index, never a key"):
        // Index 7 into a two-element sequence, not the key value `007`.
        failure(t"/contact/007")
      . assert(_ == Telp.Error.Reason.IndexOutOfRange)

      test(m"leading zeros are permitted in an index selector"):
        leaf(t"/contact/01/name")
      . assert(_ == t"chu")

    suite(m"Failures (§7)"):
      test(m"descending below a Value fails"):
        failure(t"/owner/name/x")
      . assert(_ == Telp.Error.Reason.NonStructDescent)

      test(m"an unknown keyword fails"):
        failure(t"/serpent")
      . assert(_ == Telp.Error.Reason.UnknownKeyword)

      test(m"descending below a scalar occurrence fails"):
        failure(t"/contact/bea/email/0/x")
      . assert(_ == Telp.Error.Reason.NonStructDescent)

      test(m"an absent optional member fails"):
        val extra = Text("""|tel 1.0
                        |name t
                        |record Person
                        |  field name Identifier key
                        |  field nickname String optional
                        |document
                        |  field person Person optional repeatable
                        |""".stripMargin)

        val schema = Tels.Validation.validate(Tels.Reconstructor.fromTel(extra.read[Tel]))
        val element = Tel.Type.assign(t"tel 1.0\n\nperson amy\n".read[Tel], schema)
        given Tels = schema
        capture[Telp.Error](Telp.parse(t"/person/amy/nickname").resolve(element)).reason
      . assert(_ == Telp.Error.Reason.AbsentMember)

      test(m"an index beyond the occurrence sequence fails"):
        failure(t"/dog/1")
      . assert(_ == Telp.Error.Reason.IndexOutOfRange)

      test(m"a key selector against an unkeyed type fails"):
        failure(t"/contact/bea/email/latest")
      . assert(_ == Telp.Error.Reason.TypeNotKeyed)

      test(m"an unmatched key value fails"):
        failure(t"/contact/zed")
      . assert(_ == Telp.Error.Reason.KeyNotFound)

      test(m"the failing component's index is reported"):
        given Tels = schema
        capture[Telp.Error](Telp.parse(t"/contact/zed").resolve(root)).index
      . assert(_ == 1)

    suite(m"Delimiter switch (§10)"):
      test(m"a key containing the conventional delimiters uses another"):
        val doc = Text("""|tel 1.0
                      |
                      |owner amy
                      |contact a.b/c
                      |  email x@example.com
                      |""".stripMargin)

        val element = Tel.Type.assign(doc.read[Tel], schema)
        given Tels = schema
        Telp.parse(t":contact:a.b/c:email:0").resolve(element).absolve match
          case Telp.Resolution.One(value: Tel.Element.Value) => value.text
      . assert(_ == t"x@example.com")

    suite(m"Focus pointers are TELPs"):
      test(m"prepending builds a root-first keyword path"):
        Telp.Root.prepend(t"name").prepend(t"person").components
      . assert(_ == List(t"person", t"name"))
