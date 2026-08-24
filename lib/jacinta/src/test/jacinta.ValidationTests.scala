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
package jacinta

import soundness.*


import charEncoders.utf8Encoder
import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import denominative.dysasymptotics.linearSize

case class VPerson(name: Text, age: Int, email: Text) derives CanEqual
case class VAddress(street: Text, city: Text, zip: Text) derives CanEqual
case class VContact(person: VPerson, address: VAddress) derives CanEqual

object VProbe:
  var constructions: Int = 0

// The body statement observes construction: decoding must never construct from garbage
// fallback values, so a decode with any failed field must leave the counter untouched.
case class VChecked(name: Text, age: Int) derives CanEqual:
  VProbe.constructions += 1

enum VShape derives CanEqual:
  case VCircle(radius: Int)
  case VSquare(side: Int)

case class VMix(shape: VShape, name: Text) derives CanEqual

case class Issues(items: List[(Text, Json.Error)] = Nil)(using Diagnostics)
extends Error(m"${items.size} validation issues"):
  def +(focus: Text, error: Json.Error): Issues = Issues(items :+ (focus, error))


object ValidationTests extends Suite(m"Jacinta validation tests"):

  // Inline, with a directly-constructed `Validate`: a `raises … tracks …` function VALUE
  // cannot be typed under capture checking (its honest type is a curried dependent context
  // function, an unimplemented compiler restriction), so the decode lambda must beta-reduce
  // away into `protect`'s inline position. See rep/DECISIONS.md.
  private inline def validateJson[result](json: Json)
    (inline decode: Json => result raises Json.Error tracks Json.Focus)
  :   Issues =
    Validate[Issues, [r] =>> r raises Json.Error, Json.Focus]
      ( Issues(),
        { case error: Json.Error => accrual + (prior.let(_.pointer.encode).or(t"#"), error) } )
    . protect(decode(json))

  def run(): Unit =
    suite(m"Single-error decoding (sanity)"):
      test(m"Validate a fully-valid object: no errors accrued"):
        val json = t"""{"name": "Alice", "age": 30, "email": "a@b.c"}""".read[Json]
        validateJson(json)(_.as[VPerson]).items.size
      . assert(_ == 0)

      test(m"Validate single missing field: one error"):
        val json = t"""{"name": "Bob", "age": 1}""".read[Json]
        validateJson(json)(_.as[VPerson]).items.size
      . assert(_ == 1)

      test(m"Single wrong-type field: one error"):
        val json = t"""{"name": "Bob", "age": "young", "email": "b@x"}""".read[Json]
        validateJson(json)(_.as[VPerson]).items.size
      . assert(_ == 1)

    suite(m"Multiple missing fields"):
      test(m"Two missing fields: two errors accrued"):
        val json = t"""{"name": "Alice"}""".read[Json]
        validateJson(json)(_.as[VPerson]).items.size
      . assert(_ == 2)

      test(m"Pointers identify the missing fields"):
        val json = t"""{"name": "Alice"}""".read[Json]
        validateJson(json)(_.as[VPerson]).items.map(_(0).s).pipe(xs => Set.from(xs.stdlib))
      . assert(_ == Set("#/age", "#/email"))

      test(m"Each missing-field error has reason Absent"):
        val json = t"""{"name": "Alice"}""".read[Json]
        validateJson(json)(_.as[VPerson]).items.all:
          case (_, err) => err.reason == Json.Error.Reason.Absent
      . assert(identity)

      test(m"Three missing fields: three errors accrued"):
        val json = t"""{}""".read[Json]
        validateJson(json)(_.as[VPerson]).items.size
      . assert(_ == 3)

    suite(m"Multiple wrong-type fields"):
      test(m"Two wrong types: two errors accrued"):
        val json = t"""{"name": 42, "age": "thirty", "email": "x@y"}""".read[Json]
        validateJson(json)(_.as[VPerson]).items.size
      . assert(_ == 2)

      test(m"Pointers identify the wrong-type fields"):
        val json = t"""{"name": 42, "age": "thirty", "email": "x@y"}""".read[Json]
        validateJson(json)(_.as[VPerson]).items.map(_(0).s).pipe(xs => Set.from(xs.stdlib))
      . assert(_ == Set("#/name", "#/age"))

      test(m"Wrong-type errors have reason NotType"):
        val json = t"""{"name": 42, "age": "thirty", "email": "x@y"}""".read[Json]
        validateJson(json)(_.as[VPerson]).items.all:
          case (_, err) => err.reason match
            case Json.Error.Reason.NotType(_, _) => true
            case _                              => false
      . assert(identity)

      test(m"Three wrong-type fields: three errors accrued"):
        val json = t"""{"name": 1, "age": "x", "email": false}""".read[Json]
        validateJson(json)(_.as[VPerson]).items.size
      . assert(_ == 3)

    suite(m"Missing and wrong-type mixed"):
      test(m"One wrong-type + two missing: three errors at the right pointers"):
        val json = t"""{"name": 42}""".read[Json]
        validateJson(json)(_.as[VPerson]).items.map(_(0).s).pipe(xs => Set.from(xs.stdlib))
      . assert(_ == Set("#/name", "#/age", "#/email"))

    suite(m"Nested case-class errors"):
      test(m"Nested object's missing field reports both segments"):
        val json = t"""{"person": {"name": "X", "age": 1, "email": "y@z"},
                        "address": {"street": "S"}}""".read[Json]
        validateJson(json)(_.as[VContact]).items.map(_(0).s).pipe(xs => Set.from(xs.stdlib))
      . assert(_ == Set("#/address/city", "#/address/zip"))

      test(m"Nested wrong-type field reports both segments"):
        val json = t"""{"person": {"name": "C", "age": 25, "email": "c@x"},
                        "address": {"street": "X", "city": 999, "zip": "Z"}}""".read[Json]
        validateJson(json)(_.as[VContact]).items.map(_(0).s).pipe(xs => Set.from(xs.stdlib))
      . assert(_ == Set("#/address/city"))

      test(m"Mixed errors at different depths accrue together"):
        val json = t"""{"person": {"name": "D"},
                        "address": {"street": "X", "city": "Y", "zip": "Z"}}""".read[Json]
        validateJson(json)(_.as[VContact]).items.map(_(0).s).pipe(xs => Set.from(xs.stdlib))
      . assert(_ == Set("#/person/age", "#/person/email"))

      test(m"Errors accumulate across both nested objects"):
        val json = t"""{"person": {"name": 1, "age": "x", "email": false},
                        "address": {"street": 2, "city": 3, "zip": 4}}""".read[Json]
        validateJson(json)(_.as[VContact]).items.size
      . assert(_ == 6)

    suite(m"Gated construction"):
      test(m"Constructor does not run when any field failed"):
        VProbe.constructions = 0
        val json = t"""{"name": "Zoe"}""".read[Json]
        val issues = validateJson(json)(_.as[VChecked])
        (issues.items.size, VProbe.constructions)
      . assert(_ == (1, 0))

      test(m"Constructor runs exactly once when all fields are clean"):
        VProbe.constructions = 0
        val json = t"""{"name": "Zoe", "age": 5}""".read[Json]
        validateJson(json)(_.as[VChecked])
        VProbe.constructions
      . assert(_ == 1)

      test(m"A nested record with failures does not construct its parent"):
        VProbe.constructions = 0
        val json = t"""{"person": {"name": "D"}, "address": {"street": "X"}}""".read[Json]
        validateJson(json)(_.as[VContact]).items.size
      . assert(_ == 4)

    suite(m"Sum discriminators under accrual"):
      import discriminables.jsonByKindDiscriminable

      test(m"A clean sum field decodes; no issues"):
        val json = t"""{"shape": {"kind": "VCircle", "radius": 3}, "name": "ok"}""".read[Json]
        validateJson(json)(_.as[VMix]).items.size
      . assert(_ == 0)

      test(m"A missing discriminator accrues one error without killing the scope"):
        val json = t"""{"shape": {"radius": 3}, "name": 42}""".read[Json]
        validateJson(json)(_.as[VMix]).items.size
      . assert(_ == 2)

      test(m"The missing-discriminator error and its sibling report their pointers"):
        val json = t"""{"shape": {"radius": 3}, "name": 42}""".read[Json]
        validateJson(json)(_.as[VMix]).items.map(_(0).s).pipe(xs => Set.from(xs.stdlib))
      . assert(_ == Set("#/shape", "#/name"))

    suite(m"Streaming-parse accrual"):
      // The direct-parse path: `Tactic[Parse.Error]` comes from the ambient `throwUnsafely`
      // (token-level errors stay fail-fast; a malformed stream cannot be resynchronized), while
      // `Json.Error`s — absent required fields, discovered after the object is consumed — accrue.
      inline def validateRead[result](text: Text)
        (inline decode: Text => result raises Json.Error tracks Json.Focus)
      :   Issues =
        Validate[Issues, [r] =>> r raises Json.Error, Json.Focus]
          ( Issues(),
            { case error: Json.Error => accrual + (prior.let(_.pointer.encode).or(t"#"), error) } )
        . protect(decode(text))

      test(m"Two missing fields accrue on the direct-parse path"):
        validateRead(t"""{"name": "Bob"}""")(_.read[VPerson in Json]).items.size
      . assert(_ == 2)

      test(m"Pointers identify both missing fields on the direct-parse path"):
        validateRead(t"""{"name": "Bob"}""")(_.read[VPerson in Json])
        . items.map(_(0).s).pipe(xs => Set.from(xs.stdlib))
      . assert(_ == Set("#/age", "#/email"))

      test(m"Direct-parse construction is skipped when a field is missing"):
        VProbe.constructions = 0
        val issues = validateRead(t"""{"name": "Zoe"}""")(_.read[VChecked in Json])
        (issues.items.size, VProbe.constructions)
      . assert(_ == (1, 0))

      test(m"Direct-parse construction runs once when clean"):
        VProbe.constructions = 0
        validateRead(t"""{"name": "Zoe", "age": 5}""")(_.read[VChecked in Json])
        VProbe.constructions
      . assert(_ == 1)

    suite(m"Ventures and guards over Json decoding"):
      import dynamicJsonAccess.enabled
      test(m"Failed sibling reads both accrue; dependent steps are skipped"):
        var consistencyRan = false
        var constructed = false

        val json = t"""{"name": 42, "age": "x"}""".read[Json]

        val issues = validateJson(json): json =>
          val name = venture(json.name.as[Text])
          val age = venture(json.age.as[Int])

          venture:
            val consistent = name().length < age()
            consistencyRan = true

          guard:
            constructed = true

        (issues.items.size, consistencyRan, constructed)
      . assert(_ == (2, false, false))

      test(m"Clean reads run the consistency check and the guarded construction"):
        var consistencyRan = false
        var constructed = false

        val json = t"""{"name": "Ada", "age": 36}""".read[Json]

        validateJson(json): json =>
          val name = venture(json.name.as[Text])
          val age = venture(json.age.as[Int])

          venture:
            val consistent = name().length < age()
            consistencyRan = true

          guard:
            constructed = true

        (consistencyRan, constructed)
      . assert(_ == (true, true))

    suite(m"Position-aware focus (tracked Json)"):
      case class Tagged(items: List[(Text, Optional[Int], Optional[Int])] = Nil)
                       (using Diagnostics)
      extends Error(m"${items.size} validation issues"):
        def +(focus: Text, line: Optional[Int], column: Optional[Int]): Tagged =
          Tagged(items :+ (focus, line, column))

      // `withPosition` resolves the focus's pointer against the tracked
      // Json's position index. Costs nothing on the success path because
      // `Json.Focus` is constructed (and `withPosition` invoked) only
      // for errors registered inside the surrounding `focus` block.
      // `as[T]` runs the Decodable's `position` method (which delegates
      // to `Json.Focus.withPosition`) over the accumulated foci once
      // after decoding, so accruals don't need to call `withPosition`.
      inline def validateWithPositions[result](json: Json)
        (inline decode: Json => result raises Json.Error tracks Json.Focus)
      :   List[(Text, Optional[Int], Optional[Int])] =
        Validate[Tagged, [r] =>> r raises Json.Error, Json.Focus]
          ( Tagged(),
            { case error: Json.Error =>
                val position = prior.let(_.position)
                accrual + ( prior.let(_.pointer.encode).or(t"#"),
                            position.let(_.line),
                            position.let(_.column) ) } )
        . protect(decode(json)).items

      test(m"Missing field reports a position on a tracked Json"):
        val source = t"""{"name": "Alice"}"""
        val json = Json.parseTracked(source)
        val results = validateWithPositions(json)(_.as[VPerson])
        results.map(_(0).s).pipe(xs => Set.from(xs.stdlib))
      . assert(_ == Set("#/age", "#/email"))

      test(m"Wrong-type field reports the value's line/column"):
        val source = t"{\n  \"name\": 42,\n  \"age\": 30,\n  \"email\": \"x@y\"\n}"
        val json = Json.parseTracked(source)
        val results = validateWithPositions(json)(_.as[VPerson])
        // `name` value 42 is on line 2; column points at the `4` of `42`.
        results.seek(_(0) == t"#/name").let((_, line, col) => (line, col))
      . assert(_ == (2, 11))

      test(m"Non-tracking Json has Unset positions"):
        val source = t"""{"name": "Alice"}"""
        val json = source.read[Json]
        val results = validateWithPositions(json)(_.as[VPerson])
        results.all((_, line, _) => line == Unset)
      . assert(identity)
