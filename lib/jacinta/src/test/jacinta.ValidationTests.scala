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
┃    Soundness, version 0.63.0.                                                                    ┃
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

case class VPerson(name: Text, age: Int, email: Text) derives CanEqual
case class VAddress(street: Text, city: Text, zip: Text) derives CanEqual
case class VContact(person: VPerson, address: VAddress) derives CanEqual

case class Issues(items: List[(Text, JsonError)] = Nil)(using Diagnostics)
extends Error(m"${items.length} validation issues"):
  def +(focus: Text, error: JsonError): Issues = Issues(items :+ (focus, error))


object ValidationTests extends Suite(m"Jacinta validation tests"):

  // Inline, with a directly-constructed `Validate`: a `raises … tracks …` function VALUE
  // cannot be typed under capture checking (its honest type is a curried dependent context
  // function, an unimplemented compiler restriction), so the decode lambda must beta-reduce
  // away into `protect`'s inline position. See rep/DECISIONS.md.
  private inline def validateJson[result](json: Json)
    (inline decode: Json => result raises JsonError tracks Json.Focus)
  :   Issues =
    Validate[Issues, [r] =>> r raises JsonError, Json.Focus]
      ( Issues(),
        { case error: JsonError => accrual + (prior.let(_.pointer.encode).or(t"#"), error) } )
    . protect(decode(json))

  def run(): Unit =
    suite(m"Single-error decoding (sanity)"):
      test(m"Validate a fully-valid object: no errors accrued"):
        val json = t"""{"name": "Alice", "age": 30, "email": "a@b.c"}""".read[Json]
        validateJson(json)(_.as[VPerson]).items.length
      . assert(_ == 0)

      test(m"Validate single missing field: one error"):
        val json = t"""{"name": "Bob", "age": 1}""".read[Json]
        validateJson(json)(_.as[VPerson]).items.length
      . assert(_ == 1)

      test(m"Single wrong-type field: one error"):
        val json = t"""{"name": "Bob", "age": "young", "email": "b@x"}""".read[Json]
        validateJson(json)(_.as[VPerson]).items.length
      . assert(_ == 1)

    suite(m"Multiple missing fields"):
      test(m"Two missing fields: two errors accrued"):
        val json = t"""{"name": "Alice"}""".read[Json]
        validateJson(json)(_.as[VPerson]).items.length
      . assert(_ == 2)

      test(m"Pointers identify the missing fields"):
        val json = t"""{"name": "Alice"}""".read[Json]
        validateJson(json)(_.as[VPerson]).items.map(_(0).s).to(Set)
      . assert(_ == Set("#/age", "#/email"))

      test(m"Each missing-field error has reason Absent"):
        val json = t"""{"name": "Alice"}""".read[Json]
        validateJson(json)(_.as[VPerson]).items.all:
          case (_, err) => err.reason == JsonError.Reason.Absent
      . assert(identity)

      test(m"Three missing fields: three errors accrued"):
        val json = t"""{}""".read[Json]
        validateJson(json)(_.as[VPerson]).items.length
      . assert(_ == 3)

    suite(m"Multiple wrong-type fields"):
      test(m"Two wrong types: two errors accrued"):
        val json = t"""{"name": 42, "age": "thirty", "email": "x@y"}""".read[Json]
        validateJson(json)(_.as[VPerson]).items.length
      . assert(_ == 2)

      test(m"Pointers identify the wrong-type fields"):
        val json = t"""{"name": 42, "age": "thirty", "email": "x@y"}""".read[Json]
        validateJson(json)(_.as[VPerson]).items.map(_(0).s).to(Set)
      . assert(_ == Set("#/name", "#/age"))

      test(m"Wrong-type errors have reason NotType"):
        val json = t"""{"name": 42, "age": "thirty", "email": "x@y"}""".read[Json]
        validateJson(json)(_.as[VPerson]).items.all:
          case (_, err) => err.reason match
            case JsonError.Reason.NotType(_, _) => true
            case _                              => false
      . assert(identity)

      test(m"Three wrong-type fields: three errors accrued"):
        val json = t"""{"name": 1, "age": "x", "email": false}""".read[Json]
        validateJson(json)(_.as[VPerson]).items.length
      . assert(_ == 3)

    suite(m"Missing and wrong-type mixed"):
      test(m"One wrong-type + two missing: three errors at the right pointers"):
        val json = t"""{"name": 42}""".read[Json]
        validateJson(json)(_.as[VPerson]).items.map(_(0).s).to(Set)
      . assert(_ == Set("#/name", "#/age", "#/email"))

    suite(m"Nested case-class errors"):
      test(m"Nested object's missing field reports both segments"):
        val json = t"""{"person": {"name": "X", "age": 1, "email": "y@z"},
                        "address": {"street": "S"}}""".read[Json]
        validateJson(json)(_.as[VContact]).items.map(_(0).s).to(Set)
      . assert(_ == Set("#/address/city", "#/address/zip"))

      test(m"Nested wrong-type field reports both segments"):
        val json = t"""{"person": {"name": "C", "age": 25, "email": "c@x"},
                        "address": {"street": "X", "city": 999, "zip": "Z"}}""".read[Json]
        validateJson(json)(_.as[VContact]).items.map(_(0).s).to(Set)
      . assert(_ == Set("#/address/city"))

      test(m"Mixed errors at different depths accrue together"):
        val json = t"""{"person": {"name": "D"},
                        "address": {"street": "X", "city": "Y", "zip": "Z"}}""".read[Json]
        validateJson(json)(_.as[VContact]).items.map(_(0).s).to(Set)
      . assert(_ == Set("#/person/age", "#/person/email"))

      test(m"Errors accumulate across both nested objects"):
        val json = t"""{"person": {"name": 1, "age": "x", "email": false},
                        "address": {"street": 2, "city": 3, "zip": 4}}""".read[Json]
        validateJson(json)(_.as[VContact]).items.length
      . assert(_ == 6)

    suite(m"Position-aware focus (tracked Json)"):
      case class Tagged(items: List[(Text, Optional[Int], Optional[Int])] = Nil)
                       (using Diagnostics)
      extends Error(m"${items.length} validation issues"):
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
        (inline decode: Json => result raises JsonError tracks Json.Focus)
      :   List[(Text, Optional[Int], Optional[Int])] =
        Validate[Tagged, [r] =>> r raises JsonError, Json.Focus]
          ( Tagged(),
            { case error: JsonError =>
                val position = prior.let(_.position)
                accrual + ( prior.let(_.pointer.encode).or(t"#"),
                            position.let(_.line),
                            position.let(_.column) ) } )
        . protect(decode(json)).items

      test(m"Missing field reports a position on a tracked Json"):
        val source = t"""{"name": "Alice"}"""
        val json = Json.parseTracked(source)
        val results = validateWithPositions(json)(_.as[VPerson])
        results.map(_(0).s).to(Set)
      . assert(_ == Set("#/age", "#/email"))

      test(m"Wrong-type field reports the value's line/column"):
        val source = t"{\n  \"name\": 42,\n  \"age\": 30,\n  \"email\": \"x@y\"\n}"
        val json = Json.parseTracked(source)
        val results = validateWithPositions(json)(_.as[VPerson])
        // `name` value 42 is on line 2; column points at the `4` of `42`.
        results.find(_(0) == t"#/name").map((_, line, col) => (line, col))
      . assert(_ == Some((2, 11)))

      test(m"Non-tracking Json has Unset positions"):
        val source = t"""{"name": "Alice"}"""
        val json = source.read[Json]
        val results = validateWithPositions(json)(_.as[VPerson])
        results.forall((_, line, _) => line == Unset)
      . assert(identity)
