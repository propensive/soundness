/*
    Villainy, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package villainy

import contingency.*
import fulminate.*
import gossamer.*
import hieroglyph.*, charEncoders.utf8
import jacinta.*
import kaleidoscope.*
import polyvinyl.*
import probably.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*

import errorDiagnostics.stackTraces
import strategies.throwUnsafely

object Tests extends Suite(m"Villainy tests"):
  def run(): Unit =
    val record = test(m"Construct a new record"):

      val spec: Json =
        t"""{
          "name": "Jim",
          "sub": { "date": "11/12/20" },
          "children": [
            {"height": 100, "weight": 0.8, "color": "green" },
            {"height": 9, "weight": 30.0, "color": "#ff0000"}
          ],
          "pattern": "a.b",
          "domain": "example.com"
        }""".read[Json]

      ExampleSchema.record(spec)
    .check()

    test(m"Get a text value"):
      record.name
    .assert(_ == t"Jim")

    test(m"Get an integer value"):
      record.age
    .assert(_ == Unset)

    test(m"Get an array value"):
      record.children
    .assert()

    test(m"Get the head of an array"):
      record.children.head
    .assert()

    test(m"Get a nested value"):
      record.children.head.weight
    .assert(_ == 0.8)

    test(m"A bad pattern-checked value throws an exception"):
      // FIXME: This should use `capture` to grab the error, but it doesn't seem to work, perhaps because
      // `throwUnsafely` has higher precedence.
      capture[JsonSchemaError]:
        val result = record.children.head.color
    .assert(_ == JsonSchemaError(JsonSchemaError.Reason.PatternMismatch(t"green", r"#[0-9a-f]{6}")))

    test(m"Get a color"):
      record.children(1).color
    .assert(_ == t"#ff0000")

    test(m"Get a nested item value"):
      record.sub.date
    .assert(_ == t"11/12/20")

    test(m"Get a regex value"):
      record.pattern
    .assert(_ == Regex(t"a.b"))

    test(m"Get some values in a list"):
      capture:
        record.children.map { elem => elem.height }.to(List)
    .assert(_ == BoundsError(100, 1, 99))
