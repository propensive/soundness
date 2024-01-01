/*
    Caesura, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package caesura

import probably.*
import gossamer.*
import rudiments.*
import turbulence.*

import unsafeExceptions.canThrowAny
import encodings.Utf8

given Log(_ => SystemOut.sink)

object Tests extends Suite(t"Caesura tests"):
  def run(): Unit =
    test(t"simple parse"):
      Csv.parseLine(t"""hello,world""")
    .assert(_ == Row(t"hello", t"world"))

    test(t"simple parse with quotes"):
      Csv.parseLine(t""""hello","world"""")
    .assert(_ == Row(t"hello", t"world"))

    test(t"empty unquoted field at start"):
      Csv.parseLine(t",hello,world")
    .assert(_ == Row(t"", t"hello", t"world"))

    test(t"empty unquoted field at end"):
      Csv.parseLine(t"hello,world,")
    .assert(_ == Row(t"hello", t"world", t""))

    test(t"empty unquoted field in middle"):
      Csv.parseLine(t"hello,,world")
    .assert(_ == Row(t"hello", t"", t"world"))

    test(t"empty quoted field at start"):
      Csv.parseLine(t""""","hello","world"""")
    .assert(_ == Row(t"", t"hello", t"world"))
    test(t"empty quoted field at end"):
      Csv.parseLine(t""""hello","world",""""")
    .assert(_ == Row(t"hello", t"world", t""))

    test(t"empty quoted field in middle"):
      Csv.parseLine(t""""hello","","world"""")
    .assert(_ == Row(t"hello", t"", t"world"))

    test(t"quoted comma"):
      Csv.parseLine(t""""hello,world"""")
    .assert(_ == Row(t"hello,world"))

    test(t"escaped quotes"):
      Csv.parseLine(t""""hello""world"""")
    .assert(_ == Row(t"""hello"world"""))

    test(t"decode case class"):
      Csv.parseLine(t"""hello,world""").as[Foo]
    .assert(_ == Foo(t"hello", t"world"))

    test(t"decode complex case class"):
      Csv.parseLine(t"""0.1,two,three,4,five,six""").as[Bar]
    .assert(_ == Bar(0.1, Foo(t"two", t"three"), 4, Foo(t"five", t"six")))

    test(t"encode case class"):
      Row.from(Foo(t"hello", t"world"))
    .assert(_ == Row(t"hello", t"world"))

    test(t"encode complex case class"):
      Row.from(Bar(0.1, Foo(t"two", t"three"), 4, Foo(t"five", t"six")))
    .assert(_ == Row(t"0.1", t"two", t"three", t"4", t"five", t"six"))

    test(t"convert simple row to string"):
      Csv(List(Row(t"hello", t"world"))).show.s
    .assert(_ == """hello,world""")

    test(t"convert complex row to string"):
      Csv(List(Row(t"0.1", t"two", t"three", t"4", t"five", t"six"))).show.s
    .assert(_ == """0.1,two,three,4,five,six""") // "

    test(t"convert row with escaped quote"):
      Csv(List(Row(t"hello\"world"))).show.s
    .assert(_ == """"hello""world"""")

    test(t"simple parse tsv"):
      Tsv.parseLine(t"hello\tworld")
    .assert(_ == Row(t"hello", t"world"))

    test(t"decode case class from tsv"):
      Tsv.parseLine(t"hello\tworld").as[Foo]
    .assert(_ == Foo(t"hello", t"world"))

    test(t"convert case class to tsv"):
      Seq(Foo(t"hello", t"world")).tsv.show.s
    .assert(_ == "hello\tworld") // "

case class Foo(one: Text, two: Text)
case class Bar(one: Double, foo1: Foo, four: Int, foo2: Foo)
