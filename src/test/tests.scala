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
import anticipation.*
import spectacular.*
import perforate.*, errorHandlers.throwUnsafely

import hieroglyph.*

given decimalizer: Decimalizer = Decimalizer(1)

object Tests extends Suite(t"Caesura tests"):
  def run(): Unit =
    test(t"simple parse"):
      CsvDoc.parseLine(t"""hello,world""")
    .assert(_ == Csv(t"hello", t"world"))

    test(t"simple parse with quotes"):
      CsvDoc.parseLine(t""""hello","world"""")
    .assert(_ == Csv(t"hello", t"world"))

    test(t"empty unquoted field at start"):
      CsvDoc.parseLine(t",hello,world")
    .assert(_ == Csv(t"", t"hello", t"world"))

    test(t"empty unquoted field at end"):
      CsvDoc.parseLine(t"hello,world,")
    .assert(_ == Csv(t"hello", t"world", t""))

    test(t"empty unquoted field in middle"):
      CsvDoc.parseLine(t"hello,,world")
    .assert(_ == Csv(t"hello", t"", t"world"))

    test(t"empty quoted field at start"):
      CsvDoc.parseLine(t""""","hello","world"""")
    .assert(_ == Csv(t"", t"hello", t"world"))
    test(t"empty quoted field at end"):
      CsvDoc.parseLine(t""""hello","world",""""")
    .assert(_ == Csv(t"hello", t"world", t""))

    test(t"empty quoted field in middle"):
      CsvDoc.parseLine(t""""hello","","world"""")
    .assert(_ == Csv(t"hello", t"", t"world"))

    test(t"quoted comma"):
      CsvDoc.parseLine(t""""hello,world"""")
    .assert(_ == Csv(t"hello,world"))

    test(t"escaped quotes"):
      CsvDoc.parseLine(t""""hello""world"""")
    .assert(_ == Csv(t"""hello"world"""))

    test(t"decode case class"):
      CsvDoc.parseLine(t"""hello,world""").as[Foo]
    .assert(_ == Foo(t"hello", t"world"))

    test(t"decode complex case class"):
      CsvDoc.parseLine(t"""0.1,two,three,4,five,six""").as[Bar]
    .assert(_ == Bar(0.1, Foo(t"two", t"three"), 4, Foo(t"five", t"six")))

    test(t"encode case class"):
      Foo(t"hello", t"world").csv
    .assert(_ == Csv(t"hello", t"world"))

    test(t"encode complex case class"):
      Bar(0.1, Foo(t"two", t"three"), 4, Foo(t"five", t"six")).csv
    .assert(_ == Csv(t"0.1", t"two", t"three", t"4", t"five", t"six"))

    test(t"convert simple row to string"):
      CsvDoc(List(Csv(t"hello", t"world"))).show
    .assert(_ == t"""hello,world""")

    test(t"convert complex row to string"):
      CsvDoc(List(Csv(t"0.1", t"two", t"three", t"4", t"five", t"six"))).show
    .assert(_ == t"""0.1,two,three,4,five,six""")

    test(t"convert row with escaped quote"):
      CsvDoc(List(Csv(t"hello\"world"))).show
    .assert(_ == t""""hello""world"""")

    test(t"simple parse tsv"):
      Tsv.parseLine(t"hello\tworld")
    .assert(_ == Csv(t"hello", t"world"))

    test(t"decode case class from tsv"):
      Tsv.parseLine(t"hello\tworld").as[Foo]
    .assert(_ == Foo(t"hello", t"world"))

    test(t"convert case class to tsv"):
      Seq(Foo(t"hello", t"world")).tsv.show
    .assert(_ == t"hello\tworld")

case class Foo(one: Text, two: Text)
case class Bar(one: Double, foo1: Foo, four: Int, foo2: Foo)
