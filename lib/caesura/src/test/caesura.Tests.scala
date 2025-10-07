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
┃    Soundness, version 0.43.0.                                                                    ┃
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
package caesura

import anticipation.*
import contingency.*, strategies.throwUnsafely
import fulminate.*
import gossamer.*
import probably.*
import proscenium.*
import rudiments.*
import spectacular.*

import errorDiagnostics.stackTraces

given decimalizer: Decimalizer = Decimalizer(1)

object Tests extends Suite(m"Caesura tests"):
  def run(): Unit =
    suite(m"Parsing tests"):
      import dsvFormats.csv

      test(m"simple parse"):
        Dsv.parse(t"""hello,world""").rows.head
      .assert(_ == Row(t"hello", t"world"))

      test(m"simple parse with quotes"):
        Dsv.parse(t""""hello","world"""").rows.head
      .assert(_ == Row(t"hello", t"world"))

      test(m"empty unquoted field at start"):
        Dsv.parse(t",hello,world").rows.head
      .assert(_ == Row(t"", t"hello", t"world"))

      test(m"empty unquoted field at end"):
        Dsv.parse(t"hello,world,").rows.head
      .assert(_ == Row(t"hello", t"world", t""))

      test(m"empty unquoted field in middle"):
        Dsv.parse(t"hello,,world").rows.head
      .assert(_ == Row(t"hello", t"", t"world"))

      test(m"empty quoted field at start"):
        Dsv.parse(t""""","hello","world"""").rows.head
      .assert(_ == Row(t"", t"hello", t"world"))

      test(m"empty quoted field at end"):
        Dsv.parse(t""""hello","world",""""").rows.head
      .assert(_ == Row(t"hello", t"world", t""))

      test(m"empty quoted field in middle"):
        Dsv.parse(t""""hello","","world"""").rows.head
      .assert(_ == Row(t"hello", t"", t"world"))

      test(m"quoted comma"):
        Dsv.parse(t""""hello,world"""").rows.head
      .assert(_ == Row(t"hello,world"))

      test(m"escaped quotes"):
        Dsv.parse(t""""hello""world"""").rows.head
      .assert(_ == Row(t"""hello"world"""))

      test(m"misplaced quote"):
        capture(Dsv.parse(t"""hello,wo"rld"""))
      .assert(_ == DsvError(summon[DsvFormat], DsvError.Reason.MisplacedQuote))

      test(m"multi-line CSV without trailing newline"):
        Dsv.parse(t"""foo,bar\nbaz,quux""").rows
      .assert(_ == Stream(Row(t"foo", t"bar"), Row(t"baz", t"quux")))

      test(m"multi-line CSV with trailing newline"):
        Dsv.parse(t"""foo,bar\nbaz,quux\n""").rows
      .assert(_ == Stream(Row(t"foo", t"bar"), Row(t"baz", t"quux")))

      test(m"multi-line CSV with CR and LF"):
        Dsv.parse(t"""foo,bar\r\nbaz,quux\r\n""").rows
      .assert(_ == Stream(Row(t"foo", t"bar"), Row(t"baz", t"quux")))

      test(m"multi-line CSV with quoted newlines"):
        Dsv.parse(t""""foo","bar"\n"baz","quux"\n""").rows
      .assert(_ == Stream(Row(t"foo", t"bar"), Row(t"baz", t"quux")))

      test(m"multi-line CSV with newlines and quotes in cells"):
        Dsv.parse(t""""f""oo","Hello\nWorld"\nbaz,"1\n2\n3\n"\n""").rows
      .assert(_ == Stream(Row(t"f\"oo", t"Hello\nWorld"), Row(t"baz", t"1\n2\n3\n")))

      test(m"multi-line CSV with quoted quotes adjacent to newlines"):
        Dsv.parse(t""""f""oo","Hello\nWorld"\nbaz,"1""\n""2\n3\n"\n""").rows
      .assert(_ == Stream(Row(t"f\"oo", t"Hello\nWorld"), Row(t"baz", t"1\"\n\"2\n3\n")))

      test(m"multi-line CSV with quoted quotes adjacent to open/close quotes"):
        Dsv.parse(t""""f""oo","${"\"\""}Hello\nWorld${t"\"\""}"\n""").rows
      .assert(_ == Stream(Row(t"f\"oo", t"\"Hello\nWorld\"")))

    suite(m"Alternative formats"):
      test(m"Parse TSV data"):
        import dsvFormats.tsv
        Dsv.parse(t"Hello\tWorld\n").rows
      .assert(_ == Stream(Row(t"Hello", t"World")))

      test(m"Parse TSV data"):
        import dsvFormats.tsvWithHeader
        Dsv.parse(t"Greeting\tAddressee\nHello\tWorld\n")
      .assert(_ == Dsv(Stream(Row(IArray(t"Hello", t"World"), Map(t"Greeting" -> 0, t"Addressee" -> 1))), dsvFormats.tsvWithHeader, IArray(t"Greeting", t"Addressee")))

    suite(m"Dynamic JSON access"):
      import dynamicDsvAccess.enabled

      test(m"Access field by name"):
        import dsvFormats.tsvWithHeader
        import dsvRedesignations.unchanged
        val dsv = Dsv.parse(t"greeting\taddressee\nHello\tWorld\n")
        dsv.rows.head.addressee[Text]
      .assert(_ == t"World")

      test(m"Access field by mapped name"):
        import dsvFormats.tsvWithHeader
        import dsvRedesignations.capitalizedWords
        val dsv = Dsv.parse(t"Personal Greeting\tTarget Person\nHello\tWorld\n")
        dsv.rows.head.targetPerson[Text]
      .assert(_ == t"World")

      test(m"Access field by name 2"):
        import dsvFormats.tsvWithHeader
        import dsvRedesignations.unchanged
        val dsv = Dsv.parse(t"greeting\tnumber\nHello\t23\n")
        dsv.rows.head.number[Int]
      .assert(_ == 23)

    test(m"decode case class"):
      import dsvFormats.csv
      Dsv.parse(t"""hello,world""").rows.head.as[Foo]
    .assert(_ == Foo(t"hello", t"world"))

    test(m"decode complex case class"):
      import dsvFormats.csv
      Dsv.parse(t"""0.1,two,three,4,five,six""").rows.head.as[Bar]
    .assert(_ == Bar(0.1, Foo(t"two", t"three"), 4, Foo(t"five", t"six")))

    test(m"encode case class"):
      Foo(t"hello", t"world").dsv
    .assert(_ == Row(t"hello", t"world"))

    test(m"encode complex case class"):
      Bar(0.1, Foo(t"two", t"three"), 4, Foo(t"five", t"six")).dsv
    .assert(_ == Row(t"0.1", t"two", t"three", t"4", t"five", t"six"))

    test(m"convert simple row to string"):
      import dsvFormats.csv
      Dsv(Stream(Row(t"hello", t"world"))).show
    .assert(_ == t"""hello,world""")

    test(m"convert complex row to string"):
      import dsvFormats.csv
      Dsv(Stream(Row(t"0.1", t"two", t"three", t"4", t"five", t"six"))).show
    .assert(_ == t"""0.1,two,three,4,five,six""")

    test(m"convert row with escaped quote"):
      import dsvFormats.csv
      Dsv(Stream(Row(t"hello\"world"))).show
    .assert(_ == t""""hello""world"""")

    test(m"simple parse TSV"):
      import dsvFormats.tsv
      Dsv.parse(t"hello\tworld")
    .assert(_ == Dsv(Stream(Row(t"hello", t"world")), format = dsvFormats.tsv))

    test(m"decode case class from TSV"):
      import dsvFormats.tsv
      Dsv.parse(t"hello\tworld").rows.head.as[Foo]
    .assert(_ == Foo(t"hello", t"world"))

    test(m"decode case class from CSV by headings"):
      import dsvFormats.csvWithHeader
      Dsv.parse(t"greeting,name\nhello,world").rows.head.as[Quux]
    .assert(_ == Quux(t"world", t"hello"))

    test(m"convert case class to TSV"):
      import dsvFormats.tsv
      Seq(Foo(t"hello", t"world")).dsv.show
    .assert(_ == t"hello\tworld")

case class Foo(one: Text, two: Text)
case class Bar(one: Double, foo1: Foo, four: Int, foo2: Foo)
case class Quux(name: Text, greeting: Text)
