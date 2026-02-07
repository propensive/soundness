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
┃    Soundness, version 0.53.0.                                                                    ┃
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
import turbulence.*

import errorDiagnostics.stackTraces

given decimalizer: Decimalizer = Decimalizer(1)

object Tests extends Suite(m"Caesura tests"):
  def run(): Unit =
    suite(m"Parsing tests"):
      import dsvFormats.csv

      test(m"simple parse"):
        t"""hello,world""".read[Sheet].rows.head
      .assert(_ == Dsv(t"hello", t"world"))

      test(m"simple parse with quotes"):
        t""""hello","world"""".read[Sheet].rows.head
      .assert(_ == Dsv(t"hello", t"world"))

      test(m"empty unquoted field at start"):
        t",hello,world".read[Sheet].rows.head
      .assert(_ == Dsv(t"", t"hello", t"world"))

      test(m"empty unquoted field at end"):
        t"hello,world,".read[Sheet].rows.head
      .assert(_ == Dsv(t"hello", t"world", t""))

      test(m"empty unquoted field in middle"):
        t"hello,,world".read[Sheet].rows.head
      .assert(_ == Dsv(t"hello", t"", t"world"))

      test(m"empty quoted field at start"):
        t""""","hello","world"""".read[Sheet].rows.head
      .assert(_ == Dsv(t"", t"hello", t"world"))

      test(m"empty quoted field at end"):
        t""""hello","world",""""".read[Sheet].rows.head
      .assert(_ == Dsv(t"hello", t"world", t""))

      test(m"empty quoted field in middle"):
        t""""hello","","world"""".read[Sheet].rows.head
      .assert(_ == Dsv(t"hello", t"", t"world"))

      test(m"quoted comma"):
        t""""hello,world"""".read[Sheet].rows.head
      .assert(_ == Dsv(t"hello,world"))

      test(m"escaped quotes"):
        t""""hello""world"""".read[Sheet].rows.head
      .assert(_ == Dsv(t"""hello"world"""))

      test(m"misplaced quote"):
        capture[DsvError](t"""hello,wo"rld""".read[Sheet])
      .assert(_ == DsvError(summon[DsvFormat], DsvError.Reason.MisplacedQuote))

      test(m"multi-line CSV without trailing newline"):
        t"""foo,bar\nbaz,quux""".read[Sheet].rows
      .assert(_ == Stream(Dsv(t"foo", t"bar"), Dsv(t"baz", t"quux")))

      test(m"multi-line CSV with trailing newline"):
        t"""foo,bar\nbaz,quux\n""".read[Sheet].rows
      .assert(_ == Stream(Dsv(t"foo", t"bar"), Dsv(t"baz", t"quux")))

      test(m"multi-line CSV with CR and LF"):
        t"""foo,bar\r\nbaz,quux\r\n""".read[Sheet].rows
      .assert(_ == Stream(Dsv(t"foo", t"bar"), Dsv(t"baz", t"quux")))

      test(m"multi-line CSV with quoted newlines"):
        t""""foo","bar"\n"baz","quux"\n""".read[Sheet].rows
      .assert(_ == Stream(Dsv(t"foo", t"bar"), Dsv(t"baz", t"quux")))

      test(m"multi-line CSV with newlines and quotes in cells"):
        t""""f""oo","Hello\nWorld"\nbaz,"1\n2\n3\n"\n""".read[Sheet].rows
      .assert(_ == Stream(Dsv(t"f\"oo", t"Hello\nWorld"), Dsv(t"baz", t"1\n2\n3\n")))

      test(m"multi-line CSV with quoted quotes adjacent to newlines"):
        t""""f""oo","Hello\nWorld"\nbaz,"1""\n""2\n3\n"\n""".read[Sheet].rows
      .assert(_ == Stream(Dsv(t"f\"oo", t"Hello\nWorld"), Dsv(t"baz", t"1\"\n\"2\n3\n")))

      test(m"CSV with quoted quotes adjacent to delimiters"):
        t""""f""oo","${"\"\""}Hello\nWorld${t"\"\""}"\n""".read[Sheet].rows
      .assert(_ == Stream(Dsv(t"f\"oo", t"\"Hello\nWorld\"")))


    suite(m"Alternative formats"):
      test(m"Parse TSV data without header"):
        import dsvFormats.tsv
        t"Hello\tWorld\n".read[Sheet].rows
      .assert(_ == Stream(Dsv(t"Hello", t"World")))

      test(m"Parse TSV data with header"):
        import dsvFormats.tsvWithHeader
        t"Greeting\tAddressee\nHello\tWorld\n".read[Sheet]
      .assert(_ == Sheet(Stream(Dsv(IArray(t"Hello", t"World"), Map(t"Greeting" -> 0, t"Addressee" -> 1))), dsvFormats.tsvWithHeader, IArray(t"Greeting", t"Addressee")))



    suite(m"Dynamic JSON access"):
      import dynamicDsvAccess.enabled

      test(m"Access field by name"):
        import dsvFormats.tsvWithHeader
        import dsvRedesignations.unchanged
        val dsv = t"greeting\taddressee\nHello\tWorld\n".read[Sheet]
        dsv.rows.head.addressee[Text]
      .assert(_ == t"World")

      test(m"Access field by mapped name"):
        import dsvFormats.tsvWithHeader
        import dsvRedesignations.capitalizedWords
        val dsv = t"Personal Greeting\tTarget Person\nHello\tWorld\n".read[Sheet]
        dsv.rows.head.targetPerson[Text]
      .assert(_ == t"World")

      test(m"Access field by name 2"):
        import dsvFormats.tsvWithHeader
        import dsvRedesignations.unchanged
        val dsv = t"greeting\tnumber\nHello\t23\n".read[Sheet]
        dsv.rows.head.number[Int]
      .assert(_ == 23)



    test(m"decode case class"):
      import dsvFormats.csv
      t"""hello,world""".read[Sheet].rows.head.as[Foo]
    .assert(_ == Foo(t"hello", t"world"))

    test(m"decode complex case class"):
      import dsvFormats.csv
      t"""0.1,two,three,4,five,six""".read[Sheet].rows.head.as[Bar]
    .assert(_ == Bar(0.1, Foo(t"two", t"three"), 4, Foo(t"five", t"six")))

    test(m"encode case class"):
      Foo(t"hello", t"world").dsv
    .assert(_ == Dsv(t"hello", t"world"))

    test(m"encode complex case class"):
      Bar(0.1, Foo(t"two", t"three"), 4, Foo(t"five", t"six")).dsv
    .assert(_ == Dsv(t"0.1", t"two", t"three", t"4", t"five", t"six"))

    test(m"convert simple row to string"):
      import dsvFormats.csv
      Sheet(Stream(Dsv(t"hello", t"world"))).show
    .assert(_ == t"""hello,world""")

    test(m"convert complex row to string"):
      import dsvFormats.csv
      Sheet(Stream(Dsv(t"0.1", t"two", t"three", t"4", t"five", t"six"))).show
    .assert(_ == t"""0.1,two,three,4,five,six""")

    test(m"convert row with escaped quote"):
      import dsvFormats.csv
      Sheet(Stream(Dsv(t"hello\"world"))).show
    .assert(_ == t""""hello""world"""")

    test(m"simple parse TSV"):
      import dsvFormats.tsv
      t"hello\tworld".read[Sheet]
    .assert(_ == Sheet(Stream(Dsv(t"hello", t"world")), format = dsvFormats.tsv))

    test(m"decode case class from TSV"):
      import dsvFormats.tsv
      t"hello\tworld".read[Sheet].rows.head.as[Foo]
    .assert(_ == Foo(t"hello", t"world"))

    test(m"decode case class from CSV by headings"):
      import dsvFormats.csvWithHeader
      t"greeting,name\nhello,world".read[Sheet].rows.head.as[Quux]
    .assert(_ == Quux(t"world", t"hello"))

    test(m"convert case class to TSV"):
      import dsvFormats.tsv
      Seq(Foo(t"hello", t"world")).dsv.show
    .assert(_ == t"hello\tworld")

case class Foo(one: Text, two: Text)
case class Bar(one: Double, foo1: Foo, four: Int, foo2: Foo)
case class Quux(name: Text, greeting: Text)
