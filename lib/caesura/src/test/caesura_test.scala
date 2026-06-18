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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics

given decimalizer: Decimalizer = Decimalizer(1)

object Tests extends Suite(m"Caesura tests"):
  def run(): Unit =
    suite(m"Parsing tests"):
      import dsvFormats.csvFormat

      test(m"simple parse"):
        t"""hello,world""".read[Sheet].rows.head
      . assert(_ == Dsv(t"hello", t"world"))

      test(m"simple parse with quotes"):
        t""""hello","world"""".read[Sheet].rows.head
      . assert(_ == Dsv(t"hello", t"world"))

      test(m"empty unquoted field at start"):
        t",hello,world".read[Sheet].rows.head
      . assert(_ == Dsv(t"", t"hello", t"world"))

      test(m"empty unquoted field at end"):
        t"hello,world,".read[Sheet].rows.head
      . assert(_ == Dsv(t"hello", t"world", t""))

      test(m"empty unquoted field in middle"):
        t"hello,,world".read[Sheet].rows.head
      . assert(_ == Dsv(t"hello", t"", t"world"))

      test(m"empty quoted field at start"):
        t""""","hello","world"""".read[Sheet].rows.head
      . assert(_ == Dsv(t"", t"hello", t"world"))

      test(m"empty quoted field at end"):
        t""""hello","world",""""".read[Sheet].rows.head
      . assert(_ == Dsv(t"hello", t"world", t""))

      test(m"empty quoted field in middle"):
        t""""hello","","world"""".read[Sheet].rows.head
      . assert(_ == Dsv(t"hello", t"", t"world"))

      test(m"quoted comma"):
        t""""hello,world"""".read[Sheet].rows.head
      . assert(_ == Dsv(t"hello,world"))

      test(m"escaped quotes"):
        t""""hello""world"""".read[Sheet].rows.head
      . assert(_ == Dsv(t"""hello"world"""))

      test(m"misplaced quote"):
        capture[DsvError](t"""hello,wo"rld""".read[Sheet])
      . assert(_ == DsvError(summon[DsvFormat], DsvError.Reason.MisplacedQuote, Prim, Sec, 8))

      test(m"misplaced quote reports row and offset on a later row"):
        capture[DsvError](t"""a,b\nc,d\nef,g"h""".read[Sheet].rows.to(List))
      . assert(_ == DsvError(summon[DsvFormat], DsvError.Reason.MisplacedQuote, Prim.next.next, Sec, 12))

      test(m"multi-line CSV without trailing newline"):
        t"""foo,bar\nbaz,quux""".read[Sheet].rows
      . assert(_ == Stream(Dsv(t"foo", t"bar"), Dsv(t"baz", t"quux")))

      test(m"multi-line CSV with trailing newline"):
        t"""foo,bar\nbaz,quux\n""".read[Sheet].rows
      . assert(_ == Stream(Dsv(t"foo", t"bar"), Dsv(t"baz", t"quux")))

      test(m"multi-line CSV with CR and LF"):
        t"""foo,bar\r\nbaz,quux\r\n""".read[Sheet].rows
      . assert(_ == Stream(Dsv(t"foo", t"bar"), Dsv(t"baz", t"quux")))

      test(m"multi-line CSV with quoted newlines"):
        t""""foo","bar"\n"baz","quux"\n""".read[Sheet].rows
      . assert(_ == Stream(Dsv(t"foo", t"bar"), Dsv(t"baz", t"quux")))

      test(m"multi-line CSV with newlines and quotes in cells"):
        t""""f""oo","Hello\nWorld"\nbaz,"1\n2\n3\n"\n""".read[Sheet].rows
      . assert(_ == Stream(Dsv(t"f\"oo", t"Hello\nWorld"), Dsv(t"baz", t"1\n2\n3\n")))

      test(m"multi-line CSV with quoted quotes adjacent to newlines"):
        t""""f""oo","Hello\nWorld"\nbaz,"1""\n""2\n3\n"\n""".read[Sheet].rows
      . assert(_ == Stream(Dsv(t"f\"oo", t"Hello\nWorld"), Dsv(t"baz", t"1\"\n\"2\n3\n")))

      test(m"CSV with quoted quotes adjacent to delimiters"):
        t""""f""oo","${"\"\""}Hello\nWorld${t"\"\""}"\n""".read[Sheet].rows
      . assert(_ == Stream(Dsv(t"f\"oo", t"\"Hello\nWorld\"")))


    suite(m"Alternative formats"):
      test(m"Parse TSV data without header"):
        import dsvFormats.tsvFormat
        t"Hello\tWorld\n".read[Sheet].rows
      . assert(_ == Stream(Dsv(t"Hello", t"World")))

      test(m"Parse TSV data with header"):
        import dsvFormats.tsvWithHeaderFormat
        t"Greeting\tAddressee\nHello\tWorld\n".read[Sheet]
      . assert(_ == Sheet(Stream(Dsv(IArray(t"Hello", t"World"), Map(t"Greeting" -> 0, t"Addressee" -> 1))), dsvFormats.tsvWithHeaderFormat, IArray(t"Greeting", t"Addressee")))



    suite(m"Dynamic JSON access"):
      import dynamicDsvAccess.enabled

      test(m"Access field by name"):
        import dsvFormats.tsvWithHeaderFormat
        import dsvRedesignations.unchangedRedesignation
        val dsv = t"greeting\taddressee\nHello\tWorld\n".read[Sheet]
        dsv.rows.head.addressee[Text]
      . assert(_ == t"World")

      test(m"Access field by mapped name"):
        import dsvFormats.tsvWithHeaderFormat
        import dsvRedesignations.capitalizedWordsRedesignation
        val dsv = t"Personal Greeting\tTarget Person\nHello\tWorld\n".read[Sheet]
        dsv.rows.head.targetPerson[Text]
      . assert(_ == t"World")

      test(m"Access field by name 2"):
        import dsvFormats.tsvWithHeaderFormat
        import dsvRedesignations.unchangedRedesignation
        val dsv = t"greeting\tnumber\nHello\t23\n".read[Sheet]
        dsv.rows.head.number[Int]
      . assert(_ == 23)



    test(m"decode case class"):
      import dsvFormats.csvFormat
      t"""hello,world""".read[Sheet].rows.head.as[Foo]
    . assert(_ == Foo(t"hello", t"world"))

    test(m"decode complex case class"):
      import dsvFormats.csvFormat
      t"""0.1,two,three,4,five,six""".read[Sheet].rows.head.as[Bar]
    . assert(_ == Bar(0.1, Foo(t"two", t"three"), 4, Foo(t"five", t"six")))

    test(m"`read[T in Dsv]` decodes a single record directly"):
      import dsvFormats.csvFormat
      t"""hello,world""".read[Foo in Dsv]
    . assert(_ == Foo(t"hello", t"world"))

    test(m"`read[T in Dsv]` decodes a record by headings"):
      import dsvFormats.csvWithHeaderFormat
      t"greeting,name\nhello,world".read[Quux in Dsv]
    . assert(_ == Quux(t"world", t"hello"))

    test(m"encode case class"):
      Foo(t"hello", t"world").dsv
    . assert(_ == Dsv(t"hello", t"world"))

    test(m"encode complex case class"):
      Bar(0.1, Foo(t"two", t"three"), 4, Foo(t"five", t"six")).dsv
    . assert(_ == Dsv(t"0.1", t"two", t"three", t"4", t"five", t"six"))

    test(m"convert simple row to string"):
      import dsvFormats.csvFormat
      Sheet(Stream(Dsv(t"hello", t"world"))).show
    . assert(_ == t"""hello,world""")

    test(m"convert complex row to string"):
      import dsvFormats.csvFormat
      Sheet(Stream(Dsv(t"0.1", t"two", t"three", t"4", t"five", t"six"))).show
    . assert(_ == t"""0.1,two,three,4,five,six""")

    test(m"convert row with escaped quote"):
      import dsvFormats.csvFormat
      Sheet(Stream(Dsv(t"hello\"world"))).show
    . assert(_ == t""""hello""world"""")

    test(m"convert row with delimiter in cell"):
      import dsvFormats.csvFormat
      Sheet(Stream(Dsv(t"hello, world", t"test"))).show
    . assert(_ == t""""hello, world",test""")

    test(m"convert row with newline in cell"):
      import dsvFormats.csvFormat
      Sheet(Stream(Dsv(t"line1\nline2", t"test"))).show
    . assert(_ == t""""line1\nline2",test""")

    test(m"convert row with carriage return in cell"):
      import dsvFormats.csvFormat
      Sheet(Stream(Dsv(t"line1\rline2", t"test"))).show
    . assert(_ == t""""line1\rline2",test""")

    test(m"simple parse TSV"):
      import dsvFormats.tsvFormat
      t"hello\tworld".read[Sheet]
    . assert(_ == Sheet(Stream(Dsv(t"hello", t"world")), format = dsvFormats.tsvFormat))

    test(m"decode case class from TSV"):
      import dsvFormats.tsvFormat
      t"hello\tworld".read[Sheet].rows.head.as[Foo]
    . assert(_ == Foo(t"hello", t"world"))

    test(m"decode case class from CSV by headings"):
      import dsvFormats.csvWithHeaderFormat
      t"greeting,name\nhello,world".read[Sheet].rows.head.as[Quux]
    . assert(_ == Quux(t"world", t"hello"))

    test(m"convert case class to TSV"):
      import dsvFormats.tsvFormat
      Seq(Foo(t"hello", t"world")).dsv.show
    . assert(_ == t"hello\tworld")

    suite(m"Optics"):
      import dsvFormats.csvWithHeaderFormat
      import dynamicDsvAccess.enabled
      def sheet: Sheet = t"name,age\nAlice,30\nBob,25".read[Sheet]

      test(m"cell lens reads a cell by column name"):
        summon["name" is Lens from Dsv onto Text](sheet.rows.head)
      . assert(_ == t"Alice")

      test(m"cell lens replaces a cell by column name"):
        sheet.rows.head.lens(_.name = t"Carol").data.head
      . assert(_ == t"Carol")

      test(m"row optic updates a cell in the n-th row"):
        sheet.lens(_(Sec).name = t"Carol").rows.to(List).map(_.data.head)
      . assert(_ == List(t"Alice", t"Carol"))

      test(m"each-row optic updates every row"):
        sheet.lens(_(Each).name = t"X").rows.to(List).map(_.data.head)
      . assert(_ == List(t"X", t"X"))

      test(m"filter-row optic updates only matching rows"):
        sheet.lens(_(Filter[Dsv](_.data.head == t"Bob")).name = t"X")
         .rows.to(List).map(_.data.head)
      . assert(_ == List(t"Alice", t"X"))

    suite(m"Roundtrip"):
      import dsvFormats.csvFormat

      test(m"case class survives encode, render, parse and decode"):
        Bar(0.1, Foo(t"two", t"three"), 4, Foo(t"five", t"six")).dsv.show
         .read[Sheet].rows.head.as[Bar]
      . assert(_ == Bar(0.1, Foo(t"two", t"three"), 4, Foo(t"five", t"six")))

      test(m"quoted cell survives a parse/render roundtrip"):
        t""""hello, world",test""".read[Sheet].show
      . assert(_ == t""""hello, world",test""")

      test(m"multi-row data survives a parse/render roundtrip"):
        t"foo,bar\nbaz,quux".read[Sheet].show
      . assert(_ == t"foo,bar\nbaz,quux")

    suite(m"Optional fields"):
      test(m"an Optional field spans one column"):
        Spannable.derived[Greeting].spans().to(List)
      . assert(_ == List(1, 1))

      test(m"decode a present trailing Optional positionally"):
        import dsvFormats.csvFormat
        t"hello,world".read[Greeting in Dsv]
      . assert(_ == Greeting(t"hello", t"world"))

      test(m"a short row decodes a trailing Optional to Unset"):
        import dsvFormats.csvFormat
        t"hello".read[Greeting in Dsv]
      . assert(_ == Greeting(t"hello", Unset))

      test(m"decode a present Optional column by heading"):
        import dsvFormats.csvWithHeaderFormat
        t"word,name\nhello,world".read[Greeting in Dsv]
      . assert(_ == Greeting(t"hello", t"world"))

      test(m"a missing Optional column decodes to Unset"):
        import dsvFormats.csvWithHeaderFormat
        t"word\nhello".read[Greeting in Dsv]
      . assert(_ == Greeting(t"hello", Unset))

    suite(m"Cell spanning"):
      test(m"a flat product spans one column per field"):
        Spannable.derived[Foo].spans().to(List)
      . assert(_ == List(1, 1))

      test(m"a nested product sums each field's child spans"):
        Spannable.derived[Bar].spans().to(List)
      . assert(_ == List(1, 2, 1, 2))

      test(m"the total column count is the sum of all spans"):
        Spannable.derived[Bar].spans().sum
      . assert(_ == 6)

    suite(m"Field renaming"):
      test(m"unchanged redesignation preserves the field name"):
        import dsvRedesignations.unchangedRedesignation
        summon[DsvRedesignation].transform(t"targetPerson")
      . assert(_ == t"targetPerson")

      test(m"capitalizedWords redesignation maps to capitalised words"):
        import dsvRedesignations.capitalizedWordsRedesignation
        summon[DsvRedesignation].transform(t"targetPerson")
      . assert(_ == t"Target Person")

      test(m"lowerWords redesignation maps to lower-case words"):
        import dsvRedesignations.lowerWordsRedesignation
        summon[DsvRedesignation].transform(t"targetPerson")
      . assert(_ == t"target person")

    AccrualTests()

case class Foo(one: Text, two: Text)
case class Bar(one: Double, foo1: Foo, four: Int, foo2: Foo)
case class Quux(name: Text, greeting: Text)
case class Greeting(word: Text, name: Optional[Text])
