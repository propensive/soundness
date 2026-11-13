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

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import codepages.utf8Codepage
import denominative.dysasymptotics.linearSize // the suite reads list sizes; the cost is irrelevant here

// RaTEL (spec/ratel.md): the layout of rows, the canonical writer's bytes for the worked
// example of §13, reading in place, the indexes, losslessness (P5) over the conformance
// corpus, and each of the reader's error conditions.
object RatelTests extends Suite("ratel", m"Stratiform RaTEL tests"):
  private def schemaOf(text: Text): Tels = Tels.Validation.validate(text.read[Tel].as[Tels])

  private def hex(data: Data): Text = data.fuse("")(state+"%02x".format(next & 0xff)).tt

  // A section of an image, by its position in the header's directory.
  private def section(image: Data, number: Int): Data =
    val offset = image.u64(16 + 16*number).toInt
    image.segment(offset.z till (offset + image.u64(24 + 16*number).toInt).z)

  // An image with the bytes from `offset` replaced.
  private def patched(image: Data, offset: Int, bytes: Int*): Data =
    val replacement: List[Int] = bytes.to[List]

    Array.tabulate[Byte](image.length): index =>
      if index < offset || index >= offset + replacement.size then image.u8(index).toByte
      else replacement.zip(List.range(offset, offset + replacement.size)).fuse(0.toByte):
        if next(1) == index then next(0).toByte else state

  private def reason(image: Data): Ratel.Error.Reason =
    capture[Ratel.Error](Ratel.open(Mapped(image)).verify()).reason

  private val catalogueSchema: Tels = schemaOf:
    Text("""|tel 1.0 specification.tel/tels:2.1.0
        |
        |name catalogue
        |
        |record Entry
        |  field name String key
        |  field info String optional
        |
        |document
        |  field entry Entry repeatable
        |""".stripMargin)

  private val catalogue: Tel =
    t"tel 1.0\n\nentry\n  name alpha\n  info  First letter\n\nentry\n  name beta\n".read[Tel]

  private val folioSchema: Tels = schemaOf:
    Text("""|tel 1.0 specification.tel/tels:2.1.0
        |
        |name folio
        |
        |scalar Count
        |  encoding u32
        |
        |scalar Genre
        |  intern
        |
        |scalar Id
        |  encoding uuid
        |
        |record Author
        |  field name String
        |  field born Count optional
        |
        |record Stage
        |  field theatre String
        |  field city String optional
        |
        |select Ending
        |  variant death Flag
        |  variant marriage String
        |  variant staged Stage
        |
        |select Mark
        |  variant starred Flag
        |  variant note String
        |
        |record Play
        |  field title String key
        |  field id Id
        |  field genre Genre
        |  field acts Count
        |  field lines Count optional
        |  field author Author
        |  field stage Stage optional
        |  field lost Flag optional
        |  field revised Flag optional repeatable
        |  field canon Flag
        |  field role String optional repeatable
        |    index /
        |  field year Count optional repeatable
        |    index /
        |  select Ending optional
        |  select Mark optional repeatable
        |
        |document
        |  field name String
        |  field play Play optional repeatable
        |    index /genre /acts
        |    index .stage.city
        |    index /author/born
        |""".stripMargin)

  private val folio: Tel =
    Text("""|tel 1.0
        |
        |name  First Folio
        |
        |play
        |  title Hamlet
        |  id 0192b1c2-3d4e-7f5a-8b6c-9d0e1f2a3b4c
        |  genre tragedy
        |  acts 5
        |  lines 4024
        |  author
        |    name  William Shakespeare
        |    born 1564
        |  stage
        |    theatre Globe
        |    city London
        |  revised
        |  revised
        |  canon
        |  role Hamlet
        |  role Ophelia
        |  year 1603
        |  year 1623
        |  death
        |  starred
        |  note quarto
        |
        |play
        |  title  Twelfth Night
        |  id 00000000-0000-0000-0000-000000000001
        |  genre comedy
        |  acts 5
        |  author
        |    name  William Shakespeare
        |  canon
        |  role Viola
        |  marriage Orsino
        |
        |play
        |  title Cardenio
        |  id 00000000-0000-0000-0000-000000000002
        |  genre tragedy
        |  acts 4
        |  author
        |    name  Nick Bottom
        |    born 1579
        |  stage
        |    theatre Blackfriars
        |  lost
        |  canon
        |  staged
        |    theatre Whitehall
        |    city London
        |""".stripMargin).read[Tel]

  case class Author(name: Text, born: Int)

  def run(): Unit =
    suite(m"The worked example (§13)"):
      val image: Data = Ratel.encode(Tel.Type.assign(catalogue, catalogueSchema), catalogueSchema).data

      test(m"the image begins with the magic number and version"):
        hex(image.segment(Prim till 8.z))
      . assert(_ == t"b1c4b5bb01000300")

      test(m"the committed length is the length of the image"):
        image.u64(8)
      . assert(_ == image.length.toLong)

      test(m"the string table is empty"):
        section(image, 1).length
      . assert(_ == 0)

      test(m"the variable region holds each value as an item"):
        hex(section(image, 2))
      . assert: bytes =>
          bytes == List
            ( t"05000000616c7068610000000c0000004669727374206c657474657204000000",
              t"6265746100000000" ).join

      test(m"the tables section holds the directory and the rows"):
        hex(section(image, 3))
      . assert: bytes =>
          bytes == List
            ( t"300000000000000001000000080000000000000000000000",
              t"380000000000000002000000100000000000000000000000",
              t"0000000002000000",
              t"01000000000000000c00000000000000",
              t"000000001c0000000000000000000000" ).join

      test(m"the indexes section holds the sorted entries of the primary index"):
        hex(section(image, 4))
      . assert: bytes =>
          bytes == List
            ( t"10000000000000000200000000000000",
              t"00000000100000001f000000",
              t"0000000001616c706861000000000000",
              t"000000000162657461000000000001",
              t"0000000000" ).join

      test(m"the header carries the document's value hash"):
        hex(image.segment(112.z till 144.z))
      . assert(_ == hex(catalogue.valueHash(catalogueSchema).data))

      test(m"a record is found by its key"):
        val entries = Ratel.open(Mapped(image)).root / t"entry"
        entries.find(t"beta").let { row => (row.index, row.text(t"name"), row.text(t"info")) }
      . assert(_ == (1L, t"beta", Unset))

      test(m"a key which no record has finds nothing"):
        (Ratel.open(Mapped(image)).root / t"entry").find(t"gamma").absent
      . assert(_ == true)

      test(m"an optional member which is present is read"):
        val entries = Ratel.open(Mapped(image)).root / t"entry"
        entries.find(t"alpha").let(_.text(t"info"))
      . assert(_ == t"First letter")

      test(m"the image passes verification"):
        Ratel.open(Mapped(image)).verify()
      . assert(_ == ())

    suite(m"Reading in place"):
      val element = Tel.Type.assign(folio, folioSchema, Tel.Validator.Registry.builtins, Tel.Codec.Bindings.all)
      val image: Ratel = folio.ratel(folioSchema)
      val reader = Ratel.open(image.mapped)
      val plays = reader.root / t"play"
      def play(title: Text): Ratel.Row = plays.find(title).or(throw IllegalArgumentException(title.s))

      test(m"the document materialises to the model which was written"):
        hex(reader.materialise().bintel(folioSchema, Tel.Codec.Bindings.all))
      . assert(_ == hex(element.bintel(folioSchema, Tel.Codec.Bindings.all)))

      test(m"the image passes verification"):
        reader.verify()
      . assert(_ == ())

      test(m"the reader has the schema which the image carries"):
        reader.schema.name
      . assert(_ == t"folio")

      test(m"a scalar of the root is read"):
        reader.root.text(t"name")
      . assert(_ == t"First Folio")

      test(m"a repeatable member has a size"):
        plays.size
      . assert(_ == 3L)

      test(m"a record is read by its position"):
        plays(Sec).text(t"title")
      . assert(_ == t"Twelfth Night")

      test(m"a position beyond the last record is refused"):
        capture[Ratel.Error](plays(Quat)).reason
      . assert(_ == Ratel.Error.Reason.BadRecordIndex)

      test(m"records are visited in the order they were written"):
        var titles: List[Text] = Nil
        plays.each { row => titles = row.text(t"title").or(t"") :: titles }
        titles.reverse
      . assert(_ == List(t"Hamlet", t"Twelfth Night", t"Cardenio"))

      test(m"the primary index orders records by key"):
        plays.sorted.map(_.text(t"title").or(t""))
      . assert(_ == List(t"Cardenio", t"Hamlet", t"Twelfth Night"))

      test(m"a fixed-width scalar is stored inline, as its codec's bytes"):
        hex(play(t"Hamlet").data(t"acts"))
      . assert(_ == t"00000005")

      test(m"an encoded scalar is read as its text"):
        (play(t"Hamlet").text(t"lines"), play(t"Hamlet").text(t"id"))
      . assert(_ == (t"4024", t"0192b1c2-3d4e-7f5a-8b6c-9d0e1f2a3b4c"))

      test(m"an absent optional scalar is unset"):
        (play(t"Cardenio").text(t"lines"), play(t"Cardenio").has(t"lines"))
      . assert(_ == (Unset, false))

      test(m"the bytes of an absent member cannot be read"):
        capture[Ratel.Error](play(t"Cardenio").data(t"lines")).reason
      . assert(_ == Ratel.Error.Reason.Absent(t"lines"))

      test(m"a required struct is read from its parent's row"):
        play(t"Cardenio").record(t"author").let { row => (row.text(t"name"), row.text(t"born")) }
      . assert(_ == (t"Nick Bottom", t"1579"))

      test(m"an optional struct is read from a row of its own"):
        play(t"Hamlet").record(t"stage").let(_.text(t"city"))
      . assert(_ == t"London")

      test(m"an absent optional struct is unset"):
        play(t"Twelfth Night").record(t"stage").absent
      . assert(_ == true)

      test(m"flags are read as present, absent or counted"):
        val hamlet = play(t"Hamlet")
        (hamlet.flag(t"canon"), hamlet.flag(t"lost"), hamlet.count(t"revised"), play(t"Cardenio").flag(t"lost"))
      . assert(_ == (true, false, 2L, true))

      test(m"the values of a repeatable text scalar are read in order"):
        play(t"Hamlet").texts(t"role")
      . assert(_ == List(t"Hamlet", t"Ophelia"))

      test(m"the values of a repeatable fixed-width scalar are read in order"):
        play(t"Hamlet").texts(t"year")
      . assert(_ == List(t"1603", t"1623"))

      test(m"an empty repeatable scalar has no values"):
        play(t"Cardenio").texts(t"role")
      . assert(_ == Nil)

      test(m"an indexed array is searched for a value"):
        val hamlet = play(t"Hamlet")
        (hamlet.holds(t"role", t"Ophelia"), hamlet.holds(t"role", t"Viola"), hamlet.holds(t"year", t"1623"),
            hamlet.holds(t"year", t"1600"), hamlet.holds(t"year", t"sixteen"))
      . assert(_ == (true, false, true, false, false))

      test(m"the variant a select chose is read by its keyword"):
        (play(t"Hamlet").flag(t"death"), play(t"Hamlet").text(t"marriage"),
            play(t"Twelfth Night").text(t"marriage"), play(t"Twelfth Night").flag(t"death"))
      . assert(_ == (true, Unset, t"Orsino", false))

      test(m"a struct variant is read from a row of its own"):
        play(t"Cardenio").record(t"staged").let(_.text(t"theatre"))
      . assert(_ == t"Whitehall")

      test(m"a secondary index finds records by its first path"):
        plays.where(t"/genre", t"tragedy").map(_.text(t"title").or(t""))
      . assert(_ == List(t"Cardenio", t"Hamlet"))

      test(m"a secondary index follows a path through an optional struct"):
        plays.where(t".stage.city", t"London").map(_.text(t"title").or(t""))
      . assert(_ == List(t"Hamlet"))

      test(m"a secondary index follows a path through a flattened struct"):
        plays.where(t"/author/born", t"1564").map(_.text(t"title").or(t""))
      . assert(_ == List(t"Hamlet"))

      test(m"a lookup with no index to answer it is refused"):
        capture[Ratel.Error](plays.where(t"/title", t"Hamlet")).reason
      . assert(_ == Ratel.Error.Reason.Unindexed)

      test(m"a member the record does not have is refused"):
        capture[Ratel.Error](play(t"Hamlet").text(t"epilogue")).reason
      . assert(_ == Ratel.Error.Reason.Unknown(t"epilogue"))

      test(m"a member of another kind is refused"):
        capture[Ratel.Error](play(t"Hamlet").text(t"author")).reason
      . assert(_ == Ratel.Error.Reason.Inapplicable(t"author"))

      test(m"interned values are stored once"):
        section(image.data, 1).length
      . assert(_ == 24)

      test(m"one record decodes to a value"):
        play(t"Hamlet").record(t"author").let(_.as[Author])
      . assert(_ == Author(t"William Shakespeare", 1564))

      test(m"one record materialises without the rest"):
        hex(play(t"Hamlet").record(t"stage").let(_.materialise()).let(_.bintel(folioSchema.copy(document = Tels.Struct(Array.empty, Array.empty)))).or(Array.empty[Byte])).length > 0
      . assert(_ == true)

    suite(m"Losslessness over the conformance corpus (P5)"):
      val codecs = CodecTests.bindings.orElse(Tel.Codec.Bindings.all)

      CorpusLoader.positive.each: testcase =>
        val pragma = testcase.source.utf8.load[Tel].metadata.pragma

        val schema: Optional[Tels] = pragma.let(CorpusLoader.referenceStem(_)).let: stem =>
          if stem == t"tels" then Tels.Axiom.tels
          else CorpusLoader.auxiliarySchema(t"pos", stem).let: auxiliary =>
            Tels.Validation.validate(auxiliary.read[Tel].as[Tels])

        schema.let: schema =>
          test(m"${testcase.stem} survives a round trip through RaTEL"):
            val element =
              Tel.Type.assign(testcase.source.read[Tel], schema, Tel.Validator.Registry.builtins, codecs)

            val reader = Ratel.open(Mapped(Ratel.encode(element, schema, codecs).data), codecs)
            reader.verify()
            hex(reader.materialise().bintel(schema, codecs)) == hex(element.bintel(schema, codecs))
          . assert(_ == true)
