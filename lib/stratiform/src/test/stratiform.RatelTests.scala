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
import filesystemBackends.javaBaseFilesystem
import providers.javaBaseProvider

// RaTEL (spec/ratel.md): the layout of rows, the canonical writer's bytes for the worked
// example of §13, reading in place, the indexes, losslessness (P5) over the conformance
// corpus, and each of the reader's error conditions.
object RatelTests extends Suite("ratel", m"Stratiform RaTEL tests"):
  private def schemaOf(text: Text): Tels = Tels.Validation.validate(text.read[Tel].as[Tels])

  private def hex(data: Data): Text = data.fuse("")(state+"%02x".format(next & 0xff)).tt

  // A section of an image, by its position in the header's directory.
  private def section(image: Data, number: Int): Data =
    val offset = image.u64(16 + 24*number).toInt
    image.segment(offset.z till (offset + image.u64(24 + 24*number).toInt).z)

  // An entry of the header's directory: where a section is, how much of it is in use, and
  // how much it has room for.
  private def directory(image: Data, number: Int): (Long, Long, Long) =
    (image.u64(16 + 24*number), image.u64(24 + 24*number), image.u64(32 + 24*number))

  // Where the rows of a table begin, and where the pages of an index do.
  private def rows(image: Data, table: Int): Int = image.u64(start(image, 3) + 32*table).toInt
  private def pages(image: Data, index: Int): Int = image.u64(start(image, 4) + 32*index).toInt

  private def slice(image: Data, offset: Int, length: Int): Data =
    image.segment(offset.z till (offset + length).z)

  // An image with the bytes from `offset` replaced.
  private def patched(image: Data, offset: Int, bytes: List[Int]): Data =
    Array.tabulate[Byte](image.length): index =>
      if index < offset || index >= offset + bytes.size then image.u8(index).toByte
      else bytes.zip(List.range(offset, offset + bytes.size)).fuse(0.toByte):
        if next(1) == index then next(0).toByte else state

  // The bytes of a little-endian integer.
  private def little(value: Long, width: Int): List[Int] =
    List.range(0, width).map { index => ((value >>> 8*index) & 0xff).toInt }

  // Where a section begins.
  private def start(image: Data, number: Int): Int = image.u64(16 + 24*number).toInt

  // The condition a verifying reader reports for an image.
  private def reason(image: Data): Ratel.Error.Reason =
    capture[Ratel.Error](Ratel(image).open[Ratel](Ratel.Flag.Verify)(())).reason

  private def plain(document: Tel, schema: Tels): Data =
    val element = Tel.Type.assign(document, schema, Tel.Validator.Registry.builtins, Tel.Codec.Bindings.all)
    Ratel.encode(element, schema, digests = false).data

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

  private val folioSchema: Tels = RatelFixture.folioSchema
  private val folio: Tel = RatelFixture.folio

  case class Author(name: Text, born: Int)

  def run(): Unit =
    suite(m"The worked example (§13)"):
      val image: Data = Ratel.encode(Tel.Type.assign(catalogue, catalogueSchema), catalogueSchema).data

      val schemaEnd: Long = 200L + directory(image, 0)(1)

      test(m"the image begins with the magic number, version and flags"):
        hex(image.segment(Prim till 8.z))
      . assert(_ == t"b1c4b5bb01000700")

      test(m"the committed length is the length of the image"):
        image.u64(8)
      . assert(_ == image.length.toLong)

      test(m"the schema section follows the header, and is a multiple of eight"):
        directory(image, 0)
      . assert { (offset, length, capacity) => offset == 200L && length == capacity && length%8 == 0 }

      test(m"the string table is empty"):
        directory(image, 1)
      . assert(_ == (schemaEnd, 0L, 0L))

      test(m"the variable region has no room to spare beyond its padding"):
        directory(image, 2)
      . assert(_ == (schemaEnd, 52L, 56L))

      test(m"the variable region holds the child list, then each value"):
        hex(section(image, 2))
      . assert: bytes =>
          bytes == List
            ( t"02000000020000000000000001000000",
              t"05000000616c706861000000",
              t"0c0000004669727374206c6574746572",
              t"0400000062657461" ).join

      test(m"the directories and the digests are where the extents before them end"):
        (directory(image, 3), directory(image, 4), directory(image, 5))
      . assert: directories =>
          directories ==
            ( (schemaEnd + 56, 64L, 64L), (schemaEnd + 184, 32L, 32L), (schemaEnd + 4312, 64L, 64L) )

      test(m"the table directory places each table's rows"):
        hex(section(image, 3))
      . assert: bytes =>
          bytes == List
            ( hex(Array.tabulate[Byte](8)(index => ((schemaEnd + 120) >>> 8*index).toByte)),
              t"010000000100000010000000ffffffff0000000000000000",
              hex(Array.tabulate[Byte](8)(index => ((schemaEnd + 136) >>> 8*index).toByte)),
              t"020000000200000018000000ffffffff0000000000000000" ).join

      test(m"the root's row has no parent, and refers to its child list"):
        hex(slice(image, rows(image, 0), 16))
      . assert(_ == t"ffffffffffff01000000000000000000")

      test(m"each record's row names its parent and link, then holds its slots"):
        hex(slice(image, rows(image, 1), 48))
      . assert: bytes =>
          bytes == List
            ( t"000000000000010001000000100000001c00000000000000",
              t"0000000000000100000000002c0000000000000000000000" ).join

      test(m"the index directory describes a tree of one leaf"):
        hex(section(image, 4))
      . assert: bytes =>
          bytes == List
            ( hex(Array.tabulate[Byte](8)(index => ((schemaEnd + 216) >>> 8*index).toByte)),
              t"010000000100000000000000ffffffff02000000000c0100" ).join

      test(m"the leaf's header and cell pointers are in key order"):
        hex(slice(image, pages(image, 0), 20))
      . assert(_ == t"01000200d80f0000ffffffff00000000ec0fd80f")

      test(m"the leaf's cells are placed from the end of the page downwards"):
        hex(slice(image, pages(image, 0) + 0xfd8, 40))
      . assert: bytes =>
          bytes == List
            ( t"0f00000000000162657461000000000001000000",
              t"10000000000001616c7068610000000000000000" ).join

      test(m"the rest of the leaf is zero"):
        hex(slice(image, pages(image, 0) + 20, 0xfd8 - 20)).s.forall(_ == '0')
      . assert(_ == true)

      test(m"the header carries the document's value hash"):
        hex(image.segment(168.z till 200.z))
      . assert(_ == hex(catalogue.valueHash(catalogueSchema).data))

      test(m"a sealed image says that it is"):
        Ratel(image).open[Ratel]()(ratel.state)
      . assert(_ == Ratel.State.Sealed)

      test(m"a record is found by its key"):
        Ratel(image).open[Ratel]():
          (ratel.root / t"entry").find(t"beta").let: row =>
            (row.index, row.text(t"name"), row.text(t"info"))

      . assert(_ == (1L, t"beta", Unset))

      test(m"a key which no record has finds nothing"):
        Ratel(image).open[Ratel]()((ratel.root / t"entry").find(t"gamma").absent)
      . assert(_ == true)

      test(m"an optional member which is present is read"):
        Ratel(image).open[Ratel]()((ratel.root / t"entry").find(t"alpha").let(_.text(t"info")))
      . assert(_ == t"First letter")

      test(m"the image passes verification"):
        Ratel.open(Terrain(image)).verify()
      . assert(_ == ())

    suite(m"Reading in place"):
      val element = Tel.Type.assign(folio, folioSchema, Tel.Validator.Registry.builtins, Tel.Codec.Bindings.all)
      val image: Ratel = folio.ratel(folioSchema)
      val reader: Ratel.Reader = Ratel.open(image.terrain)
      given reader.type = reader
      val plays = reader.root / t"play"
      def play(title: Text): Ratel.Row of reader.Topic =
        plays.find(title).or(throw IllegalArgumentException(title.s))

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

    suite(m"Opening an image"):
      val image: Ratel = folio.ratel(folioSchema)

      test(m"an image is opened where it lies in memory"):
        image.open[Ratel]()(ratel.root.text(t"name"))
      . assert(_ == t"First Folio")

      test(m"a provider types the root of an image by its schema"):
        image.open[Ratel]():
          val folio = FolioRecords()
          val name: Text = folio.name
          name
      . assert(_ == t"First Folio")

      test(m"a provider finds a record by its key, and reads its members"):
        image.open[Ratel]():
          val hamlet = FolioRecords().play(t"Hamlet")
          hamlet.let { play => (play.genre: Text, play.acts: Long, play.lines: Optional[Long]) }
      . assert(_ == (t"tragedy", 5L, 4024L))

      test(m"a provider reads a struct as a record of its own"):
        image.open[Ratel]():
          val cardenio = FolioRecords().play(t"Cardenio")
          cardenio.let { play => (play.author.name: Text, play.author.born: Optional[Long]) }
      . assert(_ == (t"Nick Bottom", 1579L))

      test(m"a provider reads an optional struct which is absent as unset"):
        image.open[Ratel]():
          FolioRecords().play(t"Twelfth Night").let(_.stage).absent
      . assert(_ == true)

      test(m"a provider reads flags, and repeatable flags and scalars"):
        image.open[Ratel]():
          FolioRecords().play(t"Hamlet").let: play =>
            (play.canon: Boolean, play.lost: Boolean, play.revised: Long, play.role: List[Text])
      . assert(_ == (true, false, 2L, List(t"Hamlet", t"Ophelia")))

      test(m"a provider reads each variant of a select as an optional member"):
        image.open[Ratel]():
          FolioRecords().play(t"Twelfth Night").let: play =>
            (play.marriage: Optional[Text], play.death: Boolean)
      . assert(_ == (t"Orsino", false))

      test(m"a provider's table is sized, ordered and searched"):
        image.open[Ratel]():
          val plays = FolioRecords().play
          (plays.size, plays.sorted.map(_.title: Text), plays.where(_.genre = t"comedy").size)
      . assert(_ == (3L, List(t"Cardenio", t"Hamlet", t"Twelfth Night"), 1))

      test(m"a table is searched by a path through a record"):
        image.open[Ratel]():
          FolioRecords().play.where(_.stage.city = t"London").map(_.title: Text)
      . assert(_ == List(t"Hamlet"))

      test(m"a table is searched for a value of the type its member has"):
        image.open[Ratel]():
          FolioRecords().play.where(_.author.born = 1564L).map(_.title: Text)
      . assert(_ == List(t"Hamlet"))

      test(m"a search by a member which is not indexed is refused"):
        capture[Ratel.Error]:
          image.open[Ratel]() { FolioRecords().play.where(_.title = t"Hamlet"); () }
        . reason
      . assert(_ == Ratel.Error.Reason.Unindexed)

      test(m"a search by a member the record does not have does not compile"):
        demilitarize:
          image.open[Ratel]() { FolioRecords().play.where(_.playwright = t"Fletcher"); () }
        . map(_.message.tt)
      . assert(_.exists(_.contains(t"the record has no member playwright")))

      test(m"a search for a value of another type does not compile"):
        demilitarize:
          image.open[Ratel]() { FolioRecords().play.where(_.author.born = t"1564"); () }
        . map(_.message.tt)
      . assert(_.exists(_.contains(t"the member born is read as")))

      test(m"a path does not go through a member which is not a record"):
        demilitarize:
          image.open[Ratel]() { FolioRecords().play.where(_.title.length = 6); () }
        . map(_.message.tt)
      . assert(_.exists(_.contains(t"the member title is not a record")))

      test(m"a scalar is read through an instance of the caller's own"):
        given ("u32" is Intensional in Ratel.Provider from Ratel.Cell to Text) = Intensional:
          case Ratel.Cell.Value(text) => t"$text acts"
          case _                      => t""

        image.open[Ratel]()(FolioRecords().play(t"Hamlet").let(_.acts: Text))
      . assert(_ == t"5 acts")

      test(m"a record which contains itself is read as far as its type goes"):
        val lineage: Ratel = LineageFixture.lineage.ratel(LineageFixture.schema)

        lineage.open[Ratel]():
          val root = LineageRecords()
          (root.house: Text, root.heir.let(_.name: Text), root.heir.let(_.heir: Optional[Ratel.Cell]).absent)
      . assert(_ == (t"Lancaster", t"Hal", false))

      test(m"a TEL record which contains itself is read as far as its type goes"):
        val root = LineageTelRecords.record(LineageFixture.lineage)
        (root.house: Text, root.heir.let(_.name: Text), root.heir.let(_.heir: Optional[Tel]).absent)
      . assert(_ == (t"Lancaster", t"Hal", false))

      test(m"a member the schema does not declare is not a member of the record"):
        demilitarize:
          image.open[Ratel]()(FolioRecords().playwright)
        . nil
      . assert(_ == false)

      test(m"a provider refuses an image written with another schema"):
        val other: Ratel = catalogue.ratel(catalogueSchema)
        capture[Ratel.Error](other.open[Ratel]() { FolioRecords(); () }).reason
      . assert(_ == Ratel.Error.Reason.Mismatched)

      test(m"a row may not be read through another reader"):
        demilitarize:
          val first = image.open[Ratel]()((ratel.root / t"play")(Prim))
          image.open[Ratel]()(first.text(t"title"))
        . map(_.message.tt)
      . assert(_.exists(_.contains(t"is read through the reader of that image")))

      test(m"a row is read through its own reader within another's block"):
        val other: Ratel = catalogue.ratel(catalogueSchema)

        image.open[Ratel](): outer ?=>
          val first = (outer.root / t"play")(Prim)
          other.open[Ratel]()(first.text(t"title"))
      . assert(_ == t"Hamlet")

      test(m"a record is read through its own reader within another's block"):
        val other: Ratel = catalogue.ratel(catalogueSchema)

        image.open[Ratel]():
          val hamlet = FolioRecords().play(t"Hamlet")
          other.open[Ratel]()(hamlet.let(_.title: Text))
      . assert(_ == t"Hamlet")

      test(m"the reader may be named in the block"):
        image.open[Ratel]() { reader ?=> (reader.root / t"play").size }
      . assert(_ == 3L)

      test(m"a view of an image is opened"):
        Terrain(image.data).open[Ratel]()(ratel.schema.name)
      . assert(_ == t"folio")

      test(m"an image is verified as it is opened"):
        image.open[Ratel](Ratel.Flag.Verify)(ratel.valueHash.present)
      . assert(_ == true)

      test(m"an image is not opened for writing"):
        capture[Ratel.Error](image.open[Ratel](Read & Write)(ratel.schema.name)).reason
      . assert(_ == Ratel.Error.Reason.Unwritable)

      test(m"a codec of the application's is bound as an image is opened"):
        val schema = schemaOf:
          Text("""|tel 1.0 specification.tel/tels:2.1.0
              |
              |name ledger
              |
              |scalar Amount
              |  encoding decimal-varint
              |
              |document
              |  field amount Amount
              |""".stripMargin)

        val codecs = CodecTests.bindings.orElse(Tel.Codec.Bindings.all)
        val ledger = t"tel 1.0\n\namount 300\n".read[Tel].ratel(schema, codecs)
        ledger.open[Ratel](Ratel.Flag.Codecs(CodecTests.bindings))(ratel.root.text(t"amount"))
      . assert(_ == t"300")

      test(m"an encoding with no codec bound to it is refused"):
        val schema = schemaOf:
          Text("""|tel 1.0 specification.tel/tels:2.1.0
              |
              |name ledger
              |
              |scalar Amount
              |  encoding decimal-varint
              |
              |document
              |  field amount Amount
              |""".stripMargin)

        val codecs = CodecTests.bindings.orElse(Tel.Codec.Bindings.all)
        val ledger = t"tel 1.0\n\namount 300\n".read[Tel].ratel(schema, codecs)
        capture[Ratel.Error](ledger.open[Ratel]()(ratel.root.text(t"amount"))).reason
      . assert(_ == Ratel.Error.Reason.CodecUnresolved)

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

            val reader = Ratel.open(Terrain(Ratel.encode(element, schema, codecs).data), codecs)
            reader.verify()
            hex(reader.materialise().bintel(schema, codecs)) == hex(element.bintel(schema, codecs))
          . assert(_ == true)

    suite(m"Reader error conditions (§14)"):
      import Ratel.Error.Reason
      val image: Data = plain(catalogue, catalogueSchema)
      val variable = start(image, 2)
      val tables = start(image, 3)
      val indexes = start(image, 4)
      val root = rows(image, 0)
      val entries = rows(image, 1)
      val leaf = pages(image, 0)

      test(m"an untouched image is sound"):
        Ratel.open(Terrain(image)).verify()
      . assert(_ == ())

      test(m"↯/t.5.1: another magic number is refused"):
        reason(patched(image, 0, List(0xb2)))
      . assert(_ == Reason.BadMagic)

      test(m"↯/t.5.2: another version is refused"):
        reason(patched(image, 4, List(2)))
      . assert(_ == Reason.UnsupportedVersion)

      test(m"↯/t.5.3: a reserved byte which is set is refused"):
        reason(patched(image, 5, List(1)))
      . assert(_ == Reason.ReservedSet)

      test(m"↯/t.5.3: a reserved flag which is set is refused"):
        reason(patched(image, 6, List(0x16)))
      . assert(_ == Reason.ReservedSet)

      test(m"↯/t.5.4: a file shorter than a header is refused"):
        reason(image.segment(Prim till 100.z))
      . assert(_ == Reason.BadLength)

      test(m"↯/t.5.4: a committed length beyond the file is refused"):
        reason(patched(image, 8, little(image.length + 8, 8)))
      . assert(_ == Reason.BadLength)

      test(m"↯/t.5.5: a misaligned section is refused"):
        reason(patched(image, 64, little(variable + 4, 8)))
      . assert(_ == Reason.BadDirectory)

      test(m"↯/t.5.5: a section beyond the committed length is refused"):
        reason(patched(image, 80, little(image.length, 8)))
      . assert(_ == Reason.BadDirectory)

      test(m"↯/t.5.5: a section which holds more than it has room for is refused"):
        reason(patched(image, 72, little(64, 8)))
      . assert(_ == Reason.BadDirectory)

      test(m"↯/t.5.5: a directory which is not as long as its capacity is refused"):
        reason(patched(image, 96, little(32, 8)))
      . assert(_ == Reason.BadDirectory)

      test(m"↯/t.5.6: sections which overlap are refused"):
        reason(patched(image, 88, little(variable, 8)))
      . assert(_ == Reason.ExtentsOverlap)

      test(m"↯/t.5.6: a table whose rows lie over a section is refused"):
        reason(patched(image, tables + 32, little(variable, 8)))
      . assert(_ == Reason.ExtentsOverlap)

      test(m"↯/t.5.7: a digests flag with no digests is refused"):
        reason(patched(image, 6, List(7)))
      . assert(_ == Reason.DigestMismatch)

      test(m"↯/t.5.7: a body which does not match its digest fails verification"):
        val digested = catalogue.ratel(catalogueSchema).data
        reason(patched(digested, start(digested, 2) + 0x14, List(0x62)))
      . assert(_ == Reason.DigestMismatch)

      test(m"↯/t.5.7: a header which does not match its digest fails verification"):
        val digested = catalogue.ratel(catalogueSchema).data
        reason(patched(digested, 168, List(digested.u8(168) ^ 1)))
      . assert(_ == Reason.DigestMismatch)

      test(m"↯/t.5.8: a signature which is not structurally one is refused"):
        reason(patched(image, 201, List(image.u8(201) ^ 1)))
      . assert(_ == Reason.BadSignature)

      test(m"↯/t.5.9: a schema which does not decode is refused"):
        reason(patched(image, 234, List(0)))
      . assert(_ == Reason.SchemaUndecodable)

      test(m"↯/t.5.10: a schema which does not have its signature is refused"):
        reason(patched(image, 201, List(image.u8(201) ^ 1, image.u8(202) ^ 1)))
      . assert(_ == Reason.SignatureMismatch)

      test(m"↯/t.5.11: a schema which interns a fixed-width scalar cannot be laid out"):
        val schema = schemaOf:
          Text("""|tel 1.0 specification.tel/tels:2.1.0
              |
              |name census
              |
              |scalar Count
              |  encoding u32
              |  intern
              |
              |document
              |  field players Count
              |""".stripMargin)

        capture[Ratel.Error](t"tel 1.0\n\nplayers 6\n".read[Tel].ratel(schema)).reason
      . assert(_ == Reason.SchemaUnlayoutable)

      test(m"↯/t.5.12: a table whose rows are not the size of its layout is refused"):
        reason(patched(image, tables + 48, little(32, 4)))
      . assert(_ == Reason.BadTableDirectory)

      test(m"↯/t.5.12: a table with more rows than it has room for is refused"):
        reason(patched(image, tables + 40, little(3, 4)))
      . assert(_ == Reason.BadTableDirectory)

      test(m"↯/t.5.12: a free list which begins beyond the table is refused"):
        reason(patched(image, tables + 52, little(9, 4)))
      . assert(_ == Reason.BadTableDirectory)

      test(m"↯/t.5.13: a root table of two rows is refused"):
        reason(patched(image, tables + 8, little(2, 4) + little(2, 4)))
      . assert(_ == Reason.BadRootTable)

      test(m"↯/t.5.13: a root row which is not live is refused"):
        reason(patched(image, root + 6, List(0)))
      . assert(_ == Reason.BadRootTable)

      test(m"↯/t.5.14: a ref which is not a multiple of four fails its access"):
        reason(patched(image, entries + 36, little(2, 4)))
      . assert(_ == Reason.BadRef)

      test(m"↯/t.5.14: an item which runs beyond its region fails its access"):
        reason(patched(image, variable + 0x1c, little(400, 4)))
      . assert(_ == Reason.BadRef)

      test(m"↯/t.5.14: a list which holds more than it has room for fails its access"):
        reason(patched(image, variable + 4, little(3, 4)))
      . assert(_ == Reason.BadRef)

      test(m"↯/t.5.15: a child beyond the end of its table fails its access"):
        reason(patched(image, variable + 12, little(5, 4)))
      . assert(_ == Reason.BadRecordIndex)

      test(m"↯/t.5.15: a child which is a free row fails its access"):
        reason(patched(image, entries + 30, List(0)))
      . assert(_ == Reason.BadRecordIndex)

      test(m"a lazy reader still serves the records which are sound"):
        val damaged = patched(image, entries + 36, little(2, 4))

        Ratel(damaged).open[Ratel]():
          val entries = ratel.root / t"entry"

          ( entries.find(t"alpha").let(_.text(t"info")),
            capture[Ratel.Error](entries(Sec).text(t"name")).reason )

      . assert(_ == (t"First letter", Reason.BadRef))

      test(m"↯/t.5.16: a row which is reached twice is refused"):
        val schema = schemaOf:
          Text("""|tel 1.0 specification.tel/tels:2.1.0
              |
              |name cast
              |
              |record Player
              |  field name String key
              |
              |document
              |  field player Player repeatable
              |  field lead Player optional
              |""".stripMargin)

        val cast = plain(t"tel 1.0\n\nplayer Bottom\nplayer Flute\nlead Snug\n".read[Tel], schema)
        reason(patched(cast, rows(cast, 0) + 16, little(0, 4)))
      . assert(_ == Reason.RowRevisited)

      val verdictSchema = schemaOf:
        Text("""|tel 1.0 specification.tel/tels:2.1.0
            |
            |name verdict
            |
            |select Ending
            |  variant death Flag
            |  variant marriage String
            |
            |document
            |  field title String
            |  select Ending
            |""".stripMargin)

      test(m"↯/t.5.17: a tag which is no variant's keyword index fails its access"):
        val verdict = plain(t"tel 1.0\n\ntitle Hamlet\ndeath\n".read[Tel], verdictSchema)
        reason(patched(verdict, rows(verdict, 0) + 12, little(7, 4)))
      . assert(_ == Reason.BadTag)

      test(m"↯/t.5.18: a text which is not UTF-8 fails its access"):
        reason(patched(image, variable + 0x14, List(0xff)))
      . assert(_ == Reason.BadUtf8)

      test(m"↯/t.5.19: an encoding with no codec bound to it is refused"):
        val folioImage = folio.ratel(folioSchema)
        capture[Ratel.Error](Ratel.open(folioImage.terrain, Tel.Codec.Bindings.none)).reason
      . assert(_ == Reason.CodecUnresolved)

      test(m"↯/t.5.20: bytes which a codec rejects fail their access"):
        val schema = schemaOf:
          Text("""|tel 1.0 specification.tel/tels:2.1.0
              |
              |name measure
              |
              |scalar Reading
              |  encoding f64
              |
              |document
              |  field reading Reading
              |""".stripMargin)

        val measure = plain(t"tel 1.0\n\nreading 0.5\n".read[Tel], schema)
        reason(patched(measure, rows(measure, 0) + 8, List(0xff, 0xf8, 0, 0, 0, 0, 0, 0)))
      . assert(_ == Reason.CodecDecodeFailed)

      test(m"↯/t.5.22: a value which does not have its declared width is not written"):
        val schema = schemaOf:
          Text("""|tel 1.0 specification.tel/tels:2.1.0
              |
              |name seal
              |
              |scalar Mark
              |  encoding base-256
              |  width 4
              |
              |document
              |  field mark Mark
              |""".stripMargin)

        val mark = Base256.encode(Array[Byte](1, 2))
        capture[Ratel.Error](t"tel 1.0\n\nmark $mark\n".read[Tel].ratel(schema)).reason
      . assert(_ == Reason.WidthMismatch)

      test(m"↯/t.5.23: an index of a kind which is not known is refused"):
        reason(patched(image, indexes + 28, List(1)))
      . assert(_ == Reason.BadIndex)

      test(m"↯/t.5.23: an index whose root is beyond its pages is refused"):
        reason(patched(image, indexes + 16, little(4, 4)))
      . assert(_ == Reason.BadIndex)

      test(m"↯/t.5.23: a page of a type which is not known fails its lookup"):
        val damaged = patched(image, leaf, List(3))
        capture[Ratel.Error](Ratel(damaged).open[Ratel]()((ratel.root / t"entry").find(t"beta"))).reason
      . assert(_ == Reason.BadIndex)

      test(m"↯/t.5.23: a cell which begins outside the cell area fails verification"):
        reason(patched(image, leaf + 16, little(0x20, 2)))
      . assert(_ == Reason.BadIndex)

      test(m"↯/t.5.23: an entry which does not end where its key does fails verification"):
        reason(patched(image, leaf + 0xfd8 + 2 + 9, List(1)))
      . assert(_ == Reason.BadIndex)

      test(m"↯/t.5.24: entries which are out of order fail verification"):
        reason(patched(image, leaf + 16, little(0xfd8, 2) + little(0xfec, 2)))
      . assert(_ == Reason.BadTree)

      test(m"↯/t.5.24: a branch where a leaf should be fails its lookup"):
        val damaged = patched(image, leaf, List(2))
        capture[Ratel.Error](Ratel(damaged).open[Ratel]()((ratel.root / t"entry").find(t"beta"))).reason
      . assert(_ == Reason.BadTree)

      test(m"↯/t.5.24: a leaf which is chained to itself fails verification"):
        reason(patched(image, leaf + 8, little(0, 4)))
      . assert(_ == Reason.BadTree)

      test(m"↯/t.5.25: an entry which names the wrong child fails verification"):
        reason(patched(image, leaf + 0xfd8 + 2 + 14, List(0)))
      . assert(_ == Reason.IndexDisagrees)

      test(m"↯/t.5.25: an index with fewer entries than children fails verification"):
        val single = patched(patched(image, leaf + 2, little(1, 2)), indexes + 24, little(1, 4))
        reason(single)
      . assert(_ == Reason.IndexDisagrees)

      test(m"↯/t.5.27: a document which does not have its value hash fails verification"):
        reason(patched(image, 168, List(image.u8(168) ^ 1)))
      . assert(_ == Reason.ValueHashMismatch)

      test(m"↯/t.5.28: digests in a file which is not sealed are refused"):
        reason(patched(image, 6, List(3)))
      . assert(_ == Reason.InvalidState)

      test(m"↯/t.5.28: a file which is both sealed and dirty is refused"):
        reason(patched(image, 6, List(14)))
      . assert(_ == Reason.InvalidState)

      test(m"↯/t.5.28: a sealed file which counts garbage is refused"):
        reason(patched(image, 160, little(8, 8)))
      . assert(_ == Reason.InvalidState)

      test(m"↯/t.5.28: a sealed file with a gap between its extents is refused"):
        reason(patched(image, 16, little(208, 8)))
      . assert(_ == Reason.InvalidState)

      test(m"↯/t.5.28: a sealed list with room to spare fails verification"):
        reason(patched(image, variable, little(2, 4) + little(1, 4)))
      . assert(_ == Reason.InvalidState)

      test(m"↯/t.5.29: a dirty file is refused"):
        reason(patched(image, 6, List(8)))
      . assert(_ == Reason.Dirty)

      test(m"↯/t.5.30: a row which names another parent fails verification"):
        reason(patched(image, entries, little(1, 4)))
      . assert(_ == Reason.BadRowHeader)

      test(m"↯/t.5.30: a row which names another link fails verification"):
        reason(patched(image, entries + 4, little(1, 2)))
      . assert(_ == Reason.BadRowHeader)

      test(m"↯/t.5.30: a row with a reserved flag set fails verification"):
        reason(patched(image, entries + 6, little(3, 2)))
      . assert(_ == Reason.BadRowHeader)

    suite(m"Sharing a published image between readers"):
      val directory: Text = Uuid().show
      val root: Path on Linux = unsafely((% / "tmp" / directory).on[Linux])
      unsafely(root.create[Directory]())
      val file: Path on Linux = unsafely((% / "tmp" / directory / "catalogue.ratel").on[Linux])

      val revised: Tel =
        t"tel 1.0\n\nentry\n  name alpha\n\nentry\n  name beta\n  info  Second letter\n".read[Tel]

      def info(view: Terrain, key: Text): Optional[Text] =
        view.open[Ratel]() { (ratel.root / t"entry").find(key).let(_.text(t"info")) }

      test(m"an image published under a name is read in place from the file"):
        unsafely:
          file.claim[Ratel](_.publish(catalogue.ratel(catalogueSchema)))
          file.open[Ratel]() { (ratel.root / t"entry").find(t"alpha").let(_.text(t"info")) }
      . assert(_ == t"First letter")

      test(m"a file is verified as it is opened"):
        unsafely(file.open[Ratel](Ratel.Flag.Verify)((ratel.root / t"entry").size))
      . assert(_ == 2L)

      test(m"a view of a mapped file is opened as it is"):
        unsafely(file.mapped(info(_, t"alpha")))
      . assert(_ == t"First letter")

      test(m"a file which is not an image is refused as it is opened"):
        val other: Path on Linux = unsafely((% / "tmp" / directory / "prologue").on[Linux])

        unsafely:
          other.write(t"Two households, both alike in dignity")
          capture[Ratel.Error](other.open[Ratel]()(ratel.schema.name)).reason
      . assert(_ == Ratel.Error.Reason.BadLength)

      test(m"a reader sees a new edition once it refreshes"):
        unsafely:
          file.published[Ratel]: reader =>
            val before = reader.map(info(_, t"beta"))
            file.claim[Ratel](_.publish(revised.ratel(catalogueSchema)))
            val stale = reader.map(info(_, t"beta"))
            val moved = reader.refresh()
            (before, stale, moved, reader.map(info(_, t"beta")))
      . assert(_ == (Unset, Unset, true, t"Second letter"))

      test(m"a view of an old edition outlives its replacement"):
        unsafely:
          file.published[Ratel]: reader =>
            reader.map: view =>
              view.open[Ratel]():
                val entries = ratel.root / t"entry"
                file.claim[Ratel](_.publish(catalogue.ratel(catalogueSchema)))
                reader.refresh()
                entries.find(t"beta").let(_.text(t"info"))
      . assert(_ == t"Second letter")
