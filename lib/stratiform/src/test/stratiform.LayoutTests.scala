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
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package stratiform

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import codepages.utf8Codepage
import denominative.dysasymptotics.linearSize // the suite reads list sizes; the cost is irrelevant here

// The layout annotations of a derived schema (TEL §20): `@key`, `@index`, `@encoding`, `@width`
// and `@intern` on the fields of a case class declare, in the schema derived from it, what a
// schema written by hand declares with `key`, `index`, `encoding`, `width` and `intern`; and a
// RaTEL image of a value of the type is laid out, and looked up, accordingly.
object LayoutTests extends Suite("tel-layout", m"Stratiform derived layout tests"):
  case class Role
    ( @key name: Text, play: Text, @encoding(t"u32") lines: Optional[Int], @intern kind: Text )

  case class Cast(@index(t"/play") @index(t"/kind", t"/play") role: List[Role])

  // A type with no layout annotations, whose schema is what it was before there were any.
  case class Part(name: Text, lines: Optional[Int])
  case class Troupe(part: List[Part])

  case class Sealed(@encoding(t"base-256") @width(4) mark: Text)
  case class Tagged(@intern tags: List[Text])

  case class Tally(name: Text, @encoding(t"u32") count: Int)
  case class Tallies(@encoding(t"u32") counts: List[Int])

  // Declarations which the derivation refuses, each where its schema is asked for.
  case class Padded(@width(4) @intern mark: Text)
  case class Unencoded(@width(4) mark: Text)
  case class Interned(@intern role: Role)
  case class Misindexed(@index(t"/play") lead: Role)
  case class Unkeyed(@key name: Optional[Text])

  private val cast: Cast =
    Cast
      ( List
          ( Role(t"Hamlet", t"Hamlet", 1506, t"lead"),
            Role(t"Viola", t"Twelfth Night", 337, t"lead"),
            Role(t"Ophelia", t"Hamlet", 173, t"support"),
            Role(t"Yorick", t"Hamlet", Unset, t"support") ) )

  private def hex(data: Data): Text = data.fuse("")(state+"%02x".format(next & 0xff)).tt

  private def field(schema: Tels, record: Text, keyword: Text): Tels.Field =
    val members: List[Tels.Member] =
      schema.records.to[List].filter(_.name == record).bind(_.members.to[List])
      + schema.document.members.to[List]

    members.sweep { case field: Tels.Field if field.keyword == keyword => field }.head
    . or(throw IllegalArgumentException(keyword.s))

  // The schema as the document which declares it, which is what its signature is of.
  private def rendered(schema: Tels): Text =
    hex(Tels.Renderer.element(schema).bintel(Tels.Axiom.tels))

  private def written(text: Text): Text =
    rendered(Tels.Validation.validate(text.read[Tel].as[Tels]))

  def run(): Unit =
    suite(m"Deriving a schema's layout"):
      val schema: Tels = Tels.tels[Cast](t"cast")

      test(m"a `@key` field is the key of its record"):
        field(schema, t"Role", t"name").key
      . assert(_ == true)

      test(m"a field with no `@key` is not a key"):
        field(schema, t"Role", t"play").key
      . assert(_ == false)

      test(m"each `@index` is an index over its paths, in the order they are written"):
        field(schema, t"Role", t"role").indexes.map(_.paths)
      . assert(_ == List(List(t"/play"), List(t"/kind", t"/play")))

      test(m"an `@encoding` field refers to a scalar which declares the encoding"):
        field(schema, t"Role", t"lines").fieldType
      . assert(_ == Tels.Reference(t"U32"))

      test(m"an `@intern` field refers to a scalar which is interned"):
        field(schema, t"Role", t"kind").fieldType
      . assert(_ == Tels.Reference(t"TextInterned"))

      test(m"the scalars are defined once each, by what they declare"):
        schema.scalars.to[List].map { scalar => (scalar.name, scalar.encoding, scalar.intern) }
      . assert(_ == List((t"U32", t"u32", false), (t"TextInterned", Unset, true)))

      test(m"a `@width` is declared with its encoding"):
        Tels.tels[Sealed](t"sealed").scalars.to[List].map: scalar =>
          (scalar.name, scalar.encoding, scalar.width)
      . assert(_ == List((t"Base256Of4", t"base-256", 4)))

      test(m"the derived schema is the one which would be written by hand"):
        rendered(schema)
      . assert: bytes =>
          bytes == written:
            Text("""|tel 1.0 specification.tel/tels:2.1.0
                |
                |name cast
                |
                |record Role
                |  field name String key
                |  field play String
                |  field lines U32 optional
                |  field kind TextInterned
                |
                |scalar U32
                |  encoding u32
                |
                |scalar TextInterned
                |  intern
                |
                |document
                |  field role Role optional repeatable
                |    index /play
                |    index /kind /play
                |""".stripMargin)

      test(m"the schema of a type with no annotations defines no scalars"):
        Tels.tels[Troupe](t"troupe").scalars.to[List].map(_.name)
      . assert(_ == Nil)

      test(m"the schema of a type with no annotations is unchanged"):
        rendered(Tels.tels[Troupe](t"troupe"))
      . assert: bytes =>
          bytes == written:
            Text("""|tel 1.0 specification.tel/tels:2.1.0
                |
                |name troupe
                |
                |record Part
                |  field name String
                |  field lines String optional
                |
                |document
                |  field part Part optional repeatable
                |""".stripMargin)

    suite(m"Reading an image of a derived layout"):
      val image: Ratel = cast.ratel

      test(m"a value is written under the schema of its type, and read back"):
        image.open[Ratel](Ratel.Flag.Verify)(ratel.root.as[Cast])
      . assert(_ == cast)

      test(m"a record is found by its derived key"):
        image.open[Ratel]()((ratel.root / t"role").find(t"Ophelia").let(_.as[Role]))
      . assert(_ == Role(t"Ophelia", t"Hamlet", 173, t"support"))

      test(m"records are found by a derived index"):
        image.open[Ratel]():
          (ratel.root / t"role").where(t"/play", t"Hamlet").map(_.text(t"name").or(t""))

      . assert(_ == List(t"Hamlet", t"Ophelia", t"Yorick"))

      test(m"records are found by the first path of an index over two"):
        image.open[Ratel]():
          (ratel.root / t"role").where(t"/kind", t"lead").map(_.text(t"name").or(t""))

      . assert(_ == List(t"Hamlet", t"Viola"))

      test(m"a `u32` is stored within its row, as four bytes"):
        image.open[Ratel]()((ratel.root / t"role").find(t"Hamlet").let(_.data(t"lines")).let(hex(_)))
      . assert(_ == t"000005e2")

      test(m"an interned value is stored once"):
        val strings = image.data.u64(16 + 24).toInt
        hex(image.data.segment(strings.z till (strings + image.data.u64(24 + 24).toInt).z))
      . assert(_ == t"040000006c65616407000000737570706f727400")

      test(m"a collection of interned values is written and read"):
        val tagged = Tagged(List(t"quarto", t"folio", t"quarto"))
        tagged.ratel.open[Ratel](Ratel.Flag.Verify)(ratel.root.as[Tagged])
      . assert(_ == Tagged(List(t"quarto", t"folio", t"quarto")))

      test(m"a value of another width than is declared is not written"):
        capture[Ratel.Error](Sealed(Base256.encode(Array[Byte](1, 2))).ratel).reason
      . assert(_ == Ratel.Error.Reason.WidthMismatch)

      test(m"a value of the declared width is written within its row"):
        val mark = Base256.encode(Array[Byte](1, 2, 3, 4))
        Sealed(mark).ratel.open[Ratel]()(hex(ratel.root.data(t"mark")))
      . assert(_ == t"01020304")

    suite(m"Reading an encoded field directly"):
      given (Tally is Bintel.Parsable) = BintelInlinable.parsable[Tally]
      given (Tallies is Bintel.Parsable) = BintelInlinable.parsable[Tallies]

      // The standard codecs are bound by name, as they are for a RaTEL image by default.
      val codecs = Tel.Codec.Bindings.all
      def bytes[value: Tel.Encodable](value: value, schema: Tels): Data = value.encode.bintel(schema, codecs)

      test(m"an `@encoding` field is written to BinTEL as its codec's bytes"):
        hex(bytes(Tally(t"lines", 4024), Tels.tels[Tally](t"root"))).contains(t"00000fb8")
      . assert(_ == true)

      test(m"the direct read of an `@encoding` field agrees with the read of the model"):
        val body = bytes(Tally(t"lines", 4024), Tels.tels[Tally](t"root"))
        (Bintel.parse[Tally](body, codecs), Bintel.read[Tally](body, codecs))
      . assert(_ == (Tally(t"lines", 4024), Tally(t"lines", 4024)))

      test(m"the direct read of an encoded collection agrees with the read of the model"):
        val body = bytes(Tallies(List(1506, 337, 173)), Tels.tels[Tallies](t"root"))
        (Bintel.parse[Tallies](body, codecs), Bintel.read[Tallies](body, codecs))
      . assert(_ == (Tallies(List(1506, 337, 173)), Tallies(List(1506, 337, 173))))

    suite(m"Refusing a layout"):
      test(m"a width with interning does not compile"):
        demilitarize(Tels.tels[Padded](t"padded")).map(_.message.tt)
      . assert(_.exists(_.contains(t"a scalar declares both a")))

      test(m"a width with no encoding does not compile"):
        demilitarize(Tels.tels[Unencoded](t"unencoded")).map(_.message.tt)
      . assert(_.exists(_.contains(t"a scalar declares a")))

      test(m"interning a field which is a record does not compile"):
        demilitarize(Tels.tels[Interned](t"interned")).map(_.message.tt)
      . assert(_.exists(_.contains(t"is not a scalar")))

      test(m"an index over a field which is not a collection is refused"):
        capture[Tel.Error](Tels.tels[Misindexed](t"misindexed")).reason
      . assert(_ == Tel.Error.Reason.IndexNotApplicable)

      test(m"a key which is optional is refused"):
        capture[Tel.Error](Tels.tels[Unkeyed](t"unkeyed")).reason
      . assert(_ == Tel.Error.Reason.KeyOnLooseMember)
