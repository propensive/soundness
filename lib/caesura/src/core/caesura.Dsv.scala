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
package caesura


import scala.caps

import scala.language.dynamics

import scala.compiletime.*

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*
import wisteria.*
import zephyrine.*
import fulminate.*

trait Dsv2:
  // Generic fallback: any `Decodable in Text` (custom types, enums, `Uuid`, …)
  // decodes from the row's first cell. Held at lower priority than the primitive
  // `Decodable in Dsv` givens in `object Dsv` (which accrue errors via raise+yet)
  // so those win for Int/Long/Double/… where a `Decodable in Text` also exists —
  // mirroring how distillate keeps its generic instance in `Decodable2`.
  given decoder: [decodable] => (decodable: (decodable is Decodable in Text)^)
  =>  ((decodable is Decodable in Dsv)^{decodable}) =
    value => decodable.decoded(value.data.readable.head)

  // `source.read[Foo in Dsv]` shorthand: decodes a single DSV record (the first
  // row) into `Foo` through the row AST. Held below `aggregableParsed` in
  // `object Dsv`, so a type with an explicit `Dsv.Parsable` takes the direct
  // (AST-free) path instead. The `Form` type-tag is added by an `asInstanceOf`
  // cast — `value in Dsv` is just `value { type Form = Dsv }`, so the cast is a
  // no-op at runtime.
  given aggregableIn: [value: Decodable in Dsv] => (format: Dsv.Format)
  =>  (tactic: Tactic[Dsv.Error])
  =>  (((value in Dsv) is Aggregable by Text)^{tactic}) =
    text =>
      summon[Sheet is Aggregable by Text].aggregate(text).rows.readable.head.as[value]
      . asInstanceOf[value in Dsv]

object Dsv extends Dsv2:
  def apply(iterable: Iterable[Text]): Dsv =
    new Dsv(Array.from(iterable))

  // Flattens the per-field cell arrays of a derived product row into one `Dsv`. A plain
  // method rather than part of the inline derivation body: the fresh `any.rd` of the
  // intermediate buffers must not be minted inside an inline expansion, where it leaks
  // into the expansion site's capture sets.
  private[caesura] def flatten(arrays: Array[Array[Text]^{}]^{caps.any.rd}): Dsv =
    val raw = arrays.readable
    val builder = scala.collection.immutable.List.newBuilder[Text]
    var index = 0

    while index < raw.length do
      raw(index).readable.foreach(builder += _)
      index += 1

    Dsv(Array.from(builder.result()))
  def apply(text: Text*): Dsv = new Dsv(Array.from(text))

  // An absent cell (a short positional row, or a header column missing from the row) decodes
  // to `Unset`; a present cell — even an empty one — decodes its inner value. The product
  // decoder hands an empty `Dsv` to a field whose cell is absent (see `conjunction`). Sits at
  // the same priority as the primitive cell decoders (above the generic `decoder` in `Dsv2`),
  // and short-circuits an absent cell to `Unset` before the inner decoder would `raise` Absent.
  given optionalDecodable: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( decodable: => inner is Decodable in Dsv )
  =>  value is Decodable in Dsv =
    // The by-name inner decoder shares this instance's given-resolution lifetime; laundered
    // pure per the codec-thunk seal pattern (see rep/DECISIONS.md).
    caps.unsafe.unsafeAssumePure:
      row => if row.data.length == 0 then Unset else decodable.decoded(row)

  given encoder: [encodable: Encodable in Text] => encodable is Encodable in Dsv =
    value => Dsv(encodable.encode(value))

  // Primitive cell decoders. Unlike the generic `decoder` (which bottoms out in
  // distillate's `abort`ing text decoders), these register an error and continue
  // with a sentinel value (`raise … yet sentinel`), so that when a row is decoded
  // into a product under a `validate[CellRef]` boundary every malformed cell is
  // accrued rather than the first failure aborting the whole record. An absent
  // cell (short row / missing column) is distinguished from a present-but-
  // unparseable one. The real cell location is carried by the enclosing
  // `focus(CellRef(…))` in the product derivation, so `Dsv.Error` itself is built
  // through the position-free companion `apply`.
  private def decodeCell[value]
    ( dsv: Dsv, expected: Text, sentinel: value )
    ( parse: Text => Optional[value] )
    ( using format: Dsv.Format, tactic: Tactic[Dsv.Error] )
  :   value =

    dsv.data.prim.lay(raise(Dsv.Error(format, Dsv.Error.Reason.Absent)) yet sentinel): cell =>
      parse(cell).or:
        raise(Dsv.Error(format, Dsv.Error.Reason.Unparseable(cell, expected))) yet sentinel

  // The primitive cell decoders are laundered pure: their resolution-scoped tactic shares each
  // instance's given-resolution lifetime, and the product derivation summons them against pure
  // expected types (honest capturing forms return with wisteria capture-polymorphism; see
  // rep/DECISIONS.md).
  given int: (format: Dsv.Format) => (tactic: Tactic[Dsv.Error])
  =>  Int is Decodable in Dsv =
    caps.unsafe.unsafeAssumePure: dsv =>
      decodeCell(dsv, t"Int", 0): cell =>
        try Integer.parseInt(cell.s) catch case _: NumberFormatException => Unset

  given long: (format: Dsv.Format) => (tactic: Tactic[Dsv.Error])
  =>  Long is Decodable in Dsv =
    caps.unsafe.unsafeAssumePure: dsv =>
      decodeCell(dsv, t"Long", 0L): cell =>
        try java.lang.Long.parseLong(cell.s) catch case _: NumberFormatException => Unset

  given double: (format: Dsv.Format) => (tactic: Tactic[Dsv.Error])
  =>  Double is Decodable in Dsv =
    caps.unsafe.unsafeAssumePure: dsv =>
      decodeCell(dsv, t"Double", 0.0): cell =>
        try java.lang.Double.parseDouble(cell.s) catch case _: NumberFormatException => Unset

  given float: (format: Dsv.Format) => (tactic: Tactic[Dsv.Error])
  =>  Float is Decodable in Dsv =
    caps.unsafe.unsafeAssumePure: dsv =>
      decodeCell(dsv, t"Float", 0.0f): cell =>
        try java.lang.Float.parseFloat(cell.s) catch case _: NumberFormatException => Unset

  given boolean: (format: Dsv.Format) => (tactic: Tactic[Dsv.Error])
  =>  Boolean is Decodable in Dsv =
    caps.unsafe.unsafeAssumePure: dsv =>
      decodeCell(dsv, t"Boolean", false): cell =>
        cell.s match
          case "true"  => true
          case "false" => false
          case _       => Unset

  given text: (format: Dsv.Format) => (tactic: Tactic[Dsv.Error])
  =>  Text is Decodable in Dsv =
    caps.unsafe.unsafeAssumePure: dsv => decodeCell(dsv, t"Text", t"")(cell => cell)

  given string: (format: Dsv.Format) => (tactic: Tactic[Dsv.Error])
  =>  String is Decodable in Dsv =
    caps.unsafe.unsafeAssumePure: dsv => decodeCell(dsv, t"String", "")(cell => cell.s)

  inline given decodableDerivation: [value <: Product: ProductReflection]
  =>  value is Decodable in Dsv =

    DecodableDerivation.derived[value]

  inline given encodableDerivation: [value <: Product: ProductReflection]
  =>  value is Encodable in Dsv =

    EncodableDerivation.derived[value]

  given showable: (format: Dsv.Format) => Dsv is Showable = dsv =>
    val cells =
      dsv.data.remap: cell =>
        val safe = !cell.contains(format.Quote) && !cell.contains(format.Delimiter) &&
          !cell.contains('\n') && !cell.contains('\r')

        if safe then cell else
          Text.build:
            append(format.quote)

            cell.chars.each: char =>
              if char == format.quote then append(char)
              append(char)

            append(format.quote)

    cells.readable.join(format.delimiter.show)

  // Direct-read entries, gated on an explicit `Dsv.Parsable` (they sit above
  // the AST-based `aggregableIn` in `Dsv2`, so opting in switches the path).
  // `read[Foo in Dsv]` parses the first row; `read[List[Foo] in Dsv]` parses
  // every row. Sealed per the codec-thunk pattern.
  given aggregableParsed: [value] => (parsable: value is Dsv.Parsable)
  =>  ( format: Dsv.Format, tactic: Tactic[Dsv.Error], buffering: Buffering )
  =>  ((value in Dsv) is Aggregable by Text) =
    caps.unsafe.unsafeAssumePure:
      new Aggregable:
        type Self = value in Dsv
        type Operand = Text

        def aggregate(text: Chain[Text]): value in Dsv = accept(Stream(text))

        override def accept(stream: (Stream[Text] over Credit)^): value in Dsv =
          val reader =
            Sheet.directReader(stream.asInstanceOf[AnyRef].asInstanceOf[(Stream[Text] over Credit)^])

          if reader.nextRow() then parsable.parse(reader, 0).asInstanceOf[value in Dsv]
          else tactic.abort(Dsv.Error(format, Dsv.Error.Reason.Absent))

  given aggregableParsedList: [value] => (parsable: value is Dsv.Parsable)
  =>  ( format: Dsv.Format, tactic: Tactic[Dsv.Error], buffering: Buffering )
  =>  ((List[value] in Dsv) is Aggregable by Text) =
    caps.unsafe.unsafeAssumePure:
      new Aggregable:
        type Self = List[value] in Dsv
        type Operand = Text

        def aggregate(text: Chain[Text]): List[value] in Dsv = accept(Stream(text))

        override def accept(stream: (Stream[Text] over Credit)^): List[value] in Dsv =
          val reader =
            Sheet.directReader(stream.asInstanceOf[AnyRef].asInstanceOf[(Stream[Text] over Credit)^])

          val buffer = scala.collection.mutable.ListBuffer[value]()
          while reader.nextRow() do buffer += parsable.parse(reader, 0)
          buffer.to(List).asInstanceOf[List[value] in Dsv]

  // ---- Direct (AST-free) parsing --------------------------------------------
  //
  // `Dsv.Parsable` reads a value straight off the `DsvReader` token reader:
  // no per-row `Dsv`, no per-field slice of the cell array. `Parsable` is the
  // OPT-IN surface (explicit instances and `derives Dsv.Parsable`; no blanket
  // given), so `read[T in Dsv]` changes behavior only when a type opts in;
  // `Field` is the operational sibling that always resolves (bridging any
  // `Decodable in Text`), used in field position by the derivation. Neither
  // subtypes the other.
  trait Parsing extends distillate.Parsable:
    type Transport = Dsv
    type Reader = DsvReader

    // Cells this value occupies in positional mode.
    def width: Int = 1

    // Read a value from the current row, starting at cell `offset`.
    def parse(reader: DsvReader^, offset: Int): Self

    def parse(reader: DsvReader^): Self = parse(reader, 0)

  trait Parsable extends Parsing

  object Parsable:
    inline def derived[value <: Product: ProductReflection]: value is Dsv.Parsable =
      val field = ParsableDerivation.derived[value]

      new Parsable:
        type Self = value
        override def width: Int = field.width
        def parse(reader: DsvReader^, offset: Int): value = field.parse(reader, offset)

  trait Field extends Parsing

  object Field:
    // Rim call points for generated code: the derivation's lambda holds the
    // reader as a neutral `AnyRef` carrier (a function type may not take a
    // `^` parameter, and an inline lambda may not mint the capability); these
    // named methods reassert it.
    def parseField[value](field: value is Dsv.Field, carrier: AnyRef, offset: Int): value =
      field.parse(carrier.asInstanceOf[DsvReader^], offset)

    def fieldColumn(carrier: AnyRef, name: Text): Optional[Int] =
      carrier.asInstanceOf[DsvReader].column(name)

    // Bridge: any `Decodable in Text` reads a single cell. An absent cell (a
    // short positional row, or a header column missing here) aborts through
    // the reader's own tactic; `optional` below intercepts that for
    // `Optional` fields. Sealed per the codec-thunk pattern.
    given decodable: [value] => (decodable: (value is Decodable in Text)^)
    =>  value is Dsv.Field =
      caps.unsafe.unsafeAssumePure:
        new Field:
          type Self = value

          def parse(reader: DsvReader^, offset: Int): value =
            reader.cell(offset).lay(reader.absent()): cell => decodable.decoded(cell)

    given optional: [inner <: value, value >: Unset.type: Mandatable to inner]
    =>  ( field: => inner is Dsv.Field )
    =>  value is Dsv.Field =
      caps.unsafe.unsafeAssumePure:
        new Field:
          type Self = value
          override def width: Int = field.width

          def parse(reader: DsvReader^, offset: Int): value =
            if reader.cell(offset).absent then Unset else field.parse(reader, offset)

  object ParsableDerivation extends ProductDerivable[Dsv.Field]:
    // The generated parse lambda takes the reader as a neutral `AnyRef`
    // carrier (a function type may not take a `^` parameter); the capability
    // is reasserted at the rim inside. Nothing of the reader is retained.
    class DsvProductParser[derivation](width0: Int, lambda: (AnyRef, Int) => derivation)
    extends Field:
      type Self = derivation
      override def width: Int = width0

      def parse(reader: DsvReader^, offset: Int): derivation =
        lambda(reader.asInstanceOf[AnyRef], offset)

    inline def conjunction[derivation <: Product: ProductReflection]
    :   (derivation is Dsv.Field)^ =

      val spans: Array[Int]^{} = Spannable.derived[derivation].spans()
      var total: Int = 0
      spans.foreach { span => total += span }

      DsvProductParser[derivation](total, (carrier, offset) =>
        var count = offset

        build[derivation]:
          [field] => context =>
            // Header mode locates the field by column name; positional mode
            // reads the field's span starting at the running offset.
            val at = Dsv.Field.fieldColumn(carrier, label).or(count)
            count += spans.readUnchecked(index)
            Dsv.Field.parseField(contextual, carrier, at))

  object DecodableDerivation extends ProductDerivable[Decodable in Dsv]:
    // An impure (`=>`) lambda: a derived decoder legitimately captures the consumer's
    // resolution-scoped tactics, which the fresh (`^`) `conjunction` result honestly admits.
    class DsvProductDecoder[derivation](lambda: Dsv => derivation)
    extends Decodable:
      type Self = derivation
      type Form = Dsv
      def decoded(row: Dsv): derivation = lambda(row)

    // Scans the venture slots and constructs positionally through the threaded `Mirror` — a
    // plain method: the argument buffer must not be allocated inside an inline expansion,
    // where its fresh root capability leaks into the expansion site's capture sets (see
    // `Dsv.flatten`). Returns an unused null when any slot failed: the caller's accruing
    // scope is tainted, so the result is discarded.
    private final class ArrayProduct(values: Array[Any]^{}) extends Product:
      def canEqual(that: Any): Boolean = true
      def productArity: Int = values.length
      def productElement(index: Int): Any = values.readUnchecked(index)

    private def gate[derivation <: Product]
      ( reflection: ProductReflection[derivation],
        slots:      Array[Venture[Any]]^{},
        active:     Boolean )
    :   derivation =

      var failed = false
      var slot = 0

      if active then
        while slot < slots.length do
          if !slots.readUnchecked(slot).ready then failed = true
          slot += 1

      if failed then null.asInstanceOf[derivation]
      else
        val arguments = Array.allocate[Any](slots.length)
        slot = 0

        while slot < slots.length do
          arguments(slot) = slots.readUnchecked(slot).vouch
          slot += 1

        reflection.fromProduct(ArrayProduct(Array.freeze(arguments)))

    inline def conjunction[derivation <: Product: ProductReflection]
    :   (derivation is Decodable in Dsv)^ =

      val spans: Array[Int]^{} = Spannable.derived[derivation].spans()

      // `count` must be local to each decode call, not captured per instance: a derived
      // decoder for a type used in more than one field (e.g. two `Foo` fields) is deduplicated
      // to a single shared instance, so per-instance mutable state would leak between
      // successive decodes.
      provide[Foci[CellRef]]:
        // The decode lambda may capture the consumer's `Tactic` (field decoders are
        // tactic-taking givens), with the same lifetime as the instance's given resolution;
        // the fresh (`^`) result honestly admits the capture — no seal.
        DsvProductDecoder[derivation](
          (row: Dsv) =>
            val foci = infer[Foci[CellRef]]

            // Positional mode (`columns` Unset): read the field's span starting at `count`.
            // Header mode: locate the field by column name; an absent column hands the field
            // an empty `Dsv` so `Optional` fields decode to `Unset` rather than misreading by
            // position.
            def fieldRow(label: Text, count: Int): Dsv =
              row.columns.lay(Dsv(row.data.skip(count))): columns =>
                columns(label).lay(Dsv(Array[Text]())): i =>
                  Dsv(row.data.skip(i))

            // A SINGLE field traversal serving both modes, branching on `foci.active` per
            // field. One traversal is load-bearing: every wisteria traversal re-summons each
            // field's decoder (`summonInline`), recursively deriving nested types, so
            // traversals multiply EXPONENTIALLY with nesting depth at compile time.
            //
            // Accruing mode: each slot is marked failed if its decode registered any focus,
            // and the record is constructed only when every slot is clean — user constructor
            // code never sees a garbage fallback value. On failure the returned value is never
            // used (the caller's scope is tainted), and siblings keep accruing in the
            // meantime. Fail-fast mode: the first recorded error escapes through the tactic,
            // so no focus bookkeeping and no failure scan are needed.
            val active = foci.active
            var count = 0

            val slots: Array[Venture[Any]]^{} =
              contexts[derivation]()[Venture[Any]]:
                [field] => context =>
                  val row2 = fieldRow(label, count)
                  count += spans.readUnchecked(index)

                  if !active then Venture(contextual.decoded(row2))
                  else
                    focus(CellRef(Prim, label)):
                      val before = foci.length
                      val value: field = contextual.decoded(row2)
                      if foci.length > before then Venture.failed else Venture(value)

            gate[derivation](infer[ProductReflection[derivation]], slots, active))

  object EncodableDerivation extends ProductDerivable[Encodable in Dsv]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Encodable in Dsv =

      value =>
        Dsv.flatten(fields(value) { [field] => field => contextual.encode(field).data })

  // DsvError → Dsv.Error
  object Error:
    given communicable: Reason is Communicable =
      case Reason.MisplacedQuote =>
        m"a quote was found after the start of a cell"

      case Reason.Absent =>
        m"the cell was absent"

      case Reason.Unparseable(value, expected) =>
        m"the value $value could not be parsed as $expected"

    // `MisplacedQuote` is a parse-phase error; `Absent` and `Unparseable` are
    // decode-phase errors (a cell missing from, or unparseable in, a row being
    // decoded into a product). The latter accrue through a `Foci[CellRef]`, which
    // carries the real cell location, so they construct via the position-free
    // companion `apply` below.
    enum Reason(val number: Int) extends Clarification:
      case MisplacedQuote                          extends Reason(1)
      case Absent                                  extends Reason(2)
      case Unparseable(value: Text, expected: Text) extends Reason(3)

    // Decode-phase constructor: the offending cell's position is carried by the
    // accrued `CellRef` focus, not by the `Dsv.Error` itself, so `row`/`column`/
    // `offset` are filled with placeholders.
    def apply(format: Dsv.Format, reason: Reason)(using Diagnostics): Dsv.Error =
      Dsv.Error(format, reason, Prim, Prim, 0)

  // `row` and `column` are the 1-based position of the offending cell, and `offset`
  // is the character (UTF-16 code-unit) offset into the input at which the failure
  // was detected. A byte offset is not available here because the parser operates on
  // already-decoded `Text`, the source bytes having been discarded upstream.
  case class Error
    ( format: Dsv.Format, reason: Dsv.Error.Reason, row: Ordinal, column: Ordinal, offset: Int )
    ( using Diagnostics )
  extends fulminate.Error(364, reason.number)
    ( reason match
        case Dsv.Error.Reason.MisplacedQuote =>
          m"could not parse row data at row ${row.n1}, column ${column.n1} because $reason"

        case _ =>
          m"could not decode the cell because $reason" )

  // DsvFormat → Dsv.Format
  case class Format(header: Boolean, delimiter: Char, quote: Char, escape: Char):
    val Delimiter: Char = delimiter
    val Quote: Char = quote
    val Escape: Char = escape

    def doublingEscapes: Boolean = quote == escape

  // DsvRedesignation → Dsv.Redesignation
  trait Redesignation:
    def transform(name: Text): Text

case class Dsv(data: Array[Text]^{}, columns: Optional[Map[Text, Int]] = Unset) extends Dynamic:
  def as[cell: Decodable in Dsv]: cell raises Dsv.Error tracks CellRef = cell.decoded(this)

  def header: Optional[Array[Text]^{}] = columns.let: map =>
    val columns = map.stdlib.map(_.swap)
    Array.tabulate(columns.size)(columns(_))


  def selectDynamic[value](field: String)(using erased dynamicDsvEnabler: DynamicDsvEnabler)
    ( using value: (value is Decodable in Text)^ )
    ( using Dsv.Redesignation )
  :   Optional[value] =

    apply(summon[Dsv.Redesignation].transform(field.tt))


  def apply[value](using value: (value is Decodable in Text)^)(field: Text): Optional[value] =
    columns.let(_(field)).let { index => data.at(index.z) }.let(value.decoded(_))

  override def hashCode: Int = data.readable.indices.fuse(0)(state*31 + data.readUnchecked(next).hashCode)

  override def equals(that: Any): Boolean = that.asMatchable match
    case row: Dsv =>
      data.length == row.data.length && data.readable.indices.all: index => data.readUnchecked(index) == row.data.readUnchecked(index)

    case _ =>
      false
