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
package locomotion

import scala.quoted.*

import anticipation.*
import contingency.*
import prepositional.*

object Inlinable:
  // Generates a monomorphic `Protobuf.Parsable` for a case-class message or
  // a sealed-sum oneof at compile time, composed through `Inlinable`
  // instances: nested messages, repeated-field gathering, variant dispatch
  // and custom leaf parsers all inline into one flat parser.
  inline def parsable[value]: value is Protobuf.Parsable =
    ${locomotion.stagedInternal.inlinableParsable[value]}

  // The writing counterpart: a monomorphic `Protobuf.Printable` that measures and writes a
  // message's fields straight into the output array, composed through the same instances.
  inline def printable[value]: value is Protobuf.Printable =
    ${locomotion.stagedInternal.inlinablePrintable[value]}

  // The structural instance for a case-class message: reflects `Self` when
  // invoked (no macro — `Type[Self]` arrives with the call).
  def derived[product]: product is Inlinable = ProductInlinable[product]()

  object ForProtobuf:
    def derived[value]: ForProtobuf[value] = ForProtobuf(Inlinable.derived[value])

  // The `derives`-clause carrier: a `Self`-typed typeclass cannot appear in
  // a `derives` clause (it has no type parameters), so `case class Foo(...)
  // derives Inlinable.ForProtobuf` synthesizes this parameterized subtrait —
  // which *is* a `Foo is Inlinable` — into `Foo`'s companion, where the
  // staging summon finds it. The resolution ladder unwraps the delegate so
  // structural instances keep their generator identities.
  final class ForProtobuf[value](delegate0: value is Inlinable) extends Inlinable:
    type Self = value
    private[locomotion] def delegate: value is Inlinable = delegate0

    def parse(reader: Expr[ProtobufReader])(using Quotes, Type[value]): Expr[value] =
      delegate0.parse(reader)

    override def absent(tactic: Expr[Tactic[Protobuf.Error]])(using Quotes, Type[value])
    :   Expr[value] =

      delegate0.absent(tactic)

    override def prints: Boolean = delegate0.prints

    override def measure(value: Expr[value], sizes: Expr[AnyRef])(using Quotes, Type[value])
    :   Expr[Int] =

      delegate0.measure(value, sizes)

    override def write(value: Expr[value], printer: Expr[AnyRef], sizes: Expr[AnyRef])
      ( using Quotes, Type[value] )
    :   Expr[Unit] =

      delegate0.write(value, printer, sizes)

  private[locomotion] final class ProductInlinable[product]() extends Inlinable:
    type Self = product

    def parse(reader: Expr[ProtobufReader])(using Quotes, Type[product]): Expr[product] =
      stagedInternal.productBody[product](reader)

    override def absent(tactic: Expr[Tactic[Protobuf.Error]])(using Quotes, Type[product])
    :   Expr[product] =

      stagedInternal.productAbsent[product](tactic)

    override def prints: Boolean = true

    override def measure(value: Expr[product], sizes: Expr[AnyRef])(using Quotes, Type[product])
    :   Expr[Int] =

      stagedInternal.productMeasure[product](value, sizes)

    override def write(value: Expr[product], printer: Expr[AnyRef], sizes: Expr[AnyRef])
      ( using Quotes, Type[product] )
    :   Expr[Unit] =

      stagedInternal.productWrite[product](value, printer, sizes)

  // A marker in the resolution ladder: repeated fields gather across the
  // whole message (occurrences may be scattered and packed), so collection
  // parsing lives in the *product* generator, not here. The instance only
  // carries the element for that generator; a collection cannot be parsed
  // from a single field window.
  private[locomotion] final class IterableInlinable[element](val element0: element is Inlinable)
  extends Inlinable:
    type Self = Iterable[element]

    def parse(reader: Expr[ProtobufReader])(using Quotes, Type[Iterable[element]])
    :   Expr[Iterable[element]] =

      quotes.reflect.report.errorAndAbort
        ( "locomotion: a repeated field parses through its message's generated parser; a " +
          "collection has no standalone wire form" )

  // The structural instance for a sealed-sum oneof: the variant is chosen
  // by a scan of the message (the lowest variant number present, its last
  // occurrence), then parsed from its recorded extent — exactly the AST
  // disjunction's semantics.
  private[locomotion] final class SumInlinable[sum]() extends Inlinable:
    type Self = sum

    def parse(reader: Expr[ProtobufReader])(using Quotes, Type[sum]): Expr[sum] =
      stagedInternal.sumBody[sum](reader)

    override def prints: Boolean = true

    override def measure(value: Expr[sum], sizes: Expr[AnyRef])(using Quotes, Type[sum])
    :   Expr[Int] =

      stagedInternal.sumMeasure[sum](value, sizes)

    override def write(value: Expr[sum], printer: Expr[AnyRef], sizes: Expr[AnyRef])
      ( using Quotes, Type[sum] )
    :   Expr[Unit] =

      stagedInternal.sumWrite[sum](value, printer, sizes)

  // ── Instances ──────────────────────────────────────────────────────────
  // The leaf generators read the reader's window exactly as the AST
  // accessors interpret a field's recorded payload, for hand-written
  // instances that compose over one wire value.

  given int: (Int is Inlinable) = new Inlinable:
    type Self = Int

    override def prints: Boolean = true

    override def measure(value: Expr[Int], sizes: Expr[AnyRef])(using Quotes, Type[Int])
    :   Expr[Int] =

      '{ProtobufPrinter.varintSize($value.toLong)}

    override def write(value: Expr[Int], printer: Expr[AnyRef], sizes: Expr[AnyRef])
      ( using Quotes, Type[Int] )
    :   Expr[Unit] =

      '{ProtobufPrinter.of($printer).varint($value.toLong)}

    def parse(reader: Expr[ProtobufReader])(using Quotes, Type[Int]): Expr[Int] =
      '{$reader.varint().toInt}

  given long: (Long is Inlinable) = new Inlinable:
    type Self = Long

    override def prints: Boolean = true

    override def measure(value: Expr[Long], sizes: Expr[AnyRef])(using Quotes, Type[Long])
    :   Expr[Int] =

      '{ProtobufPrinter.varintSize($value)}

    override def write(value: Expr[Long], printer: Expr[AnyRef], sizes: Expr[AnyRef])
      ( using Quotes, Type[Long] )
    :   Expr[Unit] =

      '{ProtobufPrinter.of($printer).varint($value)}

    def parse(reader: Expr[ProtobufReader])(using Quotes, Type[Long]): Expr[Long] =
      '{$reader.varint()}

  given boolean: (Boolean is Inlinable) = new Inlinable:
    type Self = Boolean

    override def prints: Boolean = true

    override def measure(value: Expr[Boolean], sizes: Expr[AnyRef])(using Quotes, Type[Boolean])
    :   Expr[Int] =

      Expr(1)

    override def write(value: Expr[Boolean], printer: Expr[AnyRef], sizes: Expr[AnyRef])
      ( using Quotes, Type[Boolean] )
    :   Expr[Unit] =

      '{ProtobufPrinter.of($printer).varint(if $value then 1L else 0L)}

    def parse(reader: Expr[ProtobufReader])(using Quotes, Type[Boolean]): Expr[Boolean] =
      '{$reader.varint() != 0L}

  given double: (Double is Inlinable) = new Inlinable:
    type Self = Double

    override def prints: Boolean = true

    override def measure(value: Expr[Double], sizes: Expr[AnyRef])(using Quotes, Type[Double])
    :   Expr[Int] =

      Expr(8)

    override def write(value: Expr[Double], printer: Expr[AnyRef], sizes: Expr[AnyRef])
      ( using Quotes, Type[Double] )
    :   Expr[Unit] =

      '{ProtobufPrinter.of($printer).fixed64(java.lang.Double.doubleToLongBits($value))}

    def parse(reader: Expr[ProtobufReader])(using Quotes, Type[Double]): Expr[Double] =
      '{java.lang.Double.longBitsToDouble($reader.fixed64())}

  given float: (Float is Inlinable) = new Inlinable:
    type Self = Float

    override def prints: Boolean = true

    override def measure(value: Expr[Float], sizes: Expr[AnyRef])(using Quotes, Type[Float])
    :   Expr[Int] =

      Expr(4)

    override def write(value: Expr[Float], printer: Expr[AnyRef], sizes: Expr[AnyRef])
      ( using Quotes, Type[Float] )
    :   Expr[Unit] =

      '{ProtobufPrinter.of($printer).fixed32(java.lang.Float.floatToIntBits($value))}

    def parse(reader: Expr[ProtobufReader])(using Quotes, Type[Float]): Expr[Float] =
      '{java.lang.Float.intBitsToFloat($reader.fixed32())}

  given text: (Text is Inlinable) = new Inlinable:
    type Self = Text

    override def prints: Boolean = true

    override def measure(value: Expr[Text], sizes: Expr[AnyRef])(using Quotes, Type[Text])
    :   Expr[Int] =

      '{ProtobufSizes.of($sizes).stashText($value.s)}

    override def write(value: Expr[Text], printer: Expr[AnyRef], sizes: Expr[AnyRef])
      ( using Quotes, Type[Text] )
    :   Expr[Unit] =

      '{ProtobufPrinter.of($printer).stashedText(ProtobufSizes.of($sizes))}

    def parse(reader: Expr[ProtobufReader])(using Quotes, Type[Text]): Expr[Text] =
      '{$reader.text()}

  given data: (Data is Inlinable) = new Inlinable:
    type Self = Data

    override def prints: Boolean = true

    override def measure(value: Expr[Data], sizes: Expr[AnyRef])(using Quotes, Type[Data])
    :   Expr[Int] =

      '{$value.length}

    override def write(value: Expr[Data], printer: Expr[AnyRef], sizes: Expr[AnyRef])
      ( using Quotes, Type[Data] )
    :   Expr[Unit] =

      '{ProtobufPrinter.of($printer).raw($value)}

    def parse(reader: Expr[ProtobufReader])(using Quotes, Type[Data]): Expr[Data] =
      '{$reader.data()}

  given iterable: [collection <: Iterable, element]
  =>  ( element0: element is Inlinable )
  =>  ( collection[element] is Inlinable ) =
    IterableInlinable[element](element0).asInstanceOf[collection[element] is Inlinable]

// The Expr-level counterpart of `Protobuf.Parsable`: a typeclass whose
// methods are macro-time code generators. An instance receives an `Expr` of
// the reader — its window set to the value's payload — and returns an `Expr`
// of the decoded value, which the deriving macro splices directly into its
// generated parser — so an instance contributes *inlined* code, with no
// runtime dispatch, no field maps and no adapter hops between composed
// parsers.
//
// Instances are ordinary runtime values: code generation is deferred to the
// `parse` call, which receives the `Quotes` and the `Type` of `Self`, so
// `derived` needs no macro of its own. At a `Protobuf.Inlinable.parsable[T]`
// expansion, the instance behind each summoned given is obtained *live* by
// running the implicit search inside an in-macro staging compiler (the
// prescience mechanism). The constraint this inherits: an instance (and its
// type) must be compiled in an earlier run than the expansion; same-run
// instances degrade to a spliced runtime call through the
// `Decodable in Protobuf` seam.
trait Inlinable extends Typeclass:
  def parse(reader: Expr[ProtobufReader])(using Quotes, Type[Self]): Expr[Self]

  // The printing side, for `Inlinable.printable`: whether the instance generates a printer,
  // and if so the two passes over a value in a field position or window — `measure` records
  // every length-delimited size it meets in the size table and returns the value's encoded
  // length; `write` writes it, reading the sizes back. The printer and the table travel as
  // neutral carriers (`ProtobufPrinter.of`, `ProtobufSizes.of` reassert them per use), since
  // generated code is capture-erased. An instance that does not print — the default, so a
  // parse-only instance stays honest — puts its field on the `Encodable in Protobuf` seam.
  def prints: Boolean = false

  def measure(value: Expr[Self], sizes: Expr[AnyRef])(using Quotes, Type[Self]): Expr[Int] =
    quotes.reflect.report.errorAndAbort
      ( s"locomotion: the Inlinable for ${Type.show[Self]} generates no printer" )

  def write(value: Expr[Self], printer: Expr[AnyRef], sizes: Expr[AnyRef])
    ( using Quotes, Type[Self] )
  :   Expr[Unit] =

    quotes.reflect.report.errorAndAbort
      ( s"locomotion: the Inlinable for ${Type.show[Self]} generates no printer" )

  // What a field of this type yields when its number is absent from the
  // message. Proto3 has no required fields: scalars default to zero values
  // and messages to all-fields-absent records — the structural instances
  // override accordingly; the default aborts, so a custom instance that
  // does not override is loud rather than silently wrong.
  def absent(tactic: Expr[Tactic[Protobuf.Error]])(using Quotes, Type[Self]): Expr[Self] =
    '{abort(Protobuf.Error(Protobuf.Error.Reason.MissingField(0)))(using $tactic)}
