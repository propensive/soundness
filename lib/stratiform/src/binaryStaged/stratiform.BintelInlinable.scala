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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import scala.quoted.*

import anticipation.*
import contingency.*
import gossamer.*
import prepositional.*

// The Expr-level counterpart of `Bintel.Parsable`: a typeclass whose
// methods are macro-time code generators, mirroring `stratiform.Inlinable`
// (the text format's) for the binary encoding. An instance receives an
// `Expr` of the reader — positioned at its value's payload — and returns an
// `Expr` of the decoded value, spliced directly into the composed parser.
//
// BinTEL is schema-driven: the wire carries no keywords, only flat keyword
// *indices* resolved against the schema derived from the value's type. The
// derivation gives every case-class field exactly one flat slot, in
// declaration order, so the generated dispatch is a positional table — the
// index IS the field's position. Decoding a BinTEL document through the AST
// path ends in the *text* format's `Tel.Decodable`, so a generated parser's
// value semantics (leaf faults, absent handling, foci) mirror the text
// format's `Inlinable` exactly.
trait BintelInlinable extends Typeclass:
  def parse(reader: Expr[BintelReader])(using Quotes, Type[Self]): Expr[Self]

  // What a field of this type yields when its index is absent from the
  // struct body, mirroring the text format's instances: an abort unless
  // overridden.
  def absent(tactic: Expr[Tactic[TelError]])(using Quotes, Type[Self]): Expr[Self] =
    '{ abort(TelError(TelError.Reason.Absent))(using $tactic) }

object BintelInlinable:
  // Generates a monomorphic `Bintel.Parsable` for a case class or a sealed
  // sum at compile time: nested records, repeated-field gathering, variant
  // dispatch and leaf parsers all inline into one flat parser over the
  // body bytes — no `Tel.Element` tree, no `Tel` presentation, no
  // text-format decode.
  inline def parsable[value]: value is Bintel.Parsable =
    ${ stratiform.bintelInternal.inlinableParsable[value] }

  // The structural instance for a case class: reflects `Self` when invoked
  // (no macro — `Type[Self]` arrives with the call).
  def derived[product]: product is BintelInlinable = ProductInlinable[product]()

  private[stratiform] final class ProductInlinable[product]() extends BintelInlinable:
    type Self = product

    def parse(reader: Expr[BintelReader])(using Quotes, Type[product]): Expr[product] =
      bintelInternal.productBody[product](reader)

  // A marker in the resolution ladder: a repeatable field's occurrences are
  // gathered by its *struct's* generated parser (they repeat the field's
  // keyword index), so collection parsing lives in the product generator.
  private[stratiform] final class IterableInlinable[element]
    (val element0: element is BintelInlinable)
  extends BintelInlinable:
    type Self = Iterable[element]

    def parse(reader: Expr[BintelReader])(using Quotes, Type[Iterable[element]])
    :   Expr[Iterable[element]] =

      quotes.reflect.report.errorAndAbort
        ("stratiform: a repeatable field parses through its struct's generated parser; a "+
          "collection has no standalone BinTEL form")

  // The structural instance for a *top-level* sealed sum, whose schema root
  // is a struct with one `SelectRef` member: one flat slot per variant, so
  // the wire index chooses the variant directly. (A sum in *field*
  // position derives to an unresolvable `Reference` on the AST path too,
  // so nested sums are rejected during planning.)
  private[stratiform] final class SumInlinable[sum]() extends BintelInlinable:
    type Self = sum

    def parse(reader: Expr[BintelReader])(using Quotes, Type[sum]): Expr[sum] =
      bintelInternal.sumBody[sum](reader)

  // ── Instances ──────────────────────────────────────────────────────────
  // The leaf generators mirror the text format's primitive semantics
  // exactly — including the `Absent`/`NotScalar` fault split and sentinel
  // values — since the AST path decodes the presented `Tel` through the
  // very same text-format instances. A BinTEL scalar always carries text,
  // so only the `NotScalar` arm of the fault split is reachable.

  given int: (Int is BintelInlinable) = new BintelInlinable:
    type Self = Int

    def parse(reader: Expr[BintelReader])(using Quotes, Type[Int]): Expr[Int] =
      '{
        val atom = $reader.scalar()

        try atom.s.toInt catch case _: NumberFormatException =>
          raise(TelError(TelError.Reason.NotScalar(atom, t"Int")))(using infer[Tactic[TelError]])
          0
      }

    override def absent(tactic: Expr[Tactic[TelError]])(using Quotes, Type[Int]): Expr[Int] =
      '{ Tel.Parsable.missing[Int](0)(using $tactic) }

  given long: (Long is BintelInlinable) = new BintelInlinable:
    type Self = Long

    def parse(reader: Expr[BintelReader])(using Quotes, Type[Long]): Expr[Long] =
      '{
        val atom = $reader.scalar()

        try atom.s.toLong catch case _: NumberFormatException =>
          raise(TelError(TelError.Reason.NotScalar(atom, t"Long")))(using infer[Tactic[TelError]])
          0L
      }

    override def absent(tactic: Expr[Tactic[TelError]])(using Quotes, Type[Long]): Expr[Long] =
      '{ Tel.Parsable.missing[Long](0L)(using $tactic) }

  given boolean: (Boolean is BintelInlinable) = new BintelInlinable:
    type Self = Boolean

    def parse(reader: Expr[BintelReader])(using Quotes, Type[Boolean]): Expr[Boolean] =
      '{
        val atom = $reader.scalar()

        atom.s match
          case "true"  => true
          case "false" => false

          case _ =>
            raise(TelError(TelError.Reason.NotScalar(atom, t"Boolean")))
              (using infer[Tactic[TelError]])

            false
      }

    override def absent(tactic: Expr[Tactic[TelError]])(using Quotes, Type[Boolean])
    :   Expr[Boolean] =

      '{ Tel.Parsable.missing[Boolean](false)(using $tactic) }

  given double: (Double is BintelInlinable) = new BintelInlinable:
    type Self = Double

    def parse(reader: Expr[BintelReader])(using Quotes, Type[Double]): Expr[Double] =
      '{
        val atom = $reader.scalar()

        try atom.s.toDouble catch case _: NumberFormatException =>
          raise(TelError(TelError.Reason.NotScalar(atom, t"Double")))
            (using infer[Tactic[TelError]])

          0.0
      }

    override def absent(tactic: Expr[Tactic[TelError]])(using Quotes, Type[Double])
    :   Expr[Double] =

      '{ Tel.Parsable.missing[Double](0.0)(using $tactic) }

  given text: (Text is BintelInlinable) = new BintelInlinable:
    type Self = Text

    def parse(reader: Expr[BintelReader])(using Quotes, Type[Text]): Expr[Text] =
      '{ $reader.scalar() }

    override def absent(tactic: Expr[Tactic[TelError]])(using Quotes, Type[Text]): Expr[Text] =
      '{ Tel.Parsable.missing[Text](t"")(using $tactic) }

  given string: (String is BintelInlinable) = new BintelInlinable:
    type Self = String

    def parse(reader: Expr[BintelReader])(using Quotes, Type[String]): Expr[String] =
      '{ $reader.scalar().s }

    override def absent(tactic: Expr[Tactic[TelError]])(using Quotes, Type[String])
    :   Expr[String] =

      '{ Tel.Parsable.missing[String]("")(using $tactic) }

  given iterable: [collection <: Iterable, element]
  =>  (element0: element is BintelInlinable)
  =>  (collection[element] is BintelInlinable) =
    IterableInlinable[element](element0).asInstanceOf[collection[element] is BintelInlinable]
