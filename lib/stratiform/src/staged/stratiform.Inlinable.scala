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
import vacuous.*

// The Expr-level counterpart of `Tel.Parsable`: a typeclass whose methods
// are macro-time code generators, following jacinta's `Inlinable` exactly.
// An instance receives an `Expr` of the reader (and, because a TEL value is
// an *entry* whose extent depends on its indent, an `Expr` of the current
// indent) and returns an `Expr` of the decoded value, which the deriving
// macro splices directly into its generated parser — so an instance
// contributes *inlined* code, with no runtime dispatch, no instance arrays
// and no adapter hops between composed parsers.
//
// Instances are ordinary runtime values: code generation is deferred to the
// `parse` call, which receives the `Quotes` and the `Type` of `Self`, so
// `derived` needs no macro of its own. At a `Inlinable.parsable[T]`
// expansion, the instance behind each summoned given is obtained *live* by
// running the implicit search inside an in-macro staging compiler (the
// prescience mechanism), which composes conditional instances — a collection
// of a custom element, for example — through ordinary given resolution. The
// constraint this inherits: an instance (and its type) must be compiled in
// an earlier run than the expansion; same-run instances degrade to a spliced
// runtime call through `Tel.Field`.
trait Inlinable extends Typeclass:
  def parse(reader: Expr[TelReader], indent: Expr[Int])(using Quotes, Type[Self]): Expr[Self]

  // What a field of this type yields when its keyword never arrives,
  // mirroring the runtime instances: an abort unless overridden (the
  // primitive instances raise and continue with a sentinel).
  def absent(tactic: Expr[Tactic[TelError]])(using Quotes, Type[Self]): Expr[Self] =
    '{ abort(TelError(TelError.Reason.Absent))(using $tactic) }

object Inlinable:
  // Generates a monomorphic `Tel.Parsable` for a case class at compile
  // time, like `Tel.Parsable.staged`, but composed through `Inlinable`
  // instances: nested records, collection gathering and custom leaf parsers
  // all inline into one flat parser.
  inline def parsable[value]: value is Tel.Parsable =
    ${ stratiform.stagedInternal.inlinableParsable[value] }

  // The structural instance for a case class: reflects `Self` when invoked
  // (no macro — `Type[Self]` arrives with the call).
  def derived[product]: product is Inlinable = ProductInlinable[product]()

  private[stratiform] final class ProductInlinable[product]() extends Inlinable:
    type Self = product

    // A record field's value is its children, one level deeper than its own
    // entry line — the derived engine's `parse(reader, indent)`.
    def parse(reader: Expr[TelReader], indent: Expr[Int])(using Quotes, Type[product])
    :   Expr[product] =

      '{
        $reader.finishLine()
        val indent1 = $indent + 1
        ${ stagedInternal.productFields[product](reader, 'indent1) }
      }

  private[stratiform] final class IterableInlinable[element](val element0: element is Inlinable)
  extends Inlinable:
    type Self = Iterable[element]

    // A single entry read as a collection: one element — the runtime
    // `Tel.Parsable.iterable`'s behavior when handed a lone compound. In
    // *field* position the deriving generator never calls this: it gathers
    // each occurrence of the keyword through the element's own generator.
    def parse(reader: Expr[TelReader], indent: Expr[Int])(using Quotes, Type[Iterable[element]])
    :   Expr[Iterable[element]] =

      stagedInternal.iterableBody[Iterable[element]](reader, indent, element0)

  // ── Instances ──────────────────────────────────────────────────────────
  // The leaf generators mirror the primitive `Tel.Parsable` instances
  // exactly — including the `Absent`/`NotScalar` fault split — so direct,
  // staged and inlined reads yield equal values and equal errors. `Double`
  // keeps the text path, like `doubleParsable`.

  given int: (Int is Inlinable) = new Inlinable:
    type Self = Int

    def parse(reader: Expr[TelReader], indent: Expr[Int])(using Quotes, Type[Int]): Expr[Int] =
      '{ $reader.int().lay(Tel.Parsable.scalarFault($reader, t"Int", 0)) { value => value } }

    override def absent(tactic: Expr[Tactic[TelError]])(using Quotes, Type[Int]): Expr[Int] =
      '{ Tel.Parsable.missing[Int](0)(using $tactic) }

  given long: (Long is Inlinable) = new Inlinable:
    type Self = Long

    def parse(reader: Expr[TelReader], indent: Expr[Int])(using Quotes, Type[Long]): Expr[Long] =
      '{ $reader.long().lay(Tel.Parsable.scalarFault($reader, t"Long", 0L)) { value => value } }

    override def absent(tactic: Expr[Tactic[TelError]])(using Quotes, Type[Long]): Expr[Long] =
      '{ Tel.Parsable.missing[Long](0L)(using $tactic) }

  given boolean: (Boolean is Inlinable) = new Inlinable:
    type Self = Boolean

    def parse(reader: Expr[TelReader], indent: Expr[Int])(using Quotes, Type[Boolean])
    :   Expr[Boolean] =

      '{
        $reader.boolean().lay(Tel.Parsable.scalarFault($reader, t"Boolean", false)):
          value => value
      }

    override def absent(tactic: Expr[Tactic[TelError]])(using Quotes, Type[Boolean])
    :   Expr[Boolean] =

      '{ Tel.Parsable.missing[Boolean](false)(using $tactic) }

  given double: (Double is Inlinable) = new Inlinable:
    type Self = Double

    def parse(reader: Expr[TelReader], indent: Expr[Int])(using Quotes, Type[Double])
    :   Expr[Double] =

      '{
        $reader.atom().lay({ $reader.fault(TelError.Reason.Absent); 0.0 }): atom =>
          try java.lang.Double.parseDouble(atom.s)
          catch case _: NumberFormatException =>
            $reader.fault(TelError.Reason.NotScalar(atom, t"Double"))
            0.0
      }

    override def absent(tactic: Expr[Tactic[TelError]])(using Quotes, Type[Double])
    :   Expr[Double] =

      '{ Tel.Parsable.missing[Double](0.0)(using $tactic) }

  given text: (Text is Inlinable) = new Inlinable:
    type Self = Text

    def parse(reader: Expr[TelReader], indent: Expr[Int])(using Quotes, Type[Text]): Expr[Text] =
      '{ $reader.atom().lay({ $reader.fault(TelError.Reason.Absent); t"" }) { atom => atom } }

    override def absent(tactic: Expr[Tactic[TelError]])(using Quotes, Type[Text]): Expr[Text] =
      '{ Tel.Parsable.missing[Text](t"")(using $tactic) }

  given string: (String is Inlinable) = new Inlinable:
    type Self = String

    def parse(reader: Expr[TelReader], indent: Expr[Int])(using Quotes, Type[String])
    :   Expr[String] =

      '{ $reader.atom().lay({ $reader.fault(TelError.Reason.Absent); "" }) { atom => atom.s } }

    override def absent(tactic: Expr[Tactic[TelError]])(using Quotes, Type[String])
    :   Expr[String] =

      '{ Tel.Parsable.missing[String]("")(using $tactic) }

  given iterable: [collection <: Iterable, element]
  =>  (element0: element is Inlinable)
  =>  (collection[element] is Inlinable) =
    IterableInlinable[element](element0).asInstanceOf[collection[element] is Inlinable]
