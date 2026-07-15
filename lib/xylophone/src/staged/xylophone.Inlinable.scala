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
package xylophone

import scala.quoted.*

import anticipation.*
import contingency.*
import gossamer.*
import prepositional.*
import vacuous.*

// The Expr-level counterpart of `Xml.Parsable`: a typeclass whose methods
// are macro-time code generators, following jacinta's `Inlinable` exactly.
// An instance receives an `Expr` of the reader (positioned with the current
// element just opened) and returns an `Expr` of the decoded value, which
// the deriving macro splices directly into its generated parser — so an
// instance contributes *inlined* code, with no runtime dispatch, no
// instance arrays and no adapter hops between composed parsers.
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
// runtime call through `Xml.Field`.
trait Inlinable extends Typeclass:
  def parse(reader: Expr[XmlReader])(using Quotes, Type[Self]): Expr[Self]

  // What a field of this type yields when no child element carries its
  // name, mirroring the runtime instances: an abort unless overridden (the
  // primitive instances raise and continue with a sentinel).
  def absent(tactic: Expr[Tactic[XmlError]], foci: Expr[Foci[Xml.Focus]])
    (using Quotes, Type[Self])
  :   Expr[Self] =

    '{ abort(XmlError())(using $tactic) }

object Inlinable:
  // Generates a monomorphic `Xml.Parsable` for a case class at compile
  // time, like `Xml.Parsable.staged`, but composed through `Inlinable`
  // instances: nested records, collection gathering and custom leaf parsers
  // all inline into one flat parser.
  inline def parsable[value]: value is Xml.Parsable =
    ${ xylophone.stagedInternal.inlinableParsable[value] }

  // The structural instance for a case class: reflects `Self` when invoked
  // (no macro — `Type[Self]` arrives with the call).
  def derived[product]: product is Inlinable = ProductInlinable[product]()

  private[xylophone] final class ProductInlinable[product]() extends Inlinable:
    type Self = product

    def parse(reader: Expr[XmlReader])(using Quotes, Type[product]): Expr[product] =
      stagedInternal.productFields[product](reader)

    // A missing (or wrong-shape) record: one raise at the current focus,
    // then a user-supplied `Default` sentinel or a per-sub-field absent
    // build — the derived engine's `absent()` exactly.
    override def absent(tactic: Expr[Tactic[XmlError]], foci: Expr[Foci[Xml.Focus]])
      (using Quotes, Type[product])
    :   Expr[product] =

      stagedInternal.productAbsent[product](tactic, foci)

  // The structural instance for a sealed sum whose variants are all
  // inlinable case classes and whose `Discriminable in Xml` is an
  // `Xml.DiscriminantAttribute` (established live, through the staging
  // summon): the variant rides in an attribute of the open tag, so the
  // generated code dispatches on it straight off the reader and parses the
  // chosen variant's fields in place — no element AST, no `delegate`.
  private[xylophone] final class SumInlinable[sum](val attribute: String) extends Inlinable:
    type Self = sum

    def parse(reader: Expr[XmlReader])(using Quotes, Type[sum]): Expr[sum] =
      stagedInternal.sumBody[sum](reader, attribute)

    // A missing sum field: the AST disjunction over the `Absent` sentinel —
    // no discriminator, so a raise-plus-`Default` or an abort.
    override def absent(tactic: Expr[Tactic[XmlError]], foci: Expr[Foci[Xml.Focus]])
      (using Quotes, Type[sum])
    :   Expr[sum] =

      stagedInternal.sumAbsent[sum](tactic)

  private[xylophone] final class IterableInlinable[element](val element0: element is Inlinable)
  extends Inlinable:
    type Self = Iterable[element]

    // A single element read as a collection: one element — the runtime
    // `Xml.Parsable.iterable`'s behavior when handed a lone element. In
    // *field* position the deriving generator never calls this: it gathers
    // each same-name occurrence through the element's own generator.
    def parse(reader: Expr[XmlReader])(using Quotes, Type[Iterable[element]])
    :   Expr[Iterable[element]] =

      stagedInternal.iterableBody[Iterable[element]](reader, element0)

    // A missing collection field is the empty collection on both paths.
    override def absent(tactic: Expr[Tactic[XmlError]], foci: Expr[Foci[Xml.Focus]])
      (using Quotes, Type[Iterable[element]])
    :   Expr[Iterable[element]] =

      stagedInternal.iterableAbsent[Iterable[element]](element0)

  // ── Instances ──────────────────────────────────────────────────────────
  // The leaf generators mirror the primitive `Xml.Parsable` instances (the
  // staged parser's builtin arms) exactly, so direct, staged and inlined
  // reads yield equal values and equal errors.

  given int: (Int is Inlinable) = new Inlinable:
    type Self = Int

    def parse(reader: Expr[XmlReader])(using Quotes, Type[Int]): Expr[Int] =
      '{ Xml.intParsable.parse($reader) }

    override def absent(tactic: Expr[Tactic[XmlError]], foci: Expr[Foci[Xml.Focus]])
      (using Quotes, Type[Int])
    :   Expr[Int] =

      '{ Xml.Parsable.missing[Int](0)(using $tactic) }

  given long: (Long is Inlinable) = new Inlinable:
    type Self = Long

    def parse(reader: Expr[XmlReader])(using Quotes, Type[Long]): Expr[Long] =
      '{ Xml.longParsable.parse($reader) }

    override def absent(tactic: Expr[Tactic[XmlError]], foci: Expr[Foci[Xml.Focus]])
      (using Quotes, Type[Long])
    :   Expr[Long] =

      '{ Xml.Parsable.missing[Long](0L)(using $tactic) }

  given double: (Double is Inlinable) = new Inlinable:
    type Self = Double

    def parse(reader: Expr[XmlReader])(using Quotes, Type[Double]): Expr[Double] =
      '{ Xml.doubleParsable.parse($reader) }

    override def absent(tactic: Expr[Tactic[XmlError]], foci: Expr[Foci[Xml.Focus]])
      (using Quotes, Type[Double])
    :   Expr[Double] =

      '{ Xml.Parsable.missing[Double](0.0)(using $tactic) }

  given float: (Float is Inlinable) = new Inlinable:
    type Self = Float

    def parse(reader: Expr[XmlReader])(using Quotes, Type[Float]): Expr[Float] =
      '{ Xml.floatParsable.parse($reader) }

    override def absent(tactic: Expr[Tactic[XmlError]], foci: Expr[Foci[Xml.Focus]])
      (using Quotes, Type[Float])
    :   Expr[Float] =

      '{ Xml.Parsable.missing[Float](0.0f)(using $tactic) }

  given boolean: (Boolean is Inlinable) = new Inlinable:
    type Self = Boolean

    def parse(reader: Expr[XmlReader])(using Quotes, Type[Boolean]): Expr[Boolean] =
      '{ Xml.booleanParsable.parse($reader) }

    override def absent(tactic: Expr[Tactic[XmlError]], foci: Expr[Foci[Xml.Focus]])
      (using Quotes, Type[Boolean])
    :   Expr[Boolean] =

      '{ Xml.Parsable.missing[Boolean](false)(using $tactic) }

  given text: (Text is Inlinable) = new Inlinable:
    type Self = Text

    def parse(reader: Expr[XmlReader])(using Quotes, Type[Text]): Expr[Text] =
      '{ $reader.text().or { $reader.fault(); t"" } }

    override def absent(tactic: Expr[Tactic[XmlError]], foci: Expr[Foci[Xml.Focus]])
      (using Quotes, Type[Text])
    :   Expr[Text] =

      '{ Xml.Parsable.missing[Text](t"")(using $tactic) }

  given string: (String is Inlinable) = new Inlinable:
    type Self = String

    def parse(reader: Expr[XmlReader])(using Quotes, Type[String]): Expr[String] =
      '{ ($reader.text().or { $reader.fault(); t"" }).s }

    override def absent(tactic: Expr[Tactic[XmlError]], foci: Expr[Foci[Xml.Focus]])
      (using Quotes, Type[String])
    :   Expr[String] =

      '{ Xml.Parsable.missing[String]("")(using $tactic) }

  given iterable: [collection <: Iterable, element]
  =>  (element0: element is Inlinable)
  =>  (collection[element] is Inlinable) =
    IterableInlinable[element](element0).asInstanceOf[collection[element] is Inlinable]
