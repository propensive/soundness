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
package breviloquence

import scala.quoted.*

import anticipation.*
import contingency.*
import prepositional.*

// The Expr-level counterpart of `Cbor.Parsable`: a typeclass whose methods
// are macro-time code generators. An instance receives an `Expr` of the
// reader and returns an `Expr` of the decoded value, which the deriving
// macro splices directly into its generated parser — so an instance
// contributes *inlined* code, with no runtime dispatch, no instance arrays
// and no adapter hops between composed parsers.
//
// Instances are ordinary runtime values: code generation is deferred to the
// `parse` call, which receives the `Quotes` and the `Type` of `Self`, so
// `derived` needs no macro of its own. At a `Cbor.Inlinable.parsable[T]`
// expansion, the instance behind each summoned given is obtained *live* by
// running the implicit search inside an in-macro staging compiler (the
// prescience mechanism), which composes conditional instances — a collection
// of a custom element, for example — through ordinary given resolution. The
// constraint this inherits: an instance (and its type) must be compiled in
// an earlier run than the expansion; same-run instances degrade to a spliced
// runtime call through the `Decodable in Cbor` bridge.
trait Inlinable extends Typeclass:
  def parse(reader: Expr[CborReader])(using Quotes, Type[Self]): Expr[Self]

  // What a field of this type yields when its key is absent from the map,
  // mirroring the runtime instances: an abort unless overridden.
  def absent(tactic: Expr[Tactic[CborError]])(using Quotes, Type[Self]): Expr[Self] =
    '{ Cbor.Parsable.missing[Self]()(using $tactic) }

object Inlinable:
  // Generates a monomorphic `Cbor.Parsable` for a case class or a
  // key-discriminated sealed sum at compile time, composed through
  // `Inlinable` instances: nested records, collection loops, variant
  // dispatch and custom leaf parsers all inline into one flat parser.
  inline def parsable[value]: value is Cbor.Parsable =
    ${ breviloquence.stagedInternal.inlinableParsable[value] }

  // The structural instance for a case class: reflects `Self` when invoked
  // (no macro — `Type[Self]` arrives with the call).
  def derived[product]: product is Inlinable = ProductInlinable[product]()

  // The `derives`-clause carrier: a `Self`-typed typeclass cannot appear in
  // a `derives` clause (it has no type parameters), so `case class Foo(...)
  // derives Inlinable.ForCbor` synthesizes this parameterized subtrait —
  // which *is* a `Foo is Inlinable` — into `Foo`'s companion, where the
  // staging summon finds it. The resolution ladder unwraps the delegate so
  // structural instances keep their generator identities.
  final class ForCbor[value](delegate0: value is Inlinable) extends Inlinable:
    type Self = value
    private[breviloquence] def delegate: value is Inlinable = delegate0

    def parse(reader: Expr[CborReader])(using Quotes, Type[value]): Expr[value] =
      delegate0.parse(reader)

    override def absent(tactic: Expr[Tactic[CborError]])(using Quotes, Type[value])
    :   Expr[value] =

      delegate0.absent(tactic)

  object ForCbor:
    def derived[value]: ForCbor[value] = ForCbor(Inlinable.derived[value])

  private[breviloquence] final class ProductInlinable[product]() extends Inlinable:
    type Self = product

    def parse(reader: Expr[CborReader])(using Quotes, Type[product]): Expr[product] =
      stagedInternal.productBody[product](reader)

  private[breviloquence] final class IterableInlinable[element](element0: element is Inlinable)
  extends Inlinable:
    type Self = Iterable[element]

    def parse(reader: Expr[CborReader])(using Quotes, Type[Iterable[element]])
    :   Expr[Iterable[element]] =

      stagedInternal.iterableBody[Iterable[element]](reader, element0)

  // The structural instance for a sealed sum whose `Discriminable in Cbor`
  // is the key-discriminated shape (`Cbor.DiscriminantKey`): the variant is
  // chosen by a scan-ahead of the discriminant entry, then parsed from the
  // start of the map as its product, with the discriminant entry skipped as
  // an unknown key — exactly the AST disjunction's semantics.
  private[breviloquence] final class SumInlinable[sum](key: String) extends Inlinable:
    type Self = sum

    def parse(reader: Expr[CborReader])(using Quotes, Type[sum]): Expr[sum] =
      stagedInternal.sumBody[sum](reader, key)

  // ── Instances ──────────────────────────────────────────────────────────
  // The leaf generators mirror the AST accessors' coercions exactly; the
  // conditional collection instance composes through given resolution when
  // the graph is resolved inside the staging compiler.

  given int: (Int is Inlinable) = new Inlinable:
    type Self = Int
    def parse(reader: Expr[CborReader])(using Quotes, Type[Int]): Expr[Int] =
      '{ $reader.int() }

  given long: (Long is Inlinable) = new Inlinable:
    type Self = Long
    def parse(reader: Expr[CborReader])(using Quotes, Type[Long]): Expr[Long] =
      '{ $reader.long() }

  given double: (Double is Inlinable) = new Inlinable:
    type Self = Double
    def parse(reader: Expr[CborReader])(using Quotes, Type[Double]): Expr[Double] =
      '{ $reader.double() }

  given float: (Float is Inlinable) = new Inlinable:
    type Self = Float
    def parse(reader: Expr[CborReader])(using Quotes, Type[Float]): Expr[Float] =
      '{ $reader.double().toFloat }

  given boolean: (Boolean is Inlinable) = new Inlinable:
    type Self = Boolean
    def parse(reader: Expr[CborReader])(using Quotes, Type[Boolean]): Expr[Boolean] =
      '{ $reader.boolean() }

  given text: (Text is Inlinable) = new Inlinable:
    type Self = Text
    def parse(reader: Expr[CborReader])(using Quotes, Type[Text]): Expr[Text] =
      '{ $reader.text() }

  given string: (String is Inlinable) = new Inlinable:
    type Self = String
    def parse(reader: Expr[CborReader])(using Quotes, Type[String]): Expr[String] =
      '{ $reader.string() }

  given byteString: (IArray[Byte] is Inlinable) = new Inlinable:
    type Self = IArray[Byte]
    def parse(reader: Expr[CborReader])(using Quotes, Type[IArray[Byte]]): Expr[IArray[Byte]] =
      '{ $reader.byteString() }

  given cbor: (Cbor is Inlinable) = new Inlinable:
    type Self = Cbor
    def parse(reader: Expr[CborReader])(using Quotes, Type[Cbor]): Expr[Cbor] =
      '{ $reader.value() }

  given iterable: [collection <: Iterable, element]
  =>  (element0: element is Inlinable)
  =>  (collection[element] is Inlinable) =
    IterableInlinable[element](element0).asInstanceOf[collection[element] is Inlinable]
