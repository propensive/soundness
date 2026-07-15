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
package jacinta

import scala.quoted.*

import anticipation.*
import contingency.*
import prepositional.*

// The Expr-level counterpart of `Json.Parsable`: a typeclass whose methods
// are macro-time code generators. An instance receives an `Expr` of the
// reader and returns an `Expr` of the decoded value, which the deriving
// macro splices directly into its generated parser — so an instance
// contributes *inlined* code, with no runtime dispatch, no instance arrays
// and no adapter hops between composed parsers.
//
// Instances are ordinary runtime values: code generation is deferred to the
// `parse` call, which receives the `Quotes` and the `Type` of `Self`, so
// `derived` needs no macro of its own. At a `Json.Inlinable.parsable[T]`
// expansion, the instance behind each summoned given is obtained *live* by
// running the implicit search inside an in-macro staging compiler (the
// prescience mechanism), which composes conditional instances — a collection
// of a custom element, for example — through ordinary given resolution. The
// constraint this inherits: an instance (and its type) must be compiled in
// an earlier run than the expansion; same-run instances degrade to a spliced
// runtime call through `Json.Field`.
trait Inlinable extends Typeclass:
  def parse(reader: Expr[JsonReader])(using Quotes, Type[Self]): Expr[Self]

  // What a field of this type yields when its key is absent from the object,
  // mirroring the runtime instances: an abort unless overridden.
  def absent(tactic: Expr[Tactic[JsonError]])(using Quotes, Type[Self]): Expr[Self] =
    '{ Json.Parsable.missing[Self]()(using $tactic) }

object Inlinable:
  // Generates a monomorphic `Json.Parsable` for a case class at compile
  // time, like `Json.Parsable.staged`, but composed through `Inlinable`
  // instances: nested records, collection loops and custom leaf parsers all
  // inline into one flat parser.
  inline def parsable[value]: value is Json.Parsable =
    ${ jacinta.stagedInternal.inlinableParsable[value] }

  // The structural instance for a case class: reflects `Self` when invoked
  // (no macro — `Type[Self]` arrives with the call).
  def derived[product]: product is Inlinable = ProductInlinable[product]()

  private[jacinta] final class ProductInlinable[product]() extends Inlinable:
    type Self = product

    def parse(reader: Expr[JsonReader])(using Quotes, Type[product]): Expr[product] =
      stagedInternal.productBody[product](reader)

  private[jacinta] final class IterableInlinable[element](element0: element is Inlinable)
  extends Inlinable:
    type Self = Iterable[element]

    def parse(reader: Expr[JsonReader])(using Quotes, Type[Iterable[element]])
    :   Expr[Iterable[element]] =

      stagedInternal.iterableBody[Iterable[element]](reader, element0)

  // ── Instances ──────────────────────────────────────────────────────────
  // The leaf generators mirror `Json.Parsable.staged`'s builtin arms; the
  // conditional collection instance composes through given resolution when
  // the graph is resolved inside the staging compiler.

  given int: (Int is Inlinable) = new Inlinable:
    type Self = Int
    def parse(reader: Expr[JsonReader])(using Quotes, Type[Int]): Expr[Int] =
      '{ $reader.long().toInt }

  given long: (Long is Inlinable) = new Inlinable:
    type Self = Long
    def parse(reader: Expr[JsonReader])(using Quotes, Type[Long]): Expr[Long] =
      '{ $reader.long() }

  given double: (Double is Inlinable) = new Inlinable:
    type Self = Double
    def parse(reader: Expr[JsonReader])(using Quotes, Type[Double]): Expr[Double] =
      '{ $reader.double() }

  given float: (Float is Inlinable) = new Inlinable:
    type Self = Float
    def parse(reader: Expr[JsonReader])(using Quotes, Type[Float]): Expr[Float] =
      '{ $reader.double().toFloat }

  given boolean: (Boolean is Inlinable) = new Inlinable:
    type Self = Boolean
    def parse(reader: Expr[JsonReader])(using Quotes, Type[Boolean]): Expr[Boolean] =
      '{ $reader.boolean() }

  given text: (Text is Inlinable) = new Inlinable:
    type Self = Text
    def parse(reader: Expr[JsonReader])(using Quotes, Type[Text]): Expr[Text] =
      '{ $reader.string() }

  given string: (String is Inlinable) = new Inlinable:
    type Self = String
    def parse(reader: Expr[JsonReader])(using Quotes, Type[String]): Expr[String] =
      '{ $reader.string().s }

  given iterable: [collection <: Iterable, element]
  =>  (element0: element is Inlinable)
  =>  (collection[element] is Inlinable) =
    IterableInlinable[element](element0).asInstanceOf[collection[element] is Inlinable]
