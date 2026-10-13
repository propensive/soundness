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
package jacinta

import scala.quoted.*

import anticipation.*
import contingency.*
import prepositional.*

object Inlinable:
  // Generates a monomorphic `Json.Parsable` for a case class at compile
  // time, like `Json.Parsable.staged`, but composed through `Inlinable`
  // instances: nested records, collection loops and custom leaf parsers all
  // inline into one flat parser.
  inline def parsable[value]: value is Json.Parsable =
    ${jacinta.inlining.inlinableParsable[value]}

  // The writing counterpart: a monomorphic `Json.Printable` that writes a value's compact JSON
  // text straight into a writer, composed through the same instances.
  inline def printable[value]: value is Json.Printable =
    ${jacinta.inlining.inlinablePrintable[value]}

  // Evidence that the generator builds a type's codec structurally, as a record: the condition
  // on `import codecs.direct`'s generated instances. Its given fails for any other type, so the
  // search for a generated instance fails too, and the derived path resolves instead.
  object Generates:
    transparent inline given generates: [value] => Generates[value] =
      ${jacinta.inlining.generates[value]}

  final class Generates[value]()

  // The expansion-time evaluation of a user-written instance. An `Inlinable` given is an
  // ordinary runtime value, so the generator can only call its `parse` by evaluating the given
  // inside the macro — which takes an in-macro staging compiler, and with it the compiler jars
  // that core does not depend on. `jacinta.staged` holds the implementation by name;
  // without it on the classpath, a field whose type has only a user-written instance takes the
  // runtime seam.
  object Stager:
    // `jacinta.staged`, when it is on the macro classpath, holds the implementation as
    // a nested object, whose singleton is its static `MODULE$` field. `Class.forName/1`
    // searches this macro's own classloader, and unlike `ServiceLoader` it compiles for Scala
    // Native.
    private lazy val loaded: Option[Stager] =
      try
        val implementation = Class.forName("jacinta.InlinableStager$Implementation$").nn
        Some(implementation.getField("MODULE$").nn.get(null).nn.asInstanceOf[Stager])
      catch case _: ClassNotFoundException | _: LinkageError => None

    def summon[field: Type](using Quotes): Option[Inlinable] = loaded.flatMap(_.summon[field])

  trait Stager:
    def summon[field: Type](using Quotes): Option[Inlinable]

  // The structural instance for a case class: reflects `Self` when invoked
  // (no macro — `Type[Self]` arrives with the call).
  def derived[product]: product is Inlinable = ProductInlinable[product]()

  object ForJson:
    def derived[value]: ForJson[value] = ForJson(Inlinable.derived[value])

  // The `derives`-clause carrier: a `Self`-typed typeclass cannot appear in
  // a `derives` clause (it has no type parameters), so `case class Foo(...)
  // derives Inlinable.ForJson` synthesizes this parameterized subtrait —
  // which *is* a `Foo is Inlinable` — into `Foo`'s companion, where the
  // staging summon finds it. The resolution ladder unwraps the delegate so
  // structural instances keep their generator identities.
  final class ForJson[value](delegate0: value is Inlinable) extends Inlinable:
    type Self = value
    private[jacinta] def delegate: value is Inlinable = delegate0

    def parse(reader: Expr[Json.Reader])(using Quotes, Type[value]): Expr[value] =
      delegate0.parse(reader)

    override def absent(tactic: Expr[Tactic[Json.Error]])(using Quotes, Type[value])
    :   Expr[value] =

      delegate0.absent(tactic)

    override def prints: Boolean = delegate0.prints

    override def write(value: Expr[value], out: Expr[AnyRef])(using Quotes, Type[value])
    :   Expr[Unit] =

      delegate0.write(value, out)

  private[jacinta] final class ProductInlinable[product]() extends Inlinable:
    type Self = product

    def parse(reader: Expr[Json.Reader])(using Quotes, Type[product]): Expr[product] =
      inlining.productBody[product](reader)

    override def prints: Boolean = true

    override def write(value: Expr[product], out: Expr[AnyRef])(using Quotes, Type[product])
    :   Expr[Unit] =

      inlining.productWrite[product](value, out)

  private[jacinta] final class IterableInlinable[element](element0: element is Inlinable)
  extends Inlinable:
    type Self = Iterable[element]

    def parse(reader: Expr[Json.Reader])(using Quotes, Type[Iterable[element]])
    :   Expr[Iterable[element]] =

      inlining.iterableBody[Iterable[element]](reader, element0)

    override def prints: Boolean = element0.prints

    override def write(value: Expr[Iterable[element]], out: Expr[AnyRef])
      ( using Quotes, Type[Iterable[element]] )
    :   Expr[Unit] =

      inlining.iterableWrite[Iterable[element]](value, out, element0)

  // ── Instances ──────────────────────────────────────────────────────────
  // The leaf generators mirror `Json.Parsable.staged`'s builtin arms; the
  // conditional collection instance composes through given resolution when
  // the graph is resolved inside the staging compiler.

  given int: (Int is Inlinable) = new Inlinable:
    type Self = Int

    def parse(reader: Expr[Json.Reader])(using Quotes, Type[Int]): Expr[Int] =
      '{$reader.long().toInt}

    override def prints: Boolean = true

    override def write(value: Expr[Int], out: Expr[AnyRef])(using Quotes, Type[Int])
    :   Expr[Unit] =

      '{Json.Printable.writeLong($out, $value.toLong)}

  given long: (Long is Inlinable) = new Inlinable:
    type Self = Long

    def parse(reader: Expr[Json.Reader])(using Quotes, Type[Long]): Expr[Long] =
      '{$reader.long()}

    override def prints: Boolean = true

    override def write(value: Expr[Long], out: Expr[AnyRef])(using Quotes, Type[Long])
    :   Expr[Unit] =

      '{Json.Printable.writeLong($out, $value)}

  given double: (Double is Inlinable) = new Inlinable:
    type Self = Double

    def parse(reader: Expr[Json.Reader])(using Quotes, Type[Double]): Expr[Double] =
      '{$reader.double()}

    override def prints: Boolean = true

    override def write(value: Expr[Double], out: Expr[AnyRef])(using Quotes, Type[Double])
    :   Expr[Unit] =

      '{Json.Printable.writeDouble($out, $value)}

  given float: (Float is Inlinable) = new Inlinable:
    type Self = Float

    def parse(reader: Expr[Json.Reader])(using Quotes, Type[Float]): Expr[Float] =
      '{$reader.double().toFloat}

  given boolean: (Boolean is Inlinable) = new Inlinable:
    type Self = Boolean

    def parse(reader: Expr[Json.Reader])(using Quotes, Type[Boolean]): Expr[Boolean] =
      '{$reader.boolean()}

    override def prints: Boolean = true

    override def write(value: Expr[Boolean], out: Expr[AnyRef])(using Quotes, Type[Boolean])
    :   Expr[Unit] =

      '{Json.Printable.writeBoolean($out, $value)}

  given text: (Text is Inlinable) = new Inlinable:
    type Self = Text

    def parse(reader: Expr[Json.Reader])(using Quotes, Type[Text]): Expr[Text] =
      '{$reader.string()}

    override def prints: Boolean = true

    override def write(value: Expr[Text], out: Expr[AnyRef])(using Quotes, Type[Text])
    :   Expr[Unit] =

      '{Json.Printable.writeString($out, $value.s)}

  given string: (String is Inlinable) = new Inlinable:
    type Self = String

    def parse(reader: Expr[Json.Reader])(using Quotes, Type[String]): Expr[String] =
      '{$reader.string().s}

    override def prints: Boolean = true

    override def write(value: Expr[String], out: Expr[AnyRef])(using Quotes, Type[String])
    :   Expr[Unit] =

      '{Json.Printable.writeString($out, $value)}

  given iterable: [collection <: Iterable, element]
  =>  ( element0: element is Inlinable )
  =>  ( collection[element] is Inlinable ) =
    IterableInlinable[element](element0).asInstanceOf[collection[element] is Inlinable]

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
  def parse(reader: Expr[Json.Reader])(using Quotes, Type[Self]): Expr[Self]

  // The printing side, for `Inlinable.printable`: whether the instance generates a printer, and
  // if so the code that writes a value into the JSON writer, which travels as a neutral carrier
  // (`Json.Printable`'s writers reassert it per use), since generated code is capture-erased. An
  // instance that does not print — the default, so a parse-only instance stays honest — puts its
  // field on the `Encodable in Json` seam.
  def prints: Boolean = false

  def write(value: Expr[Self], out: Expr[AnyRef])(using Quotes, Type[Self]): Expr[Unit] =
    quotes.reflect.report.errorAndAbort
      ( s"jacinta: the Inlinable for ${Type.show[Self]} generates no printer" )

  // What a field of this type yields when its key is absent from the object,
  // mirroring the runtime instances: an abort unless overridden.
  def absent(tactic: Expr[Tactic[Json.Error]])(using Quotes, Type[Self]): Expr[Self] =
    '{Json.Parsable.missing[Self]()(using $tactic)}
