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

import scala.quoted.*

import anticipation.*
import contingency.*
import fulminate.*
import prepositional.*
import proscenium.List
import vacuous.*

// The macro behind `Ratel.Provider`, apart from `internal`, whose imports are the standard
// library's collections.
object ratelInternal:
  // The type of the record a `Ratel.Provider` gives for its schema's document, and the
  // expression which reads it. The provider is an object which is already compiled, so its
  // schema is read from the object itself, as polyvinyl reads a specification's; the type is
  // made from the same `RatelShape` a row reads its members against.
  def ratelRecord[self: Type]
    ( reader: Expr[Ratel.Reader], tactic: Expr[Tactic[Ratel.Error]] )
    ( using Quotes )
  :   Expr[Ratel.Record] =

    import quotes.reflect.*
    import RatelShape.{Arity, Primitive, Shape}

    val repr = TypeRepr.of[self]
    val widened = repr.widenTermRefByName

    def isModule(symbol: Symbol): Boolean = symbol.exists && symbol.flags.is(Flags.Module)

    val module =
      if isModule(repr.termSymbol) then repr.termSymbol
      else if isModule(widened.typeSymbol) then widened.typeSymbol.companionModule
      else Symbol.noSymbol

    if !isModule(module) then
      halt(m"a record can only be read by an object which extends Ratel.Provider")

    def binaryName(symbol: Symbol): String =
      val owner = symbol.owner
      val simple = symbol.name.stripSuffix("$")
      if owner.isPackageDef then s"${owner.fullName}.$simple" else s"${binaryName(owner)}$$$simple"

    val provider =
      Class.forName(binaryName(module)+"$", true, ratelInternal.getClass.getClassLoader).nn
      . getField("MODULE$").nn.get(null).asInstanceOf[Ratel.Provider]

    val shape: List[(Text, Shape)] =
      given Tactic[Ratel.Error] = HaltTactic[Ratel.Error, Any]()
      provider.shape

    // The brand is the `Topic` of the reader the call is given, which must be a stable path
    // for a type to be selected from it: the reader of an `open[Ratel]` block is.
    val topic = TypeRepr.of[Ratel.Reader].typeSymbol.typeMember("Topic")

    val brand: TypeRepr = reader.asTerm.underlyingArgument.tpe match
      case path: TermRef => path.select(topic)
      case _             => halt(m"the reader of the image must be a stable value")

    def optional(tpe: TypeRepr): TypeRepr = tpe.asType.absolve match
      case '[element] => TypeRepr.of[Optional[element]]

    def list(tpe: TypeRepr): TypeRepr = tpe.asType.absolve match
      case '[element] => TypeRepr.of[List[element]]

    // Written as aliases, in quoted types: a refinement made with `TypeBounds(brand, brand)` is
    // a pair of bounds, which capture checking does not keep equal when it maps a type.
    val plain: TypeRepr = brand.asType.absolve match
      case '[brand] => TypeRepr.of[Ratel.Record of brand]

    def table(entry: TypeRepr): TypeRepr = brand.asType.absolve match
      case '[brand] => entry.asType.absolve match
        case '[entry] => TypeRepr.of[Ratel.Table[entry] of brand]

    def record(fields: Optional[List[(Text, Shape)]]): TypeRepr =
      fields.lay(plain): fields =>
        fields.stdlib.foldLeft(plain): (refined, field) =>
          Refinement(refined, field(0).s, typeOf(field(1)))

    def typeOf(shape: Shape): TypeRepr = shape match
      case Shape.Flag  => TypeRepr.of[Boolean]
      case Shape.Count => TypeRepr.of[Long]

      case Shape.Value(primitive, arity) =>
        val base = primitive match
          case Primitive.Whole   => TypeRepr.of[Int]
          case Primitive.Large   => TypeRepr.of[Long]
          case Primitive.Real    => TypeRepr.of[Double]
          case Primitive.Textual => TypeRepr.of[Text]

        arity match
          case Arity.One      => base
          case Arity.Optional => optional(base)
          case Arity.Many     => list(base)

      case Shape.Nested(fields, absent) =>
        if absent then optional(record(fields)) else record(fields)

      case Shape.Table(fields) =>
        table(record(fields))

    val target = Ref(module).asExprOf[Ratel.Provider]

    record(shape).asType.absolve match
      case '[type result <: Ratel.Record; result] =>
        '{Ratel.Provider.root($target)(using $reader, $tactic).asInstanceOf[result]}
