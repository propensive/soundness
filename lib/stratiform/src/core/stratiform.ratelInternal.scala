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
import polyvinyl.*
import prepositional.*

// The macro behind `Ratel.Provider#apply`.
object ratelInternal:
  // The root of an open image as a record of the provider `self`. The record is polyvinyl's;
  // what is decided here is the context its members are read in, which is access to the
  // image with its reader's brand, so that a record is read through the reader it came from.
  def record[self: Type]
    ( provider: Expr[Ratel.Provider],
      reader:   Expr[Ratel.Reader],
      tactic:   Expr[Tactic[Ratel.Error]] )
    ( using Quotes )
  :   Expr[Record] =

    import quotes.reflect.*

    // The brand is the `Topic` of the reader the call is given, which must be a stable path
    // for a type to be selected from it: the reader of an `open[Ratel]` block is.
    val topic = TypeRepr.of[Ratel.Reader].typeSymbol.typeMember("Topic")

    val brand: TypeRepr = reader.asTerm.underlyingArgument.tpe match
      case path: TermRef => path.select(topic)
      case _             => halt(m"the reader of the image must be a stable value")

    brand.asType.absolve match
      case '[brand] =>
        Specification.recordWith
          [ self, Ratel.Cell, Ratel.Access[brand], Ratel.Provider, Ratel.Error ]
          ( '{Ratel.Provider.root($provider)(using $reader, $tactic)} )

  // The type a member of a record is read as, without the context it is read in, and without
  // what makes it optional or repeated: the type of one value of it.
  private def member[record: Type](name: String)(using Quotes): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    def find(tpe: TypeRepr): Option[TypeRepr] = tpe.dealias match
      case Refinement(parent, `name`, info) => Some(info)
      case Refinement(parent, _, _)         => find(parent)
      case _                                => None

    def single(tpe: TypeRepr): TypeRepr = tpe.dealias match
      case contextual if contextual.isContextFunctionType =>
        single(contextual.typeArgs.last)

      case OrType(left, right) =>
        if left =:= TypeRepr.of[vacuous.Unset] then single(right)
        else if right =:= TypeRepr.of[vacuous.Unset] then single(left)
        else tpe

      case AppliedType(_, scala.List(_, element))
        if tpe.dealias.typeSymbol == TypeRepr.of[Ratel.Table[?, ?]].typeSymbol =>
          single(element)

      case AppliedType(_, scala.List(element))
        if tpe <:< TypeRepr.of[proscenium.List[Any]] =>
          single(element)

      case other =>
        other

    find(TypeRepr.of[record]) match
      case Some(info) => single(info)
      case None       => halt(m"the record has no member ${name.tt}")

  // A step of a path, to a member which is itself a record.
  def descend[record: Type](path: Expr[Ratel.Path[record]], name: Expr[String])(using Quotes)
  :   Expr[Ratel.Path[Any]] =

    import quotes.reflect.*
    val keyword = name.valueOrAbort

    member[record](keyword).asType.absolve match
      case '[type inner <: Record; inner] =>
        '{new Ratel.Path[inner](${Expr(keyword)}.tt :: $path.keywords)}

      case _ =>
        halt(m"the member ${keyword.tt} is not a record, so a path cannot go through it")

  // The end of a path, at a scalar, and the value sought there, which must have the type the
  // scalar is read as.
  def query[record: Type, value: Type]
    ( path: Expr[Ratel.Path[record]], name: Expr[String], value: Expr[value] )
    ( using Quotes )
  :   Expr[Ratel.Query] =

    import quotes.reflect.*
    val keyword = name.valueOrAbort
    val scalar = member[record](keyword)

    if !(TypeRepr.of[value] <:< scalar) then
      val expected = scalar.show.tt
      val found = TypeRepr.of[value].widen.show.tt
      halt(m"the member ${keyword.tt} is read as $expected, but the value sought is a $found")

    val text: Expr[Text] = value match
      case '{$text: Text}     => text
      case '{$number: Int}    => '{$number.toString.tt}
      case '{$number: Long}   => '{$number.toString.tt}
      case '{$number: Double} => '{$number.toString.tt}

      case _ =>
        Expr.summon[value is Encodable in Text] match
          case Some(encodable) => '{$encodable.encoded($value)}

          case None =>
            val shown = TypeRepr.of[value].widen.show.tt
            halt(m"a $shown cannot be sought, since it is not Encodable in Text")

    '{Ratel.Query(${Expr(keyword)}.tt :: $path.keywords, $text)}

