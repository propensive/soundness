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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package ypsiloid

import scala.collection.Factory
import scala.compiletime.*

import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*
import wisteria.*

import YamlError.Reason

trait Yaml2:
  inline given derived: [value] => value is Decodable in Yaml = summonFrom:
    case given Reflection[`value`] => DecodableDerivation.derived

  object DecodableDerivation extends ProductDerivation[[value] =>> value is Decodable in Yaml]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Decodable in Yaml = yaml =>
      val arr: IArray[Any] | Null = yaml.root.asMatchable match
        case xs: IArray[?] @unchecked if (xs.length & 1) == 0 =>
          xs.asInstanceOf[IArray[Any]]
        case _ => null

      build: [field] =>
        context =>
          val target = label.s
          var found: YamlAst | Null = null
          if arr != null then
            val n = arr.length
            var i = 0
            while i < n && found == null do
              val key = arr(i).asInstanceOf[YamlAst]
              key.asMatchable match
                case s: String if s == target => found = arr(i + 1).asInstanceOf[YamlAst]
                case _                        => ()
              i += 2
          if found != null then context.decoded(new Yaml(found))
          else
            default.or(context.decoded(new Yaml(YamlAst.Null)))

object Yaml extends Yaml2:
  def ast(value: YamlAst): Yaml = new Yaml(value)

  given yaml: Yaml is Decodable in Yaml = identity(_)

  private inline def typeMismatch[T]
      (yaml: Yaml, expected: YamlPrimitive, default: T)
      (using Tactic[YamlError])
  :   T =
    raise(YamlError(Reason.NotType(primitive(yaml.root), expected))) yet default

  given int: Tactic[YamlError] => Int is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case n: Long   => n.toInt
      case d: Double => d.toInt
      case _         => typeMismatch(yaml, YamlPrimitive.Integer, 0)

  given long: Tactic[YamlError] => Long is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case n: Long   => n
      case d: Double => d.toLong
      case _         => typeMismatch(yaml, YamlPrimitive.Integer, 0L)

  given double: Tactic[YamlError] => Double is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case d: Double => d
      case n: Long   => n.toDouble
      case _         => typeMismatch(yaml, YamlPrimitive.Decimal, 0.0)

  given float: Tactic[YamlError] => Float is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case d: Double => d.toFloat
      case n: Long   => n.toFloat
      case _         => typeMismatch(yaml, YamlPrimitive.Decimal, 0.0f)

  given boolean: Tactic[YamlError] => Boolean is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case b: Boolean => b
      case _          => typeMismatch(yaml, YamlPrimitive.Bool, false)

  given text: Tactic[YamlError] => Text is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case s: String => s.tt
      case _         => typeMismatch(yaml, YamlPrimitive.Str, t"")

  given string: Tactic[YamlError] => String is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case s: String => s
      case _         => typeMismatch(yaml, YamlPrimitive.Str, "")

  given unit: Tactic[YamlError] => Unit is Decodable in Yaml = yaml =>
    if yaml.root.asInstanceOf[AnyRef] == null then ()
    else typeMismatch(yaml, YamlPrimitive.Null, ())

  given iterable: [collection <: Iterable, element]
  =>  ( factory:   Factory[element, collection[element]],
        tactic:    Tactic[YamlError] )
  =>  ( decodable: => element is Decodable in Yaml )
  =>  collection[element] is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case xs: IArray[?] @unchecked if (xs.length & 1) == 1 =>
        // Sequence (odd length, possibly with a trailing pad sentinel).
        val n = xs.length
        val effective =
          if n > 0 && (xs(n - 1).asInstanceOf[AnyRef] eq YamlAst.arrayPad) then n - 1
          else n
        val builder = factory.newBuilder
        var i = 0
        while i < effective do
          builder += decodable.decoded(new Yaml(xs(i).asInstanceOf[YamlAst]))
          i += 1
        builder.result()

      case other =>
        raise(YamlError(Reason.NotType(primitive(other.asInstanceOf[YamlAst]),
                                       YamlPrimitive.Sequence)))
        factory.newBuilder.result()

  given map: [value: Decodable in Yaml] => Tactic[YamlError]
  =>  Map[Text, value] is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case xs: IArray[?] @unchecked if (xs.length & 1) == 0 =>
        // Mapping (even length, alternating keys and values flat).
        val n = xs.length / 2
        var result = Map.empty[Text, value]
        var i = 0
        while i < n do
          val rawKey = xs(i*2).asInstanceOf[YamlAst]
          val rawValue = xs(i*2 + 1).asInstanceOf[YamlAst]
          val keyText: Text =
            if rawKey.asInstanceOf[AnyRef] == null then t"null"
            else rawKey.asMatchable match
              case s: String  => s.tt
              case k: Long    => k.toString.tt
              case k: Double  => k.toString.tt
              case k: Boolean => k.toString.tt

              case other =>
                raise(YamlError(Reason.NotType(primitive(other.asInstanceOf[YamlAst]),
                                               YamlPrimitive.Str))) yet t""

          result = result.updated(keyText, value.decoded(new Yaml(rawValue)))
          i += 1
        result

      case other =>
        raise(YamlError(Reason.NotType(primitive(other.asInstanceOf[YamlAst]),
                                       YamlPrimitive.Mapping)))
        Map.empty

  given option: [value: Decodable in Yaml] => Option[value] is Decodable in Yaml = yaml =>
    if yaml.root.asInstanceOf[AnyRef] == null then None
    else Some(value.decoded(yaml))

  given decodable: Tactic[YamlError] => Yaml is Decodable in Text =
    text => Yaml(YamlParser.parse(text))

  def parseAll(input: Text)(using Tactic[YamlError]): List[Yaml] =
    YamlParser.parseAll(input).map(Yaml(_))

  given aggregable: Tactic[YamlError] => Yaml is Aggregable by Text =
    summon[Text is Aggregable by Text].map(text => Yaml(YamlParser.parse(text)))

  def primitive(ast: YamlAst): YamlPrimitive =
    if ast.asInstanceOf[AnyRef] == null then YamlPrimitive.Null
    else ast.asMatchable match
      case _: Boolean    => YamlPrimitive.Bool
      case _: Long       => YamlPrimitive.Integer
      case _: Double     => YamlPrimitive.Decimal
      case _: String     => YamlPrimitive.Str

      case xs: IArray[?] @unchecked =>
        if (xs.length & 1) == 0 then YamlPrimitive.Mapping else YamlPrimitive.Sequence

      case _ => YamlPrimitive.Null

class Yaml(val root: YamlAst) derives CanEqual:
  def as[value: Decodable in Yaml]: value raises YamlError = value.decoded(this)

  override def hashCode: Int = YamlAst.deepHash(root)

  override def equals(right: Any): Boolean = right match
    case right: Yaml => YamlAst.deepEquals(root, right.root)
    case _           => false

