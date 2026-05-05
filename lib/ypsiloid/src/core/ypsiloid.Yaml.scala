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
      val values: Map[Text, YamlAst] = yaml.root match
        case YamlAst.Mapping(entries) =>
          entries.foldLeft(Map.empty[Text, YamlAst]): (acc, pair) =>
            pair(0) match
              case YamlAst.Str(text) => acc.updated(text, pair(1))
              case _                 => acc

        case _ =>
          Map.empty[Text, YamlAst]

      build: [field] =>
        context =>
          values.get(label) match
            case Some(item) => context.decoded(new Yaml(item))
            case None       => default.or(context.decoded(new Yaml(YamlAst.Null)))

object Yaml extends Yaml2:
  def ast(value: YamlAst): Yaml = new Yaml(value)

  given yaml: Yaml is Decodable in Yaml = identity(_)

  private inline def expect[result]
    ( yaml: Yaml, expected: YamlPrimitive )
    ( default: => result )
    ( handler: PartialFunction[YamlAst, result] )
    ( using Tactic[YamlError] )
  :   result =

    handler.applyOrElse(yaml.root, other =>
      raise(YamlError(Reason.NotType(primitive(other), expected))) yet default)

  given int: Tactic[YamlError] => Int is Decodable in Yaml = yaml =>
    expect(yaml, YamlPrimitive.Integer)(0):
      case YamlAst.Integer(value) => value.toInt
      case YamlAst.Decimal(value) => value.toInt

  given long: Tactic[YamlError] => Long is Decodable in Yaml = yaml =>
    expect(yaml, YamlPrimitive.Integer)(0L):
      case YamlAst.Integer(value) => value
      case YamlAst.Decimal(value) => value.toLong

  given double: Tactic[YamlError] => Double is Decodable in Yaml = yaml =>
    expect(yaml, YamlPrimitive.Decimal)(0.0):
      case YamlAst.Decimal(value) => value
      case YamlAst.Integer(value) => value.toDouble

  given float: Tactic[YamlError] => Float is Decodable in Yaml = yaml =>
    expect(yaml, YamlPrimitive.Decimal)(0.0f):
      case YamlAst.Decimal(value) => value.toFloat
      case YamlAst.Integer(value) => value.toFloat

  given boolean: Tactic[YamlError] => Boolean is Decodable in Yaml = yaml =>
    expect(yaml, YamlPrimitive.Bool)(false):
      case YamlAst.Bool(value) => value

  given text: Tactic[YamlError] => Text is Decodable in Yaml = yaml =>
    expect(yaml, YamlPrimitive.Str)(t""):
      case YamlAst.Str(value) => value

  given string: Tactic[YamlError] => String is Decodable in Yaml = yaml =>
    expect(yaml, YamlPrimitive.Str)(""):
      case YamlAst.Str(value) => value.s

  given unit: Tactic[YamlError] => Unit is Decodable in Yaml = yaml =>
    expect(yaml, YamlPrimitive.Null)(()):
      case YamlAst.Null => ()

  given iterable: [collection <: Iterable, element]
  =>  ( factory:   Factory[element, collection[element]],
        tactic:    Tactic[YamlError] )
  =>  ( decodable: => element is Decodable in Yaml )
  =>  collection[element] is Decodable in Yaml = yaml =>
    yaml.root match
      case YamlAst.Sequence(items) =>
        val builder = factory.newBuilder
        items.foreach(item => builder += decodable.decoded(Yaml(item)))
        builder.result()

      case other =>
        raise(YamlError(Reason.NotType(primitive(other), YamlPrimitive.Sequence)))
        factory.newBuilder.result()

  given map: [value: Decodable in Yaml] => Tactic[YamlError]
  =>  Map[Text, value] is Decodable in Yaml = yaml =>
    yaml.root match
      case YamlAst.Mapping(entries) =>
        entries.foldLeft(Map.empty[Text, value]): (acc, pair) =>
          val (rawKey, rawValue) = pair
          val keyText = rawKey match
            case YamlAst.Str(text)      => text
            case YamlAst.Integer(value) => value.toString.tt
            case YamlAst.Decimal(value) => value.toString.tt
            case YamlAst.Bool(value)    => value.toString.tt
            case YamlAst.Null           => t"null"

            case other =>
              raise(YamlError(Reason.NotType(primitive(other), YamlPrimitive.Str))) yet t""

          acc.updated(keyText, value.decoded(Yaml(rawValue)))

      case other =>
        raise(YamlError(Reason.NotType(primitive(other), YamlPrimitive.Mapping)))
        Map.empty

  given option: [value: Decodable in Yaml] => Option[value] is Decodable in Yaml = yaml =>
    yaml.root match
      case YamlAst.Null => None
      case _            => Some(value.decoded(yaml))

  given decodable: Tactic[YamlError] => Yaml is Decodable in Text =
    text => Yaml(YamlParser.parse(text))

  def parseAll(input: Text)(using Tactic[YamlError]): List[Yaml] =
    YamlParser.parseAll(input).map(Yaml(_))

  given aggregable: Tactic[YamlError] => Yaml is Aggregable by Text =
    summon[Text is Aggregable by Text].map(text => Yaml(YamlParser.parse(text)))

  def primitive(ast: YamlAst): YamlPrimitive = ast match
    case YamlAst.Null        => YamlPrimitive.Null
    case YamlAst.Bool(_)     => YamlPrimitive.Bool
    case YamlAst.Integer(_)  => YamlPrimitive.Integer
    case YamlAst.Decimal(_)  => YamlPrimitive.Decimal
    case YamlAst.Str(_)      => YamlPrimitive.Str
    case YamlAst.Sequence(_) => YamlPrimitive.Sequence
    case YamlAst.Mapping(_)  => YamlPrimitive.Mapping

class Yaml(val root: YamlAst) derives CanEqual:
  def as[value: Decodable in Yaml]: value raises YamlError = value.decoded(this)

  override def hashCode: Int = root.hashCode

  override def equals(right: Any): Boolean = right match
    case right: Yaml => root == right.root
    case _           => false
