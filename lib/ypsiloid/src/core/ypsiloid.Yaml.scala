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

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import prepositional.*
import proscenium.*
import rudiments.*
import turbulence.*

import YamlError.Reason

object Yaml:
  def ast(value: YamlAst): Yaml = new Yaml(value)

  given yaml: Yaml is Decodable in Yaml = identity(_)

  given int: Tactic[YamlError] => Int is Decodable in Yaml = yaml =>
    yaml.root match
      case YamlAst.Integer(value) => value.toInt
      case YamlAst.Decimal(value) => value.toInt

      case other =>
        raise(YamlError(Reason.NotType(primitive(other), YamlPrimitive.Integer))) yet 0

  given long: Tactic[YamlError] => Long is Decodable in Yaml = yaml =>
    yaml.root match
      case YamlAst.Integer(value) => value
      case YamlAst.Decimal(value) => value.toLong

      case other =>
        raise(YamlError(Reason.NotType(primitive(other), YamlPrimitive.Integer))) yet 0L

  given double: Tactic[YamlError] => Double is Decodable in Yaml = yaml =>
    yaml.root match
      case YamlAst.Decimal(value) => value
      case YamlAst.Integer(value) => value.toDouble

      case other =>
        raise(YamlError(Reason.NotType(primitive(other), YamlPrimitive.Decimal))) yet 0.0

  given float: Tactic[YamlError] => Float is Decodable in Yaml = yaml =>
    yaml.root match
      case YamlAst.Decimal(value) => value.toFloat
      case YamlAst.Integer(value) => value.toFloat

      case other =>
        raise(YamlError(Reason.NotType(primitive(other), YamlPrimitive.Decimal))) yet 0.0f

  given boolean: Tactic[YamlError] => Boolean is Decodable in Yaml = yaml =>
    yaml.root match
      case YamlAst.Bool(value) => value

      case other =>
        raise(YamlError(Reason.NotType(primitive(other), YamlPrimitive.Bool))) yet false

  given text: Tactic[YamlError] => Text is Decodable in Yaml = yaml =>
    yaml.root match
      case YamlAst.Str(value) => value

      case other =>
        raise(YamlError(Reason.NotType(primitive(other), YamlPrimitive.Str))) yet t""

  given string: Tactic[YamlError] => String is Decodable in Yaml = yaml =>
    yaml.root match
      case YamlAst.Str(value) => value.s

      case other =>
        raise(YamlError(Reason.NotType(primitive(other), YamlPrimitive.Str))) yet ""

  given unit: Tactic[YamlError] => Unit is Decodable in Yaml = yaml =>
    yaml.root match
      case YamlAst.Null => ()

      case other =>
        raise(YamlError(Reason.NotType(primitive(other), YamlPrimitive.Null)))

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

  given decodable: Yaml is Decodable in Text = text => Yaml(YamlParser.parse(text))

  given aggregable: Yaml is Aggregable by Text = source0 =>
    var source = source0
    val builder = new StringBuilder()

    while !source.nil do
      builder.append(source.head.s)
      source = source.tail

    Yaml(YamlParser.parse(builder.toString.tt))

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
