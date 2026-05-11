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

import anticipation.*
import contextual.*
import contingency.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*
import wisteria.*
import zephyrine.*

import YamlError.Reason

extension (text: Text)
  def readAll(using Tactic[ParseError]): List[Yaml] = Yaml.parseAll(text)

extension [entity: Encodable in Yaml](value: entity) def yaml: Yaml = value.encode

extension (inline context: StringContext)
  transparent inline def y: Interpolation = interpolation[Yaml](context)

// AST predicates and accessors mirroring Jacinta's `Json.Ast` extensions.
// `isObject` / `isArray` distinguish mappings (even-length flat array of
// alternating key/value) from sequences (odd-length, with a trailing
// `arrayPad` sentinel when the user-visible item count is even).
extension (yaml: YamlAst)
  inline def isAbsent:  Boolean = yaml.asInstanceOf[AnyRef] eq Unset
  inline def isNull:    Boolean = yaml.asInstanceOf[AnyRef | Null] == null
  inline def isLong:    Boolean = yaml.isInstanceOf[Long]
  inline def isDouble:  Boolean = yaml.isInstanceOf[Double]
  inline def isBcd:     Boolean = yaml.isInstanceOf[Array[Long]]
  inline def isNumber:  Boolean = isLong || isDouble || isBcd
  inline def isString:  Boolean = yaml.isInstanceOf[String]
  inline def isBoolean: Boolean = yaml.isInstanceOf[Boolean]

  inline def isObject: Boolean =
    yaml.isInstanceOf[Array[AnyRef]]
    && (yaml.asInstanceOf[Array[?]].length & 1) == 0

  inline def isArray: Boolean =
    yaml.isInstanceOf[Array[AnyRef]]
    && (yaml.asInstanceOf[Array[?]].length & 1) == 1

  private def expected(yamlPrimitive: YamlPrimitive)(using Tactic[YamlError]): Unit =
    raise:
      YamlError(if isAbsent then Reason.Absent else Reason.NotType(primitive, yamlPrimitive))

  inline def arrayLength: Int = YamlAst.sequenceLength(yaml.asInstanceOf[IArray[Any]])

  inline def arrayElement(index: Int): YamlAst =
    yaml.asInstanceOf[IArray[YamlAst]](index)

  inline def objectSize: Int = yaml.asInstanceOf[IArray[Any]].length/2

  inline def objectKey(index: Int): String =
    yaml.asInstanceOf[IArray[Any]](index*2).asInstanceOf[String]

  inline def objectValue(index: Int): YamlAst =
    yaml.asInstanceOf[IArray[Any]](index*2 + 1).asInstanceOf[YamlAst]

  // Linear scan for a key — returns the pair-indexed position (i.e.
  // `objectKey(result)` retrieves the key, `objectValue(result)` the
  // value) or `-1` if the key is absent. Only string-typed keys match.
  def objectIndexOf(key: String): Int =
    if !isObject then -1 else
      val arr = yaml.asInstanceOf[Array[?]]
      val len = arr.length
      var i = 0
      var hit = -1
      while i < len && hit < 0 do
        arr(i) match
          case s: String if s == key => hit = i/2
          case _                     => ()
        i += 2
      hit

  def array(using Tactic[YamlError]): IArray[YamlAst] =
    if isArray then
      val full = yaml.asInstanceOf[IArray[YamlAst]]
      val n = arrayLength
      if n == full.length then full
      else IArray.tabulate(n)(full(_))
    else expected(YamlPrimitive.Sequence) yet IArray[YamlAst]()

  def double(using Tactic[YamlError]): Double = yaml.asInstanceOf[Matchable] match
    case value: Double                 => value
    case value: Long                   => value.toDouble
    case value: Array[Long] @unchecked => value.asInstanceOf[jacinta.Bcd].toDouble
    case _                             => expected(YamlPrimitive.Decimal) yet 0.0

  def long(using Tactic[YamlError]): Long = yaml.asInstanceOf[Matchable] match
    case value: Long                   => value
    case value: Double                 => value.toLong
    case value: Array[Long] @unchecked => value.asInstanceOf[jacinta.Bcd].toLong.or(0L)
    case _                             => expected(YamlPrimitive.Integer) yet 0L

  def bcd(using Tactic[YamlError]): jacinta.Bcd = yaml.asInstanceOf[Matchable] match
    case value: Array[Long] @unchecked => value.asInstanceOf[jacinta.Bcd]
    case value: Long                   => jacinta.Bcd(BigDecimal(value))
    case value: Double                 => jacinta.Bcd(BigDecimal(value))
    case _                             =>
      expected(YamlPrimitive.Decimal) yet jacinta.Bcd(BigDecimal(0))

  def string(using Tactic[YamlError]): Text =
    if isString then yaml.asInstanceOf[String].tt
    else expected(YamlPrimitive.Str) yet t""

  def boolean(using Tactic[YamlError]): Boolean =
    if isBoolean then yaml.asInstanceOf[Boolean]
    else expected(YamlPrimitive.Bool) yet false

  def primitive: YamlPrimitive = Yaml.primitive(yaml)

package yamlDiscriminables:
  given discriminatedUnionByType: [value] => value is Discriminable in Yaml =
    Yaml.discriminatedUnion[value](t"type")

  given discriminatedUnionByKind: [value] => value is Discriminable in Yaml =
    Yaml.discriminatedUnion[value](t"kind")
