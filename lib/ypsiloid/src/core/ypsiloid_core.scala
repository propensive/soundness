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

import scala.compiletime.asMatchable

import anticipation.*
import contextual.*
import contingency.*
import denominative.*
import gossamer.*
import prepositional.*
import spectacular.{Showable, show}
import wisteria.*
import zephyrine.*

// Renders a `Yaml.Ast` back to YAML text in conservative block style. The whole emission lives in
// this instance so `.show` is the single route to YAML text. The output is deliberately
// re-parseable to a structurally identical AST: scalars are emitted so the parser resolves them
// back to the same node type (integers as bare digits, decimals via `Double.toString` or
// `.inf`/`-.inf`/`.nan`, booleans as `true`/`false`, null as `null`); a string is a plain scalar
// only when unambiguously a string, else double-quoted; mappings/sequences are 2-space-indented
// block collections, empty ones the flow forms `{}` / `[]`. `Yaml.Ast` node shapes: scalars are
// boxed JVM values; a high-precision number is a `jacinta.Bcd` (`Array[Double]`); a mapping is an
// even-length `IArray[Any]` of alternating key/value; a sequence is an odd-length `IArray[Any]`
// (trailing `arrayPad` sentinel when the item count is even). Mirrors jacinta's `Json.Ast`
// `Showable`. Bring a `Yaml.Formatting` into scope to enable `.show` and HTTP encoding.
given astShowable: (formatting: Yaml.Formatting) => Yaml.Ast is Showable = yaml =>
  val spaces: Text =
    t"                                                                "

  // A string is safe to emit as a plain scalar when it is a leading-letter run of `[A-Za-z0-9_-]`
  // (no whitespace or indicator characters, never empty) that the parser would not resolve as a
  // boolean or null.
  def plainSafe(string: String): Boolean =
    val length = string.length

    if length == 0 then false else
      val head = string.charAt(0)

      if !((head >= 'A' && head <= 'Z') || (head >= 'a' && head <= 'z')) then false
      else
        var ok = true
        var i = 0

        while i < length && ok do
          val c = string.charAt(i)
          if !((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
            (c >= '0' && c <= '9') || c == '_' || c == '-')
          then ok = false

          i += 1

        if !ok then false else string match
          case "null" | "Null" | "NULL" | "true" | "True" | "TRUE" | "false" | "False"
          | "FALSE" =>
            false

          case _ =>
            true

  def unicode(char: Char): Text =
    val hex = Integer.toHexString(char.toInt).nn
    Text("\\u" + "0"*(4 - hex.length) + hex)

  Producer.collect[Text](): producer =>
    def indent(count: Int): Unit =
      var remaining = count

      while remaining > 0 do
        val chunk = remaining.min(64)
        producer.put(spaces, Prim, chunk)
        remaining -= chunk

    def writeString(string: String): Unit =
      if plainSafe(string) then producer.put(string.tt) else
        producer.put("\"")
        val length = string.length
        var start = 0
        var index = 0

        inline def escape(entity: Text): Unit =
          if index > start then producer.put(string.tt, start.z, index - start)
          producer.put(entity)
          start = index + 1

        while index < length do
          string.charAt(index) match
            case '"'          => escape(t"\\\"")
            case '\\'         => escape(t"\\\\")
            case '\n'         => escape(t"\\n")
            case '\r'         => escape(t"\\r")
            case '\t'         => escape(t"\\t")
            case c if c < ' ' => escape(unicode(c))
            case _            => ()

          index += 1

        if length > start then producer.put(string.tt, start.z, length - start)
        producer.put("\"")

    // Emit a scalar (or an empty collection) with no surrounding newlines.
    def scalar(ast: Yaml.Ast): Unit = ast.asMatchable match
      case bcd: Array[Double] @unchecked => producer.put(bcd.asInstanceOf[jacinta.Bcd].text.tt)
      case n: Long                       => producer.put(n.toString.tt)

      case d: Double =>
        if d.isNaN then producer.put(".nan")
        else if d == Double.PositiveInfinity then producer.put(".inf")
        else if d == Double.NegativeInfinity then producer.put("-.inf")
        else producer.put(d.toString.tt)

      case b: Boolean => producer.put(if b then "true" else "false")
      case s: String  => writeString(s)

      case xs: IArray[Any] @unchecked =>
        if (xs.length & 1) == 0 then producer.put("{}") else producer.put("[]")

      case _ => producer.put("null")

    // Scalars and empty collections render on one line; non-empty collections span multiple lines.
    def inlineable(ast: Yaml.Ast): Boolean = ast.asMatchable match
      case xs: IArray[Any] @unchecked =>
        if (xs.length & 1) == 0 then xs.length == 0 else Yaml.Ast.sequenceLength(xs) == 0

      case _ => true

    // Emit the value after a `key:` or `- ` marker: inline when scalar/empty, otherwise on
    // following lines indented one step deeper.
    def value(ast: Yaml.Ast, level: Int): Unit =
      if inlineable(ast) then
        producer.put(" ")
        scalar(ast)
        producer.put("\n")
      else
        producer.put("\n")
        block(ast, level + 2)

    def block(ast: Yaml.Ast, level: Int): Unit = ast.asMatchable match
      case xs: IArray[Any] @unchecked =>
        if (xs.length & 1) == 0 then
          val n = xs.length/2
          var i = 0

          while i < n do
            indent(level)
            scalar(xs(i*2).asInstanceOf[Yaml.Ast])
            producer.put(":")
            value(xs(i*2 + 1).asInstanceOf[Yaml.Ast], level)
            i += 1
        else
          val n = Yaml.Ast.sequenceLength(xs)
          var i = 0

          while i < n do
            indent(level)
            producer.put("-")
            value(xs(i).asInstanceOf[Yaml.Ast], level)
            i += 1

      case _ => ()

    if inlineable(yaml) then
      scalar(yaml)
      producer.put("\n")
    else
      block(yaml, 0)

given showable: Yaml.Formatting => Yaml is Showable = Yaml.unseal(_).show

extension (text: Text)
  def readAll(using Tactic[ParseError]): List[Yaml] = Yaml.parseAll(text)

extension [entity: Encodable in Yaml](value: entity) def yaml: Yaml = value.encode

extension (inline context: StringContext)
  transparent inline def y: Interpolation = interpolation[Yaml](context)
  transparent inline def yp: Interpolation = interpolation[YamlPath](context)

package formatting:
  // Block-style serializer. The default (and currently only) printer; flow
  // style may be added later. Mirrors jacinta's `formatting.indentedJsonFormatting`.
  given blockYamlFormatting: Yaml.Formatting = new Yaml.Formatting {}

package discriminables:
  given yamlByTypeDiscriminable: [value] => value is Discriminable in Yaml =
    Yaml.discriminatedUnion[value](t"type")

  given yamlByKindDiscriminable: [value] => value is Discriminable in Yaml =
    Yaml.discriminatedUnion[value](t"kind")
