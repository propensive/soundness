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
import gossamer.*

// Renders a `Yaml.Ast` back to YAML text in block style. Mirrors
// `jacinta.JsonPrinter`. The emitter is deliberately conservative so that
// the output re-parses (via `YamlParser`) to a structurally identical AST:
//
//   - scalars are emitted so the parser's `resolvePlainScalar` resolves them
//     back to the same node type — integers as bare digits, decimals via
//     `Double.toString` (or `.inf`/`-.inf`/`.nan`), booleans as `true`/`false`,
//     null as `null`;
//   - a string is emitted as a *plain* scalar only when it is unambiguously a
//     string (a leading-letter run of `[A-Za-z0-9_-]` that is not a reserved
//     word); otherwise it is double-quoted with escapes the parser accepts;
//   - mappings and sequences are emitted as indented block collections (2-space
//     step); empty collections use the flow forms `{}` / `[]`.
//
// `Yaml.Ast` node shapes (see `ypsiloid.Yaml.scala`): scalars are boxed JVM
// values (`String`, `Long`, `Double`, `Boolean`, `null`); a high-precision
// number is a `jacinta.Bcd` (`Array[Double]`); a mapping is an even-length
// `IArray[Any]` of alternating key/value; a sequence is an odd-length
// `IArray[Any]` (with a trailing `arrayPad` sentinel when the item count is
// even). Mappings and sequences are told apart by length parity.
object YamlPrinter:
  def print(yaml: Yaml.Ast): Text = Text.build:
    def spaces(n: Int): Unit =
      var i = 0

      while i < n do
        append(' ')
        i += 1

    // A string is safe to emit as a plain scalar when it is a leading-letter
    // run of `[A-Za-z0-9_-]` (no whitespace or indicator characters, never
    // empty) that the parser would not resolve as a boolean or null.
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
            if !((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
                || (c >= '0' && c <= '9') || c == '_' || c == '-')
            then ok = false

            i += 1

          ok && (string match
            case "null" | "Null" | "NULL" | "true" | "True" | "TRUE"
                | "false" | "False" | "FALSE" => false
            case _                            => true)

    def appendString(string: String): Unit =
      if plainSafe(string) then append(string) else
        append('"')
        var i = 0

        while i < string.length do
          string.charAt(i) match
            case '"'  => append(t"\\\"")
            case '\\' => append(t"\\\\")
            case '\n' => append(t"\\n")
            case '\r' => append(t"\\r")
            case '\t' => append(t"\\t")

            case c =>
              if c < ' ' then
                val hex = Integer.toHexString(c.toInt).nn
                append(t"\\u")
                var z = hex.length

                while z < 4 do
                  append('0')
                  z += 1

                append(hex.tt)
              else
                append(c)

          i += 1

        append('"')

    // Emit a scalar (or an empty collection) with no surrounding newlines.
    def scalar(ast: Yaml.Ast): Unit = ast.asMatchable match
      case bcd: Array[Double] @unchecked => append(bcd.asInstanceOf[jacinta.Bcd].text.tt)
      case n: Long                       => append(n.toString)

      case d: Double =>
        if d.isNaN then append(t".nan")
        else if d == Double.PositiveInfinity then append(t".inf")
        else if d == Double.NegativeInfinity then append(t"-.inf")
        else append(d.toString)

      case b: Boolean => append(if b then t"true" else t"false")
      case s: String  => appendString(s)

      case xs: IArray[Any] @unchecked =>
        if (xs.length & 1) == 0 then append(t"{}") else append(t"[]")

      case _ => append(t"null")

    // Scalars and empty collections render on one line; non-empty collections
    // span multiple indented lines.
    def inlineable(ast: Yaml.Ast): Boolean = ast.asMatchable match
      case xs: IArray[Any] @unchecked =>
        if (xs.length & 1) == 0 then xs.length == 0 else Yaml.Ast.sequenceLength(xs) == 0

      case _ => true

    // Emit the value after a `key:` or `- ` marker: inline when scalar/empty,
    // otherwise on following lines indented one step deeper.
    def value(ast: Yaml.Ast, indent: Int): Unit =
      if inlineable(ast) then
        append(' ')
        scalar(ast)
        append('\n')
      else
        append('\n')
        block(ast, indent + 2)

    def block(ast: Yaml.Ast, indent: Int): Unit = ast.asMatchable match
      case xs: IArray[Any] @unchecked =>
        if (xs.length & 1) == 0 then
          val n = xs.length/2
          var i = 0

          while i < n do
            spaces(indent)
            scalar(xs(i*2).asInstanceOf[Yaml.Ast])
            append(':')
            value(xs(i*2 + 1).asInstanceOf[Yaml.Ast], indent)
            i += 1
        else
          val n = Yaml.Ast.sequenceLength(xs)
          var i = 0

          while i < n do
            spaces(indent)
            append('-')
            value(xs(i).asInstanceOf[Yaml.Ast], indent)
            i += 1

      case _ => ()

    if inlineable(yaml) then
      scalar(yaml)
      append('\n')
    else
      block(yaml, 0)

trait YamlPrinter:
  def print(yaml: Yaml.Ast): Text
