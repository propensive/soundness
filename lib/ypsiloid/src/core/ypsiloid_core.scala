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
package ypsiloid

import scala.compiletime.asMatchable

import anticipation.*
import contextual.*
import denominative.*
import gossamer.*
import hypotenuse.Bcd
import prepositional.*
import spectacular.Showable
import symbolism.*
import wisteria.*

// Renders a `Yaml.Ast` back to YAML text in conservative block style. The whole emission lives in
// this instance so `.show` is the single route to YAML text. The output is deliberately
// re-parseable to a structurally identical AST: scalars are emitted so the parser resolves them
// back to the same node type (integers as bare digits, decimals via `Double.toString` or
// `.inf`/`-.inf`/`.nan`, booleans as `true`/`false`, null as `null`); a string is a plain scalar
// only when unambiguously a string, else double-quoted; mappings/sequences are 2-space-indented
// block collections, empty ones the flow forms `{}` / `[]`. `Yaml.Ast` node shapes: scalars are
// boxed JVM values; a high-precision number is a `Bcd` (`Array[Double]`); a mapping is an
// even-length `Array[Any]^{}` of alternating key/value; a sequence is an odd-length `Array[Any]^{}`
// (trailing `arrayPad` sentinel when the item count is even). Mirrors jacinta's `Json.Ast`
// `Showable`. Bring a `Yaml.Formatting` into scope to enable `.show` and HTTP encoding.
// The `Showable` instances themselves live in the `Yaml` and `Yaml.Ast` companions, in
// implicit scope; this is only the emitter.
private[ypsiloid] def renderAst(yaml: Yaml.Ast)(using formatting: Yaml.Formatting): Text =
  val builder = java.lang.StringBuilder()
  YamlEmitter.fill(builder, YamlEmitter.start(builder, yaml), Int.MaxValue)
  builder.toString.tt

// The block-style emitter, as a walk over an explicit stack of the collections still being
// written, so that it can stop between any two entries: `renderAst` runs it to the end, and
// `Yaml.emit` a block at a time. Each step writes one entry of the innermost unfinished
// collection, and opens its value as a new frame when that value spans several lines.
private[ypsiloid] object YamlEmitter:
  // A collection part-written: its entries, their indentation, and the next to write.
  final case class Frame(collection: Array[Any]^{}, level: Int, index: Int)

  private val spaces: Text =
    t"                                                                "

  // Writes a value that fits on one line, and returns the frame for one that does not.
  def start(builder: java.lang.StringBuilder, yaml: Yaml.Ast): List[Frame] =
    yaml.asMatchable match
      case xs: (Array[Any]^{}) @unchecked if !inlineable(yaml) =>
        List(Frame(xs, 0, 0))

      case _ =>
        scalar(builder, yaml)
        builder.append('\n')
        Nil

  // Writes entries until the stack is empty or the builder holds at least `limit` chars, and
  // returns what remains to be written.
  def fill(builder: java.lang.StringBuilder, stack0: List[Frame], limit: Int): List[Frame] =
    var stack = stack0

    while !stack.nil && builder.length < limit do stack = step(builder, stack)

    stack

  private def step(builder: java.lang.StringBuilder, stack: List[Frame]): List[Frame] =
    stack match
      case Frame(collection, level, index) :: rest =>
        val mapping = (collection.length & 1) == 0
        val count = if mapping then collection.length/2 else Yaml.Ast.sequenceLength(collection)

        if index == count then rest else
          val continuing = Frame(collection, level, index + 1) :: rest
          indent(builder, level)

          val child: Yaml.Ast =
            if mapping then
              scalar(builder, collection(index*2).asInstanceOf[Yaml.Ast])
              builder.append(':')
              collection(index*2 + 1).asInstanceOf[Yaml.Ast]
            else
              builder.append('-')
              collection(index).asInstanceOf[Yaml.Ast]

          // The value after a `key:` or `- ` marker: inline when scalar or empty, otherwise on
          // the following lines, indented one step deeper.
          child.asMatchable match
            case xs: (Array[Any]^{}) @unchecked if !inlineable(child) =>
              builder.append('\n')
              Frame(xs, level + 2, 0) :: continuing

            case _ =>
              builder.append(' ')
              scalar(builder, child)
              builder.append('\n')
              continuing

      case _ =>
        Nil

  // Scalars and empty collections render on one line; non-empty collections span multiple lines.
  private[ypsiloid] def inlineable(ast: Yaml.Ast): Boolean = ast.asMatchable match
    case xs: (Array[Any]^{}) @unchecked =>
      if (xs.length & 1) == 0 then xs.length == 0 else Yaml.Ast.sequenceLength(xs) == 0

    case _ => true

  private[ypsiloid] def indent(builder: java.lang.StringBuilder, count: Int): Unit =
    var remaining = count

    while remaining > 0 do
      val chunk = remaining.min(64)
      builder.append(spaces.s, 0, chunk)
      remaining -= chunk

  // A scalar (or an empty collection), with no surrounding newlines.
  private[ypsiloid] def scalar(builder: java.lang.StringBuilder, ast: Yaml.Ast): Unit =
    ast.asMatchable match
      case bcd: scala.Array[Double] @unchecked => builder.append(Bcd.adopt(bcd).text)
      case n: Long                             => builder.append(n)

      case d: Double =>
        if d.isNaN then builder.append(".nan")
        else if d == Double.PositiveInfinity then builder.append(".inf")
        else if d == Double.NegativeInfinity then builder.append("-.inf")
        else builder.append(d.toString)

      case b: Boolean => builder.append(if b then "true" else "false")
      case s: String  => string(builder, s)

      case xs: (Array[Any]^{}) @unchecked =>
        if (xs.length & 1) == 0 then builder.append("{}") else builder.append("[]")

      case _ => builder.append("null")

  private[ypsiloid] def string(builder: java.lang.StringBuilder, string: String): Unit =
    if plainSafe(string) then builder.append(string) else
      builder.append('"')
      val length = string.length
      var start = 0
      var index = 0

      inline def escape(entity: String): Unit =
        if index > start then builder.append(string, start, index)
        builder.append(entity)
        start = index + 1

      while index < length do
        string.charAt(index) match
          case '"'          => escape("\\\"")
          case '\\'         => escape("\\\\")
          case '\n'         => escape("\\n")
          case '\r'         => escape("\\r")
          case '\t'         => escape("\\t")
          case c if c < ' ' => escape(unicode(c))
          case _            => ()

        index += 1

      if length > start then builder.append(string, start, length)
      builder.append('"')

  // A string is safe to emit as a plain scalar when it is a leading-letter run of `[A-Za-z0-9_-]`
  // (no whitespace or indicator characters, never empty) that the parser would not resolve as a
  // boolean or null.
  private def plainSafe(string: String): Boolean =
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

  private def unicode(char: Char): String =
    val hex: Text = Integer.toHexString(char.toInt).nn.tt
    t"\\u${t"0"*(4 - hex.length)}$hex".s

extension (inline context: StringContext)
  transparent inline def y: Interpolation = interpolation[Yaml](context)
  transparent inline def yp: Interpolation = interpolation[YamlPath](context)

// How an `Optional` or `Option` field reads an absent key, a YAML `null`, or a value its inner
// decoder rejects: lenient yields `Unset`/`None`, strict raises `Yaml.Error`. Absent keys and
// nulls are lenient by default; faults are strict.
package optionalityOptions:
  import distillate.Decodable.{Absence, Nullity, Fault}

  given strictYamlAbsence:  Absence in Yaml = Absence(true)
  given lenientYamlAbsence: Absence in Yaml = Absence(false)
  given strictYamlNulls:    Nullity in Yaml = Nullity(true)
  given lenientYamlNulls:   Nullity in Yaml = Nullity(false)
  given strictYamlFaults:   Fault in Yaml   = Fault(true)
  given lenientYamlFaults:  Fault in Yaml   = Fault(false)

package formatting:
  // Block-style serializer. The default (and currently only) printer; flow
  // style may be added later. Mirrors jacinta's `formatting.indentedJsonFormatting`.
  given blockYamlFormatting: Yaml.Formatting = new Yaml.Formatting {}

package discriminables:
  given yamlByTypeDiscriminable: [value] => value is Discriminable in Yaml =
    Yaml.discriminatedUnion[value](t"type")

  given yamlByKindDiscriminable: [value] => value is Discriminable in Yaml =
    Yaml.discriminatedUnion[value](t"kind")
