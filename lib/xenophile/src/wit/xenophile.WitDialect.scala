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
package xenophile

import scala.collection.immutable.ListMap

import anticipation.*
import gossamer.*
import rudiments.*
import vacuous.*

// A minimal grammar for WIT (the WebAssembly Component Model's interface-definition language).
// `record`s become navigable foreign types whose members are their fields; an `interface`'s
// functions become members of a type named after the interface; `enum`s and `flags` are treated as
// `s32`; `type` aliases are resolved. `package`, `use`, `world`, `variant` and `resource`
// declarations are recognised but their bodies are skipped. Line and block comments are ignored.
object WitDialect extends Dialect:
  def parse(source: Text): Map[Text, Map[Text, Signature]] =
    val (types, typedefs) = items(tokenize(source.s), Map(), Map())

    resolve(types, typedefs)

  // WIT identifiers are kebab-case, so `-` is an identifier character — except in `->` (the
  // return-type arrow). Other punctuation becomes single-character tokens; `//` and `/* … */`
  // comments are skipped.
  private def tokenize(source: String): List[String] =
    def skipLine(index: Int): Int =
      if index >= source.length || source.charAt(index) == '\n' then index + 1
      else skipLine(index + 1)

    def skipBlock(index: Int): Int =
      if index + 1 >= source.length then source.length
      else if source.charAt(index) == '*' && source.charAt(index + 1) == '/' then index + 2
      else skipBlock(index + 1)

    def ident(char: Char): Boolean =
      char.isLetterOrDigit || char == '_' || char == '-'

    def recur(index: Int, current: String, tokens: List[String]): List[String] =
      val flushed = if current.isEmpty then tokens else current :: tokens

      if index >= source.length then flushed.reverse else
        val char = source.charAt(index)
        val next = if index + 1 < source.length then source.charAt(index + 1) else ' '

        if char == '/' && next == '/' then recur(skipLine(index), "", flushed)
        else if char == '/' && next == '*' then recur(skipBlock(index + 2), "", flushed)
        else if char == '-' && next == '>' then recur(index + 2, "", "->" :: flushed)
        else if ident(char) then recur(index + 1, current + char, tokens)
        else if char.isWhitespace then recur(index + 1, "", flushed)
        else recur(index + 1, "", char.toString :: flushed)

    recur(0, "", Nil)

  // Parses a WIT type. `name<args>` is a generic application — `option<T>` becomes the union
  // `T | none` (an `Optional`), and `list`/`tuple`/`result` stay applications. Primitive names are
  // kept verbatim (`u32`, `s8`, `char`, …); the ecosystem maps each to a Hypotenuse type.
  private def typeOf(tokens: List[String]): (Foreign.Type, List[String]) = tokens match
    case name :: "<" :: rest =>
      val (args, after) = typeArguments(rest, Nil)

      if name == "option" then args match
        case inner :: Nil => (Foreign.Type.Union(List(inner, Foreign.Type.Named(t"none"))), after)
        case _            => (Foreign.Type.Applied(t"option", args), after)
      else (Foreign.Type.Applied(name.tt, args), after)

    case name :: rest =>
      (Foreign.Type.Named(name.tt), rest)

    case Nil =>
      (Foreign.Type.Named(t""), Nil)

  private def typeArguments(tokens: List[String], acc: List[Foreign.Type])
  :   (List[Foreign.Type], List[String]) =

    tokens match
      case ">" :: rest =>
        (acc.reverse, rest)

      case _ =>
        val (arg, rest) = typeOf(tokens)

        rest match
          case "," :: more => typeArguments(more, arg :: acc)
          case ">" :: more => ((arg :: acc).reverse, more)
          case _           => (acc.reverse, skipTo(rest, t">"))

  // Walks the top-level items, accumulating navigable types (records, and each interface's
  // functions) and `type` aliases.
  private def items
    ( tokens:   List[String],
      types:    Map[Text, Map[Text, Signature]],
      typedefs: Map[Text, Foreign.Type] )
  :   (Map[Text, Map[Text, Signature]], Map[Text, Foreign.Type]) =

    tokens match
      case Nil =>
        (types, typedefs)

      case "interface" :: name :: "{" :: rest =>
        val (types2, typedefs2, after) = interface(rest, name.tt, types, typedefs)
        items(after, types2, typedefs2)

      case "type" :: name :: "=" :: rest =>
        val (kind, after) = typeOf(rest)
        items(skipTo(after, t";"), types, typedefs.updated(name.tt, kind))

      case ("world" | "interface") :: _ :: "{" :: rest =>
        items(skipBraces(rest, 1), types, typedefs)

      case _ =>
        items(skipTo(tokens, t";"), types, typedefs)

  // Walks an interface body: `record`s and the interface's own functions become navigable types;
  // an `enum` becomes the unsigned discriminant (`u8`/`u16`/`u32`) that holds its cases and `flags`
  // a bit-vector (`b8`/`b16`/`b32`/`b64`) that holds its members; `variant` is an opaque type; and
  // `resource`/`use` declarations are skipped (their `{ … }` bodies and statements bypassed).
  private def interface
    ( tokens:   List[String],
      name:     Text,
      types:    Map[Text, Map[Text, Signature]],
      typedefs: Map[Text, Foreign.Type] )
  :   (Map[Text, Map[Text, Signature]], Map[Text, Foreign.Type], List[String]) =

    def recur
      ( todo:      List[String],
        functions: Map[Text, Signature],
        types:     Map[Text, Map[Text, Signature]],
        typedefs:  Map[Text, Foreign.Type] )
    :   (Map[Text, Map[Text, Signature]], Map[Text, Foreign.Type], List[String]) =

      todo match
        case "}" :: rest =>
          (types.updated(name, functions), typedefs, rest)

        case Nil =>
          (types.updated(name, functions), typedefs, Nil)

        case "record" :: record :: "{" :: rest =>
          val (fields, after) = recordFields(rest, Map())
          recur(after, functions, types.updated(record.tt, fields), typedefs)

        case "enum" :: alias :: "{" :: rest =>
          val (count, after) = enumerators(rest, 0)
          val topic = if count <= 256 then t"u8" else if count <= 65536 then t"u16" else t"u32"
          recur(after, functions, types, typedefs.updated(alias.tt, Foreign.Type.Named(topic)))

        case "flags" :: alias :: "{" :: rest =>
          val (count, after) = enumerators(rest, 0)

          val topic =
            if count <= 8 then t"b8" else if count <= 16 then t"b16"
            else if count <= 32 then t"b32" else t"b64"

          recur(after, functions, types, typedefs.updated(alias.tt, Foreign.Type.Named(topic)))

        case "variant" :: variant :: "{" :: rest =>
          val updated = types.updated(variant.tt, Map[Text, Signature]())
          recur(skipBraces(rest, 1), functions, updated, typedefs)

        case "resource" :: _ :: "{" :: rest =>
          recur(skipBraces(rest, 1), functions, types, typedefs)

        case "resource" :: _ :: ";" :: rest =>
          recur(rest, functions, types, typedefs)

        case "use" :: rest =>
          recur(skipTo(rest, t";"), functions, types, typedefs)

        case "type" :: alias :: "=" :: rest =>
          val (kind, after) = typeOf(rest)
          recur(skipTo(after, t";"), functions, types, typedefs.updated(alias.tt, kind))

        case function :: ":" :: "func" :: "(" :: rest =>
          val (parameters, after) = params(rest, Nil)

          val (result, after2) = after match
            case "->" :: more => typeOf(more)
            case more         => (Foreign.Type.Named(t"unit"), more)

          val signature = Signature(parameters, result)
          recur(skipTo(after2, t";"), functions.updated(function.tt, signature), types, typedefs)

        // Any unrecognised `{ … }` block is skipped wholesale rather than token-by-token, so its
        // closing brace is never mistaken for the end of the interface.
        case "{" :: rest =>
          recur(skipBraces(rest, 1), functions, types, typedefs)

        case _ :: rest =>
          recur(rest, functions, types, typedefs)

    recur(tokens, Map(), types, typedefs)

  private def recordFields(tokens: List[String], acc: Map[Text, Signature])
  :   (Map[Text, Signature], List[String]) =

    tokens match
      case "}" :: rest =>
        (acc, rest)

      case name :: ":" :: rest =>
        val (kind, after) = typeOf(rest)
        val updated = acc.updated(name.tt, Signature(Unset, kind))

        after match
          case "," :: more => recordFields(more, updated)
          case _           => recordFields(after, updated)

      case _ :: rest =>
        recordFields(rest, acc)

      case Nil =>
        (acc, Nil)

  private def params(tokens: List[String], acc: List[Foreign.Type])
  :   (Optional[List[Foreign.Type]], List[String]) =

    tokens match
      case ")" :: rest =>
        (acc.reverse, rest)

      case name :: ":" :: rest =>
        val (kind, after) = typeOf(rest)

        after match
          case "," :: more => params(more, kind :: acc)
          case ")" :: more => ((kind :: acc).reverse, more)
          case _           => (acc.reverse, skipTo(after, t")"))

      case _ :: rest =>
        params(rest, acc)

      case Nil =>
        (acc.reverse, Nil)

  // Resolves every `type` alias appearing in a type, transitively.
  private def resolve
    ( definitions: Map[Text, Map[Text, Signature]], typedefs: Map[Text, Foreign.Type] )
  :   Map[Text, Map[Text, Signature]] =

    def expand(foreign: Foreign.Type): Foreign.Type = foreign match
      case Foreign.Type.Named(name) =>
        typedefs.at(name).lay(foreign)(expand)

      case Foreign.Type.Union(members) =>
        Foreign.Type.Union(members.map(expand))

      case Foreign.Type.Applied(constructor, arguments) =>
        Foreign.Type.Applied(constructor, arguments.map(expand))

    def signature(sig: Signature): Signature =
      Signature(sig.parameters.let(_.map(expand)), expand(sig.result))

    definitions.map: (name, members) =>
      (name, members.map { (member, sig) => (member, signature(sig)) })

  // Skips tokens up to and including the next occurrence of `token`.
  private def skipTo(tokens: List[String], token: Text): List[String] = tokens match
    case Nil          => Nil
    case head :: rest => if head == token.s then rest else skipTo(rest, token)

  // Skips a balanced `{ … }` block, given the tokens just inside the opening brace (depth 1).
  private def skipBraces(tokens: List[String], depth: Int): List[String] = tokens match
    case Nil         => Nil
    case "{" :: rest => skipBraces(rest, depth + 1)
    case "}" :: rest => if depth <= 1 then rest else skipBraces(rest, depth - 1)
    case _ :: rest   => skipBraces(rest, depth)

  // Counts the case/flag names inside an `enum`/`flags` body (used to size the discriminant or
  // bit-vector), returning the count and the tokens after the closing `}`.
  private def enumerators(tokens: List[String], count: Int): (Int, List[String]) = tokens match
    case "}" :: rest                          => (count, rest)
    case Nil                                  => (count, Nil)
    case name :: rest if name.head.isLetter   => enumerators(rest, count + 1)
    case _ :: rest                            => enumerators(rest, count)
