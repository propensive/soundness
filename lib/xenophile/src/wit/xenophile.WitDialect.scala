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
┃    Soundness, version 0.63.0.                                                                    ┃
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
// `s32`; `type` aliases are resolved. The `package` declaration is read to qualify each interface's
// functions with their Component Model module id (e.g. `wasi:random/random@0.2.0`); `use`, `world`,
// `variant` and `resource` declarations are recognised but their bodies are skipped. Line and block
// comments are ignored.
object WitDialect extends Dialect:
  def parse(source: Text): Map[Text, Map[Text, Prototype]] =
    val (types, typedefs) = items(tokenize(source.s), Map(), Map(), Unset)

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
        // `%` escapes an identifier that collides with a WIT keyword (`%stream`); the name
        // itself is the unescaped form.
        else if char == '%' then recur(index + 1, current, tokens)
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
      else if name == "result" then (result(args), after)
      else (Foreign.Type.Applied(name.tt, args), after)

    case "result" :: rest =>
      (result(Nil), rest)

    case name :: rest =>
      (Foreign.Type.Named(name.tt), rest)

    case Nil =>
      (Foreign.Type.Named(t""), Nil)

  // A `result` type always carries exactly two arms: `result` and `result<T>` pad the missing
  // `ok`/`err` arm(s) with `_`, so consumers see one uniform shape.
  private def result(args: List[Foreign.Type]): Foreign.Type =
    val unit = Foreign.Type.Named(t"_")
    Foreign.Type.Applied(t"result", (args ++ List(unit, unit)).take(2))

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
      types:    Map[Text, Map[Text, Prototype]],
      typedefs: Map[Text, Foreign.Type],
      pkg:      Optional[Text] )
  :   (Map[Text, Map[Text, Prototype]], Map[Text, Foreign.Type]) =

    tokens match
      case Nil =>
        (types, typedefs)

      case "package" :: rest =>
        val (id, after) = packageId(rest)
        items(after, types, typedefs, id)

      case "interface" :: name :: "{" :: rest =>
        val (types2, typedefs2, after) = interface(rest, name.tt, types, typedefs, pkg)
        items(after, types2, typedefs2, pkg)

      case "type" :: name :: "=" :: rest =>
        val (kind, after) = typeOf(rest)
        items(skipTo(after, t";"), types, typedefs.updated(name.tt, kind), pkg)

      case ("world" | "interface") :: _ :: "{" :: rest =>
        items(skipBraces(rest, 1), types, typedefs, pkg)

      case _ =>
        items(skipTo(tokens, t";"), types, typedefs, pkg)

  // Walks an interface body: `record`s and the interface's own functions become navigable types;
  // an `enum` becomes the unsigned discriminant (`u8`/`u16`/`u32`) that holds its cases and `flags`
  // a bit-vector (`b8`/`b16`/`b32`/`b64`) that holds its members; `variant` is an opaque type; and
  // `resource`/`use` declarations are skipped (their `{ … }` bodies and statements bypassed).
  private def interface
    ( tokens:   List[String],
      name:     Text,
      types:    Map[Text, Map[Text, Prototype]],
      typedefs: Map[Text, Foreign.Type],
      pkg:      Optional[Text] )
  :   (Map[Text, Map[Text, Prototype]], Map[Text, Foreign.Type], List[String]) =

    val module = moduleId(pkg, name)

    def recur
      ( todo:      List[String],
        functions: Map[Text, Prototype],
        types:     Map[Text, Map[Text, Prototype]],
        typedefs:  Map[Text, Foreign.Type] )
    :   (Map[Text, Map[Text, Prototype]], Map[Text, Foreign.Type], List[String]) =

      todo match
        case "}" :: rest =>
          (types.updated(name, functions), typedefs, rest)

        case Nil =>
          (types.updated(name, functions), typedefs, Nil)

        case "record" :: record :: "{" :: rest =>
          val (fields, after) = recordFields(rest, ListMap())
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

        // A variant (or a bodyless resource) has no navigable members, but must still record
        // which module defines it, so functions in *other* interfaces that mention it (e.g. in a
        // `result` error arm) resolve its facade class through `definingModule`: a single
        // unnameable member carries the module.
        case "variant" :: variant :: "{" :: rest =>
          val updated = types.updated(variant.tt, declaration(variant.tt, module))
          recur(skipBraces(rest, 1), functions, updated, typedefs)

        case "resource" :: resource :: "{" :: rest =>
          val (methods, after) = resourceBody(rest, resource.tt, module, ListMap())
          recur(after, functions, types.updated(resource.tt, methods), typedefs)

        case "resource" :: resource :: ";" :: rest =>
          val updated = types.updated(resource.tt, declaration(resource.tt, module))
          recur(rest, functions, updated, typedefs)

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

          val signature = Prototype(parameters, result, module)
          recur(skipTo(after2, t";"), functions.updated(function.tt, signature), types, typedefs)

        // Any unrecognised `{ … }` block is skipped wholesale rather than token-by-token, so its
        // closing brace is never mistaken for the end of the interface.
        case "{" :: rest =>
          recur(skipBraces(rest, 1), functions, types, typedefs)

        case _ :: rest =>
          recur(rest, functions, types, typedefs)

    recur(tokens, ListMap(), types, typedefs)

  // Walks a `resource … { … }` body: its methods become members of a type named after the
  // resource, each marked with the resource it belongs to (so an invocation can address it as
  // `[method]resource.name` and thread the receiver's handle). `constructor` and `static`
  // declarations are recognised but skipped for now.
  private def resourceBody
    ( tokens:   List[String],
      resource: Text,
      module:   Optional[Text],
      methods:  Map[Text, Prototype] )
  :   (Map[Text, Prototype], List[String]) =

    tokens match
      case "}" :: rest =>
        (methods, rest)

      case Nil =>
        (methods, Nil)

      // A constructor is registered as the member `constructor`, returning the resource itself; a
      // `static func` is a member addressed through the resource but taking no receiver.
      case "constructor" :: "(" :: rest =>
        val (parameters, after) = params(rest, Nil)
        val signature = Prototype(parameters, Foreign.Type.Named(resource), module, resource, true)
        val updated = methods.updated(t"constructor", signature)
        resourceBody(skipTo(after, t";"), resource, module, updated)

      case method :: ":" :: "static" :: "func" :: "(" :: rest =>
        val (parameters, after) = params(rest, Nil)

        val (result, after2) = after match
          case "->" :: more => typeOf(more)
          case more         => (Foreign.Type.Named(t"unit"), more)

        val signature = Prototype(parameters, result, module, resource, true)
        resourceBody(skipTo(after2, t";"), resource, module, methods.updated(method.tt, signature))

      case method :: ":" :: "func" :: "(" :: rest =>
        val (parameters, after) = params(rest, Nil)

        val (result, after2) = after match
          case "->" :: more => typeOf(more)
          case more         => (Foreign.Type.Named(t"unit"), more)

        val signature = Prototype(parameters, result, module, resource)
        resourceBody(skipTo(after2, t";"), resource, module, methods.updated(method.tt, signature))

      case _ :: rest =>
        resourceBody(rest, resource, module, methods)

  // The pseudo-member recording, for a memberless type declaration, the module that defines it.
  private def declaration(name: Text, module: Optional[Text]): Map[Text, Prototype] =
    ListMap(t"" -> Prototype(Unset, Foreign.Type.Named(name), module))

  private def recordFields(tokens: List[String], acc: Map[Text, Prototype])
  :   (Map[Text, Prototype], List[String]) =

    tokens match
      case "}" :: rest =>
        (acc, rest)

      case name :: ":" :: rest =>
        val (kind, after) = typeOf(rest)
        val updated = acc.updated(name.tt, Prototype(Unset, kind))

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
    ( definitions: Map[Text, Map[Text, Prototype]], typedefs: Map[Text, Foreign.Type] )
  :   Map[Text, Map[Text, Prototype]] =

    def expand(foreign: Foreign.Type): Foreign.Type = foreign match
      case Foreign.Type.Named(name) =>
        typedefs.at(name).lay(foreign)(expand)

      case Foreign.Type.Union(members) =>
        Foreign.Type.Union(members.map(expand))

      case Foreign.Type.Applied(constructor, arguments) =>
        Foreign.Type.Applied(constructor, arguments.map(expand))

    def signature(sig: Prototype): Prototype =
      Prototype
        ( sig.parameters.let(_.map(expand)),
          expand(sig.result),
          sig.module,
          sig.resource,
          sig.static )

    definitions.map: (name, members) =>
      (name, members.map { (member, sig) => (member, signature(sig)) })

  // Reads the tokens of a `package …;` declaration up to (not including) the `;` and joins them
  // with no separator — WIT package ids contain no internal whitespace, so an id such as
  // `wasi:random@0.2.0` reassembles exactly — returning the id and the tokens after the `;`.
  private def packageId(tokens: List[String]): (Text, List[String]) =
    def recur(todo: List[String], acc: List[String]): (Text, List[String]) = todo match
      case ";" :: rest  => (acc.reverse.mkString.tt, rest)
      case Nil          => (acc.reverse.mkString.tt, Nil)
      case head :: rest => recur(rest, head :: acc)

    recur(tokens, Nil)

  // Builds the Component Model module id for an interface from the enclosing package id: package
  // `wasi:random@0.2.0` and interface `random` give `wasi:random/random@0.2.0` — the interface name
  // is spliced in before the `@version`. `Unset` when there is no `package` declaration.
  private def moduleId(pkg: Optional[Text], iface: Text): Optional[Text] = pkg.let: id =>
    id.cut(t"@") match
      case base :: version :: _ => t"$base/$iface@$version"
      case _                    => t"$id/$iface"

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
