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

// A grammar for the subset of WebIDL (https://webidl.spec.whatwg.org/) needed to navigate the web
// platform — most importantly the DOM. An `interface` becomes a navigable foreign type whose
// members are its attributes (fields) and operations (methods); a `dictionary` becomes a record;
// and an `enum` is a `string`. WebIDL's numeric types canonicalise to the Hypotenuse-aligned
// tokens the ecosystem maps (`octet` → `u8`, `unsigned long` → `u32`, `long long` → `s64`,
// `double` → `f64`, …) and `DOMString`/`USVString`/`ByteString` to `string`.
//
// Crucially, inheritance is flattened here: each interface's members include those of its base
// chain (`interface B : A`) and of every mixin applied to it (`B includes M`), so navigating an
// inherited member (e.g. `Node`'s `nodeName` on an `HTMLElement`) resolves. `partial interface`
// bodies are merged into the interface they extend. `typedef` aliases are resolved transitively.
// `callback`, `namespace`, `[extended attributes]`, and unrecognised constructs are skipped.
object WebIdlDialect extends Dialect:
  // The accumulated structure of a WebIDL source before inheritance is flattened: each type's own
  // members, each interface's base type, the mixins applied by `includes`, and the `typedef`/`enum`
  // aliases.
  private case class Idl
    ( types:    Map[Text, Map[Text, Prototype]],
      parents:  Map[Text, Text],
      includes: Map[Text, List[Text]],
      typedefs: Map[Text, Foreign.Type] )

  def parse(source: Text): Map[Text, Map[Text, Prototype]] =
    val idl = items(tokenize(source.s), Idl(Map(), Map(), Map(), Map()))

    resolve(flatten(idl), idl.typedefs)

  // WebIDL identifiers are `[A-Za-z0-9_]` (a leading `_` escapes a keyword). String literals (in
  // `enum` bodies and default values) are lexed whole so their contents never break brace/semicolon
  // tracking; `...` (variadic) is a single token; `//` and `/* … */` comments are skipped.
  private def tokenize(source: String): List[String] =
    def skipLine(index: Int): Int =
      if index >= source.length || source.charAt(index) == '\n' then index + 1
      else skipLine(index + 1)

    def skipBlock(index: Int): Int =
      if index + 1 >= source.length then source.length
      else if source.charAt(index) == '*' && source.charAt(index + 1) == '/' then index + 2
      else skipBlock(index + 1)

    def endString(index: Int): Int =
      if index >= source.length then source.length
      else if source.charAt(index) == '"' then index + 1
      else endString(index + 1)

    def ident(char: Char): Boolean = char.isLetterOrDigit || char == '_'

    def recur(index: Int, current: String, tokens: List[String]): List[String] =
      val flushed = if current.isEmpty then tokens else current :: tokens

      if index >= source.length then flushed.reverse else
        val char = source.charAt(index)
        val next = if index + 1 < source.length then source.charAt(index + 1) else ' '
        val next2 = if index + 2 < source.length then source.charAt(index + 2) else ' '

        if char == '/' && next == '/' then
          recur(skipLine(index), "", flushed)
        else if char == '/' && next == '*' then
          recur(skipBlock(index + 2), "", flushed)
        else if char == '"' then
          val end = endString(index + 1)
          recur(end, "", source.substring(index, end).nn :: flushed)
        else if char == '.' && next == '.' && next2 == '.' then
          recur(index + 3, "", "..." :: flushed)
        else if ident(char) then
          recur(index + 1, current + char, tokens)
        else if char.isWhitespace then
          recur(index + 1, "", flushed)
        else
          recur(index + 1, "", char.toString :: flushed)

    recur(0, "", Nil)

  // Parses a WebIDL type, including a trailing `?` (nullable, read as the union `T | null`).
  private def typeOf(tokens: List[String]): (Foreign.Type, List[String]) =
    val (base, rest) = baseType(tokens)

    rest match
      case "?" :: more => (Foreign.Type.Union(List(base, Foreign.Type.Named(t"null"))), more)
      case _           => (base, rest)

  // Parses a non-nullable WebIDL type: a parenthesised `(A or B …)` union, a multi-word numeric
  // primitive canonicalised to its Hypotenuse-aligned token, a `name<args>` generic application
  // (`sequence`, `FrozenArray`, `record`, `Promise`, …), or a bare named reference.
  private def baseType(tokens: List[String]): (Foreign.Type, List[String]) = tokens match
    case "(" :: rest                                  => union(rest, Nil)
    case "unsigned" :: "long" :: "long" :: rest       => (Foreign.Type.Named(t"u64"), rest)
    case "unsigned" :: "long" :: rest                 => (Foreign.Type.Named(t"u32"), rest)
    case "unsigned" :: "short" :: rest                => (Foreign.Type.Named(t"u16"), rest)
    case "long" :: "long" :: rest                     => (Foreign.Type.Named(t"s64"), rest)
    case "long" :: rest                               => (Foreign.Type.Named(t"s32"), rest)
    case "short" :: rest                              => (Foreign.Type.Named(t"s16"), rest)
    case "byte" :: rest                               => (Foreign.Type.Named(t"s8"), rest)
    case "octet" :: rest                              => (Foreign.Type.Named(t"u8"), rest)
    case "unrestricted" :: "float" :: rest            => (Foreign.Type.Named(t"f32"), rest)
    case "unrestricted" :: "double" :: rest           => (Foreign.Type.Named(t"f64"), rest)
    case "float" :: rest                              => (Foreign.Type.Named(t"f32"), rest)
    case "double" :: rest                             => (Foreign.Type.Named(t"f64"), rest)
    case "boolean" :: rest                            => (Foreign.Type.Named(t"boolean"), rest)
    case "void" :: rest                               => (Foreign.Type.Named(t"undefined"), rest)

    case ("DOMString" | "USVString" | "ByteString" | "CSSOMString") :: rest =>
      (Foreign.Type.Named(t"string"), rest)

    case name :: "<" :: rest =>
      val (args, after) = typeArguments(rest, Nil)
      (Foreign.Type.Applied(name.tt, args), after)

    case name :: rest =>
      (Foreign.Type.Named(name.tt), rest)

    case Nil =>
      (Foreign.Type.Named(t""), Nil)

  // Parses the members of a `(A or B or …)` union, given the tokens just inside the opening `(`.
  private def union(tokens: List[String], acc: List[Foreign.Type]): (Foreign.Type, List[String]) =
    val (member, rest) = typeOf(tokens)

    rest match
      case "or" :: more => union(more, member :: acc)
      case ")" :: more  => (Foreign.Type.Union((member :: acc).reverse), more)
      case _            => (Foreign.Type.Union((member :: acc).reverse), skipTo(rest, t")"))

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

  // Walks the top-level declarations. `interface`/`dictionary`/`namespace`/`interface mixin` become
  // navigable types; `partial` declarations merge into the type they extend; `enum`/`typedef`
  // become aliases; `X includes Y;` records a mixin application; `[extended attributes]`,
  // `callback`s and anything unrecognised are skipped.
  private def items(tokens: List[String], idl: Idl): Idl = tokens match
    case Nil =>
      idl

    case "[" :: rest =>
      items(skipBrackets(rest, 1), idl)

    case ";" :: rest =>
      items(rest, idl)

    case "partial" :: rest =>
      items(rest, idl)

    case "callback" :: "interface" :: name :: ":" :: base :: "{" :: rest =>
      val (idl2, after) = body(rest, name.tt, base.tt, idl)
      items(after, idl2)

    case "callback" :: "interface" :: name :: "{" :: rest =>
      val (idl2, after) = body(rest, name.tt, Unset, idl)
      items(after, idl2)

    case "callback" :: rest =>
      items(skipTo(rest, t";"), idl)

    case "interface" :: "mixin" :: name :: "{" :: rest =>
      val (idl2, after) = body(rest, name.tt, Unset, idl)
      items(after, idl2)

    case "interface" :: name :: ":" :: base :: "{" :: rest =>
      val (idl2, after) = body(rest, name.tt, base.tt, idl)
      items(after, idl2)

    case "interface" :: name :: "{" :: rest =>
      val (idl2, after) = body(rest, name.tt, Unset, idl)
      items(after, idl2)

    case "dictionary" :: name :: ":" :: base :: "{" :: rest =>
      val (idl2, after) = body(rest, name.tt, base.tt, idl)
      items(after, idl2)

    case "dictionary" :: name :: "{" :: rest =>
      val (idl2, after) = body(rest, name.tt, Unset, idl)
      items(after, idl2)

    case "namespace" :: name :: "{" :: rest =>
      val (idl2, after) = body(rest, name.tt, Unset, idl)
      items(after, idl2)

    case "enum" :: name :: "{" :: rest =>
      val updated = idl.typedefs.updated(name.tt, Foreign.Type.Named(t"string"))
      items(skipBraces(rest, 1), idl.copy(typedefs = updated))

    case "typedef" :: rest =>
      val rest1 = skipExtAttrs(rest)
      val (kind, after) = typeOf(rest1)

      after match
        case name :: more =>
          items(skipTo(more, t";"), idl.copy(typedefs = idl.typedefs.updated(name.tt, kind)))

        case Nil =>
          idl

    case name :: "includes" :: mixin :: rest =>
      val existing = idl.includes.get(name.tt).getOrElse(Nil)
      val updated = idl.includes.updated(name.tt, existing :+ mixin.tt)
      items(skipTo(rest, t";"), idl.copy(includes = updated))

    case _ =>
      items(skipTo(tokens, t";"), idl)

  // Parses a `{ … }` body (of an interface, mixin, dictionary or namespace), merging its members
  // into any already recorded for `name` (so `partial` declarations accumulate) and recording its
  // base type, if any.
  private def body(tokens: List[String], name: Text, parent: Optional[Text], idl: Idl)
  :   (Idl, List[String]) =

    val (members, rest) = memberList(tokens, ListMap())
    val merged = idl.types.get(name).getOrElse(ListMap[Text, Prototype]()) ++ members

    val parents = parent.lay(idl.parents): base =>
      idl.parents.updated(name, base)

    (idl.copy(types = idl.types.updated(name, merged), parents = parents), rest)

  // Walks a type body, collecting attributes (and constants) as fields and operations as methods.
  // Modifiers (`readonly`, `static`, `inherit`, `required`) are stripped; special members
  // (`getter`, `setter`, `iterable`, `constructor`, …), `[extended attributes]` and stray `;` are
  // skipped. A bare `Type name;` (a dictionary member) is read as a field; `Type name(args);` as a
  // method.
  private def memberList(tokens: List[String], members: Map[Text, Prototype])
  :   (Map[Text, Prototype], List[String]) =

    tokens match
      case "}" :: rest =>
        (members, rest)

      case Nil =>
        (members, Nil)

      case "[" :: rest =>
        memberList(skipBrackets(rest, 1), members)

      case ";" :: rest =>
        memberList(rest, members)

      case ("readonly" | "static" | "inherit" | "required" | "async") :: rest =>
        memberList(rest, members)

      case "attribute" :: rest =>
        val (kind, after) = typeOf(skipExtAttrs(rest))

        after match
          case name :: more =>
            memberList(skipTo(more, t";"), members.updated(name.tt, Prototype(Unset, kind)))

          case Nil =>
            (members, Nil)

      case "const" :: rest =>
        val (kind, after) = typeOf(rest)

        after match
          case name :: more =>
            memberList(skipTo(more, t";"), members.updated(name.tt, Prototype(Unset, kind)))

          case Nil =>
            (members, Nil)

      case ( "getter" | "setter" | "deleter" | "stringifier" | "iterable" | "maplike"
           | "setlike" | "constructor" ) :: rest =>
        memberList(skipTo(rest, t";"), members)

      case _ =>
        val (kind, after) = typeOf(tokens)

        after match
          case name :: "(" :: more =>
            val (parameters, rest) = params(more, Nil)
            memberList(skipTo(rest, t";"), members.updated(name.tt, Prototype(parameters, kind)))

          case name :: more =>
            memberList(skipTo(more, t";"), members.updated(name.tt, Prototype(Unset, kind)))

          case Nil =>
            (members, Nil)

  // Parses an operation's parameter list, given the tokens just inside the opening `(`. Each
  // argument may carry `[extended attributes]`, an `optional` keyword, or a `...` variadic marker
  // (all recorded only by their type), and a `= default` value (skipped). Returns a non-`Unset`
  // (possibly empty) list, marking the member as a method.
  private def params(tokens: List[String], acc: List[Foreign.Type])
  :   (Optional[List[Foreign.Type]], List[String]) =

    tokens match
      case ")" :: rest =>
        (acc.reverse, rest)

      case "[" :: rest =>
        params(skipBrackets(rest, 1), acc)

      case "optional" :: rest =>
        params(rest, acc)

      case _ =>
        val (kind, rest) = typeOf(tokens)

        val rest1 = rest match
          case "..." :: more => more
          case more          => more

        rest1 match
          case name :: more =>
            val after = more match
              case "=" :: value => skipDefault(value, 0)
              case _            => more

            after match
              case "," :: tail => params(tail, kind :: acc)
              case ")" :: tail => ((kind :: acc).reverse, tail)
              case _           => ((kind :: acc).reverse, skipTo(after, t")"))

          case Nil =>
            (acc.reverse, Nil)

  // Flattens inheritance: each type's members are those of its base chain, then of every applied
  // mixin, then its own (so a type's own members override inherited ones of the same name). A
  // visited set guards against cycles.
  private def flatten(idl: Idl): Map[Text, Map[Text, Prototype]] =
    def collect(name: Text, visiting: Set[Text]): Map[Text, Prototype] =
      if visiting.contains(name) then idl.types.get(name).getOrElse(ListMap())
      else
        val visiting2 = visiting + name
        val own = idl.types.get(name).getOrElse(ListMap[Text, Prototype]())

        val inherited = idl.parents.at(name).lay(ListMap[Text, Prototype]()): base =>
          collect(base, visiting2)

        val mixedIn = idl.includes.get(name).getOrElse(Nil).foldLeft(inherited): (acc, mixin) =>
          acc ++ collect(mixin, visiting2)

        mixedIn ++ own

    idl.types.map: (name, _) =>
      (name, collect(name, Set()))

  // Resolves every `typedef`/`enum` alias appearing in a type, transitively.
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
      Prototype(sig.parameters.let(_.map(expand)), expand(sig.result))

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

  // Skips a balanced `[ … ]` extended-attribute block, given the tokens just inside the `[`.
  private def skipBrackets(tokens: List[String], depth: Int): List[String] = tokens match
    case Nil         => Nil
    case "[" :: rest => skipBrackets(rest, depth + 1)
    case "]" :: rest => if depth <= 1 then rest else skipBrackets(rest, depth - 1)
    case _ :: rest   => skipBrackets(rest, depth)

  // Skips a `[ … ]` block at the head of `tokens`, if present.
  private def skipExtAttrs(tokens: List[String]): List[String] = tokens match
    case "[" :: rest => skipBrackets(rest, 1)
    case _           => tokens

  // Skips an argument's `= default` value: tokens up to (but not including) the next top-level `,`
  // or `)`, ignoring separators nested inside balanced brackets, braces or parentheses.
  private def skipDefault(tokens: List[String], depth: Int): List[String] = tokens match
    case Nil =>
      Nil

    case head :: rest =>
      if depth == 0 && (head == "," || head == ")") then tokens
      else if head == "(" || head == "{" || head == "[" then skipDefault(rest, depth + 1)
      else if head == ")" || head == "}" || head == "]" then skipDefault(rest, depth - 1)
      else skipDefault(rest, depth)
