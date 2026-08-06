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
package xenophile

import scala.collection.immutable.List as SList
import scala.collection.immutable.{::, Nil as SNil}

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import rudiments.*
import vacuous.*

import WitParseError.Reason

// A declaration parser for WIT (WebAssembly Interface Types), retaining what `wit/1` atomizes:
// packages with their versions, interfaces with their full item vocabulary — records, variants,
// enums, flags, resources, aliases, functions, `use` clauses — and worlds with their imports
// and exports.
//
// A construct outside the vocabulary is a hard error, never a skip (`wit.md` §4). `@since`
// gates are consumed — the lineage already records when an item arrived — while `@unstable`
// is refused: an unstable item in a published contract would be a stable claim about an
// unstable surface.
object WitParser:

  // One `WitDocument` per `package` section: a curated `.wit` file may hold several packages
  // (the WASI subsets the backends carry do), and every interface and world belongs to the
  // package declared above it.
  def parse(source: Text): List[WitDocument] raises WitParseError =
    List.from(documents(tokenize(source.s)))

  private def fail(detail: Text, tokens: SList[String]): Nothing raises WitParseError =
    val near = Text(tokens.take(5).mkString(" "))
    abort(WitParseError(Reason.Syntax(detail, if near.s.isEmpty then t"the end" else near)))

  private def unsupported(construct: Text): Nothing raises WitParseError =
    abort(WitParseError(Reason.Unsupported(construct)))

  // --- lexing ---------------------------------------------------------------------------------

  // WIT identifiers are kebab-case words; `%` escapes a keyword. Package references
  // (`wasi:io/streams@0.2.0`) are lexed as words including `:`, `/`, `@` and version dots, so
  // one token carries one reference.
  private def tokenize(source: String): SList[String] =
    def skipLine(index: Int): Int =
      if index >= source.length || source.charAt(index) == '\n' then index + 1
      else skipLine(index + 1)

    def skipBlock(index: Int): Int =
      if index + 1 >= source.length then source.length
      else if source.charAt(index) == '*' && source.charAt(index + 1) == '/' then index + 2
      else skipBlock(index + 1)

    def ident(char: Char): Boolean =
      char.isLetterOrDigit || char == '-' || char == '_' || char == '%'

    // The reference punctuation (`:`, `/`, `@`, `.`) joins a token only *between* ident
    // characters, so `wasi:io/streams@0.2.0` is one token while `value: u64` splits at the
    // colon and `one.{id}` splits at the dot.
    def glue(char: Char): Boolean =
      char == ':' || char == '/' || char == '@' || char == '.'

    def recur(index: Int, current: String, tokens: SList[String]): SList[String] =
      val flushed = if current.isEmpty then tokens else current :: tokens

      if index >= source.length then flushed.reverse else
        val char = source.charAt(index)
        val next = if index + 1 < source.length then source.charAt(index + 1) else ' '

        if char == '/' && next == '/' then recur(skipLine(index), "", flushed)
        else if char == '/' && next == '*' then recur(skipBlock(index + 2), "", flushed)
        else if ident(char) then recur(index + 1, current + char, tokens)
        else if glue(char) && current.nonEmpty && ident(next) then
          recur(index + 1, current + char, tokens)
        else if char == '@' && current.isEmpty && ident(next) then
          recur(index + 1, "@", tokens)
        else if char.isWhitespace then recur(index + 1, "", flushed)
        else recur(index + 1, "", char.toString :: flushed)

    recur(0, "", SList())

  // --- gates ----------------------------------------------------------------------------------

  private def gates(tokens: SList[String]): SList[String] raises WitParseError = tokens match
    case "@since" :: "(" :: rest =>
      rest.dropWhile(_ != ")") match
        case ")" :: more => gates(more)
        case other       => fail(t"an unterminated @since gate", other)

    case "@unstable" :: _ => unsupported(t"an @unstable item")
    case _                => tokens

  // --- types ----------------------------------------------------------------------------------

  private val primitives: Set[String] =
    Set("bool", "u8", "u16", "u32", "u64", "s8", "s16", "s32", "s64", "f32", "f64", "char",
        "string")

  private def typeOf(tokens: SList[String]): (Foreign.Type, SList[String]) raises WitParseError =
    tokens match
      case ("stream" | "future") :: _ => unsupported(t"a stream or future type")

      case name :: "<" :: rest =>
        def arguments(tokens: SList[String], acc: SList[Foreign.Type])
        :   (SList[Foreign.Type], SList[String]) raises WitParseError =

          val (arg, after) = tokens match
            case "_" :: more => (Foreign.Type.Named(t"_"), more)
            case _           => typeOf(tokens)

          after match
            case "," :: more => arguments(more, arg :: acc)
            case ">" :: more => ((arg :: acc).reverse, more)
            case _           => fail(t"a type argument must be followed by `,` or `>`", after)

        val (args, after) = arguments(rest, SList())
        (Foreign.Type.Applied(name.tt, List.from(args)), after)

      case name :: rest if name.headOption.exists { char => char.isLetter || char == '%' } =>
        (Foreign.Type.Named(Text(name.stripPrefix("%").nn)), rest)

      case _ => fail(t"a type was expected", tokens)

  // --- functions ------------------------------------------------------------------------------

  private def parameters(tokens: SList[String])
  :   (List[(Text, Foreign.Type)], SList[String]) raises WitParseError =

    def recur(tokens: SList[String], acc: SList[(Text, Foreign.Type)])
    :   (List[(Text, Foreign.Type)], SList[String]) raises WitParseError =

      tokens match
        case ")" :: rest => (List.from(acc.reverse), rest)
        case "," :: rest => recur(rest, acc)

        case name :: ":" :: rest =>
          val (typed, after) = typeOf(rest)
          recur(after, (Text(name.stripPrefix("%").nn), typed) :: acc)

        case _ => fail(t"a parameter was expected", tokens)

    tokens match
      case "(" :: rest => recur(rest, SList())
      case _           => fail(t"a parameter list was expected", tokens)

  private def functionType(name: Text, tokens: SList[String], static: Boolean)
  :   (WitFunction, SList[String]) raises WitParseError =

    val afterAsync = tokens match
      case "async" :: rest => unsupported(t"an async function")
      case _               => tokens

    afterAsync match
      case "func" :: rest =>
        val (params, afterParams) = parameters(rest)

        val (result, afterResult) = afterParams match
          case "-" :: ">" :: more =>
            val (typed, after) = typeOf(more)
            (Optional(typed), after)

          case _ => (Unset, afterParams)

        afterResult match
          case ";" :: more => (WitFunction(name, params, result, static), more)
          case _           => fail(t"a `;` was expected", afterResult)

      case _ => fail(t"`func` was expected", afterAsync)

  // --- interface items ------------------------------------------------------------------------

  private def nameList(tokens: SList[String])
  :   (List[(Text, Text)], SList[String]) raises WitParseError =

    def recur(tokens: SList[String], acc: SList[(Text, Text)])
    :   (List[(Text, Text)], SList[String]) raises WitParseError =

      tokens match
        case "}" :: rest => (List.from(acc.reverse), rest)
        case "," :: rest => recur(rest, acc)

        case name :: "as" :: alias :: rest =>
          recur(rest, (name.tt, alias.tt) :: acc)

        case name :: rest => recur(rest, (name.tt, name.tt) :: acc)
        case SNil         => fail(t"a use list is unterminated", tokens)

    tokens match
      case "{" :: rest => recur(rest, SList())
      case _           => fail(t"a `{` was expected", tokens)

  private def fieldList(tokens: SList[String], closer: Text)
  :   (List[(Text, Foreign.Type)], SList[String]) raises WitParseError =

    def recur(tokens: SList[String], acc: SList[(Text, Foreign.Type)])
    :   (List[(Text, Foreign.Type)], SList[String]) raises WitParseError =

      gates(tokens) match
        case "}" :: rest => (List.from(acc.reverse), rest)
        case "," :: rest => recur(rest, acc)

        case name :: ":" :: rest =>
          val (typed, after) = typeOf(rest)
          recur(after, (Text(name.stripPrefix("%").nn), typed) :: acc)

        case _ => fail(t"a field was expected", tokens)

    recur(tokens, SList())

  private def caseList(tokens: SList[String])
  :   (List[(Text, Optional[Foreign.Type])], SList[String]) raises WitParseError =

    def recur(tokens: SList[String], acc: SList[(Text, Optional[Foreign.Type])])
    :   (List[(Text, Optional[Foreign.Type])], SList[String]) raises WitParseError =

      gates(tokens) match
        case "}" :: rest => (List.from(acc.reverse), rest)
        case "," :: rest => recur(rest, acc)

        case name :: "(" :: rest =>
          val (typed, after) = typeOf(rest)

          after match
            case ")" :: more => recur(more, (name.tt, Optional(typed)) :: acc)
            case _           => fail(t"a `)` was expected", after)

        case name :: rest => recur(rest, (name.tt, Unset) :: acc)
        case SNil         => fail(t"a case list is unterminated", tokens)

    recur(tokens, SList())

  private def bareList(tokens: SList[String])
  :   (List[Text], SList[String]) raises WitParseError =

    def recur(tokens: SList[String], acc: SList[Text])
    :   (List[Text], SList[String]) raises WitParseError =

      gates(tokens) match
        case "}" :: rest  => (List.from(acc.reverse), rest)
        case "," :: rest  => recur(rest, acc)
        case name :: rest => recur(rest, name.tt :: acc)
        case SNil         => fail(t"a name list is unterminated", tokens)

    recur(tokens, SList())

  private def resourceBody(tokens: SList[String])
  :   (List[WitFunction], SList[String]) raises WitParseError =

    def recur(tokens: SList[String], acc: SList[WitFunction])
    :   (List[WitFunction], SList[String]) raises WitParseError =

      gates(tokens) match
        case "}" :: rest => (List.from(acc.reverse), rest)

        case "constructor" :: "(" :: rest =>
          val (params, afterParams) = parameters("(" :: rest)

          afterParams match
            case ";" :: more =>
              recur(more, WitFunction(t"constructor", params, constructor = true) :: acc)

            case _ => fail(t"a `;` was expected", afterParams)

        case name :: ":" :: "static" :: rest =>
          val (function, after) = functionType(Text(name.stripPrefix("%").nn), rest, true)
          recur(after, function :: acc)

        case name :: ":" :: rest =>
          val (function, after) = functionType(Text(name.stripPrefix("%").nn), rest, false)
          recur(after, function :: acc)

        case _ => fail(t"a resource member was expected", tokens)

    recur(tokens, SList())

  private def interfaceBody(tokens: SList[String])
  :   (List[WitItem], SList[String]) raises WitParseError =

    def recur(tokens: SList[String], acc: SList[WitItem])
    :   (List[WitItem], SList[String]) raises WitParseError =

      gates(tokens) match
        case "}" :: rest => (List.from(acc.reverse), rest)

        case "use" :: from :: "." :: rest =>
          val (names, after) = nameList(rest)

          after match
            case ";" :: more => recur(more, WitItem.Use(from.tt, names) :: acc)
            case _           => fail(t"a `;` was expected", after)

        case "type" :: name :: "=" :: rest =>
          val (typed, after) = typeOf(rest)

          after match
            case ";" :: more => recur(more, WitItem.Alias(name.tt, typed) :: acc)
            case _           => fail(t"a `;` was expected", after)

        case "record" :: name :: "{" :: rest =>
          val (fields, after) = fieldList(rest, t"}")
          recur(after, WitItem.Record(name.tt, fields) :: acc)

        case "variant" :: name :: "{" :: rest =>
          val (cases, after) = caseList(rest)
          recur(after, WitItem.Variant(name.tt, cases) :: acc)

        case "enum" :: name :: "{" :: rest =>
          val (cases, after) = bareList(rest)
          recur(after, WitItem.Enumeration(name.tt, cases) :: acc)

        case "flags" :: name :: "{" :: rest =>
          val (names, after) = bareList(rest)
          recur(after, WitItem.Flags(name.tt, names) :: acc)

        case "resource" :: name :: "{" :: rest =>
          val (methods, after) = resourceBody(rest)
          recur(after, WitItem.Resource(name.tt, methods) :: acc)

        case "resource" :: name :: ";" :: rest =>
          recur(rest, WitItem.Resource(name.tt, List()) :: acc)

        case name :: ":" :: rest =>
          val (function, after) = functionType(Text(name.stripPrefix("%").nn), rest, false)
          recur(after, WitItem.Function(function) :: acc)

        case construct :: _ => unsupported(construct.tt)
        case SNil           => fail(t"an interface body is unterminated", tokens)

    recur(tokens, SList())

  // --- worlds ---------------------------------------------------------------------------------

  private def worldBody(name: Text, tokens: SList[String])
  :   (WitWorldModel, SList[String]) raises WitParseError =

    def recur
      ( tokens:        SList[String],
        imports:       SList[Text],
        exports:       SList[Text],
        inlineImports: SList[(Text, Optional[WitFunction])],
        inlineExports: SList[(Text, Optional[WitFunction])] )
    :   (WitWorldModel, SList[String]) raises WitParseError =

      def inlineItem(name: String, tokens: SList[String])
      :   (Optional[WitFunction], SList[String]) raises WitParseError =

        tokens match
          case "func" :: _ =>
            val (function, after) = functionType(Text(name.stripPrefix("%").nn), tokens, false)
            (Optional(function), after)

          // An inline interface's body is parsed — so its vocabulary is still checked — but
          // only its name enters the world model.
          case "interface" :: "{" :: body =>
            val (_, after) = interfaceBody(body)
            (Unset, after)

          case _ => fail(t"`func` or `interface` was expected", tokens)

      gates(tokens) match
        case "}" :: rest =>
          val world =
            WitWorldModel
              ( name,
                List.from(imports.reverse),
                List.from(exports.reverse),
                List.from(inlineImports.reverse),
                List.from(inlineExports.reverse) )

          (world, rest)

        case "import" :: name :: ":" :: rest =>
          val (function, after) = inlineItem(name, rest)
          recur(after, imports, exports, (name.tt, function) :: inlineImports, inlineExports)

        case "export" :: name :: ":" :: rest =>
          val (function, after) = inlineItem(name, rest)
          recur(after, imports, exports, inlineImports, (name.tt, function) :: inlineExports)

        case "import" :: name :: ";" :: rest =>
          recur(rest, name.tt :: imports, exports, inlineImports, inlineExports)

        case "export" :: name :: ";" :: rest =>
          recur(rest, imports, name.tt :: exports, inlineImports, inlineExports)

        case "include" :: _ => unsupported(t"a world include")
        case construct :: _ => unsupported(construct.tt)
        case SNil           => fail(t"a world body is unterminated", tokens)

    recur(tokens, SList(), SList(), SList(), SList())

  // --- documents ------------------------------------------------------------------------------

  private def documents(tokens: SList[String]): SList[WitDocument] raises WitParseError =
    def flush
      ( pkg:        Optional[Text],
        version:    Optional[Text],
        interfaces: SList[WitInterface],
        worlds:     SList[WitWorldModel],
        acc:        SList[WitDocument] )
    :   SList[WitDocument] =

      if pkg.absent && interfaces.isEmpty && worlds.isEmpty then acc
      else
        WitDocument(pkg, version, List.from(interfaces.reverse), List.from(worlds.reverse))
          :: acc

    def recur
      ( tokens:     SList[String],
        pkg:        Optional[Text],
        version:    Optional[Text],
        interfaces: SList[WitInterface],
        worlds:     SList[WitWorldModel],
        acc:        SList[WitDocument] )
    :   SList[WitDocument] raises WitParseError =

      gates(tokens) match
        case SNil =>
          flush(pkg, version, interfaces, worlds, acc).reverse

        case "package" :: name :: ";" :: rest =>
          val at = name.indexOf('@')

          val (bare, declared) =
            if at < 0 then (name.tt, Unset)
            else (name.substring(0, at).nn.tt, Optional(name.substring(at + 1).nn.tt))

          recur(rest, bare, declared, SList(), SList(),
              flush(pkg, version, interfaces, worlds, acc))

        case "interface" :: name :: "{" :: rest =>
          val (items, after) = interfaceBody(rest)
          recur(after, pkg, version, WitInterface(name.tt, items) :: interfaces, worlds, acc)

        case "world" :: name :: "{" :: rest =>
          val (world, after) = worldBody(name.tt, rest)
          recur(after, pkg, version, interfaces, world :: worlds, acc)

        case construct :: _ => unsupported(construct.tt)

    recur(tokens, Unset, Unset, SList(), SList(), SList())
