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

import anticipation.*
import fulminate.*
import gossamer.*

// The C header front end: the declaration model a header parses to, the `Dialect` that
// projects it for the FFI backends, the parser itself, and the error it raises. `Dialect`
// and `Parser` each keep their own imports, whose scopes differ from this file's.
object CHeader:
  // CDeclaration → CHeader.Declaration, CHeaderError → CHeader.Error
  // The declaration model of a C header, as `cheader/1` atomizes it (`cheader.md`). Unlike
  // `Dialect`, which canonicalizes for FFI marshalling — collapsing signedness, pointer
  // depth and enumerators, none of which a downcall needs — this model retains the declared
  // surface: exact arithmetic spellings, pointer structure, struct and union fields, and
  // enumerator names with their values.
  enum Declaration:
    case Function
      ( name:       Text,
        result:     Foreign.Type,
        parameters: List[Foreign.Type],
        variadic:   Boolean = false )

    case Alias(name: Text, target: Foreign.Type)

    case Structure
      ( name:   Text,
        union:  Boolean,
        fields: List[(Text, Foreign.Type)],
        opaque: Boolean = false )

    case Enumeration(name: Text, cases: List[(Text, Long)])

    def named: Text = this match
      case Function(name, _, _, _)  => name
      case Alias(name, _)           => name
      case Structure(name, _, _, _) => name
      case Enumeration(name, _)     => name

  object Error:
    enum Reason(val number: Int) extends Clarification:
      case Syntax(detail: Text, near: Text)  extends Reason(1)
      case Unsupported(construct: Text)      extends Reason(2)

    given communicable: Reason is Communicable =
      case Reason.Syntax(detail, near) => m"$detail, near $near"

      case Reason.Unsupported(construct) =>
        m"the construct $construct is outside the grammar this parser accepts"

  // A C header could not be read as declarations. `Unsupported` is deliberately an error and not
  // a silent skip: a partially-read header understates the contract, and every claim computed
  // from it would be unsound.
  case class Error(reason: Error.Reason)(using Diagnostics)
  extends fulminate.Error(646, reason.number)(m"the C header could not be read because $reason")

  // CHeaderDialect → CHeader.Dialect
  // The C grammar for foreign navigation: a *projection* of `Parser`'s declaration model
  // onto the flat form the FFI backends marshal against — one reader per language, two views,
  // exactly as `TypescriptDialect` projects from `Typescript.Parser`.
  //
  // The parser retains the declared surface faithfully — exact arithmetic spellings, pointer
  // structure, enumerators — and this projection deliberately erases what a downcall cannot use:
  // `struct`/`union` definitions become navigable foreign types whose members are their fields,
  // top-level prototypes become members of a synthetic `"library"` type, `enum`s are treated as
  // `int`, `typedef` aliases are resolved transitively, signedness and pointer depth collapse to
  // keep the FFM layouts correct, and a plain `char*` is the C-string type. Array fields,
  // function-pointer typedefs and opaque tags are outside the marshalling vocabulary and are not
  // navigable.
  object Dialect extends xenophile.Dialect:
    // The dialect works with ordinary Scala collections internally; the single `parse`
    // boundary re-wraps as the opaque `Map` (erasure-identical cast).
    import scala.collection.immutable.Map

    import anticipation.*
    import contingency.*
    import gossamer.*
    import rudiments.*
    import vacuous.*

    val library: Text = t"library"

    def parse(source: Text): proscenium.Map[Text, proscenium.Map[Text, Prototype]] =
      parse0(source).asInstanceOf[proscenium.Map[Text, proscenium.Map[Text, Prototype]]]

    private def parse0(source: Text): Map[Text, Map[Text, Prototype]] =
      import strategies.throwUnsafely
      val declarations = Parser.parse(source).stdlib

      val typedefs: Map[Text, Foreign.Type] =
        declarations.collect:
          case Declaration.Enumeration(name, _) => name -> Foreign.Type.Named(t"int")

          case Declaration.Alias(name, target) if !functionPointer(target) =>
            name -> project(target)

        . toMap

      val structs: Map[Text, Map[Text, Prototype]] =
        declarations.collect:
          case Declaration.Structure(name, _, fields, false) =>
            val members = fields.stdlib.filter { (_, typed) => !array(typed) }.map:
              (field, typed) => field -> Prototype(Unset, project(typed))

            name -> members.toMap

        . toMap

      val functions: Map[Text, Prototype] =
        declarations.collect:
          case Declaration.Function(name, result, parameters, _) =>
            name -> Prototype(List.from(parameters.stdlib.map(project(_))), project(result))

        . toMap

      val all = if functions.isEmpty then structs else structs.updated(library, functions)
      resolve(all, typedefs)

    private def functionPointer(typed: Foreign.Type): Boolean = typed match
      case applied: Foreign.Type.Applied =>
        applied.constructor == t"fn" || applied.constructor == t"variadic"

      case _ => false

    private def array(typed: Foreign.Type): Boolean = typed match
      case applied: Foreign.Type.Applied => applied.constructor == t"array"
      case _                             => false

    // Collapses the parser's faithful types to the marshalling vocabulary. Only a *plain* `char*`
    // is the C-string type: `unsigned char*`/`signed char*` conventionally mean a byte buffer,
    // not text, so they stay pointers.
    private def project(typed: Foreign.Type): Foreign.Type = typed match
      case Foreign.Type.Named(name) => Foreign.Type.Named(canonical(name))

      case applied: Foreign.Type.Applied =>
        if applied.constructor == t"ptr" then
          val (base, count) = unwrap(typed)

          if base == t"char" && count == 1 then Foreign.Type.Named(t"string")
          else Foreign.Type.Applied(t"ptr", List(Foreign.Type.Named(canonical(base))))
        else if applied.constructor == t"const" then project(applied.arguments.stdlib.head)
        else Foreign.Type.Applied(applied.constructor, applied.arguments.map(project(_)))

      case other => other

    // The innermost named base beneath `ptr` and `const` wrappers, with the pointer depth.
    private def unwrap(typed: Foreign.Type): (Text, Int) = typed match
      case Foreign.Type.Named(name) => (name, 0)

      case applied: Foreign.Type.Applied =>
        if applied.constructor == t"const" then unwrap(applied.arguments.stdlib.head)
        else if applied.constructor == t"ptr" then
          val (base, inner) = unwrap(applied.arguments.stdlib.head)
          (base, inner + 1)
        else (t"*", 0)

      case _ => (t"*", 0)

    // The width-exact and sign-qualified names map to the primitive of the same size, so the FFM
    // layout stays correct; widths without a matching primitive are left as-is.
    private def canonical(name: Text): Text = name.s match
      case "unsigned-int" | "int32_t" | "uint32_t"              => t"int"
      case "unsigned-char" | "signed-char"                      => t"char"
      case "unsigned-short"                                     => t"short"
      case "long-long" | "unsigned-long" | "unsigned-long-long" => t"long"
      case "int64_t" | "uint64_t" | "intptr_t" | "uintptr_t"    => t"long"
      case "size_t" | "ssize_t"                                 => t"long"
      case "long-double"                                        => t"long double"
      case _                                                    => name

    // Resolves every `typedef` alias appearing in a type, transitively.
    private def resolve
      ( definitions: Map[Text, Map[Text, Prototype]], typedefs: Map[Text, Foreign.Type] )
    :   Map[Text, Map[Text, Prototype]] =

      def expand(foreign: Foreign.Type): Foreign.Type = foreign match
        case Foreign.Type.Named(name) =>
          typedefs.get(name).optional.lay(foreign)(expand)

        case Foreign.Type.Union(members) =>
          Foreign.Type.Union(members.map(expand))

        case Foreign.Type.Applied(constructor, arguments) =>
          Foreign.Type.Applied(constructor, arguments.map(expand))

      def signature(sig: Prototype): Prototype =
        Prototype(sig.parameters.let(_.map(expand)), expand(sig.result))

      definitions.map: (name, members) =>
        (name, members.map { (member, sig) => (member, signature(sig)) })

  // CHeaderParser → CHeader.Parser
  // A declaration parser for C headers, retaining what `cheader/1` atomizes: prototypes with
  // exact arithmetic spellings and pointer structure, typedefs, structs and unions with their
  // fields, and enumerations with their enumerator values.
  //
  // Preprocessor lines are skipped by sanction (`cheader.md` §6: macros are not part of the
  // parsed vocabulary), and comments likewise; any other unrecognized construct is a hard error.
  object Parser:
    import scala.collection.immutable.List as SList
    import scala.collection.immutable.{::, Nil as SNil}

    import anticipation.*
    import contingency.*
    // `Error as _`: an inner wildcard import of `fulminate` would make the bare `Error` ambiguous
    // with this object's sibling `CHeader.Error`.
    import fulminate.{Error as _, *}
    import gossamer.*
    import rudiments.*
    import vacuous.*

    import Error.Reason

    def parse(source: Text): List[Declaration] raises Error =
      val declarations = scala.collection.mutable.ListBuffer[Declaration]()
      var tokens = tokenize(source.s)

      while tokens.nonEmpty do
        val (declaration, rest) = item(tokens)
        declaration.let { value => declarations += value }
        tokens = rest

      List.from(declarations.toList)

    private def fail(detail: Text, tokens: SList[String]): Nothing raises Error =
      val near = Text(tokens.take(6).mkString(" "))
      abort(Error(Reason.Syntax(detail, if near.s.isEmpty then t"the end" else near)))

    private def unsupported(construct: Text): Nothing raises Error =
      abort(Error(Reason.Unsupported(construct)))

    // --- lexing ---------------------------------------------------------------------------------

    private def tokenize(source: String): SList[String] =
      def skipLine(index: Int): Int =
        if index >= source.length || source.charAt(index) == '\n' then index + 1
        else skipLine(index + 1)

      def skipBlock(index: Int): Int =
        if index + 1 >= source.length then source.length
        else if source.charAt(index) == '*' && source.charAt(index + 1) == '/' then index + 2
        else skipBlock(index + 1)

      def ident(char: Char): Boolean = char.isLetterOrDigit || char == '_'

      def recur(index: Int, current: String, tokens: SList[String], line: Boolean)
      :   SList[String] =

        val flushed = if current.isEmpty then tokens else current :: tokens

        if index >= source.length then flushed.reverse else
          val char = source.charAt(index)
          val next = if index + 1 < source.length then source.charAt(index + 1) else ' '
          val next2 = if index + 2 < source.length then source.charAt(index + 2) else ' '

          if char == '/' && next == '/' then recur(skipLine(index), "", flushed, true)
          else if char == '/' && next == '*' then recur(skipBlock(index + 2), "", flushed, line)
          else if char == '#' && line then recur(skipLine(index), "", flushed, true)
          else if char == '.' && next == '.' && next2 == '.' then
            recur(index + 3, "", "..." :: flushed, false)
          else if ident(char) then recur(index + 1, current + char, tokens, false)
          else if char == '\n' then recur(index + 1, "", flushed, true)
          else if char.isWhitespace then recur(index + 1, "", flushed, line)
          else recur(index + 1, "", char.toString :: flushed, false)

      recur(0, "", SList(), true)

    // --- types ----------------------------------------------------------------------------------

    // The canonical arithmetic spellings (`cheader.md` §7): sign and length normalize to one
    // hyphenated name, so `unsigned int` and `int unsigned` could never hash apart (the latter
    // is not accepted; C headers do not write it).
    private def baseType(tokens: SList[String]): (Foreign.Type, SList[String]) raises Error =
      def named(name: Text, rest: SList[String]): (Foreign.Type, SList[String]) =
        (Foreign.Type.Named(name), rest)

      tokens match
        case "unsigned" :: "long" :: "long" :: "int" :: rest => named(t"unsigned-long-long", rest)
        case "unsigned" :: "long" :: "long" :: rest          => named(t"unsigned-long-long", rest)
        case "unsigned" :: "long" :: "int" :: rest           => named(t"unsigned-long", rest)
        case "unsigned" :: "long" :: rest                    => named(t"unsigned-long", rest)
        case "unsigned" :: "short" :: "int" :: rest          => named(t"unsigned-short", rest)
        case "unsigned" :: "short" :: rest                   => named(t"unsigned-short", rest)
        case "unsigned" :: "char" :: rest                    => named(t"unsigned-char", rest)
        case "unsigned" :: "int" :: rest                     => named(t"unsigned-int", rest)
        case "unsigned" :: rest                              => named(t"unsigned-int", rest)
        case "signed" :: "char" :: rest                      => named(t"signed-char", rest)
        case "signed" :: "int" :: rest                       => named(t"int", rest)
        case "signed" :: rest                                => named(t"int", rest)
        case "long" :: "long" :: "int" :: rest               => named(t"long-long", rest)
        case "long" :: "long" :: rest                        => named(t"long-long", rest)
        case "long" :: "int" :: rest                         => named(t"long", rest)
        case "long" :: "double" :: rest                      => named(t"long-double", rest)
        case "long" :: rest                                  => named(t"long", rest)
        case "short" :: "int" :: rest                        => named(t"short", rest)
        case "short" :: rest                                 => named(t"short", rest)

        case ("struct" | "union" | "enum") :: tag :: rest =>
          named(tag.tt, rest)

        case name :: rest if name.headOption.exists { char => char.isLetter || char == '_' } =>
          named(name.tt, rest)

        case _ => fail(t"a type was expected", tokens)

    // A type as it appears in a parameter, return or field position: qualifiers, a base, then
    // pointer structure. `const` on a by-value type is not contract and drops away; `const`
    // through a pointer is, and folds (`cheader.md` §7).
    private def typeOf(tokens: SList[String]): (Foreign.Type, SList[String]) raises Error =
      val (constant, afterConst) = tokens match
        case "const" :: rest    => (true, rest)
        case "volatile" :: rest => (false, rest)
        case _                  => (false, tokens)

      val (base, afterBase) = baseType(afterConst)

      def pointers(tokens: SList[String], typed: Foreign.Type, constant: Boolean)
      :   (Foreign.Type, SList[String]) =

        tokens match
          case "*" :: rest =>
            val pointee = if constant then Foreign.Type.Applied(t"const", List(typed)) else typed
            pointers(rest, Foreign.Type.Applied(t"ptr", List(pointee)), false)

          case "const" :: rest => pointers(rest, typed, constant)
          case _               => (typed, tokens)

      pointers(afterBase, base, constant)

    // --- parameters -----------------------------------------------------------------------------

    private def parameters(tokens: SList[String])
    :   (List[Foreign.Type], Boolean, SList[String]) raises Error =

      def recur(tokens: SList[String], acc: SList[Foreign.Type])
      :   (List[Foreign.Type], Boolean, SList[String]) raises Error =

        tokens match
          case ")" :: rest   => (List.from(acc.reverse), false, rest)
          case "," :: rest   => recur(rest, acc)
          case "..." :: ")" :: rest => (List.from(acc.reverse), true, rest)

          case _ =>
            val (typed, afterType) = typeOf(tokens)

            // The parameter name is optional and never contract (C calls are positional); an
            // array suffix degrades to a pointer, which is what the caller passes.
            val afterName = afterType match
              case name :: rest if name.headOption.exists { c => c.isLetter || c == '_' } => rest
              case rest                                                                   => rest

            val (adjusted, afterArray) = afterName match
              case "[" :: "]" :: rest      => (Foreign.Type.Applied(t"ptr", List(typed)), rest)
              case "[" :: _ :: "]" :: rest => (Foreign.Type.Applied(t"ptr", List(typed)), rest)
              case rest                    => (typed, rest)

            recur(afterArray, adjusted :: acc)

      tokens match
        case "(" :: "void" :: ")" :: rest => (List(), false, rest)
        case "(" :: rest                  => recur(rest, SList())
        case _                            => fail(t"a parameter list was expected", tokens)

    // --- declarations ---------------------------------------------------------------------------

    private def fieldList(tokens: SList[String])
    :   (List[(Text, Foreign.Type)], SList[String]) raises Error =

      def recur(tokens: SList[String], acc: SList[(Text, Foreign.Type)])
      :   (List[(Text, Foreign.Type)], SList[String]) raises Error =

        tokens match
          case "}" :: rest => (List.from(acc.reverse), rest)

          case _ =>
            val (typed, afterType) = typeOf(tokens)

            afterType match
              case name :: ";" :: rest => recur(rest, (name.tt, typed) :: acc)

              case name :: "[" :: size :: "]" :: ";" :: rest =>
                val array = Foreign.Type.Applied(t"array", List(typed,
                    Foreign.Type.Named(size.tt)))
                recur(rest, (name.tt, array) :: acc)

              case _ => fail(t"a field was expected", afterType)

      recur(tokens, SList())

    private def enumerators(tokens: SList[String])
    :   (List[(Text, Long)], SList[String]) raises Error =

      def recur(tokens: SList[String], next: Long, acc: SList[(Text, Long)])
      :   (List[(Text, Long)], SList[String]) raises Error =

        tokens match
          case "}" :: rest => (List.from(acc.reverse), rest)
          case "," :: rest => recur(rest, next, acc)

          case name :: "=" :: value :: rest =>
            val parsed =
              try java.lang.Long.decode(value).nn.longValue
              catch case _: NumberFormatException =>
                fail(t"an enumerator value must be numeric", tokens)

            recur(rest, parsed + 1, (name.tt, parsed) :: acc)

          case name :: rest => recur(rest, next + 1, (name.tt, next) :: acc)
          case SNil         => fail(t"an enumerator list is unterminated", tokens)

      recur(tokens, 0L, SList())

    private def item(tokens: SList[String])
    :   (Optional[Declaration], SList[String]) raises Error =

      tokens match
        case ";" :: rest      => (Unset, rest)
        case "extern" :: rest => item(rest)
        case "static" :: rest => item(rest)

        case "typedef" :: ("struct" | "union") :: rest =>
          val union = tokens(1) == "union"

          rest match
            case tag :: "{" :: body =>
              val (fields, after) = fieldList(body)

              after match
                case alias :: ";" :: more =>
                  (Declaration.Structure(alias.tt, union, fields), more)

                case _ => fail(t"a typedef name was expected", after)

            case "{" :: body =>
              val (fields, after) = fieldList(body)

              after match
                case alias :: ";" :: more =>
                  (Declaration.Structure(alias.tt, union, fields), more)

                case _ => fail(t"a typedef name was expected", after)

            case tag :: alias :: ";" :: more =>
              (Declaration.Alias(alias.tt, Foreign.Type.Named(tag.tt)), more)

            case _ => fail(t"a struct typedef was expected", rest)

        case "typedef" :: "enum" :: rest =>
          rest match
            case "{" :: body =>
              val (cases, after) = enumerators(body)

              after match
                case alias :: ";" :: more => (Declaration.Enumeration(alias.tt, cases), more)
                case _                    => fail(t"a typedef name was expected", after)

            case tag :: "{" :: body =>
              val (cases, after) = enumerators(body)

              after match
                case alias :: ";" :: more => (Declaration.Enumeration(alias.tt, cases), more)
                case _                    => fail(t"a typedef name was expected", after)

            case _ => fail(t"an enum typedef was expected", rest)

        case "typedef" :: rest =>
          val (typed, afterType) = typeOf(rest)

          afterType match
            // A function-pointer typedef: `typedef ret (*name)(params);`.
            case "(" :: "*" :: name :: ")" :: more =>
              val (params, variadic, after) = parameters(more)
              val fn = Foreign.Type.Applied(t"fn", List.from(typed :: params.stdlib))

              after match
                case ";" :: rest2 =>
                  val target = if variadic then Foreign.Type.Applied(t"variadic", List(fn)) else fn
                  (Declaration.Alias(name.tt, target), rest2)

                case _ => fail(t"a `;` was expected", after)

            case name :: ";" :: more => (Declaration.Alias(name.tt, typed), more)
            case _                   => fail(t"a typedef name was expected", afterType)

        case ("struct" | "union") :: tag :: "{" :: body =>
          val union = tokens.head == "union"
          val (fields, after) = fieldList(body)

          after match
            case ";" :: more => (Declaration.Structure(tag.tt, union, fields), more)
            case _           => fail(t"a `;` was expected", after)

        case ("struct" | "union") :: tag :: ";" :: rest =>
          (Declaration.Structure(tag.tt, tokens.head == "union", List(), opaque = true), rest)

        case "enum" :: tag :: "{" :: body =>
          val (cases, after) = enumerators(body)

          after match
            case ";" :: more => (Declaration.Enumeration(tag.tt, cases), more)
            case _           => fail(t"a `;` was expected", after)

        case _ =>
          val (result, afterType) = typeOf(tokens)

          afterType match
            case name :: "(" :: _ =>
              val (params, variadic, after) = parameters(afterType.tail)

              after match
                case ";" :: more =>
                  (Declaration.Function(name.tt, result, params, variadic), more)

                case _ => fail(t"a `;` was expected", after)

            case construct :: _ => unsupported(construct.tt)
            case SNil           => fail(t"a declaration was expected", afterType)
