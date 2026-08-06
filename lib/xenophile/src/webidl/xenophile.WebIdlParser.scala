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

import WebIdlError.Reason

// A declaration parser for WebIDL (https://webidl.spec.whatwg.org/), retaining what `webidl/1`
// atomizes: definitions with their partiality, mixin identity, `[Exposed]` scopes,
// required-versus-optional dictionary members, enumeration values, and special operations. It
// deliberately does *not* resolve partials, mixins or typedefs — resolution is the atomizer's
// first step (`webidl.md` §4), and folding it into parsing would erase the very structure the
// atomizer needs to merge deterministically.
//
// A construct outside the vocabulary is a hard error, never a skip (`webidl.md` §4): a
// partially-read capability contract understates what the file declares. Extended attributes
// other than `[Exposed]` are consumed and dropped — they are recognized structure, not
// unrecognized constructs.
//
// Types are canonicalized as `WebIdlDialect` canonicalizes them — numeric primitives to their
// width-explicit names, the string types to `string`, `T?` to a union with `null` — so the two
// readers of one `.idl` file agree about what a type is called.
object WebIdlParser:

  def parse(source: Text): List[WebIdlDefinition] raises WebIdlError =
    val definitions = scala.collection.mutable.ListBuffer[WebIdlDefinition]()
    var tokens = tokenize(source.s)

    while tokens.nonEmpty do
      val (definition, rest) = item(tokens)
      definitions += definition
      tokens = rest

    List.from(definitions.toList)

  private def fail(detail: Text, tokens: SList[String]): Nothing raises WebIdlError =
    val near = Text(tokens.take(5).mkString(" "))
    abort(WebIdlError(Reason.Syntax(detail, if near.s.isEmpty then t"the end" else near)))

  private def unsupported(construct: Text): Nothing raises WebIdlError =
    abort(WebIdlError(Reason.Unsupported(construct)))

  // --- lexing ---------------------------------------------------------------------------------

  private def tokenize(source: String): SList[String] =
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

    def ident(char: Char): Boolean = char.isLetterOrDigit || char == '_' || char == '-'

    def recur(index: Int, current: String, tokens: SList[String]): SList[String] =
      val flushed = if current.isEmpty then tokens else current :: tokens

      if index >= source.length then flushed.reverse else
        val char = source.charAt(index)
        val next = if index + 1 < source.length then source.charAt(index + 1) else ' '
        val next2 = if index + 2 < source.length then source.charAt(index + 2) else ' '

        if char == '/' && next == '/' then recur(skipLine(index), "", flushed)
        else if char == '/' && next == '*' then recur(skipBlock(index + 2), "", flushed)
        else if char == '"' then
          val end = endString(index + 1)
          recur(end, "", source.substring(index, end).nn :: flushed)
        else if char == '.' && next == '.' && next2 == '.' then
          recur(index + 3, "", "..." :: flushed)
        else if ident(char) then recur(index + 1, current + char, tokens)
        else if char.isWhitespace then recur(index + 1, "", flushed)
        else recur(index + 1, "", char.toString :: flushed)

    recur(0, "", SList())

  // --- extended attributes --------------------------------------------------------------------

  // Consumes one `[…]` extended-attribute group, returning the `[Exposed]` scopes it names.
  // `Exposed=Window`, `Exposed=(Window,Worker)` and `Exposed=*` are all read; every other
  // attribute is structure to skip, including argument lists and nested parentheses.
  private def attributes(tokens: SList[String]): (List[Text], SList[String]) raises WebIdlError =
    def scopes(tokens: SList[String], acc: SList[Text]): (SList[Text], SList[String]) =
      tokens match
        case ")" :: rest        => (acc.reverse, rest)
        case "," :: rest        => scopes(rest, acc)
        case name :: rest       => scopes(rest, name.tt :: acc)
        case SNil            => (acc.reverse, SList())

    def group(tokens: SList[String], depth: Int, acc: SList[Text])
    :   (SList[Text], SList[String]) raises WebIdlError =
      tokens match
        case "]" :: rest if depth == 0 => (acc, rest)
        case "[" :: rest               => group(rest, depth + 1, acc)
        case "]" :: rest               => group(rest, depth - 1, acc)

        case "Exposed" :: "=" :: "(" :: rest if depth == 0 =>
          val (names, more) = scopes(rest, SList())
          group(more, depth, acc ++ names)

        case "Exposed" :: "=" :: name :: rest if depth == 0 =>
          group(rest, depth, acc :+ name.tt)

        case _ :: rest => group(rest, depth, acc)
        case SNil   => fail(t"an extended attribute is unterminated", tokens)

    tokens match
      case "[" :: rest =>
        val (scopes, more) = group(rest, 0, SList())
        (List.from(scopes), more)

      case _ => (List(), tokens)

  // --- types ----------------------------------------------------------------------------------

  private def typeOf(tokens: SList[String]): (Foreign.Type, SList[String]) raises WebIdlError =
    // A type may carry its own extended-attribute group (`[LegacyNullToEmptyString] DOMString`);
    // none of them is contract, so the group is consumed here for every type position at once.
    val (_, afterAttrs) = attributes(tokens)
    val (base, rest) = baseType(afterAttrs)

    rest match
      case "?" :: more => (Foreign.Type.Union(List(base, Foreign.Type.Named(t"null"))), more)
      case _           => (base, rest)

  private def baseType(tokens: SList[String]): (Foreign.Type, SList[String]) raises WebIdlError =
    tokens match
      case "(" :: rest                            => union(rest, SList())
      case "unsigned" :: "long" :: "long" :: rest => (Foreign.Type.Named(t"u64"), rest)
      case "unsigned" :: "long" :: rest           => (Foreign.Type.Named(t"u32"), rest)
      case "unsigned" :: "short" :: rest          => (Foreign.Type.Named(t"u16"), rest)
      case "long" :: "long" :: rest               => (Foreign.Type.Named(t"s64"), rest)
      case "long" :: rest                         => (Foreign.Type.Named(t"s32"), rest)
      case "short" :: rest                        => (Foreign.Type.Named(t"s16"), rest)
      case "byte" :: rest                         => (Foreign.Type.Named(t"s8"), rest)
      case "octet" :: rest                        => (Foreign.Type.Named(t"u8"), rest)
      case "unrestricted" :: "float" :: rest      => (Foreign.Type.Named(t"f32"), rest)
      case "unrestricted" :: "double" :: rest     => (Foreign.Type.Named(t"f64"), rest)
      case "float" :: rest                        => (Foreign.Type.Named(t"f32"), rest)
      case "double" :: rest                       => (Foreign.Type.Named(t"f64"), rest)
      case "boolean" :: rest                      => (Foreign.Type.Named(t"boolean"), rest)
      case "void" :: rest                         => (Foreign.Type.Named(t"undefined"), rest)

      case ("DOMString" | "USVString" | "ByteString" | "CSSOMString") :: rest =>
        (Foreign.Type.Named(t"string"), rest)

      case name :: "<" :: rest =>
        val (args, after) = typeArguments(rest, SList())
        (Foreign.Type.Applied(name.tt, args), after)

      case name :: rest if name.headOption.exists(_.isLetter) =>
        (Foreign.Type.Named(name.tt), rest)

      case _ => fail(t"a type was expected", tokens)

  private def union(tokens: SList[String], acc: SList[Foreign.Type])
  :   (Foreign.Type, SList[String]) raises WebIdlError =

    val (member, rest) = typeOf(tokens)

    rest match
      case "or" :: more => union(more, member :: acc)
      case ")" :: more  => (Foreign.Type.Union(List.from((member :: acc).reverse)), more)
      case _            => fail(t"a union member must be followed by `or` or `)`", rest)

  private def typeArguments(tokens: SList[String], acc: SList[Foreign.Type])
  :   (List[Foreign.Type], SList[String]) raises WebIdlError =

    val (arg, rest) = typeOf(tokens)

    rest match
      case "," :: more => typeArguments(more, arg :: acc)
      case ">" :: more => (List.from((arg :: acc).reverse), more)
      case _           => fail(t"a type argument must be followed by `,` or `>`", rest)

  // --- default values -------------------------------------------------------------------------

  // A default's *value* is behaviour rather than contract; its existence is what folds. The
  // value forms are WebIDL's own: a literal, `null`, `[]`, `{}`.
  private def skipDefault(tokens: SList[String]): (Boolean, SList[String]) = tokens match
    case "=" :: "[" :: "]" :: rest => (true, rest)
    case "=" :: "{" :: "}" :: rest => (true, rest)
    case "=" :: "-" :: _ :: rest   => (true, rest)
    case "=" :: _ :: rest          => (true, rest)
    case _                         => (false, tokens)

  // --- members --------------------------------------------------------------------------------

  private def argumentList(tokens: SList[String])
  :   (List[WebIdlArgument], SList[String]) raises WebIdlError =

    def recur(tokens: SList[String], acc: SList[WebIdlArgument])
    :   (List[WebIdlArgument], SList[String]) raises WebIdlError =

      tokens match
        case ")" :: rest => (List.from(acc.reverse), rest)

        case _ =>
          val (_, afterAttrs) = attributes(tokens)

          val (optional, afterOptional) = afterAttrs match
            case "optional" :: rest => (true, rest)
            case _                  => (false, afterAttrs)

          val (typed, afterType) = typeOf(afterOptional)

          val (variadic, afterVariadic) = afterType match
            case "..." :: rest => (true, rest)
            case _             => (false, afterType)

          afterVariadic match
            case name :: rest =>
              val (default, afterDefault) = skipDefault(rest)
              val argument = WebIdlArgument(name.tt, typed, optional, variadic, default)

              afterDefault match
                case "," :: more => recur(more, argument :: acc)
                case ")" :: more => (List.from((argument :: acc).reverse), more)
                case _           => fail(t"an argument must be followed by `,` or `)`", afterDefault)

            case SNil => fail(t"an argument name was expected", afterVariadic)

    tokens match
      case "(" :: rest => recur(rest, SList())
      case _           => fail(t"an argument list was expected", tokens)

  private def member(tokens: SList[String])
  :   (Optional[WebIdlMember], Optional[(Text, List[Foreign.Type])], SList[String]) raises
        WebIdlError =

    val (_, afterAttrs) = attributes(tokens)

    val (static, afterStatic) = afterAttrs match
      case "static" :: rest => (true, rest)
      case _                => (false, afterAttrs)

    def semicolon(tokens: SList[String]): SList[String] raises WebIdlError = tokens match
      case ";" :: rest => rest
      case _           => fail(t"a `;` was expected", tokens)

    def attribute(tokens: SList[String], readonly: Boolean)
    :   (Optional[WebIdlMember], Optional[(Text, List[Foreign.Type])], SList[String]) raises
          WebIdlError =

      val (typed, afterType) = typeOf(tokens)

      afterType match
        case name :: rest =>
          val member =
            WebIdlMember(WebIdlMember.Kind.Attribute, name.tt, typed, readonly = readonly,
                static = static)

          (member, Unset, semicolon(rest))

        case SNil => fail(t"an attribute name was expected", afterType)

    def intrinsic(keyword: Text, tokens: SList[String])
    :   (Optional[WebIdlMember], Optional[(Text, List[Foreign.Type])], SList[String]) raises
          WebIdlError =

      tokens match
        case "<" :: rest =>
          val (args, after) = typeArguments(rest, SList())
          (Unset, (keyword, args), semicolon(after))

        case _ => fail(t"`$keyword` needs type arguments", tokens)

    def operation(tokens: SList[String], special: Optional[Text])
    :   (Optional[WebIdlMember], Optional[(Text, List[Foreign.Type])], SList[String]) raises
          WebIdlError =

      val (typed, afterType) = typeOf(tokens)

      afterType match
        case "(" :: _ =>
          // An anonymous special operation: the name position is empty.
          val (arguments, after) = argumentList(afterType)

          val member =
            WebIdlMember(WebIdlMember.Kind.Operation, t"", typed, arguments, static = static,
                special = special)

          (member, Unset, semicolon(after))

        case name :: rest =>
          val (arguments, after) = argumentList(rest)

          val member =
            WebIdlMember(WebIdlMember.Kind.Operation, name.tt, typed, arguments, static = static,
                special = special)

          (member, Unset, semicolon(after))

        case SNil => fail(t"an operation was expected", afterType)

    afterStatic match
      case "readonly" :: "attribute" :: rest => attribute(rest, readonly = true)
      case "inherit" :: "attribute" :: rest  => attribute(rest, readonly = false)
      case "attribute" :: rest               => attribute(rest, readonly = false)

      case "readonly" :: ("maplike" | "setlike") :: _ =>
        member(afterStatic.tail)

      case "const" :: rest =>
        val (typed, afterType) = typeOf(rest)

        afterType match
          case name :: more =>
            val (_, afterValue) = skipDefault(more)

            val constant =
              WebIdlMember(WebIdlMember.Kind.Constant, name.tt, typed, readonly = true)

            (constant, Unset, semicolon(afterValue))

          case SNil => fail(t"a constant name was expected", afterType)

      case "constructor" :: rest =>
        val (arguments, after) = argumentList(rest)

        val member =
          WebIdlMember(WebIdlMember.Kind.Constructor, t"", Foreign.Type.Named(t"undefined"),
              arguments)

        (member, Unset, semicolon(after))

      case "stringifier" :: ";" :: rest      => (Unset, (t"stringifier", List()), rest)
      case "stringifier" :: rest             => member(rest)
      case "iterable" :: rest                => intrinsic(t"iterable", rest)
      case "maplike" :: rest                 => intrinsic(t"maplike", rest)
      case "setlike" :: rest                 => intrinsic(t"setlike", rest)
      case "async" :: "iterable" :: rest     => intrinsic(t"async-iterable", rest)
      case "getter" :: rest                  => operation(rest, t"getter")
      case "setter" :: rest                  => operation(rest, t"setter")
      case "deleter" :: rest                 => operation(rest, t"deleter")
      case _                                 => operation(afterStatic, Unset)

  private def memberList(tokens: SList[String])
  :   (List[WebIdlMember], List[(Text, List[Foreign.Type])], SList[String]) raises
        WebIdlError =

    def recur
      ( tokens:     SList[String],
        members:    SList[WebIdlMember],
        intrinsics: SList[(Text, List[Foreign.Type])] )
    :   (List[WebIdlMember], List[(Text, List[Foreign.Type])], SList[String]) raises
          WebIdlError =

      tokens match
        case "}" :: ";" :: rest =>
          (List.from(members.reverse), List.from(intrinsics.reverse), rest)

        case "}" :: rest => fail(t"a definition must end `};`", rest)
        case SNil     => fail(t"a definition body is unterminated", tokens)

        case _ =>
          val (parsed, intrinsic, rest) = member(tokens)
          val nextMembers = parsed.let(_ :: members).or(members)
          val nextIntrinsics = intrinsic.let(_ :: intrinsics).or(intrinsics)
          recur(rest, nextMembers, nextIntrinsics)

    tokens match
      case "{" :: rest => recur(rest, SList(), SList())
      case _           => fail(t"a definition body was expected", tokens)

  // --- definitions ----------------------------------------------------------------------------

  private def item(tokens: SList[String]): (WebIdlDefinition, SList[String]) raises WebIdlError =
    val (exposed, afterAttrs) = attributes(tokens)

    def interface(tokens: SList[String], partial: Boolean, mixin: Boolean, callback: Boolean)
    :   (WebIdlDefinition, SList[String]) raises WebIdlError =

      tokens match
        case name :: rest =>
          val (parent, afterParent) = rest match
            case ":" :: parent :: more => (Optional(parent.tt), more)
            case _                     => (Unset, rest)

          val (members, intrinsics, after) = memberList(afterParent)

          (WebIdlDefinition.Interface(name.tt, parent, exposed, members, intrinsics, partial,
              mixin, callback), after)

        case SNil => fail(t"an interface name was expected", tokens)

    def dictionary(tokens: SList[String], partial: Boolean)
    :   (WebIdlDefinition, SList[String]) raises WebIdlError =

      def fields(tokens: SList[String], acc: SList[WebIdlField])
      :   (List[WebIdlField], SList[String]) raises WebIdlError =

        tokens match
          case "}" :: ";" :: rest => (List.from(acc.reverse), rest)
          case SNil            => fail(t"a dictionary body is unterminated", tokens)

          case _ =>
            val (_, afterAttrs) = attributes(tokens)

            val (required, afterRequired) = afterAttrs match
              case "required" :: rest => (true, rest)
              case _                  => (false, afterAttrs)

            val (typed, afterType) = typeOf(afterRequired)

            afterType match
              case name :: rest =>
                val (default, afterDefault) = skipDefault(rest)

                afterDefault match
                  case ";" :: more =>
                    fields(more, WebIdlField(name.tt, typed, required, default) :: acc)

                  case _ => fail(t"a `;` was expected", afterDefault)

              case SNil => fail(t"a dictionary member name was expected", afterType)

      tokens match
        case name :: rest =>
          val (parent, afterParent) = rest match
            case ":" :: parent :: more => (Optional(parent.tt), more)
            case _                     => (Unset, rest)

          afterParent match
            case "{" :: body =>
              val (members, after) = fields(body, SList())
              (WebIdlDefinition.Dictionary(name.tt, parent, members, partial), after)

            case _ => fail(t"a dictionary body was expected", afterParent)

        case SNil => fail(t"a dictionary name was expected", tokens)

    afterAttrs match
      case "partial" :: "interface" :: "mixin" :: rest => interface(rest, true, true, false)
      case "partial" :: "interface" :: rest            => interface(rest, true, false, false)
      case "partial" :: "dictionary" :: rest           => dictionary(rest, true)
      case "interface" :: "mixin" :: rest              => interface(rest, false, true, false)
      case "interface" :: rest                         => interface(rest, false, false, false)
      case "callback" :: "interface" :: rest           => interface(rest, false, false, true)
      case "dictionary" :: rest                        => dictionary(rest, false)

      case "partial" :: "namespace" :: name :: rest =>
        val (members, _, after) = memberList(rest)
        (WebIdlDefinition.Namespace(name.tt, exposed, members, partial = true), after)

      case "namespace" :: name :: rest =>
        val (members, _, after) = memberList(rest)
        (WebIdlDefinition.Namespace(name.tt, exposed, members), after)

      case "enum" :: name :: "{" :: rest =>
        def values(tokens: SList[String], acc: SList[Text])
        :   (List[Text], SList[String]) raises WebIdlError =

          tokens match
            case "}" :: ";" :: rest => (List.from(acc.reverse), rest)
            case "," :: rest        => values(rest, acc)

            case value :: rest if value.startsWith("\"") =>
              values(rest, Text(value.substring(1, value.length - 1).nn) :: acc)

            case _ => fail(t"an enumeration value was expected", tokens)

        val (list, after) = values(rest, SList())
        (WebIdlDefinition.Enumeration(name.tt, list), after)

      case "typedef" :: rest =>
        val (typed, afterType) = typeOf(rest)

        afterType match
          case name :: ";" :: more => (WebIdlDefinition.Alias(name.tt, typed), more)
          case _                   => fail(t"a typedef name and `;` were expected", afterType)

      case "callback" :: name :: "=" :: rest =>
        val (result, afterResult) = typeOf(rest)
        val (arguments, after) = argumentList(afterResult)

        after match
          case ";" :: more => (WebIdlDefinition.CallbackFunction(name.tt, result, arguments), more)
          case _           => fail(t"a `;` was expected", after)

      case target :: "includes" :: mixin :: ";" :: rest =>
        (WebIdlDefinition.Includes(target.tt, mixin.tt), rest)

      case construct :: _ => unsupported(construct.tt)
      case SNil        => fail(t"a definition was expected", afterAttrs)
