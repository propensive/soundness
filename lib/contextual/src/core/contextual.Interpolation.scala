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
package contextual

import scala.language.dynamics

import scala.quoted.*

import gigantism.*
import prepositional.*

object Interpolation:
  def buildMapping(sourceText: String, value: String): Int -> Int =
    val arr = new scala.Array[Int](value.length + 1)
    var srcIdx = 0
    var valIdx = 0

    while valIdx < value.length do
      arr(valIdx) = srcIdx
      val srcChar = if srcIdx < sourceText.length then sourceText.charAt(srcIdx) else '\u0000'
      val valChar = value.charAt(valIdx)

      val isDollarEscape =
        srcChar == '$' &&
          srcIdx + 1 < sourceText.length &&
          sourceText.charAt(srcIdx + 1) == '$' &&
          valChar == '$'

      val isUnicodeEscape =
        srcChar == '\\' &&
          srcIdx + 5 < sourceText.length &&
          sourceText.charAt(srcIdx + 1) == 'u' &&
          valChar != '\\'

      if isDollarEscape then srcIdx += 2
      else if isUnicodeEscape then srcIdx += 6
      else srcIdx += 1 // identity or defensive fallback

      valIdx += 1

    arr(value.length) = srcIdx
    val mapping: Array[Int]^{} = Array.unsafeFrozen(arr)
    i => if i < 0 then 0 else if i < mapping.length then mapping.readable(i) else mapping.readable(mapping.length - 1)

  // Walk a `StringContext.apply(...)` Term to recover each literal part's source-file
  // (start, end) range. We're permissive about the surrounding tree shape (Inlined / Typed /
  // Block wrappers, Apply nestings) and just collect every Literal(StringConstant) we
  // encounter. If the count doesn't match `count` we fall back to (0, 0) for every part, so
  // errors land at the macro call site instead of at random positions.
  def literalOrigins(using Quotes)(context: Expr[StringContext], count: Int): List[(Int, Int)] =
    import quotes.reflect.*

    def collectLiterals(term: Term, acc: List[(Int, Int)]): List[(Int, Int)] = term match
      case Literal(StringConstant(_))  => (term.pos.start, term.pos.end) :: acc
      case Inlined(_, _, body)         => collectLiterals(body, acc)
      case Typed(body, _)              => collectLiterals(body, acc)
      case Block(_, expr)              => collectLiterals(expr, acc)

      case Apply(fn, args) =>
        val withFn = collectLiterals(fn, acc)
        args.foldLeft(withFn): (a, arg) => collectLiterals(arg, a)

      case Repeated(elems, _)          => elems.foldLeft(acc): (a, e) => collectLiterals(e, a)
      case _                           => acc

    val collected = List.of(collectLiterals(context.asTerm, Nil).stdlib.reverse)
    if collected.stdlib.length == count then collected else List.fill(count)((0, 0))

  // Decode a type-level `Transport` tuple of string-literal types back into the parts. The
  // tuple is built innermost-first by `transportType`, so it holds the parts in reverse;
  // prepend-accumulation restores source order.
  def decodeParts[parts <: Tuple: Type](using Quotes): List[String] =
    import quotes.reflect.*

    def recur[tuple: Type](acc: scala.List[String]): scala.List[String] = Type.of[tuple] match
      case '[head *: tail] => TypeRepr.of[head].dealias match
        case ConstantType(StringConstant(part)) => recur[tail](acc.prepended(part))
        case _ => report.errorAndAbort("an interpolator's parts are string-literal types")

      case _ =>
        acc

    List.of(recur[parts](scala.Nil))

  // Decode a type-level `Origins` tuple of (start, end) constant-integer pairs, as encoded by
  // `Interpolation.apply`. Like `Transport`, the tuple holds the origins in reverse.
  // Unrecognized elements decode as (0, 0), the "unknown origin" marker.
  def decodeOrigins[origins <: Tuple: Type](using Quotes): List[(Int, Int)] =
    import quotes.reflect.*

    def recur[tuple: Type](acc: scala.List[(Int, Int)]): scala.List[(Int, Int)] =
      Type.of[tuple] match
        case '[head *: tail] =>
          val pair = TypeRepr.of[head].dealias match
            case AppliedType(_, scala.List(ConstantType(IntConstant(s)), ConstantType(IntConstant(e)))) =>
              (s, e)

            case _ =>
              (0, 0)

          recur[tail](acc.prepended(pair))

        case _ =>
          acc

    List.of(recur[origins](scala.Nil))

  // Translate a parser character offset into an "assembled" input — the literal parts joined
  // with `substitutionWidth` characters standing in for each substitution — to a source-file
  // position of at least one character. Escape sequences whose source spelling is longer than
  // their value (`$$`, `\uHHHH`) are compensated for with `buildMapping`. Falls back to the
  // whole macro expansion when the offset lies in a substitution gap or the part's origin is
  // unknown.
  def sourcePosition(using Quotes)
    ( parts:             List[String],
      origins:           List[(Int, Int)],
      substitutionWidth: Int,
      offset:            Int,
      length:            Int = 1 )
  :   quotes.reflect.Position =

    import quotes.reflect.*

    val macroPos = Position.ofMacroExpansion
    val sourceFile = macroPos.sourceFile

    val content: String | Null = sourceFile.content match
      case Some(content: String) => content
      case _                     => null

    val partVector = parts.stdlib.toVector
    val originVector = origins.stdlib.toVector

    var acc = 0
    var i = 0

    while i < partVector.length && i < originVector.length do
      val part = partVector(i)
      val start = originVector(i)(0)

      if offset >= acc && offset < acc + part.length && start > 0 && content != null then
        val upper = (start + part.length*6 + 16).min(content.length)
        val mapping = buildMapping(content.substring(start, upper).nn, part)
        val inPart = offset - acc
        val endIn = (inPart + length.max(1)).min(part.length)
        val rawStart = (start + mapping(inPart)).max(start)
        val rawEnd = (start + mapping(endIn)).max(rawStart + 1)
        return Position(sourceFile, rawStart, rawEnd)

      acc += part.length + substitutionWidth
      i += 1

    macroPos

  def apply[topic: Type](context: Expr[StringContext]): Macro[Interpolation of topic] =
    import quotes.reflect.*

    val parts: List[String] = context.valueOrAbort.parts.to(List)
    val partOrigins: List[(Int, Int)] = literalOrigins(context, parts.stdlib.length)

    // The tuple terminator must be `scala.EmptyTuple.type` itself. Bare `EmptyTuple.type` would
    // name the `proscenium` prelude's *export forwarder*, which is a (nullary) method, not the
    // `scala` object, so its singleton type is a `TermRef` to a method rather than the module's
    // singleton. A tuple ending in that type still conforms to `Tuple`, but `constValueTuple` (and
    // every other `EmptyTuple`-terminated match type) fails to see the end of the tuple, so call
    // sites report "Tuple element types must be known at compile time".
    def transportType
      ( parts: List[String], repr: TypeRepr = TypeRepr.of[scala.EmptyTuple.type] )
    :   TypeRepr =

      parts match
        case head :: tail =>
          ConstantType(StringConstant(head)).asType.absolve match
            case '[label] => repr.asType.absolve match
              case '[type tuple <: Tuple; tuple] => transportType(tail, TypeRepr.of[label *: tuple])

        case Nil =>
          repr

    def originsType
      ( origins: List[(Int, Int)], repr: TypeRepr = TypeRepr.of[scala.EmptyTuple.type] )
    :   TypeRepr =

      origins match
        case (start, end) :: tail =>
          ConstantType(IntConstant(start)).asType.absolve match
            case '[type s <: Int; s] => ConstantType(IntConstant(end)).asType.absolve match
              case '[type e <: Int; e] => repr.asType.absolve match
                case '[type tuple <: Tuple; tuple] =>
                  originsType(tail, TypeRepr.of[(s, e) *: tuple])

        case Nil =>
          repr

    transportType(parts).asType.absolve match
      case '[type transport <: Tuple; transport] =>
        originsType(partOrigins).asType.absolve match
          case '[type origins <: Tuple; origins] =>
            // The quoted instance's abstract type members (`topic`, `transport`, `origins`) acquire
            // spurious universal capture annotations under capture checking, so the inferred `Expr`
            // type does not match the declared `Macro` return type. The generated code is pure
            // plumbing, so the `Expr` is re-typed to the expected interface.
            ' {
                new Interpolation():
                  type Topic = topic
                  type Transport = transport
                  type Origins = origins
              } . asInstanceOf[Expr[Interpolation of topic]]


trait Interpolation extends scala.caps.Pure:
  type Topic
  type Transport <: Tuple
  type Origins <: Tuple

  transparent inline def apply(inline insertions: Any*)(using Topic is Interpolable): Topic =
    summon[Topic is Interpolable].interpolate[Transport, Origins](insertions*)

  transparent inline def unapply(using extrapolable: Topic is Extrapolable)(scrutinee: Topic): Any =
    extrapolable.extrapolate[Transport, Origins](scrutinee)
