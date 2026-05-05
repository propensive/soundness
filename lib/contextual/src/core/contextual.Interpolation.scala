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
package contextual

import language.dynamics

import scala.quoted.*

import gigantism.*
import prepositional.*
import proscenium.*

object Interpolation:
  /** Build a value-offset → source-offset mapping for a Scala interpolator
    * part, accounting for the source-level escapes that may make the source
    * longer than the value:
    *
    *   - `\\u####` decodes to 1 value char (6 source chars).
    *   - `\\X` decodes to 1 value char (2 source chars), for any other X.
    *   - `$$` collapses to a single `$` value char (2 source chars). A
    *     literal `$` in a static part must have been written as either `$$`
    *     or `\\u0024` in source — the `\\u` form is already covered above.
    *
    * `sourceText` is the source text covering the literal's content (the
    * literal Term's `pos` excludes the surrounding quote delimiters);
    * `value` is the Scala-decoded string the macro receives. Triple-quoted
    * literals (or any literal whose value matches its source byte-for-byte)
    * collapse to the identity.
    */
  def buildMapping(sourceText: String, value: String): Int => Int =
    // If the source matches the value byte-for-byte at the start, it's a
    // triple-quoted literal (or a single-quoted one with no escapes); the
    // mapping is the identity. The byte-for-byte check is what distinguishes
    // a literal `\n` (no escape) from a source `\n` that decodes to newline.
    val matchesPrefix =
      sourceText.length >= value.length
      && sourceText.regionMatches(0, value, 0, value.length)

    if matchesPrefix then i => i.max(0).min(value.length)
    else
      val arr = new Array[Int](value.length + 1)
      var srcIdx = 0
      var valIdx = 0
      while valIdx < value.length do
        arr(valIdx) = srcIdx
        if srcIdx + 1 < sourceText.length && sourceText.charAt(srcIdx) == '\\' then
          if sourceText.charAt(srcIdx + 1) == 'u' then srcIdx += 6 else srcIdx += 2
        else if srcIdx + 1 < sourceText.length
                && sourceText.charAt(srcIdx) == '$'
                && sourceText.charAt(srcIdx + 1) == '$'
        then srcIdx += 2
        else srcIdx += 1
        valIdx += 1
      arr(value.length) = srcIdx
      i => if i < 0 then 0 else if i < arr.length then arr(i) else arr(arr.length - 1)

  def apply[topic: Type](context: Expr[StringContext]): Macro[Interpolation of topic] =
    import quotes.reflect.*

    val parts: List[String] = context.valueOrAbort.parts.to(List)

    // Walk the StringContext.apply(...) Term to recover each part's source-file
    // (start, end) range. We're permissive about the surrounding tree shape
    // (Inlined / Typed / Block wrappers, Apply nestings) and just collect every
    // Literal(StringConstant) we encounter. If the count doesn't match the
    // resolved parts we fall back to (0, 0) so the typeclass macros land at the
    // macro call site instead of at random positions.
    def collectLiterals(term: Term, acc: List[(Int, Int)]): List[(Int, Int)] = term match
      case Literal(StringConstant(_))  => (term.pos.start, term.pos.end) :: acc
      case Inlined(_, _, body)         => collectLiterals(body, acc)
      case Typed(body, _)              => collectLiterals(body, acc)
      case Block(_, expr)              => collectLiterals(expr, acc)
      case Apply(fn, args)             =>
        val withFn = collectLiterals(fn, acc)
        args.foldLeft(withFn)((a, arg) => collectLiterals(arg, a))
      case Repeated(elems, _)          => elems.foldLeft(acc)((a, e) => collectLiterals(e, a))
      case _                           => acc

    val collected = collectLiterals(context.asTerm, Nil).reverse
    val partOrigins: List[(Int, Int)] =
      if collected.length == parts.length then collected
      else List.fill(parts.length)((0, 0))

    def transportType
      (parts: List[String], repr: TypeRepr = TypeRepr.of[EmptyTuple.type])
    :   TypeRepr =

      parts match
        case head :: tail =>
          ConstantType(StringConstant(head)).asType.absolve match
            case '[label] => repr.asType.absolve match
              case '[type tuple <: Tuple; tuple] => transportType(tail, TypeRepr.of[label *: tuple])

        case Nil =>
          repr

    def originsType
      (origins: List[(Int, Int)], repr: TypeRepr = TypeRepr.of[EmptyTuple.type])
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
            '{
                new Interpolation():
                  type Topic = topic
                  type Transport = transport
                  type Origins = origins
              }


trait Interpolation:
  type Topic
  type Transport <: Tuple
  type Origins <: Tuple

  transparent inline def apply(inline insertions: Any*)(using Topic is Interpolable): Topic =
    summon[Topic is Interpolable].interpolate[Transport, Origins](insertions*)

  transparent inline def unapply(using extrapolable: Topic is Extrapolable)(scrutinee: Topic): Any =
    extrapolable.extrapolate[Transport, Origins](scrutinee)
