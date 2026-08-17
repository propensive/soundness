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
package guillotine

import scala.collection.immutable.Seq

import scala.language.experimental.pureFunctions

import scala.collection.immutable.{List, Nil, ::}
import scala.quoted.*

import anticipation.*
import contextual.*
import fulminate.*

object internal:
  def sh(context: Expr[StringContext], insertions: Expr[Seq[Any]])(using Quotes)
  :   Expr[Any] =

    import quotes.reflect.*

    val parts: List[String] =
      context.value.getOrElse:
        halt(m"the StringContext extension method parameter does not appear to be inline")

      . parts.toList

    val insertionExprs: List[Expr[Any]] = insertions.absolve match
      case Varargs(exprs) => exprs.toList

    // Recover each literal part's source-file range from the `StringContext.apply(...)` term,
    // so that parse errors can be positioned at the offending character within the literal. The
    // walk is permissive about the surrounding tree shape; if the collected literals don't match
    // the resolved parts, positions fall back to the whole macro expansion.
    def collectLiterals(term: Term, acc: List[(Int, Int)]): List[(Int, Int)] = term match
      case Literal(StringConstant(_)) => (term.pos.start, term.pos.end) :: acc
      case Inlined(_, _, body)        => collectLiterals(body, acc)
      case Typed(body, _)             => collectLiterals(body, acc)
      case Block(_, expr)             => collectLiterals(expr, acc)

      case Apply(fn, args) =>
        val withFn = collectLiterals(fn, acc)
        args.foldLeft(withFn): (acc2, arg) => collectLiterals(arg, acc2)

      case Repeated(elems, _) => elems.foldLeft(acc): (acc2, elem) => collectLiterals(elem, acc2)
      case _                  => acc

    val collected = collectLiterals(context.asTerm, Nil).reverse

    val partOrigins: List[(Int, Int)] =
      if collected.length == parts.length then collected else List.fill(parts.length)((0, 0))

    val macroPos = Position.ofMacroExpansion
    val sourceFile = macroPos.sourceFile

    val sourceContent: Option[String] = sourceFile.content match
      case Some(content: String) => Some(content)
      case _                     => None

    // Per part, precompute the value→source offset mapping (accounting for `$$` and `\uHHHH`
    // escapes, whose source spelling is longer than their value).
    val perPart: Seq[((String, Int), Int -> Int)] =
      parts.zip(partOrigins).map: (part, origin) =>
        val srcStart = origin(0)

        val mapping: Int -> Int = sourceContent match
          case Some(content) if srcStart > 0 && srcStart < content.length =>
            val upper = (srcStart + part.length*6 + 16).min(content.length)
            Interpolation.buildMapping(content.substring(srcStart, upper).nn, part)

          case _ =>
            identity

        ((part, srcStart), mapping)

      . toIndexedSeq

    // Map a parser offset (in "value space", where each substitution occupies one position, as
    // in `Sh.Runtime.insert`) to a source-file position.
    def position(offset: Int, length: Int): Position =
      var acc = 0
      var i = 0

      while i < perPart.length do
        val ((part, srcStart), mapping) = perPart(i)

        if offset < acc + part.length && srcStart > 0 then
          val inPart = offset - acc
          val endIn = (inPart + length.max(1)).min(part.length)
          val rawStart = (srcStart + mapping(inPart)).max(srcStart)
          val rawEnd = (srcStart + mapping(endIn)).max(rawStart + 1)
          return Position(sourceFile, rawStart, rawEnd)

        acc += part.length + 1
        i += 1

      macroPos

    def rethrow[result](block: => result): result =
      try block
      catch case error: Sh.Error => halt(error.detail, position(error.offset, 1))

    var checkState = rethrow(Sh.Runtime.parse(Sh.Runtime.initial, parts.head.tt))

    var runtimeExpr: Expr[Sh.State] =
      '{Sh.Runtime.parse(Sh.Runtime.initial, ${Expr(parts.head)}.tt)}

    var i = 0

    while i < insertionExprs.length do
      val head = insertionExprs(i)
      val nextPart = parts(i + 1)

      head.absolve match
        case '{$value: tpe} =>
          val typeclassExpr: Expr[Insertion[Sh.Parameters, tpe]] =
            Expr.summon[Insertion[Sh.Parameters, tpe]].getOrElse:
              halt(m"can't substitute ${TypeRepr.of[tpe].show} into a sh-interpolator")

          checkState = rethrow(Sh.Runtime.skip(checkState))
          checkState = rethrow(Sh.Runtime.parse(checkState, nextPart.tt))

          val current = runtimeExpr

          runtimeExpr =
            ' {
                Sh.Runtime.parse
                  ( Sh.Runtime.insert($current, $typeclassExpr.embed($value)),
                    ${Expr(nextPart)}.tt )
              }

      i += 1

    rethrow(Sh.Runtime.complete(checkState))

    val execType =
      ConstantType(StringConstant(parts.head.split(" ").nn.head.nn))

    val bounds = TypeBounds(execType, execType)

    Refinement(TypeRepr.of[Command], "Exec", bounds).asType.absolve match
      case '[type commandType <: Command; commandType] =>
        '{Sh.Runtime.complete($runtimeExpr).asInstanceOf[commandType]}
