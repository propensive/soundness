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
import scala.collection.immutable as sci
import scala.collection.immutable.{List, Nil}
import scala.language.experimental.pureFunctions
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

    // The literal is tokenized once, here, with a character which appears nowhere in it standing
    // for each substitution. The arguments which result are the command's, and say where in
    // them each substitution goes, so the code which is made constructs the command: nothing is
    // tokenized when it runs.
    val marker: Char =
      scala.Iterator.range(0xe000, 0xf900).map(_.toChar).find: char =>
        parts.forall(_.indexOf(char) < 0)

      . getOrElse(halt(m"the command uses every private-use character"))

    // The words in which a quotation was opened: such a word is an argument even if it turns
    // out to be empty, which a word made only of a substitution of no parameters is not.
    var quoted: sci.Set[Int] = sci.Set()

    def tokenize(state: Sh.State, part: String): Sh.State =
      var current = state
      var index = 0

      while index < part.length do
        val before = current.current
        current = rethrow(Sh.Runtime.parse(current, part.charAt(index).toString.tt))

        if current.current != before
          && (current.current == Sh.Context.Quotes1 || current.current == Sh.Context.Quotes2)
        then quoted = quoted + (current.arguments.length - 1)

        index += 1

      current

    var checkState = tokenize(rethrow(Sh.Runtime.initial), parts.head)

    // Each substitution's parameters, and whether it stands within a quotation.
    var holes: List[(Expr[Sh.Parameters], Boolean)] = Nil
    var i = 0

    while i < insertionExprs.length do
      val head = insertionExprs(i)
      val nextPart = parts(i + 1)

      head.absolve match
        case '{$value: tpe} =>
          val typeclassExpr: Expr[Insertion[Sh.Parameters, tpe]] =
            Expr.summon[Insertion[Sh.Parameters, tpe]].getOrElse:
              halt(m"can't substitute ${TypeRepr.of[tpe].show} into a sh-interpolator")

          val inQuotes =
            checkState.current == Sh.Context.Quotes1 || checkState.current == Sh.Context.Quotes2

          holes = holes :+ ('{$typeclassExpr.embed($value)}, inQuotes)

          checkState =
            rethrow(Sh.Runtime.insert(checkState, Sh.Parameters(marker.toString.tt)))

          checkState = tokenize(checkState, nextPart)

      i += 1

    val arguments: List[String] = rethrow(Sh.Runtime.complete(checkState)).arguments.toList.map(_.s)

    // A word of the command: the literal text and the substitutions which make up one argument,
    // or, where a substitution outside a quotation gives several parameters, several.
    var next = 0

    val words: List[(Boolean, Expr[Text], Expr[sci.List[Text]])] =
      arguments.zipWithIndex.map: (argument, index) =>
        var spread = false
        var text: Expr[String] = '{""}
        var pieces: List[Expr[sci.List[Text]]] = Nil
        var start = 0

        def literal(end: Int): Unit = if end > start then
          val part = argument.substring(start, end).nn
          text = '{$text+${Expr(part)}}
          pieces = pieces :+ '{sci.List(${Expr(part)}.tt)}

        var position = argument.indexOf(marker)

        while position >= 0 do
          literal(position)
          val (parameters, inQuotes) = holes(next)
          next += 1

          if inQuotes then
            text = '{$text+Sh.Runtime.quoted($parameters).s}
            pieces = pieces :+ '{sci.List(Sh.Runtime.quoted($parameters))}
          else
            spread = true
            pieces = pieces :+ '{$parameters.params.toList}

          start = position + 1
          position = argument.indexOf(marker, start)

        literal(argument.length)
        if quoted.contains(index) then pieces = pieces :+ '{sci.List("".tt)}

        val word = if argument.indexOf(marker) < 0 then '{${Expr(argument)}.tt} else '{$text.tt}
        (spread, word, '{Sh.Runtime.word(${Expr.ofList(pieces)})})

    val command: Expr[Command] =
      if words.exists(_(0)) then '{Sh.Runtime.command(${Expr.ofList(words.map(_(2)))})}
      else '{Command(${Varargs(words.map(_(1)))}*)}

    val execType =
      ConstantType(StringConstant(parts.head.split(" ").nn.head.nn))

    val bounds = TypeBounds(execType, execType)

    Refinement(TypeRepr.of[Command], "Exec", bounds).asType.absolve match
      case '[type commandType <: Command; commandType] =>
        '{$command.asInstanceOf[commandType]}
