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
┃    Soundness, version 0.27.0.                                                                    ┃
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
package harlequin

import anthology.*
import anticipation.*
import denominative.*
import gossamer.*
import proscenium.*
import rudiments.*
import vacuous.*

import dotty.tools.dotc.*, core.*, parsing.*, util.*, reporting.*

import scala.collection.mutable as scm

case class SourceCode
   (language: ProgrammingLanguage,
    offset:   Int,
    lines:    IArray[List[SourceToken]],
    focus:    Optional[((Int, Int), (Int, Int))] = Unset):

  def lastLine: Int = offset + lines.length - 1
  def apply(line: Int): List[SourceToken] = lines(line - offset)

  def extract(range: CodeRange): SourceCode =
    val focus = ((range.startLine, range.startColumn), (range.endLine, range.endColumn))
    if range.startLine != range.endLine
    then fragment(range.startLine, (range.endLine + 2).min(lastLine), focus)
    else fragment(range.startLine, (range.endLine + 1).min(lastLine), focus)

  def fragment(startLine: Int, endLine: Int, focus: Optional[((Int, Int), (Int, Int))] = Unset)
  :     SourceCode =
    SourceCode(language, startLine, lines.slice(startLine - offset, endLine - offset + 1), focus)

object SourceCode:
  private def accent(token: Int): Accent =
    if token <= 2 then Accent.Error
    else if token == 3 || token == 10 || token == 11 then Accent.String
    else if token >= 4 && token <= 9 || token == 23 || token == 24 || token == 42 || token == 43
    then Accent.Number
    else if token == 14 then Accent.Ident
    else if token >= 20 && token <= 62 && Tokens.modifierTokens.contains(token) then Accent.Modifier
    else if token >= 20 && token <= 62 then Accent.Keyword
    else if token >= 63 && token <= 84 then Accent.Symbol
    else Accent.Parens

  // This list was found in scala/scala3:compiler/src/dotty/tools/dotc/core/parsing/Parserse.scala
  private val soft: Set[Text] =
    Set(t"inline", t"opaque", t"open", t"transparent", t"infix", t"mut", t"erased", t"tracked")

  def apply(language: ProgrammingLanguage, text: Text): SourceCode =
    val source: SourceFile = SourceFile.virtual("<highlighting>", text.s)
    val ctx0 = Contexts.ContextBase().initialCtx.fresh.setReporter(Reporter.NoReporter)

    ctx0.setSetting(ctx0.settings.source, "future")

    given ctx: Contexts.Context =
      ctx0.setCompilationUnit(CompilationUnit(source, mustExist = false)(using ctx0))

    val trees = Trees()
    val parser = Parsers.Parser(source)

    trees.traverse(parser.compilationUnit())

    val scanner =
      if language == Java then JavaScanners.JavaScanner(source) else Scanners.Scanner(source)

    def untab(text: Text): Stream[SourceToken] =
      Stream(SourceToken(text.sub(t"\t", t"  "), Accent.Unparsed), SourceToken.Newline)

    def hard(stream: Stream[SourceToken]): Boolean = stream match
      case SourceToken(_, Accent.Unparsed) #:: more                   => hard(more)
      case SourceToken(text, Accent.Ident) #:: more if soft.has(text) => hard(more)
      case SourceToken(text, Accent.Keyword | Accent.Modifier) #:: _  => true
      case other                                                      => false

    def soften(stream: Stream[SourceToken]): Stream[SourceToken] = stream match
      case (token@SourceToken(text, Accent.Ident)) #:: more if soft.has(text) =>
        if hard(more) then SourceToken(text, Accent.Modifier) #:: soften(more)
        else token #:: soften(more)

      case token #:: more =>
        token #:: soften(more)

      case Stream() =>
        Stream()

    def stream(lastEnd: Int = 0): Stream[SourceToken] = scanner.token match
      case Tokens.EOF =>
        untab(text.segment(lastEnd.z ~ Ult.of(text))).filter(_.length > 0)

      case token =>
        val start = scanner.offset max lastEnd

        val unparsed: Stream[SourceToken] =
          if lastEnd != start
          then
            text.segment(lastEnd.z ~ Ordinal.natural(start))
            . cut(t"\n")
            . to(Stream)
            . flatMap(untab(_).filter(_.length > 0))
            . init

          else Stream()

        scanner.nextToken()
        val end = scanner.lastOffset max start

        val content: Stream[SourceToken] =
          if start == end then Stream() else
            text.segment(start.z ~ Ordinal.natural(end)).cut(t"\n").to(Stream).flatMap: line =>
              Stream
               (SourceToken(line, trees(start, end).getOrElse(accent(token))), SourceToken.Newline)
            . init

        unparsed #::: content #::: stream(end)

    def lines(seq: List[SourceToken], acc: List[List[SourceToken]] = Nil): List[List[SourceToken]] =
      seq match
        case Nil => acc
        case xs  => xs.indexOf(SourceToken.Newline) match
          case -1  => xs :: acc
          case idx => lines(xs.drop(idx + 1), xs.take(idx) :: acc)

    SourceCode(language, 1, IArray(lines(soften(stream()).to(List)).reverse*))

  private class Trees() extends ast.untpd.UntypedTreeTraverser:
    import ast.*, untpd.*

    private val trees: scm.HashMap[(Int, Int), Accent] = scm.HashMap()
    def apply(start: Int, end: Int): Option[Accent] = trees.get((start, end))

    def ignored(tree: NameTree): Boolean =
      val name = tree.name.toTermName
      name == StdNames.nme.ERROR || name == StdNames.nme.CONSTRUCTOR

    def traverse(tree: Tree)(using Contexts.Context): Unit =
      tree match
        case tree: NameTree if ignored(tree) =>
          ()

        case tree: ValOrDefDef =>
          if tree.nameSpan.exists
          then trees += (tree.nameSpan.start, tree.nameSpan.end) -> Accent.Term

        case tree: MemberDef =>
          if tree.nameSpan.exists
          then trees += (tree.nameSpan.start, tree.nameSpan.end) -> Accent.Typed

        case tree: Ident if tree.isType =>
          if tree.span.exists then trees += (tree.span.start, tree.span.end) -> Accent.Typed

        case _: TypTree =>
          if tree.span.exists then trees += (tree.span.start, tree.span.end) -> Accent.Typed

        case other =>
          ()

      traverseChildren(tree)
