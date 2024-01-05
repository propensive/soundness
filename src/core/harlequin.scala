/*
    Harlequin, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package harlequin

import rudiments.*
import anticipation.*
import gossamer.*
import spectacular.*
import kaleidoscope.*

import dotty.tools.dotc.*, core.*, parsing.*, util.*, reporting.*

import scala.collection.mutable as scm

enum Token:
  case Unparsed(text: Text)
  case Markup(text: Text)
  case Newline
  case Code(text: Text, accent: Accent)

  def length: Int = this match
    case Unparsed(text) => text.length
    case Markup(text)   => text.length
    case Newline        => 1
    case Code(text, _)  => text.length

enum Accent:
  case Error, Number, String, Ident, Term, Type, Keyword, Symbol, Parens, Modifier

object ScalaSyntax: 
  def accent(token: Int): Accent =
    if token <= 2 then Accent.Error
    else if token == 3 || token == 10 || token == 13 then Accent.String
    else if token >= 4 && token <= 9 then Accent.Number
    else if token == 14 || token == 15 then Accent.Ident
    else if token >= 20 && token <= 62 && Tokens.modifierTokens.contains(token) then Accent.Modifier
    else if token >= 20 && token <= 62 then Accent.Keyword
    else if token >= 63 && token <= 84 then Accent.Symbol
    else Accent.Parens
  
  def highlight(text: Text): IArray[Seq[Token]] =
    val initCtx = Contexts.ContextBase().initialCtx.fresh.setReporter(Reporter.NoReporter)
    val source = SourceFile.virtual("<highlighting>", text.s)
    val ctx = initCtx.setCompilationUnit(CompilationUnit(source, mustExist = false)(using initCtx))
    val trees = Trees()
    val parser = Parsers.Parser(source)(using ctx)
    
    parser.blockStatSeq().each(trees.traverse(_)(using ctx))
    
    val scanner = Scanners.Scanner(source)(using ctx)
    
    def markup(text: Text): LazyList[Token] = text match
      case r"$before(.*)\/\*!$inside([^*]*)\*\/$after(.*)" =>
        LazyList(Token.Unparsed(before.show), Token.Markup(inside.show)) #::: markup(after.show)
      
      case unparsed =>
        LazyList(Token.Unparsed(unparsed.sub(t"\t", t"  ")), Token.Newline)
        
    def stream(lastEnd: Int = 0): LazyList[Token] = scanner.token match
      case Tokens.EOF =>
        markup(text.slice(lastEnd, text.length)).filter(_.length > 0)
      
      case token =>
        val start = scanner.offset max lastEnd
        
        val unparsed: LazyList[Token] =
          if lastEnd != start
          then text.slice(lastEnd, start).cut(t"\n").to(LazyList)
              .flatMap(markup(_).filter(_.length > 0)).init
          else LazyList()

        scanner.nextToken()
        val end = scanner.lastOffset max start
        
        val content: LazyList[Token] =
          if start == end then LazyList()
          else
            text.slice(start, end).cut(t"\n").to(LazyList).flatMap:
              line => LazyList(Token.Code(line, trees(start, end).getOrElse(accent(token))),
                  Token.Newline)
            .init
        
        unparsed #::: content #::: stream(end)

    def lines(seq: List[Token], acc: List[List[Token]] = Nil): List[List[Token]] = seq match
      case Nil => acc
      case xs  => xs.indexOf(Token.Newline) match
        case -1  => xs :: acc
        case idx => lines(xs.drop(idx + 1), xs.take(idx) :: acc)
        
    IArray(lines(stream().to(List)).reverse*)


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
          then trees += (tree.nameSpan.start, tree.nameSpan.end) -> Accent.Type
        
        case tree: Ident if tree.isType =>
          if tree.span.exists
          then trees += (tree.span.start, tree.span.end) -> Accent.Type
        
        case _: TypTree =>
          if tree.span.exists then trees += (tree.span.start, tree.span.end) -> Accent.Type
        
        case _ =>
          ()
      
      traverseChildren(tree)
