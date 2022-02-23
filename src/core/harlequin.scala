/*
    Harlequin, version 0.4.0. Copyright 2021-22 Jon Pretty, Propensive OÜ.

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
import gossamer.*
import kaleidoscope.*

import dotty.tools.dotc.*, printing.*, core.*, parsing.*, util.*, reporting.*

import scala.collection.mutable as scm

enum Token:
  case Unparsed(text: Text)
  case Markup(text: Text)
  case Newline
  case Code(text: Text, flair: Flair)

  def length: Int = this match
    case Unparsed(text) => text.length
    case Markup(text)   => text.length
    case Newline        => 1
    case Code(text, _)  => text.length

enum Flair:
  case Error, Number, String, Ident, Term, Type, Keyword, Symbol, Parens, Modifier

object ScalaSyntax: 
  def flair(token: Int): Flair =
    if token <= 2 then Flair.Error
    else if token == 3 || token == 10 || token == 13 then Flair.String
    else if token >= 4 && token <= 9 then Flair.Number
    else if token == 14 || token == 15 then Flair.Ident
    else if token >= 20 && token <= 62 && Tokens.modifierTokens.contains(token) then Flair.Modifier
    else if token >= 20 && token <= 62 then Flair.Keyword
    else if token >= 63 && token <= 84 then Flair.Symbol
    else Flair.Parens
  
  def highlight(text: Text): IArray[Seq[Token]] =
    val initCtx = Contexts.ContextBase().initialCtx.fresh.setReporter(Reporter.NoReporter)
    val source = SourceFile.virtual("<highlighting>", text.s)
    val ctx = initCtx.setCompilationUnit(CompilationUnit(source, mustExist = false)(using initCtx))
    val trees = Trees()
    val parser = Parsers.Parser(source)(using ctx)
    
    parser.blockStatSeq().foreach(trees.traverse(_)(using ctx))
    
    val scanner = Scanners.Scanner(source)(using ctx)
    
    def markup(text: Text): LazyList[Token] = text match
      case r"$before@(.*)\/\*!$inside@([^*]*)\*\/$after@(.*)" =>
        LazyList(Token.Unparsed(Text(before)), Token.Markup(Text(inside))) #::: markup(Text(after))
      case unparsed =>
        LazyList(Token.Unparsed(unparsed.sub(t"\t", t"  ")), Token.Newline)
        
    def stream(lastEnd: Int = 0): LazyList[Token] = scanner.token match
      case Tokens.EOF =>
        markup(text.slice(lastEnd, text.length)).filter(_.length > 0)
      
      case token =>
        val start = scanner.offset max lastEnd
        
        val unparsed: LazyList[Token] =
          if lastEnd != start then
            text.slice(lastEnd, start).cut(t"\n").to(LazyList).flatMap(markup(_).filter(_.length > 0)).init
          else LazyList()
        
        scanner.nextToken()
        val end = scanner.lastOffset max start
        
        val content =
          if start == end then LazyList()
          else LazyList(Token.Code(text.slice(start, end), trees(start, end).getOrElse(flair(
              token))))
        
        unparsed #::: content #::: stream(end)

    def lines(seq: List[Token], acc: List[List[Token]] = Nil): List[List[Token]] = seq match
      case Nil =>
        acc
        
      case xs => xs.indexOf(Token.Newline) match
        case -1  => xs :: acc
        case idx => lines(xs.drop(idx + 1), xs.take(idx) :: acc)
        
    IArray(lines(stream().to(List)).reverse*)


  private class Trees() extends ast.untpd.UntypedTreeTraverser:
    import ast.*, untpd.*
    
    private val trees: scm.HashMap[(Int, Int), Flair] = scm.HashMap()
    
    def apply(start: Int, end: Int): Option[Flair] = trees.get((start, end))
    
    def ignored(tree: NameTree): Boolean =
      val name = tree.name.toTermName
      name == StdNames.nme.ERROR || name == StdNames.nme.CONSTRUCTOR
    
    def traverse(tree: Tree)(using Contexts.Context): Unit =
      tree match
        case tree: NameTree if ignored(tree) =>
          ()
        
        case tree: ValOrDefDef =>
          if tree.nameSpan.exists
          then trees += (tree.nameSpan.start, tree.nameSpan.end) -> Flair.Term
        
        case tree: MemberDef =>
          if tree.nameSpan.exists
          then trees += (tree.nameSpan.start, tree.nameSpan.end) -> Flair.Type
        
        case tree: Ident if tree.isType =>
          if tree.span.exists
          then trees += (tree.span.start, tree.span.end) -> Flair.Type
        
        case _: TypTree =>
          if tree.span.exists then trees += (tree.span.start, tree.span.end) -> Flair.Type
        
        case _ =>
          ()
      
      traverseChildren(tree)