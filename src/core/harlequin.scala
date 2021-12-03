package harlequin

import gossamer.*

import dotty.tools.dotc.*, printing.*, core.*, parsing.*, util.*, reporting.*

import scala.collection.mutable as scm

enum Token:
  case Space(size: Int)
  case Newline
  case Code(value: Text, flair: Flair)

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
  
  def highlight(text: Text): Seq[Token] =
    val initCtx = Contexts.ContextBase().initialCtx.fresh.setReporter(Reporter.NoReporter)
    val source = SourceFile.virtual("<highlighting>", text.s)
    val ctx = initCtx.setCompilationUnit(CompilationUnit(source, mustExist = false)(using initCtx))
    val trees = Trees()
    val parser = Parsers.Parser(source)(using ctx)
    
    parser.blockStatSeq().foreach(trees.traverse(_)(using ctx))
    
    val scanner = Scanners.Scanner(source)(using ctx)
    def stream(lastEnd: Int = 0): LazyList[Token] = scanner.token match
      case Tokens.EOF =>
        LazyList()
      
      case token =>
        val start = scanner.offset max lastEnd
        val whitespace: LazyList[Token] =
          if lastEnd != start then
            text.slice(lastEnd, start).cut(t"\n").to(LazyList).flatMap:
              spaces => LazyList(Token.Space(spaces.sub(t"\t", t"  ").length), Token.Newline)
            .init
          else LazyList()
        
        scanner.nextToken()
        val end = scanner.lastOffset max start
        
        val content =
          if start == end then LazyList()
          else LazyList(Token.Code(text.slice(start, end), trees(start, end).getOrElse(flair(
              token))))
        
        whitespace #::: content #::: stream(end)
    
    stream()


  private class Trees() extends ast.untpd.UntypedTreeTraverser:
    import ast.*, untpd.*
    
    private val trees: scm.HashMap[(Int, Int), Flair] = scm.HashMap()
    
    def apply(start: Int, end: Int): Option[Flair] = trees.get((start, end))
    
    def ignored(tree: NameTree) =
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