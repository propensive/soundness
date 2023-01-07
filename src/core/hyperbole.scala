package hyperbole

import tableStyles.horizontalGaps
import harlequin.*
import escapade.*
import rudiments.*
import gossamer.*
import iridescence.*

import scala.quoted.*
import dotty.tools.*, dotc.util as dtdu

object reflection:
  def expand[T](expr: Expr[T])(using Quotes): Text =
    import quotes.reflect.*
    expr.asTerm.show.show

    def init = expr.asTerm.pos.startColumn

    def source(term: Term): AnsiText = term.pos match
      case pos: dtdu.SourcePosition =>
        ((t" "*(pos.startColumn - init))+pos.lineContent.show.slice(pos.startColumn, pos.endColumn)).ansi
      case _ =>
        ansi""
    
    def typeSource(term: TypeTree): AnsiText = term.pos match
      case pos: dtdu.SourcePosition =>
        ((t" "*(pos.startColumn - init))+pos.lineContent.show.slice(pos.startColumn, pos.endColumn)).ansi
      case _ =>
        ansi""
    
    def caseDefSource(term: CaseDef): AnsiText = term.pos match
      case pos: dtdu.SourcePosition =>
        ((t" "*(pos.startColumn - init))+pos.lineContent.show.slice(pos.startColumn, pos.endColumn)).ansi
      case _ =>
        ansi""

    object Expansion:
      def apply(i: List[Boolean], name: Text, term: Term, parameter: Maybe[Text] = Unset): Expansion =
        Expansion(i, name, term.show.show, source(term).plain, parameter)
      
      def ofType(i: List[Boolean], name: Text, tt: TypeTree, parameter: Maybe[Text] = Unset): Expansion =
        Expansion(i, name, tt.show.show, typeSource(tt).plain, parameter)
      
      def ofCaseDef(i: List[Boolean], name: Text, caseDef: CaseDef, parameter: Maybe[Text] = Unset): Expansion =
        Expansion(i, name, caseDef.show.show, caseDefSource(caseDef).plain, parameter)
    
    case class Expansion(lines: List[Boolean], name: Text, code: Text, source: Text, parameter: Maybe[Text]):
      def heading: AnsiText = if lines.isEmpty then ansi"▪ ${Srgb(0.2, 0.8, 0.1)}($name)" else
        val end = if lines.head then t"└─" else t"├─"
        ansi"${lines.tail.reverse.map { l => if l then t"  " else t"│ " }.join}${end}▪ $name"
      
      def param: AnsiText = parameter match
        case txt: Text => txt.ansi
        case unset: Unset.type => ansi""

    def expandType(i: List[Boolean], tt: TypeTree): List[Expansion] = tt match
      case _ =>
        Expansion.ofType(i, t"Type", tt, tt.show.show) :: Nil

    def expandCaseDef(i: List[Boolean], caseDef: CaseDef): List[Expansion] = caseDef match
      case CaseDef(_, t1, t2) =>
        Expansion.ofCaseDef(i, t"CaseDef", caseDef) :: t1.to(List).flatMap(expandTerm(false :: i, _)) ::: expandTerm(true :: i, t2)

    def expandTerm(i: List[Boolean], term: Term): List[Expansion] = term match
      case Inlined(_, _, child) =>
        Expansion(i, t"Inlined", term) :: expandTerm(true :: i, child)
      case Apply(focus, children) =>
        Expansion(i, t"Apply", term) :: expandTerm(children.isEmpty :: i, focus) ::: children.zipWithIndex.flatMap: (ch, idx) =>
          expandTerm((idx == children.length - 1) :: i, ch)
      case TypeApply(focus, children) =>
        Expansion(i, t"TypeApply", term) :: expandTerm(children.isEmpty :: i, focus) ::: children.zipWithIndex.flatMap: (ch, idx) =>
          expandType((idx == children.length - 1) :: i, ch)
      case Select(focus, name) =>
        Expansion(i, t"Select", term, name.show) :: expandTerm(true :: i, focus)
      case Ident(name) =>
        Expansion(i, t"Ident", term, name.show) :: Nil
      case Typed(focus, tt) =>
        Expansion(i, t"Typed", term) :: expandTerm(false :: i, focus) ::: expandType(true :: i, tt)
      case Block(statements, focus) =>
        Expansion(i, t"Block", term) :: expandTerm(true :: i, focus)
      case Closure(focus, _) =>
        Expansion(i, t"Closure", term) :: expandTerm(true :: i, focus)
      case Literal(value) =>
        Expansion(i, t"Literal", term, value.show.show) :: Nil
      case Bind(name, tree) =>
        Expansion(i, t"Bind", term, name.show) :: Nil
      case Match(focus, cases) =>
        Expansion(i, t"Match", term) :: cases.zipWithIndex.flatMap: (ch, idx) =>
          expandCaseDef((idx == cases.length - 1) :: i, ch)
      case dotc.ast.Trees.SeqLiteral(elems, _) =>
        Expansion(i, t"SeqLiteral", term) :: Nil
      case _ =>
        Expansion(i, t"?${term.toString}: ${term.getClass.toString}", term, source(term).plain) :: Nil


    Table[Expansion](
      Column(ansi"TASTy")(_.heading.ansi),
      Column(ansi"Param")(_.param),
      Column(ansi"Code")(_.code.upto(_ != '\n')),
      Column(ansi"Source")(_.source)
    ).tabulate(expandTerm(Nil, expr.asTerm), 200, DelimitRows.None).join(ansi"${'\n'}").render

