/*
    Hyperbole, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package hyperbole

import escapade.*
import rudiments.*
import vacuous.*
import gossamer.*
import spectacular.*
import anticipation.*
import hieroglyph.*, textWidthCalculation.uniform
import escritoire.*, tableStyles.horizontalGaps
import dendrology.*

import scala.quoted.*
import dotty.tools.*, dotc.util as dtdu

object reflection:
  def expand[T](expr: Expr[T])(using Quotes): Text =
    import quotes.reflect.*
    import treeStyles.default

    def init = expr.asTerm.pos.startColumn
    
    def source(tree: Tree): Output = tree.pos match
      case pos: dtdu.SourcePosition =>
        ((t" "*(pos.startColumn - init))+pos.lineContent.show.slice(pos.startColumn, pos.endColumn)).display
      case _ =>
        e""

    case class TastyTree(name: Text, expr: Text, source: Text, children: List[TastyTree], param: Optional[Text]):
      def shortCode: Text =
        val c = expr.upto(_ != '\n')
        if c.length != expr.length then t"$c..." else expr

   
    object TastyTree:
      def apply(name: Text, tree: Tree, children: List[TastyTree], parameter: Optional[Text] = Unset): TastyTree =
        TastyTree(name, tree.show.show, source(tree).plain, children, parameter)
    
      def expand(tree: Tree): TastyTree = tree match
        case PackageClause(ref, chs)    => TastyTree(t"PackageClause", tree, expand(ref) :: chs.map(expand))
        case Bind(name, term)           => TastyTree(t"Bind", tree, List(expand(term)), name.show)
        case Typed(focus, tt)           => TastyTree(t"Typed", tree, List(expand(focus), expand(tt)))
        case CaseDef(focus, t1, t2)     => TastyTree(t"CaseDef", tree, expand(focus) +: t1.to(List).map(expand) :+ expand(t2))
        case Inlined(_, _, child)       => TastyTree(t"Inlined", tree, List(expand(child)))
        case Apply(focus, children)     => TastyTree(t"Apply", tree, expand(focus) :: children.map(expand))
        case TypeApply(focus, children) => TastyTree(t"TypeApply", tree, expand(focus) :: children.map(expand))
        case Select(focus, name)        => TastyTree(t"Select", tree, List(expand(focus)), name.show)
        case Ident(name)                => TastyTree(t"Ident", tree, Nil, name.show)
        case TypeIdent(name)            => TastyTree(t"TypeIdent", tree, Nil, name.show)
        case TypeSelect(term, name)     => TastyTree(t"TypeIdent", tree, List(expand(term)), name.show)
        case Block(statements, last)    => TastyTree(t"Block", tree, statements.map(expand) :+ expand(last))
        case Closure(focus, _)          => TastyTree(t"Closure", tree, List(expand(focus)))
        case Literal(value)             => TastyTree(t"Literal", tree, Nil, value.show.show)
        case Match(focus, cases)        => TastyTree(t"Match", tree, expand(focus) :: cases.map(expand))
        case Applied(name, tts)         => TastyTree(t"Applied", tree, expand(name) :: tts.map(expand))
        case Repeated(xs, _)            => TastyTree(t"Repeated", tree, Nil)
        case DefDef(name, ps, typs, ch) => TastyTree(t"DefDef", tree, ch.to(List).map(expand))
        case _                          => TastyTree(t"?${tree.toString}: ${tree.getClass.toString}", tree, Nil)

    val tree = TastyTree.expand(expr.asTerm)
    def exp(prefix: List[TreeTile], node: TastyTree) = Expansion(prefix.drop(1).map(_.show).join+t"▪ "+node.name, node.param, node.shortCode, node.source)
    val seq: Seq[Expansion] = drawTree[TastyTree, Expansion](_.children, exp)(List(tree))


    Table[Expansion](
      Column(e"TASTy")(_.text.display),
      Column(e"Param")(_.param.or(t"")),
      Column(e"Source")(_.source),
      Column(e"Code")(_.expr)
    ).tabulate(seq, 400, DelimitRows.None).join(e"\n").render


case class Expe(text: Text, param: Optional[Text], expr: Text, source: Text)

