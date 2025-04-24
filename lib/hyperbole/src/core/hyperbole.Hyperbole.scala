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
package hyperbole

import scala.collection.mutable as scm

import anthology.*
import anticipation.*
import contingency.*
import dendrology.*
import denominative.*
import escapade.*
import escritoire.*, tableStyles.minimal, columnAttenuation.ignore
import galilei.*
import gossamer.*
import harlequin.*
import hellenism.*
import hieroglyph.*, textMetrics.uniform
import inimitable.*
import iridescence.*
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import symbolism.*
import vacuous.*

import scala.quoted.*
import dotty.tools.*, dotc.util as dtdu

import syntaxHighlighting.teletypeable

object Hyperbole:
  def introspection[value](value: Expr[value])(using Quotes): Expr[Text] =
    Expr(introspect(value).render(termcapDefinitions.xterm256))

  def introspect[value](expr: Expr[value])(using Quotes): Teletype =
    import quotes.reflect.*

    def init = expr.asTerm.pos.startColumn

    val sources: scm.HashMap[Text, SourceCode] = scm.HashMap()

    def source(tree: Tree): Teletype = tree.pos match
      case pos: dtdu.SourcePosition =>
        val content: Teletype =
          val sourceCode = sources.establish(pos.source.toString.tt):
            val text = Scala.highlight(new String(pos.source.content()).tt)
            text

          val lineContent: Teletype = sourceCode.lines(pos.line).map(_.teletype).join

          try
            lineContent.segment(pos.startColumn.z ~ Ordinal.natural(pos.endColumn))
          catch case e: Exception => e""

        ((e" "*(pos.startColumn - init))+content)

      case _ =>
        e""

    case class TastyTree
                (name:         Text,
                 expr:         Text,
                 source:       Teletype,
                 nodes:        List[TastyTree],
                 param:        Optional[Text],
                 term:         Boolean,
                 definitional: Boolean):

      def shortCode: Text =
        val c = expr.upto(_ != '\n')
        if c.length != expr.length then t"$c..." else expr

      def children(nodes2: Tree*): TastyTree =
        copy(nodes = nodes ::: nodes2.to(List).map(TastyTree.expand))

      def typeNode: TastyTree = copy(term = false)
      def definition: TastyTree = copy(definitional = true)


    object TastyTree:
      def apply
           (name: Text,
            tree: Tree,
            children: List[TastyTree] = Nil,
            parameter: Optional[Text] = Unset)
      :     TastyTree =

        TastyTree(name, tree.show.tt, source(tree), children, parameter, true, false)

      def expand(tree: Tree): TastyTree = tree match
        case PackageClause(ref, chs) =>
          TastyTree(t"PackageClause", tree, List(expand(ref)))
          . children(chs*)
          . definition

        case Import(expr, selectors) =>
          TastyTree(t"Import", tree)

        case Export(tree, selectors) =>
          TastyTree(t"Export", tree)

        case ClassDef(name, constructor, parents, selfOpt, body) =>
          TastyTree(t"ClassDef", tree, Nil, name.tt)
          . children(body*)
          . definition

        case TypeDef(name, rhs) =>
          TastyTree(t"TypeDef", tree, Nil, name.tt)
          . children(rhs)
          . typeNode
          . definition

        case Wildcard() =>
          TastyTree(t"Wildcard", tree)

        case This(qual) =>
          TastyTree(t"This", tree, Nil, qual.map(_.tt).getOrElse(t""))

        case New(tpt) =>
          TastyTree(t"New", tree, Nil)
          . children(tpt)

        case NamedArg(name, arg) =>
          TastyTree(t"NamedArg", tree, List(expand(arg)), name.tt)

        case Bind(name, term) =>
          TastyTree(t"Bind", tree, List(expand(term)), name.tt)

        case Typed(expr, tpt) =>
          TastyTree(t"Typed", tree, List(expand(expr), expand(tpt)))

        case TypedOrTest(focus, tpt) =>
          TastyTree(t"TypedOrTest", tree, List(expand(focus), expand(tpt)))

        case Inlined(call, bindings, child) =>
          TastyTree
           (t"Inlined",
            tree,
            call.map(expand).to(List) ::: bindings.map(expand))
          . children(child)

        case Apply(fun, args) =>
          TastyTree(t"Apply", tree, expand(fun) :: args.map(expand))

        case Assign(lhs, rhs) =>
          TastyTree(t"Assign", tree, List(expand(lhs), expand(rhs)))

        case TypeApply(fun, args) =>
          TastyTree(t"TypeApply", tree, expand(fun) :: args.map(expand))

        case Select(qualifier, name) =>
          TastyTree(t"Select", tree, List(expand(qualifier)), name.tt)

        case SelectOuter(qualifier, name, levels) =>
          TastyTree(t"SelectOuter", tree, List(expand(qualifier)), t"$name^$levels")

        case Singleton(ref) =>
          TastyTree(t"Singleton", tree, List(expand(ref)))
          . typeNode

        case Super(qual, mix) =>
          TastyTree(t"Super", tree, List(expand(qual)), mix.map(_.tt).getOrElse(t""))

        case Ident(name) =>
          TastyTree(t"Ident", tree, Nil, name.tt)

        case If(cond, thenp, elsep) =>
          TastyTree(t"If", tree, List(cond, thenp, elsep).map(expand))

        case While(cond, body) =>
          TastyTree(t"While", tree, List(cond, body).map(expand))

        case TypeIdent(name) =>
          TastyTree(t"TypeIdent", tree, Nil, name.tt)
          . typeNode

        case TypeProjection(qualifier, name) =>
          TastyTree(t"TypeProjection", tree, List(expand(qualifier)), name.tt)
          . typeNode

        case Inferred() =>
          TastyTree(t"Inferred", tree, Nil)
          . typeNode

        case TypeSelect(term, name) =>
          TastyTree(t"TypeIdent", tree, List(expand(term)), name.tt)
          . typeNode

        case Try(expr, cases, finalizer) =>
          TastyTree
           (t"Try",
            tree,
            expand(expr) :: cases.map(expand) ::: finalizer.map(expand).to(List))

        case Block(statements, last) =>
          TastyTree(t"Block", tree, statements.map(expand) :+ expand(last))

        case ByName(result) =>
          TastyTree(t"ByName", tree, List(expand(result)))

        case Closure(focus, tpe) =>
          TastyTree(t"Closure", tree, List(expand(focus)))

        case Literal(value) =>
          TastyTree(t"Literal", tree, Nil, value.show.tt)

        case Lambda(defs, term) =>
          TastyTree(t"Lambda", tree, defs.map(expand) ::: List(expand(term)))

        case LambdaTypeTree(tparams, body) =>
          TastyTree(t"LambdaTypeTree", tree, tparams.map(expand) ::: List(expand(body)))
          . typeNode

        case TypeBoundsTree(low, high) =>
          TastyTree(t"TypeBoundsTree", tree, List(low, high).map(expand))
          . typeNode

        case WildcardTypeTree() =>
          TastyTree(t"WildcardTypeTree", tree, Nil)
          . typeNode

        case TypeBind(name, tpt) =>
          TastyTree(t"TypeBind", tree, List(expand(tpt)), name.tt)

        case TypeBlock(aliases, tpt) =>
          TastyTree(t"TypeBlock", tree, aliases.map(expand) ::: List(expand(tpt)))
          . typeNode

        case Match(selector, cases) =>
          TastyTree(t"Match", tree, expand(selector) :: cases.map(expand))

        case MatchTypeTree(bound, selector, cases) =>
          TastyTree
           (t"MatchTypeTree",
            tree,
            bound.map(expand).to(List) ++ List(expand(selector)) ++ cases.map(expand))
          . typeNode

        case Applied(tpt, args) =>
          TastyTree(t"Applied", tree, expand(tpt) :: args.map(expand))
          . typeNode

        case Annotated(arg, annotation) =>
          TastyTree(t"Annotated", tree, List(expand(arg), expand(annotation)))

        case Repeated(elems, tpt) =>
          TastyTree(t"Repeated", tree, elems.map(expand) ::: List(expand(tpt)))

        case Refined(tpt, refinements) =>
          TastyTree(t"Refined", tree, expand(tpt) :: refinements.map(expand))
          . typeNode

        case Return(expr, from) =>
          TastyTree(t"Return", tree, List(expand(expr)))

        case Unapply(fun, implicits, patterns) =>
          TastyTree(t"Unapply", tree, expand(fun) :: implicits.map(expand) ::: patterns.map(expand))

        case Alternatives(patterns) =>
          TastyTree(t"Alternatives", tree, patterns.map(expand))
          . typeNode

        case TypeCaseDef(pattern, rhs) =>
          TastyTree(t"TypeCaseDef", tree, List(expand(pattern), expand(rhs)))
          . typeNode

        case DefDef(name, paramss, tpt, rhs) =>
          TastyTree(t"DefDef", tree, rhs.to(List).map(expand))
          . definition

        case SummonFrom(cases) =>
          TastyTree(t"SummonFrom", tree, cases.map(expand))

        case ValDef(name, tpe, rhs) =>
          TastyTree(t"ValDef", tree, rhs.to(List).map(expand))
          . definition

        case CaseDef(pattern, guard, rhs) =>
          TastyTree(t"CaseDef", tree, expand(pattern) +: guard.to(List).map(expand) :+ expand(rhs))

        // case TermParamClause(params) =>
        //   TastyTree(t"TermParamClause", tree, params.map(expand))

        // case TypeParamClause(params) =>
        //   TastyTree(t"TypeParamClause", tree, params.map(expand))

        case _ =>
          TastyTree(t"?${tree.toString}: ${tree.getClass.toString}", tree, Nil)

    val tree: TastyTree = TastyTree.expand(expr.asTerm)

    val seq: Seq[Expansion] = TreeDiagram.by[TastyTree](_.nodes)(tree).map: node =>
      val color = (node.term, node.definitional) match
        case (true, true)   => webColors.Tomato
        case (false, true)  => webColors.YellowGreen
        case (true, false)  => webColors.FireBrick
        case (false, false) => webColors.MediumSeaGreen

      val text = e"$color(${node.name})"
      Expansion
       (e"${tiles.drop(1).map(treeStyles.default.text(_)).join+t"⦾  "}$text",
        node.param,
        node.shortCode,
        node.source)

    Table[Expansion]
     (Column(e"TASTy"): node =>
        val param = node.param.let { param => e"$Italic(${webColors.Orange}($param))" }.or(e"")
        e"${node.text} $param",
      Column(e"Source")(_.source),
      /*Column(e"Code")(_.expr)*/)

    . tabulate(seq)
    . grid(10000)
    . render
    . join(e"\n")
