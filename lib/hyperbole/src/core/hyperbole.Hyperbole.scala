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
import digression.*
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
                (tag:          Char,
                 typeName:     Text,
                 name:         Text,
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
        copy(nodes = nodes ::: nodes2.to(List).map(TastyTree.expand(' ', _)))

      def typed(nodes2: Tree*): TastyTree =
        copy(nodes = nodes ::: nodes2.to(List).map(TastyTree.expand('t', _)))

      def add(tag: Char, nodes2: Tree*): TastyTree =
        copy(nodes = nodes ::: nodes2.to(List).map(TastyTree.expand(tag,_)))

      def typeNode: TastyTree = copy(term = false)
      def definition: TastyTree = copy(definitional = true)


    object TastyTree:
      def apply
           (tag: Char, typeName: Text, name: Text, tree: Tree, parameter: Optional[Text] = Unset)
      :     TastyTree =
        TastyTree(tag, typeName, name, tree.show.tt, source(tree), Nil, parameter, true, false)

      def expand(tag: Char, tree: Tree): TastyTree =
        val typeName =
          safely:
            tree.asExpr match
              case '{ $term: tpe } => TypeRepr.of[tpe].show.tt
              case _               => Unset
          . or(t"")

        tree match
          case PackageClause(ref, chs) =>
            TastyTree(tag, typeName, t"PackageClause", tree)
            . add('r', ref)
            . children(chs*)
            . definition

          case Import(expr, selectors) =>
            TastyTree(tag, typeName, t"Import", tree)

          case Export(tree, selectors) =>
            TastyTree(tag, typeName, t"Export", tree)

          case ClassDef(name, constructor, parents, selfOpt, body) =>
            TastyTree(tag, typeName, t"ClassDef", tree, name.tt)
            . children(body*)
            . definition

          case TypeDef(name, rhs) =>
            TastyTree(tag, typeName, t"TypeDef", tree, name.tt)
            . children(rhs)
            . typeNode
            . definition

          case Wildcard() =>
            TastyTree(tag, typeName, t"Wildcard", tree)

          case This(qual) =>
            TastyTree(tag, typeName, t"This", tree, qual.map(_.tt).getOrElse(t""))

          case New(tpt) =>
            TastyTree(tag, typeName, t"New", tree)
            . children(tpt)

          case NamedArg(name, arg) =>
            TastyTree(tag, typeName, t"NamedArg", tree, name.tt)
            . add('a', arg)

          case Bind(name, term) =>
            TastyTree(tag, typeName, t"Bind", tree, name.tt)
            . children(term)

          case Typed(expr, tpt) =>
            TastyTree(tag, typeName, t"Typed", tree)
            . children(expr)
            . typed(tpt)

          case TypedOrTest(focus, tpt) =>
            TastyTree(tag, typeName, t"TypedOrTest", tree)
            . children(focus)
            . typed(tpt)

          case Inlined(call, bindings, child) =>
            TastyTree(tag, typeName, t"Inlined", tree)
            . add('c', call.to(List)*)
            . add('b', bindings*)
            . children(child)

          case Apply(fun, args) =>
            TastyTree(tag, typeName, t"Apply", tree)
            . children(fun)
            . add('a', args*)

          case Assign(lhs, rhs) =>
            TastyTree(tag, typeName, t"Assign", tree)
            . add('d', lhs)
            . children(rhs)

          case TypeApply(fun, args) =>
            TastyTree(tag, typeName, t"TypeApply", tree)
            . children(fun)
            . add('a', args*)

          case Select(qualifier, name) =>
            TastyTree(tag, typeName, t"Select", tree, name.tt)
            . children(qualifier)

          case SelectOuter(qualifier, name, levels) =>
            TastyTree(tag, typeName, t"SelectOuter", tree, t"$name^$levels")
            . children(qualifier)

          case Singleton(ref) =>
            TastyTree(tag, typeName, t"Singleton", tree)
            . typeNode
            . children(ref)

          case Super(qual, mix) =>
            TastyTree(tag, typeName, t"Super", tree, mix.map(_.tt).getOrElse(t""))
            . children(qual)

          case Ident(name) =>
            TastyTree(tag, typeName, t"Ident", tree, name.tt)

          case If(cond, thenp, elsep) =>
            TastyTree(tag, typeName, t"If", tree)
            . add('p', cond)
            . add('t', thenp)
            . add('f', elsep)

          case While(cond, body) =>
            TastyTree(tag, typeName, t"While", tree)
            . add('p', cond)
            . children(body)

          case TypeIdent(name) =>
            TastyTree(tag, typeName, t"TypeIdent", tree, name.tt)
            . typeNode

          case TypeProjection(qualifier, name) =>
            TastyTree(tag, typeName, t"TypeProjection", tree, name.tt)
            . typeNode
            . children(qualifier)

          case Inferred() =>
            TastyTree(tag, typeName, t"Inferred", tree)
            . typeNode

          case TypeSelect(term, name) =>
            TastyTree(tag, typeName, t"TypeIdent", tree, name.tt)
            . typeNode
            . children(term)

          case Try(expr, cases, finalizer) =>
            TastyTree(tag, typeName, t"Try", tree)
             . add('t', expr)
             . add('c', cases*)
             . add('f', finalizer.to(List)*)

          case Block(statements, last) =>
            TastyTree(tag, typeName, t"Block", tree)
            . children(statements*)
            . add('r', last)

          case ByName(result) =>
            TastyTree(tag, typeName, t"ByName", tree)
            . children(result)

          case Closure(focus, tpe) =>
            TastyTree(tag, typeName, t"Closure", tree)
            . children(focus)

          case Literal(value) =>
            TastyTree(tag, typeName, t"Literal", tree, value.show.tt)

          case Lambda(defs, term) =>
            TastyTree(tag, typeName, t"Lambda", tree)
            . add('a', defs*)
            . children(term)

          case LambdaTypeTree(tparams, body) =>
            TastyTree(tag, typeName, t"LambdaTypeTree", tree)
            . add('a', tparams*)
            . children(body)
            . typeNode

          case TypeBoundsTree(low, high) =>
            TastyTree(tag, typeName, t"TypeBoundsTree", tree)
            . add('l', low)
            . add('h', high)
            . typeNode

          case WildcardTypeTree() =>
            TastyTree(tag, typeName, t"WildcardTypeTree", tree)
            . typeNode

          case TypeBind(name, tpt) =>
            TastyTree(tag, typeName, t"TypeBind", tree, name.tt)
            . typed(tpt)

          case TypeBlock(aliases, tpt) =>
            TastyTree(tag, typeName, t"TypeBlock", tree)
            . add('a', aliases*)
            . typed(tpt)
            . typeNode

          case Match(selector, cases) =>
            TastyTree(tag, typeName, t"Match", tree)
            . add('s', selector)
            . add('c', cases*)

          case MatchTypeTree(bound, selector, cases) =>
            TastyTree(tag, typeName, t"MatchTypeTree", tree)
            . add('b', bound.to(List)*)
            . add('s', selector)
            . add('c', cases*)
            . typeNode

          case Applied(tpt, args) =>
            TastyTree(tag, typeName, t"Applied", tree)
            . typed(tpt)
            . add('a', args*)
            . typeNode

          case Annotated(arg, annotation) =>
            TastyTree(tag, typeName, t"Annotated", tree)
            . add('a', arg)
            . children(annotation)

          case Repeated(elems, tpt) =>
            TastyTree(tag, typeName, t"Repeated", tree)
            . children(elems*)
            . typed(tpt)

          case Refined(tpt, refinements) =>
            TastyTree(tag, typeName, t"Refined", tree)
            . typed(tpt)
            . add('m', refinements*)
            . typeNode

          case Return(expr, from) =>
            TastyTree(tag, typeName, t"Return", tree)
            . children(expr)

          case Unapply(fun, implicits, patterns) =>
            TastyTree(tag, typeName, t"Unapply", tree)
            . children(fun)
            . add('i', implicits*)
            . add('p', patterns*)

          case Alternatives(patterns) =>
            TastyTree(tag, typeName, t"Alternatives", tree)
            . add('p', patterns*)
            . typeNode

          case TypeCaseDef(pattern, rhs) =>
            TastyTree(tag, typeName, t"TypeCaseDef", tree)
            . add('p', pattern)
            . children(rhs)
            . typeNode

          case DefDef(name, paramss, tpt, rhs) =>
            val clauses = paramss.map:
             case TermParamClause(params) =>
               TastyTree(tag, typeName, t"TermParamClause", tree).add('a', params*)

             case TypeParamClause(params) =>
               TastyTree(tag, typeName, t"TypeParamClause", tree).add('a', params*)

            TastyTree(tag, typeName, t"DefDef", tree, name.tt)
            . copy(nodes = clauses)
            . typed(tpt)
            . children(rhs.to(List)*)
            . definition

          case SummonFrom(cases) =>
            TastyTree(tag, typeName, t"SummonFrom", tree)
            . add('c', cases*)

          case ValDef(name, tpt, rhs) =>
            TastyTree(tag, typeName, t"ValDef", tree, name.tt)
            . typed(tpt)
            . children(rhs.to(List)*)
            . definition

          case CaseDef(pattern, guard, rhs) =>
            TastyTree(tag, typeName, t"CaseDef", tree)
            . add('p', pattern)
            . add('g', guard.to(List)*)
            . children(rhs)

          case _ =>
            TastyTree(tag, typeName, t"?${tree.toString}: ${tree.getClass.toString}", tree)

    val tree: TastyTree = TastyTree.expand(' ', expr.asTerm)

    val seq: Seq[Expansion] = TreeDiagram.by[TastyTree](_.nodes)(tree).map: node =>
      val color = (node.term, node.definitional) match
        case (true, true)   => webColors.Tomato
        case (false, true)  => webColors.YellowGreen
        case (true, false)  => webColors.FireBrick
        case (false, false) => webColors.MediumSeaGreen

      val text = e"$color(${node.name})"
      val tag2: Char = if node.tag == ' ' then '▪' else (node.tag+'ⓐ' - 'a').toChar

      Expansion
       (e"${tiles.drop(1).map(treeStyles.default.text(_)).join}$tag2 $text",
        node.typeName,
        node.param,
        node.shortCode,
        node.source)

    Table[Expansion]
     (Column(e"TASTy"): node =>
        val param = node.param.let { param => e"$Italic(${webColors.Orange}($param))" }.or(e"")
        e"${node.text} $param",
      Column(e"Type"): node =>
        val name = StackTrace.rewrite(node.typeName.s, false)
        if node.typeName.empty then e"" else e"${webColors.Gray}(: $Italic(${name}))",
      Column(e"Source")(_.source))

    . tabulate(seq)
    . grid(10000)
    . render
    . join(e"\n")
