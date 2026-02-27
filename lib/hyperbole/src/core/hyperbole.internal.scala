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
┃    Soundness, version 0.54.0.                                                                    ┃
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
import scala.quoted.*

import anticipation.*
import contingency.*
import denominative.*
import escapade.*
import fulminate.*
import gigantism.*
import gossamer.*
import harlequin.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

import dotty.tools.*, dotc.util as dtdu

import syntaxHighlighting.teletypeable

object internal:
  def introspection[value](value: Expr[value], inlining: Expr[Boolean]): Macro[TastyTree] =
    def serialize(tree: TastyTree): Expr[TastyTree] = tree match
      case TastyTree(tag, typeName, name, expr, source, nodes, param, term, definitional) =>
        val nodes2 = nodes.map(serialize(_))
        val param2 = if param.absent then '{Unset} else Expr(param.vouch)

        ' {
            TastyTree
              ( ${Expr(tag)},
                ${Expr(typeName)},
                ${Expr(name)},
                ${Expr(expr)},
                ${Expr(source)},
                ${Expr.ofList(nodes2)},
                $param2.asInstanceOf[Optional[Text]],
                ${Expr(term)},
                ${Expr(definitional)}
              )
          }

    serialize(tastyTree[value](value, inlining))

  def tastyTree[value](expr: Expr[value], inlining0: Expr[Boolean])(using Quotes): TastyTree =
    import quotes.reflect.*

    val inlining = inlining0.valueOrAbort
    val sources: scm.HashMap[Text, SourceCode] = scm.HashMap()

    def source(tree: Tree): Teletype = tree.pos match
      case position: dtdu.SourcePosition =>
        val init: Ordinal = position.lineContent.tt.where(_ != ' ').or(Prim)
        val content: Teletype =
          val sourceCode = sources.establish(position.source.toString.tt):
            val text = Scala.highlight(new String(position.source.content()).tt)
            text

          val lineContent: Teletype = sourceCode.lines(position.line).map(_.teletype).join

          try lineContent.segment(position.startColumn.z thru position.endColumn.u)
          catch case e: Exception => e""

        ((e" "*(position.startColumn - init.n0))+content)

      case _ =>
        e""

    extension (tastyTree: TastyTree)
      def children(nodes2: Tree*): TastyTree =
        tastyTree.copy(nodes = tastyTree.nodes ::: nodes2.to(List).map(TastyTree.expand(' ', _)))

      def typeChildren(nodes2: TypeRepr*): TastyTree =
        tastyTree.copy(nodes = tastyTree.nodes ::: nodes2.to(List).map(TastyTree.expandType(_)))

      def typed(nodes2: Tree*): TastyTree =
        tastyTree.copy(nodes = tastyTree.nodes ::: nodes2.to(List).map(TastyTree.expand('t', _)))

      def add(tag: Char, nodes2: Tree*): TastyTree =
        tastyTree.copy(nodes = tastyTree.nodes ::: nodes2.to(List).map(TastyTree.expand(tag,_)))



    object TastyTree:
      def apply
        ( tag:       Char,
          typeName:  Text,
          name:      Text,
          tree:      Optional[Tree],
          repr:      Optional[Text] = Unset,
          parameter: Optional[Text] = Unset )
      :   TastyTree =

        val shown = tree.let(_.show.tt).or(repr.let(_.show)).or(t"")
        val code = tree.let(source(_)).or(e"")

        new TastyTree(tag, typeName, name, shown, code.plain, Nil, parameter, true, false)


      def repr(name: Text, repr: Optional[TypeRepr], parameter: Optional[Text] = Unset): TastyTree =
        apply(' ', t"", name, Unset, repr.let(stenography.internal.name(_)), parameter).typeNode

      def expandType(repr: TypeRepr): TastyTree =
        repr match
          case TypeRef(ref, name) => TastyTree.repr(t"TypeRef", repr, name)

          case AndType(left, right) =>
            TastyTree.repr(t"AndType", repr).typeChildren(left, right)

          case OrType(left, right) =>
            TastyTree.repr(t"OrType", repr).typeChildren(left, right)

          case AppliedType(tycon, arguments) =>
            TastyTree.repr(t"AppliedType", repr).typeChildren(tycon :: arguments*)

          case AnnotatedType(underlying, term) =>
            TastyTree.repr(t"AnnotatedType", repr).typeChildren(underlying).children(term)

          case Refinement(parent, name, info) =>
            TastyTree.repr(t"Refinement", repr, name)
            . typeChildren(parent, info)

          case MatchType(bound, scrutinee, cases) =>
            TastyTree.repr(t"MatchType", repr).typeChildren(bound :: scrutinee :: cases*)

          case SuperType(thisType, superType) =>
            TastyTree.repr(t"SuperType", repr).typeChildren(thisType, superType)

          case ByNameType(byName) =>
            TastyTree.repr(t"ByNameType", repr).typeChildren(byName)

          case ParamRef(lambdaType, n) =>
            TastyTree.repr(t"ParamRef", repr, n.show).typeChildren(lambdaType)

          case ThisType(thisType) =>
            TastyTree.repr(t"ThisType", repr).typeChildren(thisType)

          case RecursiveThis(thisType) =>
            TastyTree.repr(t"RecursiveThis", repr).typeChildren(thisType)

          case RecursiveType(recursive) =>
            TastyTree.repr(t"RecursiveType", repr).typeChildren(recursive)

          case MethodType(names, params, result) =>
            TastyTree.repr(t"MethodType", repr, names.map(_.tt).join(t", "))
            . typeChildren(params :+ result*)

          case PolyType(names, bounds, result) =>
            TastyTree.repr(t"PolyType", repr, names.map(_.tt).join(t", "))
            . typeChildren(bounds :+ result*)

          case TypeLambda(names, bounds, result) =>
            TastyTree.repr(t"TypeLambda", repr, names.map(_.tt).join(t", "))
            . typeChildren(bounds :+ result*)

          case MatchCase(pattern, rhs) =>
            TastyTree.repr(t"MatchCase", repr).typeChildren(pattern, rhs)

          case TypeBounds(low, high) =>
            TastyTree.repr(t"TypeBounds", repr).typeChildren(low, high)

          case NoPrefix() =>
            TastyTree.repr(t"NoPrefix", repr)

          case FlexibleType(tpe) =>
            TastyTree.repr("FlexibleType", repr).typeChildren(tpe)

          case constant: ConstantType =>
            def value = constant.constant.value.toString.tt

            constant.constant.absolve match
              case BooleanConstant(_) => TastyTree.repr(t"BooleanConstant", repr, value)
              case ByteConstant(_)    => TastyTree.repr(t"ByteConstant", repr, value)
              case ShortConstant(_)   => TastyTree.repr(t"ShortConstant", repr, value)
              case IntConstant(_)     => TastyTree.repr(t"IntConstant", repr, value)
              case LongConstant(_)    => TastyTree.repr(t"LongConstant", repr, value)
              case FloatConstant(_)   => TastyTree.repr(t"FloatConstant", repr, value)
              case DoubleConstant(_)  => TastyTree.repr(t"DoubleConstant", repr, value)
              case CharConstant(_)    => TastyTree.repr(t"CharConstant", repr, value)
              case StringConstant(_)  => TastyTree.repr(t"StringConstant", repr, value)
              case UnitConstant()     => TastyTree.repr(t"UnitConstant", repr, t"()")
              case NullConstant()     => TastyTree.repr(t"NullConstant", repr, t"null")
              case ClassOfConstant(_) => TastyTree.repr(t"ClassOfConstant", repr, value)

          case TermRef(qual, name) =>
            TastyTree.repr("TermRef", repr, name.tt).typeChildren(qual)

          case _ =>
            TastyTree.repr(t"unknown", repr)

      def expand(tag: Char, tree: Tree): TastyTree =
        val typeName =
          safely:
            tree.asExpr match
              case '{$term: tpe} => stenography.internal.name[tpe]
              case _             => Unset

          . or(t"")

        tree match
          case typeTree: TypeTree => expandType(typeTree.tpe)

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
            TastyTree(tag, typeName, t"ClassDef", tree, parameter = name.tt)
            . children(body*)
            . definition

          case TypeDef(name, rhs) =>
            TastyTree(tag, typeName, t"TypeDef", tree, parameter = name.tt)
            . children(rhs)
            . typeNode
            . definition

          case Wildcard() =>
            TastyTree(tag, typeName, t"Wildcard", tree)

          case This(qual) =>
            TastyTree(tag, typeName, t"This", tree, parameter = qual.map(_.tt).getOrElse(t""))

          case New(tpt) =>
            val typeName = stenography.internal.name(tpt.tpe)
            TastyTree(tag, typeName, t"New", tree)
            . children(tpt)

          case NamedArg(name, argument) =>
            TastyTree(tag, typeName, t"NamedArg", tree, parameter = name.tt)
            . add('a', argument)

          case Bind(name, term) =>
            TastyTree(tag, typeName, t"Bind", tree, parameter = name.tt)
            . children(term)

          case Typed(expr, tpt) =>
            val typeName = stenography.internal.name(tpt.tpe)
            TastyTree(tag, typeName, t"Typed", tree)
            . children(expr)
            . typed(tpt)

          case TypedOrTest(focus, tpt) =>
            val typeName = stenography.internal.name(tpt.tpe)
            TastyTree(tag, typeName, t"TypedOrTest", tree)
            . children(focus)
            . typed(tpt)

          case Inlined(call, bindings, child) =>
            if inlining then expand(tag, child) else
              TastyTree(tag, typeName, t"Inlined", tree)
              . add('c', call.to(List)*)
              . add('b', bindings*)
              . children(child)

          case Apply(fun, arguments) =>
            TastyTree(tag, typeName, t"Apply", tree)
            . children(fun)
            . add('a', arguments*)

          case Assign(lhs, rhs) =>
            TastyTree(tag, typeName, t"Assign", tree)
            . add('d', lhs)
            . children(rhs)

          case TypeApply(fun, arguments) =>
            TastyTree(tag, typeName, t"TypeApply", tree)
            . children(fun)
            . add('a', arguments*)

          case Select(qualifier, name) =>
            TastyTree(tag, typeName, t"Select", tree, parameter = name.tt)
            . children(qualifier)

          case SelectOuter(qualifier, name, levels) =>
            TastyTree(tag, typeName, t"SelectOuter", tree, parameter = t"$name^$levels")
            . children(qualifier)

          case Singleton(ref) =>
            TastyTree(tag, typeName, t"Singleton", tree)
            . typeNode
            . children(ref)

          case Super(qual, mix) =>
            TastyTree(tag, typeName, t"Super", tree, parameter = mix.map(_.tt).getOrElse(t""))
            . children(qual)

          case Ident(name) =>
            TastyTree(tag, typeName, t"Ident", tree, parameter = name.tt)

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
            TastyTree(tag, typeName, t"TypeIdent", tree, parameter = name.tt)
            . typeNode

          case TypeProjection(qualifier, name) =>
            TastyTree(tag, typeName, t"TypeProjection", tree, parameter = name.tt)
            . typeNode
            . children(qualifier)

          case TypeSelect(term, name) =>
            TastyTree(tag, typeName, t"TypeIdent", tree, parameter = name.tt)
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
            TastyTree(tag, typeName, t"Literal", tree, parameter = value.show.tt)

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
            val typeName = tpt.show.tt
            TastyTree(tag, typeName, t"TypeBind", tree, parameter = name.tt)
            . typed(tpt)

          case TypeBlock(aliases, tpt) =>
            val typeName = stenography.internal.name(tpt.tpe)
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

          case Applied(tpt, arguments) =>
            val typeName = stenography.internal.name(tpt.tpe)
            TastyTree(tag, typeName, t"Applied", tree)
            . typed(tpt)
            . add('a', arguments*)
            . typeNode

          case Annotated(argument, annotation) =>
            TastyTree(tag, typeName, t"Annotated", tree)
            . add('a', argument)
            . children(annotation)

          case Repeated(elems, tpt) =>
            val typeName = stenography.internal.name(tpt.tpe)
            TastyTree(tag, typeName, t"Repeated", tree)
            . children(elems*)
            . typed(tpt)

          case Refined(tpt, refinements) =>
            val typeName = stenography.internal.name(tpt.tpe)
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
            val typeName = stenography.internal.name(tpt.tpe)
            val clauses = paramss.map:
              case TermParamClause(params) =>
                TastyTree('a', typeName, t"TermParamClause", tree).add('a', params*)

              case TypeParamClause(params) =>
                TastyTree('t', typeName, t"TypeParamClause", tree).add('a', params*)

              case clause =>
                panic(m"unexpected parameter clause: ${clause.toString}")

            TastyTree(tag, typeName, t"DefDef", tree, parameter = name.tt)
            . copy(nodes = clauses)
            . typed(tpt)
            . children(rhs.to(List)*)
            . definition

          case SummonFrom(cases) =>
            TastyTree(tag, typeName, t"SummonFrom", tree)
            . add('c', cases*)

          case ValDef(name, tpt, rhs) =>
            val typeName = stenography.internal.name(tpt.tpe)
            TastyTree(tag, typeName, t"ValDef", tree, parameter = name.tt)
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

    TastyTree.expand(' ', expr.asTerm)

  def semantics[meta: Type]: Macro[TastySymbol] =
    import quotes.reflect.*
    introspect(TypeRepr.of[meta].dealias.typeSymbol)

  def semantics(): Macro[TastySymbol] =
    import quotes.reflect.*
    introspect(Symbol.spliceOwner)

  def semantics(expr: Expr[Any]): Macro[TastySymbol] =
    import quotes.reflect.*

    introspect:
      expr.asTerm match
        case Inlined(_, _, value) => value.symbol
        case other                => other.symbol


  def introspect(using Quotes)(symbol: quotes.reflect.Symbol): Expr[TastySymbol] =
    serialize(tastySymbol(symbol))

  def tastySymbol(using Quotes)(symbol: quotes.reflect.Symbol): TastySymbol =
    import quotes.reflect.*

    def render(term: Term): Text = term match
      case Select(New(tree), right) => stenography.internal.name(tree.tpe)
      case Select(left, right)      => t"${render(left)}.$right"
      case Apply(left, params)      => t"${render(left)}(${params.map(render(_)).join(t", ")})"
      case New(tree)                => stenography.internal.name(tree.tpe).show

      case TypeApply(subject, params) =>
        val params2 = params.map(_.tpe).map(stenography.internal.name(_)).join(t", ")
        t"${render(subject)}[$params2]"

      case _ =>
        term.show.unless(term.show.length > 12).or("...").tt

    def annotation(term: Term): Teletype = e"@${render(term)}"

    val flags: List[(Text, Boolean)] =
      List
        ( t"abstract"          -> symbol.flags.is(Flags.Abstract),
          t"abs-override"      -> symbol.flags.is(Flags.AbsOverride),
          t"artifact"          -> symbol.flags.is(Flags.Artifact),
          t"case"              -> symbol.flags.is(Flags.Case),
          t"case-accessor"     -> symbol.flags.is(Flags.CaseAccessor),
          t"contravariant"     -> symbol.flags.is(Flags.Contravariant),
          t"covariant"         -> symbol.flags.is(Flags.Covariant),
          t"deferred"          -> symbol.flags.is(Flags.Deferred),
          t"empty-flags"       -> symbol.flags.is(Flags.EmptyFlags),
          t"enum"              -> symbol.flags.is(Flags.Enum),
          t"erased"            -> symbol.flags.is(Flags.Erased),
          t"exported"          -> symbol.flags.is(Flags.Exported),
          t"extension-method"  -> symbol.flags.is(Flags.ExtensionMethod),
          t"field-accessor"    -> symbol.flags.is(Flags.FieldAccessor),
          t"final"             -> symbol.flags.is(Flags.Final),
          t"given"             -> symbol.flags.is(Flags.Given),
          t"has-default"       -> symbol.flags.is(Flags.HasDefault),
          t"implicit"          -> symbol.flags.is(Flags.Implicit),
          t"infix"             -> symbol.flags.is(Flags.Infix),
          t"inline"            -> symbol.flags.is(Flags.Inline),
          t"invisible"         -> symbol.flags.is(Flags.Invisible),
          t"java-defined"      -> symbol.flags.is(Flags.JavaDefined),
          t"java-static"       -> symbol.flags.is(Flags.JavaStatic),
          t"java-annotation"   -> symbol.flags.is(Flags.JavaAnnotation),
          t"lazy"              -> symbol.flags.is(Flags.Lazy),
          t"local"             -> symbol.flags.is(Flags.Local),
          t"macro"             -> symbol.flags.is(Flags.Macro),
          t"method"            -> symbol.flags.is(Flags.Method),
          t"module"            -> symbol.flags.is(Flags.Module),
          t"mutable"           -> symbol.flags.is(Flags.Mutable),
          t"no-inits"          -> symbol.flags.is(Flags.NoInits),
          t"opaque"            -> symbol.flags.is(Flags.Opaque),
          t"open"              -> symbol.flags.is(Flags.Open),
          t"override"          -> symbol.flags.is(Flags.Override),
          t"package"           -> symbol.flags.is(Flags.Package),
          t"param"             -> symbol.flags.is(Flags.Param),
          t"param-accessor"    -> symbol.flags.is(Flags.ParamAccessor),
          t"private"           -> symbol.flags.is(Flags.Private),
          t"private-local"     -> symbol.flags.is(Flags.PrivateLocal),
          t"protected"         -> symbol.flags.is(Flags.Protected),
          t"scala2x"           -> symbol.flags.is(Flags.Scala2x),
          t"sealed"            -> symbol.flags.is(Flags.Sealed),
          t"stable-realizable" -> symbol.flags.is(Flags.StableRealizable),
          t"synthetic"         -> symbol.flags.is(Flags.Synthetic),
          t"trait"             -> symbol.flags.is(Flags.Trait),
          t"transparent"       -> symbol.flags.is(Flags.Transparent) )

    val properties: List[(Text, Boolean)] =
      List
        ( t"abstractType"        -> symbol.isAbstractType,
          t"aliasType"           -> symbol.isAliasType,
          t"anonymousClass"      -> symbol.isAnonymousClass,
          t"anonymousFunction"   -> symbol.isAnonymousFunction,
          t"bind"                -> symbol.isBind,
          t"classConstructor"    -> symbol.isClassConstructor,
          t"classDef"            -> symbol.isClassDef,
          t"defDef"              -> symbol.isDefDef,
          t"definedInCurrentRun" -> symbol.isDefinedInCurrentRun,
          t"exists"              -> symbol.exists,
          t"localDummy"          -> symbol.isLocalDummy,
          t"noSymbol"            -> symbol.isNoSymbol,
          t"packageDef"          -> symbol.isPackageDef,
          t"refinementClass"     -> symbol.isRefinementClass,
          t"superAccessor"       -> symbol.isSuperAccessor,
          t"term"                -> symbol.isTerm,
          t"type"                -> symbol.isType,
          t"typeDef"             -> symbol.isTypeDef,
          t"typeParam"           -> symbol.isTypeParam,
          t"valDef"              -> symbol.isValDef )

    val prefix = symbol.fullName.tt.skip(symbol.name.length, Rtl)

    def position: Text =
      symbol.pos.map: position =>
        if position.start == 0 then t"${position.sourceFile.toString}"
        else if position.start == position.end
        then t"${position.sourceFile.name}:${position.start}"
        else t"${position.sourceFile.name}:${position.start}-${position.end}"

      . getOrElse(t"")

    val details: List[(Text, Text | List[Text])] =
      List
        ( t"Owner"            -> symbol.owner.fullName.tt,
          t"Info"             -> symbol.info.show,
          t"Position"         -> position,
          t"Private within"   -> symbol.privateWithin.map(_.show).getOrElse(t""),
          t"Protected within" -> symbol.protectedWithin.map(_.show).getOrElse(t""),
          t"Documentation"    -> symbol.docstring.getOrElse("").tt,
          t"Annotations"      -> symbol.annotations.map(annotation(_).plain),
          t"Declared fields"  -> symbol.declaredFields.sortBy(_.name).map(_.name.tt),
          t"Field members"    -> symbol.fieldMembers.sortBy(_.name).map(_.name.tt),
          t"Declared methods" -> symbol.declaredMethods.sortBy(_.name).map(_.name.tt),
          t"Method members"   -> symbol.methodMembers.sortBy(_.name).map(_.name),
          t"Declared types"   -> symbol.declaredTypes.sortBy(_.name).map(_.name.tt),
          t"Type members"     -> symbol.typeMembers.sortBy(_.name).map(_.name.tt),
          t"Declarations"     -> symbol.declarations.sortBy(_.name).map(_.name.tt),
          t"Children"         -> symbol.children.sortBy(_.name).map(_.name.tt),

          t"Parameters"
          ->  symbol.paramSymss.map(_.map(_.name.tt).join(t"(", t" ", t")")),

          t"All overridden symbols"
          ->  symbol.allOverriddenSymbols.map(_.name.tt).to(List),

          t"Primary constructor"
          ->  symbol.primaryConstructor.name.tt,

          t"Case fields"
          ->  symbol.caseFields.map: field =>
                t"${field.name}: ${field.info.show}"
              . join(t"\n"),

          t"Signature"        -> symbol.signature.resultSig,

          t"Module class"
          -> (if symbol.moduleClass.exists then symbol.moduleClass.fullName else t""),

          t"Companion class"
          -> (if symbol.companionClass.exists then symbol.companionClass.fullName else t""),

          t"Companion module"
          -> (if symbol.companionModule.exists then symbol.companionModule.fullName else t"") )

    TastySymbol(prefix, symbol.name, flags, properties, details)


  def serialize(symbol: TastySymbol): Macro[TastySymbol] =
    val flags = symbol.flags.map: (key, value) => '{(${Expr(key)}, ${Expr(value)})}
    val properties = symbol.properties.map: (key, value) => '{(${Expr(key)}, ${Expr(value)})}

    val details =
      symbol.details.map: pair =>
        pair.absolve match
          case (key, text: Text) => '{(${Expr(key)}, ${Expr(text)})}

          case (key, list: List[Text] @unchecked) =>
            '{(${Expr(key)}, ${Expr.ofList(list.map(Expr(_)))})}

    ' {
        TastySymbol
          ( ${Expr(symbol.prefix)},
            ${Expr(symbol.name)},
            ${Expr.ofList(flags)},
            ${Expr.ofList(properties)},
            ${Expr.ofList(details)} )
      }
