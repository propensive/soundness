                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                               ╭───╮                              ┃
┃                                                               │   │                              ┃
┃                                                               │   │                              ┃
┃                     ╭─────────╮ ╭───╮╌────╮╌────╮ ╭─────────╮ │   │ ╭───╮                        ┃
┃                     ╰─────╮   │ │   ╭─╮   ╭─╮   │ │   ╭─╮   │ │   │╌╯   │                        ┃
┃                     ╭─────╯   │ │   │ │   │ │   │ │   │ │   │ │        ╌╯                        ┃
┃                     │   ╭─╮   │ │   │ │   │ │   │ │   │ │   │ │   ╭─╮   │                        ┃
┃                     │   ╰─╯   │ │   │ │   │ │   │ │   ╰─╯   │ │   │ │   │                        ┃
┃                     ╰─────────╯ ╰───╯ ╰───╯ ╰───╯ ╰─────────╯ ╰───╯ ╰───╯                        ┃
┃                                                                                                  ┃
┃    Amok, prerelease version                                                                      ┃
┃    © Copyright 2023-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://github.com/propensive/amok/                                                       ┃
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
package stenography

import scala.quoted.*
import scala.collection.mutable as scm
import scala.collection.immutable.ListMap

import anticipation.*
import gossamer.*
import proscenium.*
import rudiments.{is as _, *}
import spectacular.*
import turbulence.*
import vacuous.*

enum Syntax:
  case Simple(typename: Typename, term: Boolean)
  case Compound(precedence: Int, syntax: Syntax*)
  case Symbolic(text: Text)
  case Constant(text: Text)
  case Member(text: Text)
  case Refined(syntax: Syntax, members: ListMap[Text, Syntax])

object Syntax:
  val OpenParens = Syntax.Symbolic(t"\u200b(")
  val CloseParens = Syntax.Symbolic(t")\u200b")
  val OpenBracket = Syntax.Symbolic(t"\u200b[")
  val CloseBracket = Syntax.Symbolic(t"]\u200b")
  val Space = Syntax.Symbolic(t"\u00a0\u200b")
  val Intersect = Syntax.Symbolic(t"\u00a0\u200b&\u00a0")
  val Union = Syntax.Symbolic(t"\u00a0\u200b|\u00a0")
  val Comma = Syntax.Symbolic(t",\u200b\u00a0")
  val Colon = Syntax.Symbolic(t"\u200b:\u00a0")
  val Into = Syntax.Symbolic(t"\u200binto\u00a0")
  val ClassOf = Syntax.Symbolic(t"classOf\u200b[")
  val Arrow = Syntax.Symbolic(t"\u200b=>\u00a0")
  val FunctionArrow = Syntax.Symbolic(t"\u00a0\u200b=>\u00a0")
  val ContextualArrow = Syntax.Symbolic(t"\u00a0\u200b?=>\u00a0")
  val LambdaArrow = Syntax.Symbolic(t"\u00a0\u200b=>>\u00a0")
  val Qmark = Syntax.Symbolic(t"?")
  val Dot = Syntax.Symbolic(t"\u200b.")
  val Project = Syntax.Symbolic(t"#")
  val LowerBound = Syntax.Symbolic(t"\u00a0\u200b>:\u00a0")
  val UpperBound = Syntax.Symbolic(t"\u00a0\u200b<:\u00a0")
  val AndUpperBound = Syntax.Symbolic(t"\u00a0\u200b<:\u00a0")

  def apply(outer: Int, syntax: Syntax*): Syntax.Compound =
    val elements = syntax.flatMap:
      case Syntax.Compound(inner, elements*) =>
        if inner < outer then OpenParens +: elements :+ CloseParens else elements
      case refinement: Refined => Seq(OpenParens, refinement, CloseParens)
      case other                  => Seq(other)

    Syntax.Compound(outer, elements*)

  given showable: Scope => Syntax is Showable:
    def text(syntax: Syntax): Text = syntax match
      case Simple(typename, _)    => typename.text
      case Compound(_, syntaxes*) => syntaxes.map(text(_)).join
      case Symbolic(text)         => text
      case Member(text)           => text
      case Constant(text)         => text
      case Refined(base, members) => t"$base { ... }"

  val cache: scm.HashMap[Any, Syntax] = scm.HashMap()

  def sequence(elements: List[Syntax], separator: Syntax): Optional[Syntax] =
    if elements.isEmpty then Unset else
      Syntax
       (0, elements.flatMap: element =>
          List(element, separator)
        . dropRight(1)*)

  def precedence(char: Char): Int = char match
    case '!'  => 4
    case '%'  => 8
    case '&'  => 3
    case '*'  => 8
    case '+'  => 7
    case '-'  => 7
    case '/'  => 8
    case ':'  => 6
    case '<'  => 5
    case '='  => 4
    case '>'  => 5
    case '^'  => 2
    case '|'  => 1
    case char => if char.isLetter then 0 else 9

  def clause(using quotes: Quotes)(clause: quotes.reflect.ParamClause, showUsing: Boolean)(using Stdio): Syntax =
    import quotes.reflect.*
    clause match
      case TermParamClause(termDefs) =>
        val contextual = termDefs.exists(_.symbol.flags.is(Flags.Given))
        val defs = termDefs.flatMap:
          case valDef@ValDef(name, rtn, default) =>
            val syntax =
              if name.tt.starts(t"evidence$$") || name.tt.starts(t"x$$") then List(apply(rtn.tpe), Comma)
              else List(Syntax.Member(name.tt), Colon, apply(rtn.tpe), Comma)

            if valDef.symbol.flags.is(Flags.Inline)
            then Syntax.Member(t"inline\u00a0") :: syntax
            else syntax

        val usingKeyword = if contextual && showUsing then List(Syntax.Symbolic(t"using ")) else Nil
        Syntax(10, OpenParens +: (usingKeyword ++ defs.dropRight(1)) :+ CloseParens*)

      case TypeParamClause(typeDefs) =>
        val defs = typeDefs.flatMap:
          case typeDef@TypeDef(name, bounds) =>
            val flags = typeDef.symbol.flags

            val list =
              bounds match
                case TypeBoundsTree(lb, ub) =>
                  List(Syntax.typeBounds(Syntax.Member(name.tt), lb.tpe, ub.tpe), Comma)
                case LambdaTypeTree(typeDefs, other) =>
                  List(Syntax.Member(name.tt), Comma)
                case other =>
                  Out.println("Not type bounds: "+other)
                  List(Syntax.Member(name.tt), Comma)

            if flags.is(Flags.Covariant) then (Syntax.Symbolic(t"+") :: list)
            else if flags.is(Flags.Contravariant) then (Syntax.Symbolic(t"-") :: list)
            else list


        Syntax(10, OpenBracket +: defs.dropRight(1) :+ CloseBracket*)

  def typeBounds(using quotes: Quotes)(sub: Syntax, lb: quotes.reflect.TypeRepr, ub: quotes.reflect.TypeRepr)(using Stdio): Syntax =
    import quotes.reflect.*
    if lb == ub then apply(lb)
    else if lb.typeSymbol == defn.NothingClass && ub.typeSymbol == defn.AnyClass
    then Syntax(10, sub)
    else if lb.typeSymbol == defn.NothingClass then Syntax(10, sub, UpperBound, apply(ub))
    else if lb.typeSymbol == defn.AnyClass then Syntax(10, sub, LowerBound, apply(lb))
    else Syntax(10, sub, LowerBound, apply(lb), AndUpperBound, apply(ub))

  def apply(using quotes: Quotes)(repr: quotes.reflect.TypeRepr)(using Stdio)
  : Syntax = cache.establish(repr):
    import quotes.reflect.*

    repr.absolve match
      case ThisType(tpe) =>
        apply(tpe)

      case typeRef@TypeRef(NoPrefix(), name) =>
        if name.tt.starts(t"_$$") then Syntax.Member(name.tt.skip(2))
        else Syntax.Simple(Typename(typeRef.typeSymbol.fullName.tt), true)

      case typeRef@TypeRef(prefix, name) =>
        apply(prefix) match
          case simple@Syntax.Simple(typename, isTerm) =>
            val module = typeRef.typeSymbol.flags.is(Flags.Module)

            val name2 = if module then name.tt.skip(1, Rtl) else name.tt
            if name2.ends(t"$$package") || name2 == t"package" then simple else
              if isTerm then Syntax.Simple(Typename.Term(typename, name2), module)
              else Syntax.Simple(Typename.Type(typename, name2), module)

          case compound: Syntax.Compound =>
            if compound.precedence < 10 then Syntax(10, compound, Dot, Syntax.Member(name.tt))
            else Syntax(10, OpenParens, compound, CloseParens, Dot, Syntax.Member(name.tt))

          case refined@Syntax.Refined(base, members) =>
            if members.contains(name) then members(name.tt)
            else Syntax(10, OpenParens, refined, CloseParens, Project, Syntax.Member(name.tt))

          case other =>
            Out.println(t"OTHER: ${other.toString}") yet Syntax.Constant(t"<unknown>")

      case termRef@TermRef(NoPrefix(), name) =>
        Syntax.Simple(Typename(termRef.termSymbol.fullName.tt), true)

      case termRef@TermRef(ThisType(TypeRef(NoPrefix(), "<root>")), name) =>
        Syntax.Simple(Typename(termRef.termSymbol.fullName.tt), true)

      case termRef@TermRef(prefix, name) =>
        apply(prefix) match
          case simple@Syntax.Simple(typename, isTerm) =>
            if name.tt.ends(t"$$package") || name == t"package" then simple
            else if isTerm then Syntax.Simple(Typename.Term(typename, name), true)
            else Syntax.Simple(Typename.Type(typename, name), true)

          case compound: Syntax.Compound =>
            if compound.precedence < 10 then Syntax(10, compound, Dot, Syntax.Member(name.tt))
            else Syntax(10, OpenParens, compound, CloseParens, Dot, Syntax.Member(name.tt))

          case refined@Syntax.Refined(base, members) =>
            if members.contains(name) then members(name.tt)
            else Syntax(10, OpenParens, refined, CloseParens, Project, Syntax.Member(name.tt))

          case other =>
            Out.println(t"OTHER: ${other.toString}") yet Syntax.Constant(t"<unknown>")

      case AnnotatedType(tpe, annotation) =>
        // FIXME: We don't have access to `into` information, so this is hack
        if annotation.toString.contains("object annotation),into)")
        then Syntax(0, Into, apply(tpe))
        else apply(tpe)

      case OrType(left, right)   => Syntax(1, apply(left), Union, apply(right))
      case AndType(left, right)  => Syntax(3, apply(left), Intersect, apply(right))
      case ByNameType(tpe)       => Syntax(0, Arrow, apply(tpe))
      case FlexibleType(tpe)     => Syntax(0, apply(tpe), Qmark)

      case typ@AppliedType(base, args0) =>
        if typ.isFunctionType then
          val args = args0.init.flatMap(apply(_) :: Comma :: Nil).dropRight(1)
          val result = args0.last
          val arrow = if typ.isContextFunctionType then ContextualArrow else FunctionArrow
          if args0.length == 2
          then Syntax(4, args :+ arrow :+ apply(result)*)
          else Syntax(10, OpenParens +: args :+ CloseParens :+ arrow :+ apply(result)*)
        else if args0.length == 2 && repr.typeSymbol.flags.is(Flags.Infix)
        then Syntax(4, apply(args0(0)), Space, apply(base), Space, apply(args0(1)))
        else
          val args = args0.map(apply(_)).flatMap(List(_, Comma)).dropRight(1)
          if defn.isTupleClass(base.typeSymbol)
          then Syntax(10, OpenParens +: args :+ CloseParens*)
          else Syntax(10, apply(base) +: OpenBracket +: args :+ CloseBracket*)

      case ConstantType(constant) => constant match
        case ByteConstant(byte)     => Syntax.Constant(t"$byte.toByte")
        case ShortConstant(short)   => Syntax.Constant(t"$short.toShort")
        case IntConstant(int)       => Syntax.Constant(int.show)
        case LongConstant(long)     => Syntax.Constant(t"${long}L")
        case BooleanConstant(true)  => Syntax.Constant(t"true")
        case BooleanConstant(false) => Syntax.Constant(t"false")
        case StringConstant(str)    => Syntax.Constant(t"\"$str\"")
        case CharConstant(char)     => Syntax.Constant(t"'$char'")
        case DoubleConstant(double) => Syntax.Constant(t"${double.toString}")
        case FloatConstant(float)   => Syntax.Constant(t"${float.toString}F")
        case UnitConstant()         => Syntax.Constant(t"()")
        case NullConstant()         => Syntax.Constant(t"null")
        case ClassOfConstant(cls)   => Syntax(10, ClassOf, apply(cls), CloseBracket)

      case Refinement(base, name, member) =>
        if name == "Self" then Syntax(10, apply(base), Syntax.Symbolic(t" is "), apply(member))
        else apply(base) match
          case Syntax.Refined(base, members) =>
            Syntax.Refined(base, members.updated(name, apply(member)))

          case other =>
            if base.typeSymbol.fullName == "scala.PolyFunction" && name == "apply"
            then apply(member)

            else if base.isFunctionType && name == "apply" then other
            else Syntax.Refined(other, ListMap(name.tt -> apply(member)))

      case TypeBounds(lb, ub) =>
        typeBounds(Qmark, lb, ub)

      case method@MethodType(args0, types, result) =>
        val unnamed = args0.forall(_.startsWith("x$"))
        val args =
          if args0.isEmpty then Nil
          else if unnamed then types.flatMap(apply(_) :: Comma :: Nil).dropRight(1)
          else
            args0.zip(types).flatMap: (member, typ) =>
              Syntax.Member(member) :: Colon :: Syntax(10, apply(typ)) :: Comma :: Nil
            . dropRight(1)

        val arrow = if method.isContextFunctionType then ContextualArrow else FunctionArrow
        if unnamed && args0.length == 1
        then Syntax(4, args :+ arrow :+ apply(result)*)
        else Syntax(10, OpenParens +: args :+ CloseParens :+ arrow :+ apply(result)*)

      case typ@PolyType(args0, types, result) =>
        val args =
          if args0.isEmpty then Nil else
            args0.zip(types).flatMap:
              case (name, TypeBounds(lb, ub)) =>
              typeBounds(Syntax.Member(name), lb, ub) :: Comma :: Nil

            . dropRight(1)

        Syntax(0, OpenBracket +: args :+ CloseBracket :+ FunctionArrow :+ apply(result)*)

      case TypeLambda(args0, _, tpe)            =>
        val args = args0.map(Syntax.Member(_)).flatMap(List(_, Comma)).dropRight(1)
        val params = (OpenBracket +: args :+ CloseBracket :+ LambdaArrow) :+ apply(tpe)
        Syntax(0, params*)

      case ParamRef(binder, n) => binder match
        case TypeLambda(params, _, _) => Syntax.Member(params(n))
        case MethodType(params, _, _) => Syntax.Member(params(n))
        case PolyType(params, _, _)   => Syntax.Member(params(n))
        case other =>
          Syntax.Constant(t"ParamRef")

      // case classInfo: dotty.tools.dotc.core.Types.ClassInfo =>
      //   val parents = classInfo.declaredParents.flatMap: tpe =>
      //     List(apply(tpe.asInstanceOf[TypeRepr]), Comma)
      //   Syntax(0, parents.dropRight(1)*)

      // case TypeDef(name, tree: TypeTree) =>
      //   Syntax(0, Syntax.Member(name.tt), apply(tree.tpe))

      // case ref: dotty.tools.dotc.core.Types.LazyRef =>
      //   Out.println(ref.rf(using quotes.ctx.compilerContext))
      //   Syntax.Constant(t"...lazy ref...")

      case other =>
        //Out.println(t"Other kind of type: ${other.toString}")
        Syntax.Constant(t"...other: ${other.toString}...")
