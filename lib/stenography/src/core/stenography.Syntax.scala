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
import symbolism.*
import vacuous.*


enum Syntax:
  case Simple(typename: Typename)
  case Symbolic(text: Text)
  case Constant(text: Text)
  case Project(base: Syntax, text: Text)
  case Refined(syntax: Syntax, members: ListMap[Text, Syntax])
  case Infix(left: Optional[Syntax], middle: Text, right: Optional[Syntax])
  case Prefix(middle: Text, right: Syntax)
  case Application(left: Syntax, elements: List[Syntax], infix: Boolean)
  case Selection(left: Syntax, right: Text)
  case Named(isUsing: Boolean, name: Text, syntax: Syntax)
  case Tuple(isType: Boolean, syntaxes: List[Syntax])
  case Singleton(typename: Typename)

  def precedence: Int = this match
    case Simple(_)            => 10
    case Symbolic(_)          => 10
    case Constant(_)          => 10
    case Project(_, _)        => 9
    case Refined(_, _)        => 0
    case Infix(_, middle, _)  => Syntax.precedence(middle.s.head)
    case Prefix(_, _)         => 0
    case Application(_, _, _) => 10
    case Selection(_, _)      => 10
    case Named(_, _, _)       => 0
    case Tuple(_, _)          => 10
    case Singleton(_)         => 10

object Syntax:
  inline def name[typename <: AnyKind]: Text = ${Stenography.name[typename]}

  def suffix(syntax: Syntax, suffix: Text): Syntax.Infix = Syntax.Infix(syntax, suffix, Unset)

  given showable: (scope: Scope) => Syntax is Showable:
    def text(syntax: Syntax): Text = syntax match
      case Simple(typename)                    => typename.show
      case Symbolic(text)                      => text
      case Project(base, text)                 => t"$base#$text"
      case Constant(text)                      => text
      case Selection(left, right)              => t"$left.$right"
      case Prefix(prefix, base)                => t"$prefix $base"
      case Infix(Unset, middle, right: Syntax) => t"$middle $right"
      case Infix(left: Syntax, middle, Unset)  => t"$left $middle"
      case Tuple(false, elements)              => t"(${elements.map(_.show).join(t", ")})"
      case Tuple(true, elements)               => t"[${elements.map(_.show).join(t", ")}]"
      case Singleton(typename)                 => t"$typename.type"
      case Application(left, elements, infix)   => left match
        case Simple(Typename.Type(parent, name)) if infix && scope.has(parent) =>
          text(Infix(elements(0), name, elements(1)))

        case _ =>
          left.show+elements.map(_.show).join(t"[", t", ", t"]")

      case Refined(base, members)              =>
        val members2 = members.map { (name, syntax) => t"type $name = $syntax" }.join(t"; ")
        t"$base { $members2 }"

      case Infix(left: Syntax, middle, right: Syntax) =>
        val left2 = if left.precedence < syntax.precedence then Syntax.Tuple(false, List(left)) else left
        val right2 = if right.precedence < syntax.precedence then Syntax.Tuple(false, List(right)) else right
        t"$left2 $middle $right2"

      case Named(isUsing, name, syntax) =>
        if isUsing then t"using $name: $syntax" else t"$name: $syntax"

  val cache: scm.HashMap[Any, Syntax] = scm.HashMap()

  def precedence(char: Char): Int = char match
    case '|'                   => 1
    case '^'                   => 2
    case '&'                   => 3
    case '!' | '='             => 4
    case '<' | '>'             => 5
    case ':'                   => 6
    case '+' | '-'             => 7
    case '%' | '*' | '/'       => 8
    case char if char.isLetter => 0
    case _                     => 9

  def bounds(using Quotes)(sub: Syntax, lb: quotes.reflect.TypeRepr, ub: quotes.reflect.TypeRepr)
  : Syntax =

      import quotes.reflect.*

      if lb == ub then apply(lb)
      else if lb.typeSymbol == defn.NothingClass && ub.typeSymbol == defn.AnyClass
      then sub
      else if lb.typeSymbol == defn.NothingClass then Syntax.Infix(sub, "<:", apply(ub))
      else if ub.typeSymbol == defn.AnyClass then Syntax.Infix(sub, ">:", apply(lb))
      else Syntax.Infix(Syntax.Infix(sub, ">:", apply(lb)), "<:", apply(ub))

  def apply(using Quotes)(repr: quotes.reflect.TypeRepr): Syntax = cache.establish(repr):
    import quotes.reflect.*

    def isPackage(name: Text): Boolean = name.ends(t"$$package") || name == t"package"

    repr.absolve match
      case ThisType(tpe) =>
        apply(tpe)

      case typeRef@TypeRef(NoPrefix(), name) =>
        if name.tt.starts(t"_$$") then Syntax.Symbolic(name.tt.skip(2))
        else Syntax.Simple(Typename(typeRef.typeSymbol.fullName.tt))

      case typeRef@TypeRef(prefix, name) =>
        apply(prefix) match
          case singleton@Syntax.Singleton(typename) =>
            val module = typeRef.typeSymbol.flags.is(Flags.Module)

            val name2 = if module then name.tt.skip(1, Rtl) else name.tt
            if isPackage(name2) then singleton else Syntax.Simple(Typename.Type(typename, name2))

          case simple@Syntax.Simple(typename) =>
            val module = typeRef.typeSymbol.flags.is(Flags.Module)

            val name2 = if module then name.tt.skip(1, Rtl) else name.tt
            if isPackage(name2) then simple else typename match
              case Typename.Term(_, _) => Syntax.Simple(Typename.Type(typename, name2))
              case Typename.Top(_)     => Syntax.Simple(Typename.Type(typename, name2))
              case Typename.Type(_, _) => Syntax.Project(simple, name2)

          case refined@Syntax.Refined(base, members) =>
            if members.contains(name) then members(name.tt) else Syntax.Project(refined, name.tt)

          case other =>
            Syntax.Constant(t"<unknown>")

      case termRef@TermRef(NoPrefix(), name) =>
        Syntax.Singleton(Typename(termRef.termSymbol.fullName.tt))

      case termRef@TermRef(ThisType(TypeRef(NoPrefix(), "<root>")), name) =>
        Syntax.Singleton(Typename(termRef.termSymbol.fullName.tt))

      case termRef@TermRef(prefix, name) =>
        apply(prefix) match
          case singleton@Syntax.Singleton(typename) =>
            if isPackage(name.tt) then singleton else Syntax.Singleton(Typename.Term(typename, name))

          case simple@Syntax.Simple(typename) =>
            if isPackage(name.tt) then simple else Syntax.Singleton(Typename.Term(typename, name))

          case refined@Syntax.Refined(base, members) =>
            if members.contains(name) then members(name.tt) else Syntax.Project(refined, name.tt)

          case other =>
            Syntax.Constant(t"<unknown>")

      case AnnotatedType(tpe, annotation) =>
        // FIXME: We don't have access to `into` information, so this is a hack
        if annotation.toString.contains("object annotation),into)")
        then Syntax.Prefix(t"into", apply(tpe))
        else apply(tpe)

      case OrType(left, right)   => Syntax.Infix(apply(left), "|", apply(right))
      case AndType(left, right)  => Syntax.Infix(apply(left), "&", apply(right))
      case ByNameType(tpe)       => Syntax.Prefix("=>", apply(tpe))
      case FlexibleType(tpe)     => Syntax.Infix(apply(tpe), "?", Unset)

      case typ@AppliedType(base, args0) =>
        if typ.isFunctionType then
          val args = args0.init match
            case List(one) => apply(one)
            case many      => Syntax.Tuple(false, many.map(apply(_)))

          val result = args0.last
          val arrow = if typ.isContextFunctionType then "?=>" else "=>"

          Syntax.Infix(args, arrow, apply(result))

        else if args0.length == 2 && repr.typeSymbol.flags.is(Flags.Infix)
        then Syntax.Application(apply(base), args0.map(apply(_)), true)

        else
          if defn.isTupleClass(base.typeSymbol) then Syntax.Tuple(false, args0.map(apply(_)))
          else
            if base <:< TypeRepr.of[NamedTuple.NamedTuple] then
              args0(0) match
                case AppliedType(_, names) => apply(args0(1)) match
                  case Syntax.Tuple(_, elements) =>
                    Syntax.Tuple
                     (false,
                      names.zip(elements).map:
                        case (ConstantType(StringConstant(name)), element) =>
                          Syntax.Named(false, name.tt, element))

            else Syntax.Application(apply(base), args0.map(apply(_)), false)

      case ConstantType(constant) => constant match
        case ByteConstant(byte)     => Syntax.Constant(t"$byte.toByte")
        case ShortConstant(short)   => Syntax.Constant(t"$short.toShort")
        case IntConstant(int)       => Syntax.Constant(int.show)
        case LongConstant(long)     => Syntax.Constant(t"${long}L")
        case BooleanConstant(true)  => Syntax.Constant("true")
        case BooleanConstant(false) => Syntax.Constant("false")
        case StringConstant(str)    => Syntax.Constant(t"\"$str\"")
        case CharConstant(char)     => Syntax.Constant(t"'$char'")
        case DoubleConstant(double) => Syntax.Constant(t"${double.toString}")
        case FloatConstant(float)   => Syntax.Constant(t"${float.toString}F")
        case UnitConstant()         => Syntax.Constant("()")
        case NullConstant()         => Syntax.Constant("null")
        case ClassOfConstant(cls)   => Syntax.Application(Syntax.Constant("classOf"), List(apply(cls)), false)

      case Refinement(base, "apply", member) =>
        apply(member)

      case Refinement(base, name, member) =>
        if name == "Self" then Syntax.Infix(apply(base), t"is", apply(member))
        else apply(base) match
          case Syntax.Refined(base, members) =>
            Syntax.Refined(base, members.updated(name, apply(member)))

          case other =>
            if base.typeSymbol.fullName == "scala.PolyFunction" && name == "apply"
            then apply(member)

            else if base.isFunctionType && name == "apply" then other
            else Syntax.Refined(other, ListMap(name.tt -> apply(member)))

      case TypeBounds(lb, ub) =>
        bounds(Syntax.Symbolic("?"), lb, ub)

      case method@MethodType(args0, types, result) =>
        val unnamed = args0.forall(_.startsWith("x$"))

        val args =
          if args0.isEmpty then Syntax.Tuple(false, Nil)
          else if unnamed then Syntax.Tuple(false, types.map(apply(_)))
          else Syntax.Tuple(false, args0.zip(types).map { (member, typ) => Syntax.Named(false, member, apply(typ)) })

        val arrow = if method.isContextual then "?=>" else "=>"
        if unnamed && args0.length == 1
        then Syntax.Infix(apply(types.head), arrow, apply(result))
        else Syntax.Infix(args, arrow, apply(result))

      case typ@PolyType(args0, types, result) =>
        val args = args0.zip(types).map:
          case (name, TypeBounds(lb, ub)) => bounds(Syntax.Symbolic(name), lb, ub)

        Syntax.Infix(Syntax.Tuple(true, args), "=>", apply(result))

      case TypeLambda(args0, _, tpe)            =>
        Syntax.Infix(Syntax.Tuple(true, args0.map(Syntax.Symbolic(_))), "=>>", apply(tpe))

      case ParamRef(binder, n) => binder match
        case TypeLambda(params, _, _) => Syntax.Symbolic(params(n))
        case MethodType(params, _, _) => Syntax.Symbolic(params(n))
        case PolyType(params, _, _)   => Syntax.Symbolic(params(n))
        case other => Syntax.Constant(t"ParamRef")

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
