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
import proscenium.*
import rudiments.{is as _, *}
import symbolism.*
import vacuous.*


enum Syntax:
  case Simple(typename: Typename)
  case Symbolic(text: Text)
  case Constant(text: Text)
  case Project(base: Syntax, text: Text)
  case Refined(syntax: Syntax, types: ListMap[Text, Syntax], terms: ListMap[Text, Syntax])
  case Infix(left: Syntax, middle: Text, right: Syntax)
  case Prefix(middle: Text, right: Syntax)
  case Suffix(left: Syntax, suffix: Text)
  case Application(left: Syntax, elements: List[Syntax], infix: Boolean)
  case Selection(left: Syntax, right: Text)
  case Named(isUsing: Boolean, name: Text, syntax: Syntax)
  case Tuple(isType: Boolean, syntaxes: List[Syntax])
  case Signature(method: Boolean, syntaxes: List[Syntax], result: Syntax)
  case Singleton(typename: Typename)
  case Compound(syntaxes: List[Syntax])

  def precedence: Int = this match
    case Simple(_)             => 10
    case Symbolic(_)           => 10
    case Constant(_)           => 10
    case Project(_, _)         => 9
    case Refined(_, _, _)      => 0
    case Infix(_, middle, _)   => Syntax.precedence(middle.s.head)
    case Prefix(_, _)          => 0
    case Suffix(_, _)          => 0
    case Application(_, _, _)  => 10
    case Selection(_, _)       => 10
    case Named(_, _, _)        => 0
    case Tuple(_, _)           => 10
    case Signature(_, _, _)    => 10
    case Singleton(_)          => 10
    case Compound(_)           => 0

  def text(using scope: Scope): Text = this match
    case Simple(typename)                    => typename.text
    case Symbolic(text)                      => text
    case Project(base, text)                 => s"${base.text}#$text".tt
    case Constant(text)                      => text
    case Selection(left, right)              => s"${left.text}.${right}"
    case Prefix(prefix, base)                => s"$prefix ${base.text}".tt
    case Suffix(base, suffix)                => s"${base.text}$suffix".tt
    case Tuple(false, elements)              => s"(${elements.map(_.text).mkString(", ")})".tt
    case Tuple(true, elements)               => s"[${elements.map(_.text).mkString(", ")}]".tt
    case Singleton(typename)                 => s"${typename.text}.type".tt
    case Compound(syntaxes)                  => syntaxes.map(_.text).mkString.tt

    case Signature(method, syntaxes, result) =>
      s"${syntaxes.map(_.text).mkString}${if method then ": " else ""}${result.text}".tt

    case Application(left, elements, infix) => left match
      case Simple(Typename.Type(parent, name)) if infix && scope.has(parent) =>
       Infix(elements(0), name, elements(1)).text

      case _ =>
        left.text+elements.map(_.text).mkString("[", ", ", "]").tt

    case Refined(base, members, defs) =>
      val members2 = members.map { (name, syntax) => s"type $name = ${syntax.text}".tt }
      val defs2 = defs.map { (name, syntax) => s"def $name${syntax.text}".tt }
      s"${base.text} { ${(members2 ++ defs2).mkString("; ")} }".tt

    case Infix(left: Syntax, middle, right: Syntax) =>
      val left2 = if left.precedence < precedence then Syntax.Tuple(false, List(left)) else left
      val right2 = if right.precedence < precedence then Syntax.Tuple(false, List(right)) else right
      s"${left2.text} $middle ${right2.text}".tt

    case Named(isUsing, name, syntax) =>
      if isUsing then s"using $name: ${syntax.text}".tt else s"$name: ${syntax.text}".tt

object Syntax:
  inline def name[typename <: AnyKind]: Text = ${Stenography.name[typename]}

  val Space: Syntax.Symbolic = Syntax.Symbolic(" ")
  val Colon: Syntax.Symbolic = Syntax.Symbolic(": ")
  val Comma: Syntax.Symbolic = Syntax.Symbolic(", ")
  val Open: Syntax.Symbolic = Syntax.Symbolic("(")
  val Close: Syntax.Symbolic = Syntax.Symbolic(")")
  val OpenType: Syntax.Symbolic = Syntax.Symbolic("[")
  val CloseType: Syntax.Symbolic = Syntax.Symbolic("]")
  val Plus: Syntax.Symbolic = Syntax.Symbolic("+")
  val Minus: Syntax.Symbolic = Syntax.Symbolic("-")

  private val cache: scm.HashMap[Any, Syntax] = scm.HashMap()
  def clear(): Unit = cache.clear()

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

  def bounds(using Quotes)
       (sub: Syntax, lower: quotes.reflect.TypeRepr, upper: quotes.reflect.TypeRepr)
  : Syntax =

      import quotes.reflect.*

      if lower == upper then apply(lower)
      else if lower.typeSymbol == defn.NothingClass && upper.typeSymbol == defn.AnyClass
      then sub
      else if lower.typeSymbol == defn.NothingClass then Syntax.Infix(sub, "<:", apply(upper))
      else if upper.typeSymbol == defn.AnyClass then Syntax.Infix(sub, ">:", apply(lower))
      else Syntax.Infix(Syntax.Infix(sub, ">:", apply(lower)), "<:", apply(upper))


  def clause(using Quotes)(clause: quotes.reflect.ParamClause, showUsing: Boolean): Syntax =
    import quotes.reflect.*
    clause match
      case TermParamClause(termDefs) =>
        val contextual = termDefs.exists(_.symbol.flags.is(Flags.Given))

        val defs = termDefs.flatMap:
          case valDef@ValDef(name, rtn, default) =>
            val syntax =
              if name.startsWith("evidence$") || name.startsWith("x$")
              then List(apply(rtn.tpe), Comma)
              else List(Syntax.Symbolic(name.tt), Colon, apply(rtn.tpe), Comma)

            if valDef.symbol.flags.is(Flags.Inline)
            then Syntax.Symbolic("inline ") :: syntax
            else syntax


        val usingKeyword = if contextual && showUsing then List(Syntax.Symbolic("using ")) else Nil

        Syntax.Compound(Open +: (usingKeyword ++ defs.dropRight(1)) :+ Close)

      case TypeParamClause(typeDefs) =>
        val defs = typeDefs.flatMap:
          case typeDef@TypeDef(name, bounds) =>
            val flags = typeDef.symbol.flags

            def variance(list: List[Syntax]): List[Syntax] =
              if flags.is(Flags.Covariant) then Syntax.Plus :: list
              else if flags.is(Flags.Contravariant) then Syntax.Minus :: list
              else list

            bounds match
              case TypeBoundsTree(lower, upper) =>
                variance(List(Syntax.bounds(Syntax.Symbolic(name.tt), lower.tpe, upper.tpe), Comma))

              case LambdaTypeTree(typeDefs, other) =>
                variance(List(Syntax.Symbolic(name), Comma))

              case other =>
                variance(List(Syntax.Symbolic(name), Comma))

        Syntax.Compound(OpenType +: defs.dropRight(1) :+ CloseType)

  def signature(using Quotes)(name: Text, repr: quotes.reflect.TypeRepr): Syntax.Signature =
    import quotes.reflect.*

    repr.absolve match
      case MethodType(args0, types, result) =>
        val params =
          args0.zip(types).map: (arg, tpe) =>
            Syntax.Named(false, arg, apply(tpe))

        Syntax.Signature(true, List(Syntax.Tuple(false, params)), apply(result))

      case ByNameType(tpe) =>
        Syntax.Signature(true, List(), apply(tpe))

      case TypeBounds(lower, upper) =>
        Syntax.Signature(false, Nil, bounds(Syntax.Symbolic("?"), lower, upper))

      case TypeLambda(args0, boundsList, tpe) =>
        val args = args0.zip(boundsList).map:
          case (arg, TypeBounds(lower, upper)) => bounds(Syntax.Symbolic(arg), lower, upper)

        Syntax.Signature(false, List(Syntax.Tuple(true, args)), apply(tpe))

      case other =>
        Syntax.Signature(true, List(), apply(other))


  def apply(using Quotes)(repr: quotes.reflect.TypeRepr): Syntax = cache.establish(repr):
    import quotes.reflect.*

    def isPackage(name: String): Boolean = name.endsWith("$package") || name == "package"

    repr.absolve match
      case ThisType(tpe) =>
        apply(tpe)

      case typeRef@TypeRef(NoPrefix(), name) =>
        if name.startsWith("_$$") then Syntax.Symbolic(name.drop(2))
        else Syntax.Simple(Typename(typeRef.typeSymbol.fullName.tt))

      case typeRef@TypeRef(prefix, name) =>
        apply(prefix) match
          case singleton@Syntax.Singleton(typename) =>
            val module = typeRef.typeSymbol.flags.is(Flags.Module)

            val name2 = if module then name.dropRight(1) else name
            if isPackage(name2) then singleton else Syntax.Simple(Typename.Type(typename, name2))

          case simple@Syntax.Simple(typename) =>
            val module = typeRef.typeSymbol.flags.is(Flags.Module)

            val name2 = if module then name.dropRight(1) else name
            if isPackage(name2) then simple else typename match
              case Typename.Term(_, _) => Syntax.Simple(Typename.Type(typename, name2))
              case Typename.Top(_)     => Syntax.Simple(Typename.Type(typename, name2))
              case Typename.Type(_, _) => Syntax.Project(simple, name2)

          case refined@Syntax.Refined(base, members, defs) =>
            if members.contains(name) then members(name.tt) else Syntax.Project(refined, name.tt)

          case symbolic@Syntax.Symbolic(_) =>
            Syntax.Selection(symbolic, name)

          case other =>
            Syntax.Constant("<unknown>")

      case termRef@TermRef(NoPrefix(), name) =>
        Syntax.Singleton(Typename(termRef.termSymbol.fullName.tt))

      case termRef@TermRef(ThisType(TypeRef(NoPrefix(), "<root>")), name) =>
        Syntax.Singleton(Typename(termRef.termSymbol.fullName.tt))

      case termRef@TermRef(prefix, name) =>
        apply(prefix) match
          case singleton@Syntax.Singleton(typename) =>
            if isPackage(name) then singleton else Syntax.Singleton(Typename.Term(typename, name))

          case simple@Syntax.Simple(typename) =>
            if isPackage(name) then simple else Syntax.Singleton(Typename.Term(typename, name))

          case refined@Syntax.Refined(base, members, defs) =>
            if members.contains(name) then members(name.tt) else Syntax.Project(refined, name.tt)

          case symbolic@Syntax.Symbolic(_) =>
            Syntax.Selection(symbolic, name)

          case other =>
            other

      case AnnotatedType(tpe, annotation) =>
        // FIXME: We don't have access to `into` information, so this is a hack
        if annotation.toString.contains("object annotation),into)")
        then Syntax.Prefix("into", apply(tpe))
        else apply(tpe)

      case OrType(left, right)   => Syntax.Infix(apply(left), "|", apply(right))
      case AndType(left, right)  => Syntax.Infix(apply(left), "&", apply(right))
      case ByNameType(tpe)       => Syntax.Prefix("=>", apply(tpe))
      case FlexibleType(tpe)     => Syntax.Suffix(apply(tpe), "?")

      case typ@AppliedType(base, args0) =>
        if typ.isFunctionType then
          val args = args0.init match
            case List(one) => apply(one)
            case many      => Syntax.Tuple(false, many.map(apply(_)))

          val arrow = if typ.isContextFunctionType then "?=>" else "=>"

          Syntax.Infix(args, arrow, apply(args0.last))
        else if args0.length == 2 && repr.typeSymbol.flags.is(Flags.Infix)
        then Syntax.Application(apply(base), args0.map(apply(_)), true)
        else if defn.isTupleClass(base.typeSymbol)
        then Syntax.Tuple(false, args0.map(apply(_)))
        else if base <:< TypeRepr.of[NamedTuple.NamedTuple]
        then args0(0).absolve match
          case AppliedType(_, names) => apply(args0(1)).absolve match
            case Syntax.Tuple(_, elements) =>
              Syntax.Tuple
               (false,
                names.zip(elements).map:
                  _.absolve match
                    case (ConstantType(StringConstant(name)), element) =>
                      Syntax.Named(false, name.tt, element))

          case ref@TypeRef(prefix, name) =>
            apply(ref)

        else Syntax.Application(apply(base), args0.map(apply(_)), false)

      case ConstantType(constant) => constant.absolve match
        case ByteConstant(byte)     => Syntax.Constant(s"$byte.toByte")
        case ShortConstant(short)   => Syntax.Constant(s"$short.toShort")
        case IntConstant(int)       => Syntax.Constant(int.toString.tt)
        case LongConstant(long)     => Syntax.Constant(s"${long}L")
        case BooleanConstant(true)  => Syntax.Constant("true")
        case BooleanConstant(false) => Syntax.Constant("false")
        case StringConstant(str)    => Syntax.Constant(s"\"$str\"")
        case CharConstant(char)     => Syntax.Constant(s"'$char'")
        case DoubleConstant(double) => Syntax.Constant(s"${double.toString}")
        case FloatConstant(float)   => Syntax.Constant(s"${float.toString}F")
        case UnitConstant()         => Syntax.Constant("()")
        case NullConstant()         => Syntax.Constant("null")
        case ClassOfConstant(cls)   => Syntax.Application(Syntax.Constant("classOf"), List(apply(cls)), false)

      case Refinement(base, "apply", member) =>
        apply(member)

      case Refinement(base, name, member) =>
        if name == "Self" then Syntax.Infix(apply(base), "is", apply(member)) else
          val refined: Syntax.Refined = apply(base) match
            case refined@Syntax.Refined(base, members, defs) => refined
            case other =>
              Syntax.Refined(other, ListMap(), ListMap())

          signature(name, member) match
            case signature@Syntax.Signature(method, _, _) =>
              if method then refined.copy(terms = refined.terms.updated(name, signature))
              else refined.copy(types = refined.types.updated(name, signature))

      case TypeBounds(lower, upper) =>
        bounds(Syntax.Symbolic("?"), lower, upper)

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
          case (name, TypeBounds(lower, upper)) => bounds(Syntax.Symbolic(name), lower, upper)

        Syntax.Infix(Syntax.Tuple(true, args), "=>", apply(result))

      case TypeLambda(args0, boundsList, tpe) =>
        val args = args0.zip(boundsList).map:
          case (arg, TypeBounds(lower, upper)) => bounds(Syntax.Symbolic(arg), lower, upper)

        Syntax.Infix(Syntax.Tuple(true, args), "=>>", apply(tpe))

      case ParamRef(binder, n) => binder match
        case TypeLambda(params, _, _) => Syntax.Symbolic(params(n))
        case MethodType(params, _, _) => Syntax.Symbolic(params(n))
        case PolyType(params, _, _)   => Syntax.Symbolic(params(n))
        case other => Syntax.Constant("ParamRef")

      case RecursiveType(tpe) =>
        apply(tpe)

      case RecursiveThis(tpe) =>
        Syntax.Constant("<recursive>")

      case other =>
        Syntax.Constant(s"...other: ${other.toString}...")
