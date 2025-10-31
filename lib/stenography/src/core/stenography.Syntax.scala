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
┃    Soundness, version 0.45.0.                                                                    ┃
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
  case Primitive(text: Text)
  case Projection(base: Syntax, text: Text)
  case Structural(syntax: Syntax, types: ListMap[Text, Syntax], terms: ListMap[Text, Syntax])
  case Infix(left: Syntax, middle: Text, right: Syntax)
  case Prefix(middle: Text, right: Syntax)
  case Suffix(left: Syntax, suffix: Text)
  case Application(left: Syntax, elements: List[Syntax], infix: Boolean)
  case Selection(left: Syntax, right: Text)
  case Named(isUsing: Boolean, name: Text, syntax: Syntax)
  case Sequence(isType: Boolean, syntaxes: List[Syntax])
  case Declaration(method: Boolean, syntaxes: List[Syntax], result: Syntax)
  case Value(typename: Typename)
  case Compound(syntaxes: List[Syntax])

  def precedence: Int = this match
    case Structural(_, _, _)   => 0
    case Prefix(_, _)          => 0
    case Named(_, _, _)        => 0
    case Compound(_)           => 0
    case Suffix(_, _)          => 0
    case Infix(_, middle, _)   => middle.s.head match
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
    case Projection(_, _)      => 9
    case Simple(_)             => 10
    case Symbolic(_)           => 10
    case Primitive(_)          => 10
    case Application(_, _, _)  => 10
    case Selection(_, _)       => 10
    case Sequence(_, _)        => 10
    case Declaration(_, _, _)    => 10
    case Value(_)          => 10

  def text(using imports: Imports): Text = this match
    case Simple(typename)          => typename.text
    case Symbolic(text)            => text
    case Projection(base, text)    => s"${base.text}#$text".tt
    case Primitive(text)           => text
    case Selection(left, right)    => s"${left.text}.${right}"
    case Prefix(prefix, base)      => s"$prefix ${base.text}".tt
    case Suffix(base, suffix)      => s"${base.text}$suffix".tt
    case Sequence(false, elements) => s"(${elements.map(_.text).mkString(", ")})".tt
    case Sequence(true, elements)  => s"[${elements.map(_.text).mkString(", ")}]".tt
    case Value(typename)       => s"${typename.text}.type".tt
    case Compound(syntaxes)        => syntaxes.map(_.text).mkString.tt

    case Declaration(method, syntaxes, result) =>
      s"${syntaxes.map(_.text).mkString}${if method then ": " else ""}${result.text}".tt

    case Application(left, elements, infix) => left match
      case Simple(Typename.Type(parent, name)) if infix && imports.has(parent) =>
       Infix(elements(0), name, elements(1)).text

      case _ =>
        left.text+elements.map(_.text).mkString("[", ", ", "]").tt

    case Structural(base, members, defs) =>
      val members2 = members.map { (name, syntax) => s"type $name = ${syntax.text}".tt }
      val defs2 = defs.map { (name, syntax) => s"def $name${syntax.text}".tt }
      s"${base.text} { ${(members2 ++ defs2).mkString("; ")} }".tt

    case Infix(left: Syntax, middle, right: Syntax) =>
      val left2 = if left.precedence < precedence then Sequence(false, List(left)) else left
      val right2 = if right.precedence < precedence then Sequence(false, List(right)) else right
      s"${left2.text} $middle ${right2.text}".tt

    case Named(isUsing, name, syntax) =>
      if isUsing then s"using $name: ${syntax.text}".tt else s"$name: ${syntax.text}".tt

object Syntax:
  inline def name[typename <: AnyKind]: Text = ${Stenography.typename[typename]}

  val Space: Symbolic = Symbolic(" ")
  val Colon: Symbolic = Symbolic(": ")
  val Comma: Symbolic = Symbolic(", ")
  val Open: Symbolic = Symbolic("(")
  val Close: Symbolic = Symbolic(")")
  val OpenType: Symbolic = Symbolic("[")
  val CloseType: Symbolic = Symbolic("]")
  val Plus: Symbolic = Symbolic("+")
  val Minus: Symbolic = Symbolic("-")

  private val cache: scm.HashMap[Any, Syntax] = scm.HashMap()
  def clear(): Unit = cache.clear()

  def symbolic(name: Text): Symbolic =
    Symbolic(if name.s.startsWith("_$") then name.s.drop(2).tt else name)

  def typeBounds(using Quotes)
       (sub: Syntax, lower: quotes.reflect.TypeRepr, upper: quotes.reflect.TypeRepr)
  : Syntax =

      import quotes.reflect.*

      if lower == upper then apply(lower)
      else if lower.typeSymbol == defn.NothingClass && upper.typeSymbol == defn.AnyClass
      then sub
      else if lower.typeSymbol == defn.NothingClass then Infix(sub, "<:", apply(upper))
      else if upper.typeSymbol == defn.AnyClass then Infix(sub, ">:", apply(lower))
      else Infix(Infix(sub, ">:", apply(lower)), "<:", apply(upper))


  def clause(using Quotes)(clause: quotes.reflect.ParamClause, showUsing: Boolean): Syntax =
    import quotes.reflect.*

    clause match
      case TermParamClause(termDefs) =>
        val contextual = termDefs.exists(_.symbol.flags.is(Flags.Given))

        val defs = termDefs.flatMap:
          case valDef@ValDef(name, meta, default) =>
            val syntax =
              if name.startsWith("evidence$") || name.startsWith("x$")
              then List(apply(meta.tpe), Comma)
              else List(symbolic(name.tt), Colon, apply(meta.tpe), Comma)

            if valDef.symbol.flags.is(Flags.Inline)
            then symbolic("inline ") :: syntax
            else syntax


        val usingKeyword = if contextual && showUsing then List(symbolic("using ")) else Nil

        Compound(Open +: (usingKeyword ++ defs.dropRight(1)) :+ Close)

      case TypeParamClause(typeDefs) =>
        val defs = typeDefs.flatMap:
          case typeDef@TypeDef(name, bounds) =>
            val flags = typeDef.symbol.flags

            def variance(list: List[Syntax]): List[Syntax] =
              if flags.is(Flags.Covariant) then Plus :: list
              else if flags.is(Flags.Contravariant) then Minus :: list
              else list

            bounds match
              case TypeBoundsTree(lower, upper) =>
                variance(List(typeBounds(symbolic(name.tt), lower.tpe, upper.tpe), Comma))

              case LambdaTypeTree(typeDefs, other) =>
                variance(List(symbolic(name), Comma))

              case other =>
                variance(List(symbolic(name), Comma))

        Compound(OpenType +: defs.dropRight(1) :+ CloseType)

  def signature(using Quotes)(name: Text, repr: quotes.reflect.TypeRepr): Declaration =
    import quotes.reflect.*

    repr.absolve match
      case MethodType(args0, types, result) =>
        val params =
          args0.zip(types).map: (arg, tpe) =>
            Named(false, arg, apply(tpe))

        Declaration(true, List(Sequence(false, params)), apply(result))

      case ByNameType(tpe) =>
        Declaration(true, List(), apply(tpe))

      case TypeBounds(lower, upper) =>
        Declaration(false, Nil, typeBounds(symbolic("?"), lower, upper))

      case TypeLambda(args0, bounds, tpe) =>
        val args = args0.zip(bounds).map:
          case (arg, TypeBounds(lower, upper)) => typeBounds(symbolic(arg), lower, upper)

        Declaration(false, List(Sequence(true, args)), apply(tpe))

      case other =>
        Declaration(true, List(), apply(other))


  def apply(using Quotes)(repr: quotes.reflect.TypeRepr): Syntax = cache.establish(repr):
    import quotes.reflect.*

    def isPackage(name: String): Boolean = name.endsWith("$package") || name == "package"

    repr.absolve match
      case ThisType(ref) =>
        apply(ref) match
          case Simple(Typename.Type(parent, name)) => Simple(Typename.Term(parent, name))
          case syntax => syntax

      case typeRef@TypeRef(NoPrefix(), name) =>
        Simple(Typename.Top(name))

      case typeRef@TypeRef(prefix, name) =>
        val module = typeRef.typeSymbol.flags.is(Flags.Module)
        val name2 = if module then name.dropRight(1) else name

        if prefix.typeSymbol.flags.is(Flags.Package)
        then Simple(Typename.Type(Typename(prefix.show.tt), name2))
        else apply(prefix) match
          case value@Value(typename) =>
            if isPackage(name2) then value
            else Simple(Typename.Type(typename, name2))

          case simple@Simple(typename) =>
            if isPackage(name2) then simple else Simple(Typename.Type(typename, name2))

          case refined@Structural(base, members, defs) =>
            if members.contains(name) then members(name.tt) else Projection(refined, name.tt)

          case symbolic@Symbolic(_) =>
            Selection(symbolic, name)

          case selection: Selection =>
            Selection(selection, name)

          case other =>
            Primitive("<unknown>")


      case termRef@TermRef(NoPrefix(), name) =>
        Value(Typename.Top(name))

      case termRef@TermRef(ThisType(TypeRef(NoPrefix(), "<root>")), name) =>
        Value(Typename.Top(name))

      case termRef@TermRef(prefix, name) =>
        apply(prefix) match
          case value@Value(typename) =>
            if isPackage(name) then value else Value(Typename.Term(typename, name))

          case simple@Simple(typename) =>
            if isPackage(name) then simple else Value(Typename.Term(typename, name))

          case refined@Structural(base, members, defs) =>
            if members.contains(name) then members(name.tt) else Projection(refined, name.tt)

          case symbolic@Symbolic(_) =>
            Selection(symbolic, name)

          case selection: Selection =>
            Selection(selection, name)

          case other =>
            Primitive("<unknown>")

      case AnnotatedType(tpe, annotation) =>
        // FIXME: We don't have access to `into` information, so this is a hack
        if annotation.toString.contains("object annotation),into)")
        then Prefix("into", apply(tpe))
        else apply(tpe)

      case OrType(left, right)   => Infix(apply(left), "|", apply(right))
      case AndType(left, right)  => Infix(apply(left), "&", apply(right))
      case ByNameType(tpe)       => Prefix("=>", apply(tpe))
      case FlexibleType(tpe)     => Suffix(apply(tpe), "?")

      case typ@AppliedType(base, args0) =>
        if typ.isFunctionType then
          val args = args0.init match
            case List(one) => apply(one)
            case many      => Sequence(false, many.map(apply(_)))

          val arrow = if typ.isContextFunctionType then "?=>" else "=>"

          Infix(args, arrow, apply(args0.last))
        else if args0.length == 2 && repr.typeSymbol.flags.is(Flags.Infix)
        then Application(apply(base), args0.map(apply(_)), true)
        else if defn.isTupleClass(base.typeSymbol)
        then Sequence(false, args0.map(apply(_)))
        else if base <:< TypeRepr.of[NamedTuple.NamedTuple]
        then args0(0).absolve match
          case AppliedType(_, names) => apply(args0(1)).absolve match
            case Sequence(_, elements) =>
              Sequence
               (false,
                names.zip(elements).map:
                  _.absolve match
                    case (ConstantType(StringConstant(name)), element) =>
                      Named(false, name.tt, element))

          case ref@TypeRef(prefix, name) =>
            apply(ref)

        else Application(apply(base), args0.map(apply(_)), false)

      case ConstantType(constant) => constant.absolve match
        case ByteConstant(byte)     => Primitive(s"$byte.toByte")
        case ShortConstant(short)   => Primitive(s"$short.toShort")
        case IntConstant(int)       => Primitive(int.toString.tt)
        case LongConstant(long)     => Primitive(s"${long}L")
        case BooleanConstant(true)  => Primitive("true")
        case BooleanConstant(false) => Primitive("false")
        case StringConstant(str)    => Primitive(s"\"$str\"")
        case CharConstant(char)     => Primitive(s"'$char'")
        case DoubleConstant(double) => Primitive(s"${double.toString}")
        case FloatConstant(float)   => Primitive(s"${float.toString}F")
        case UnitConstant()         => Primitive("()")
        case NullConstant()         => Primitive("null")
        case ClassOfConstant(cls)   => Application
                                        (Primitive("classOf"), List(apply(cls)), false)

      case Refinement(base, "apply", member) => apply(member)

      case Refinement(base, name, member) =>
        if name == "Self" then Infix(apply(member), "is", apply(base)) else
          val refined: Structural = apply(base) match
            case refined@Structural(base, members, defs) => refined
            case other =>
              Structural(other, ListMap(), ListMap())

          signature(name, member) match
            case signature@Declaration(method, _, _) =>
              if method then refined.copy(terms = refined.terms.updated(name, signature))
              else refined.copy(types = refined.types.updated(name, signature))

      case TypeBounds(lower, upper) => typeBounds(Symbolic("?"), lower, upper)

      case method@MethodType(args0, types, result) =>
        val unnamed = args0.forall(_.startsWith("x$"))

        val args =
          if args0.isEmpty then Sequence(false, Nil)
          else if unnamed then Sequence(false, types.map(apply(_)))
          else Sequence
                (false,
                 args0.zip(types).map { (member, typ) => Named(false, member, apply(typ)) })

        val arrow = if method.isContextual then "?=>" else "=>"
        if unnamed && args0.length == 1
        then Infix(apply(types.head), arrow, apply(result))
        else Infix(args, arrow, apply(result))

      case typ@PolyType(args0, types, result) =>
        val args = args0.zip(types).map:
          case (name, TypeBounds(lower, upper)) =>
            typeBounds(symbolic(name), lower, upper)

        Infix(Sequence(true, args), "=>", apply(result))

      case TypeLambda(args0, bounds, tpe) =>
        val args = args0.zip(bounds).map:
          case (arg, TypeBounds(lower, upper)) => typeBounds(symbolic(arg), lower, upper)

        Infix(Sequence(true, args), "=>>", apply(tpe))

      case ParamRef(binder, n) => binder match
        case TypeLambda(params, _, _) => symbolic(params(n))
        case MethodType(params, _, _) => symbolic(params(n))
        case PolyType(params, _, _)   => symbolic(params(n))
        case other => Primitive("ParamRef")

      case RecursiveType(tpe) => apply(tpe)
      case RecursiveThis(tpe) => Primitive("???")

      case other =>
        Primitive(s"...other: ${other.toString}...")
