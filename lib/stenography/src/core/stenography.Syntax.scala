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
┃    Soundness, version 0.64.0.                                                                    ┃
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

import scala.annotation

import proscenium.compat.*

private def sciList[element](elements: element*): scala.collection.immutable.List[element] =
  scala.collection.immutable.List(elements*)

import scala.quoted.*

import anticipation.*
import denominative.*
import fulminate.*
import rudiments.*
import symbolism.*
import vacuous.*

object Syntax:
  inline def name[designator <: AnyKind]: Text = ${stenography.internal.designator[designator]}

  // Capture sets survive into a macro's view of a type as ordinary annotations, since a macro
  // runs long before the capture checker turns them into capturing types. `T^{a, b}` carries
  // `@retains[a.type | b.type]`, `T^` carries `@retainsCap`, and an impure by-name type
  // carries `@retainsByName`. The refinements a capability can have within a set — reach,
  // read-only and classifier restriction — are annotations on the reference itself. These are
  // compared by full name rather than by symbol so that a compiler version which does not
  // define one of them cannot break the others.
  private val Into = "scala.annotation.internal.$into"
  private val Retains = "scala.annotation.retains"
  private val RetainsCap = "scala.annotation.retainsCap"
  private val Reach = "scala.annotation.internal.reachCapability"
  private val ReadOnly = "scala.annotation.internal.readOnlyCapability"
  private val Only = "scala.annotation.internal.onlyCapability"
  private val RetainsByName = "scala.annotation.retainsByName"
  private val CapSet = "scala.caps.CapSet"

  val Space: Symbolic = Symbolic(" ")
  val Colon: Symbolic = Symbolic(": ")
  val Comma: Symbolic = Symbolic(", ")
  val Open: Symbolic = Symbolic("(")
  val Close: Symbolic = Symbolic(")")
  val OpenType: Symbolic = Symbolic("[")
  val CloseType: Symbolic = Symbolic("]")
  val Plus: Symbolic = Symbolic("+")
  val Minus: Symbolic = Symbolic("-")

  def symbolic(name: Text): Symbolic =
    Symbolic(if name.s.startsWith("_$") then name.s.drop(2).tt else name)

  private def candidateName(using Quotes)(rt: quotes.reflect.RecursiveType): Text =
    import quotes.reflect.*

    def base(repr: TypeRepr): String = repr.absolve match
      case Refinement(parent, _, _) => base(parent)
      case AppliedType(tycon, _)    => base(tycon)
      case TypeRef(_, name)         => name
      case other                    => other.typeSymbol.name

    val raw = base(rt.underlying)

    if raw.isEmpty then "self".tt else (raw.head.toLower.toString + raw.drop(1)).tt

  // The elements of a `@retains[…]` capture set. Several capabilities are joined into a single
  // type argument with `|`, and `Nothing` stands for the empty set.
  private def retained(using Quotes)(annotation: quotes.reflect.TypeRepr)
  :   scala.List[quotes.reflect.TypeRepr] =

    import quotes.reflect.*

    def elements(repr: TypeRepr): scala.List[TypeRepr] = repr.absolve match
      case OrType(left, right) => elements(left) ++ elements(right)

      case repr =>
        if repr.typeSymbol == defn.NothingClass then scala.Nil else sciList(repr)

    annotation.absolve match
      case AppliedType(_, scala.List(argument)) => elements(argument)
      case _                                    => sciList()


  // A single capability within a capture set. Unlike a singleton type in any other position, a
  // capability is written without a `.type` suffix, and the roots are named as the compiler
  // prints them rather than by their qualified paths.
  private def captureRef(using Quotes, Bindings)(repr: quotes.reflect.TypeRepr): Syntax =
    import quotes.reflect.*

    def reference(repr: TypeRepr): Syntax =
      val symbol = repr.termSymbol

      if symbol.exists && symbol.owner.fullName == "scala.caps" then symbol.name match
        case name@("any" | "fresh" | "cap") => Symbolic(name.tt)
        case _                              => plain(repr)
      else plain(repr)

    def plain(repr: TypeRepr): Syntax = apply(repr) match
      case Value(designator) => Simple(designator)
      case syntax            => syntax

    repr.absolve match
      case AnnotatedType(tpe, annotation) =>
        annotation.tpe.typeSymbol.fullName match
          case Reach    => Suffix(captureRef(tpe), "*")
          case ReadOnly => Suffix(captureRef(tpe), ".rd")

          case Only => annotation.tpe.absolve match
            case AppliedType(_, scala.List(classifier)) =>
              val only: List[Syntax] = List(Primitive(".only["), apply(classifier), Primitive("]"))
              Compound(captureRef(tpe) :: only)

            case _ =>
              captureRef(tpe)

          case _ =>
            captureRef(tpe)

      case repr =>
        reference(repr)

  // The capture set attached to one bound of a capture-set parameter, or `Unset` if the bound
  // is trivial (a bare `CapSet`, or the universal set written `CapSet^`).
  private def captureBound(using Quotes, Bindings)(repr: quotes.reflect.TypeRepr)
  :   Optional[List[Syntax]] =

    import quotes.reflect.*

    repr.absolve match
      case AnnotatedType(tpe, annotation) => annotation.tpe.typeSymbol.fullName match
        case Retains    => List.of(retained(annotation.tpe).map(captureRef(_)))
        case RetainsCap => Unset
        case _          => captureBound(tpe)

      case _ =>
        Unset

  private def derivesFromCapSet(using Quotes)(repr: quotes.reflect.TypeRepr): Boolean =
    import quotes.reflect.*

    repr.absolve match
      case AnnotatedType(tpe, _) => derivesFromCapSet(tpe)
      case repr                  => repr.typeSymbol.fullName == CapSet

  // A capture-set parameter, `C^`, is encoded as the bounds `>: CapSet <: CapSet^`. Rendering
  // those bounds literally would expose the encoding, so show the parameter as it was written,
  // with only whichever of its bounds are nontrivial.
  private def captureVarBounds(using Quotes, Bindings)
    ( sub: Syntax, lower: quotes.reflect.TypeRepr, upper: quotes.reflect.TypeRepr )
  :   Optional[Syntax] =

    if !derivesFromCapSet(lower) || !derivesFromCapSet(upper) then Unset else
      // The bounds are written after the `^`, so they compose by concatenation rather than as
      // infix operators, which would parenthesise the `^`.
      def bound(operator: Text, refs: List[Syntax]): List[Syntax] =
        List(Primitive(operator), Sequence('{', refs))

      val lowered = captureBound(lower).lay(Nil): refs =>
        if refs.nil then Nil else bound(" >: ", refs)

      val uppered = captureBound(upper).lay(Nil)(bound(" <: ", _))

      Compound(Capturing(sub, Unset) :: (lowered ::: uppered))


  def typeBounds(using Quotes, Bindings)
    ( sub: Syntax, lower: quotes.reflect.TypeRepr, upper: quotes.reflect.TypeRepr )
  :   Syntax =

    import quotes.reflect.*

    captureVarBounds(sub, lower, upper).or:
      if lower == upper then apply(lower)
      else if lower.typeSymbol == defn.NothingClass && upper.typeSymbol == defn.AnyClass
      then sub
      else if lower.typeSymbol == defn.NothingClass then Infix(sub, "<:", apply(upper))
      else if upper.typeSymbol == defn.AnyClass then Infix(sub, ">:", apply(lower))
      else Infix(Infix(sub, ">:", apply(lower)), "<:", apply(upper))


  def contextBounds(using Quotes, Bindings)(clauses: List[quotes.reflect.ParamClause])
  :   scala.collection.immutable.Map[Text, Syntax] =

    import quotes.reflect.*

    clauses.bind:
      case TermParamClause(defs) =>
        defs.collect:
          case ValDef(name, meta, default) if name.startsWith("evidence$") =>

          apply(meta.tpe) match
            case Infix(Simple(designator), "is", right)          => sciList(designator -> right)
            case Application(Simple(designator), List(right), _) => sciList(designator -> right)
            case _                                               => sciList()

      case _ =>
        sciList()

    . flatMap(elements => elements)
    . group(_(0).name)
    . stdlib
    . view
    . mapValues: bounds =>
        bounds.length match
          case 1 => bounds.prim.vouch(1)
          case _ => Sequence('{', bounds.map(_(1)))

    . to(scala.collection.immutable.Map)


  def clause(using Quotes, Bindings)
    ( clause: quotes.reflect.ParamClause, showUsing: Boolean, context: Map[Text, Syntax] )
  :   Syntax =

    import quotes.reflect.*

    clause.absolve match
      case TermParamClause(termDefs) =>
        val contextual = termDefs.exists(_.symbol.flags.is(Flags.Given))

        val items0 = termDefs.filter:
          case ValDef(name, _, _) if !name.startsWith("evidence$") => true
          case _                                                   => false

        var parens = items0.length != 1 || showUsing

        val items = items0.map: value =>
          value.absolve match
            case valDef@ValDef(name, meta, default) if !name.startsWith("evidence$") =>
              val evidence = name.startsWith("x$")

              val syntax =
                if evidence then apply(meta.tpe)
                else
                  parens = true
                  Named(contextual && showUsing, name.tt, apply(meta.tpe))

              if valDef.symbol.flags.is(Flags.Inline) then Prefix("inline", syntax) else syntax

        if !parens then items(0) else Sequence('(', List.of(items))

      case TypeParamClause(typeDefs) =>
        val items = typeDefs.map:
          case typeDef@TypeDef(name, bounds) =>
            val flags = typeDef.symbol.flags

            val ref = symbolic(name)

            val syntax = bounds match
              case LambdaTypeTree(typeDefs, other) => symbolic(name) // FIXME: todo

              case TypeBoundsTree(lower, upper) =>
                typeBounds(symbolic(name.tt), lower.tpe, upper.tpe)

              case other =>
                symbolic(name)

            context(name.tt).lay(syntax)(Infix(syntax, ": ", _))

        Sequence('[', List.of(items))


  def signature(using Quotes, Bindings)(name: Text, repr: quotes.reflect.TypeRepr): Declaration =
    import quotes.reflect.*

    repr.absolve match
      case MethodType(arguments0, types, result) =>
        val params =
          arguments0.zip(types).map: (argument, tpe) => Named(false, argument, apply(tpe))

        Declaration(true, List(Sequence('(', List.of(params))), apply(result))

      case ByNameType(tpe) =>
        Declaration(true, List(), apply(tpe))

      case TypeBounds(lower, upper) =>
        Declaration(false, Nil, typeBounds(symbolic("?"), lower, upper))

      case TypeLambda(arguments0, bounds, tpe) =>
        val arguments = arguments0.zip(bounds).map:
          case (argument, TypeBounds(lower, upper)) => typeBounds(symbolic(argument), lower, upper)

        Declaration(false, List(Sequence('[', List.of(arguments))), apply(tpe))

      case other =>
        Declaration(true, List(), apply(other))

  // Render a method type as a function type. This is the shape a dependent function type takes
  // inside its `apply` refinement, and the shape a polymorphic function type's body takes.
  private def methodSyntax(using Quotes, Bindings)
    ( method: quotes.reflect.MethodType, refs: List[Syntax], impure: Boolean )
  :   Syntax =

    import quotes.reflect.*

    method.absolve match
      case MethodType(names, types, result) =>
        val unnamed = names.forall(_.startsWith("x$"))

        val parameters =
          if names.isEmpty then Sequence('(', Nil)
          else if unnamed && names.length == 1 then apply(types.head)
          else if unnamed then Sequence('(', List.of(types.map(apply(_))))
          else
            Sequence
              ( '(',
                List.of:
                  names.zip(types).map: (name, typ) =>
                    Named(false, name, apply(typ)) )

        Function(parameters, method.isContextual, impure, refs, apply(result))


  // Render `repr` as a function type, or `Unset` if it is not one. `refs` is the capture set
  // from an enclosing `@retains` annotation, and `universal` records whether that annotation
  // was instead `@retainsCap`, which is how `A => B` reaches here once its `ImpureFunctionN`
  // alias has been expanded.
  private def functionSyntax(using Quotes)(using bindings: Bindings)
    ( repr: quotes.reflect.TypeRepr, refs: List[Syntax], universal: Boolean )
  :   Optional[Syntax] =

    import quotes.reflect.*

    // Without pure functions in scope at the use site, `A => B` is a bare `FunctionN` and
    // there is no such thing as a pure function type to distinguish it from.
    def impure(base: TypeRepr): Boolean =
      universal || !bindings.pureFuns || base.typeSymbol.name.startsWith("Impure")

    repr.absolve match
      case typ@Refinement(base, "apply", method: MethodType) if typ.isFunctionType =>
        methodSyntax(method, refs, impure(base))

      case typ@AppliedType(base, arguments) if typ.isFunctionType =>
        val parameters = arguments.init match
          case scala.List(one) => apply(one)
          case many            => Sequence('(', List.of(many.map(apply(_))))

        val result = apply(arguments.last)

        Function(parameters, typ.isContextFunctionType, impure(base), refs, result)

      case _ =>
        Unset


  def term(using Quotes, Bindings)(repr: quotes.reflect.TermRef): Designator = apply(repr) match
    case Value(value) => value
    case _            => panic(m"expected a Value")


  // A refinement of a single type member can be written with an infix type alias, where one
  // which refines that member is in scope: `Foo { type Form = Bar }` is `Foo in Bar`. Only an
  // alias member qualifies; `Foo { type Form <: Bar }` is not what `Foo in Bar` expands to, so
  // it stays a refinement rather than acquiring a wildcard operand.
  private def infixAlias(using Quotes)(using bindings: Bindings)
    ( base: quotes.reflect.TypeRepr, name: String, member: quotes.reflect.TypeRepr )
  :   Optional[Syntax] =

    import quotes.reflect.*

    if !bindings.infixAliases.contains(name) then Unset else
      val operator = bindings.infixAliases(name)

      member match
        case TypeBounds(lower, upper) if lower == upper =>
          Infix(apply(base), operator, apply(lower))

        case _ =>
          Unset


  def apply(using Quotes)(using bindings: Bindings = Bindings())
    ( repr: quotes.reflect.TypeRepr, retry: Boolean = true )
  :   Syntax =

    bindings.cache.establish(repr):
      import quotes.reflect.*

      def isPackage(name: String): Boolean = name.endsWith("$package") || name == "package"

      repr.absolve match
        case ThisType(ref) =>
          apply(ref) match
            case Simple(Designator.Type(parent, name)) => Simple(Designator.Term(parent, name))
            case syntax                                => syntax

        case typeRef@TypeRef(NoPrefix(), name) =>
          Simple(Designator.Top(name))

        case typeRef@TypeRef(prefix, name) =>
          val module = typeRef.typeSymbol.flags.is(Flags.Module)
          val name2 = if module then name.dropRight(1) else name

          if prefix.typeSymbol.flags.is(Flags.Package)
          then Simple(Designator.Type(Designator(prefix.show.tt), name2))
          else apply(prefix) match
            case value@Value(designator) =>
              if isPackage(name2) then value else Simple(Designator.Type(designator, name2))

            case simple@Simple(designator) =>
              if isPackage(name2) then simple else Simple(Designator.Type(designator, name2))

            case refined@Structural(base, members, defs) =>
              members(name.tt).or(Projection(refined, name.tt))

            case symbolic@Symbolic(_) =>
              Selection(symbolic, name)

            case selection: Selection =>
              Selection(selection, name)

            case other =>
              Primitive("<unknown>")

        case termRef@TermRef(NoPrefix(), name) =>
          Value(Designator.Top(name))

        case termRef@TermRef(ThisType(TypeRef(NoPrefix(), "<root>")), name) =>
          Value(Designator.Top(name))

        case termRef@TermRef(prefix, name) =>
          apply(prefix) match
            case value@Value(designator) =>
              if repr.toString.contains("inline") then System.out.nn.println(name)
              if isPackage(name) then value else Value(Designator.Term(designator, name))

            case simple@Simple(designator) =>
              if isPackage(name) then simple else Value(Designator.Term(designator, name))

            case refined@Structural(base, members, defs) =>
              members(name.tt).or(Projection(refined, name.tt))

            case symbolic@Symbolic(_) =>
              Selection(symbolic, name)

            case selection: Selection =>
              Selection(selection, name)

            case other =>
              Primitive("<unknown>")

        case AnnotatedType(tpe, annotation) =>
          annotation.tpe.typeSymbol.fullName match
            case Into =>
              Prefix("into", apply(tpe))

            // A capture set on a function type belongs in its arrow, and anywhere else it is
            // written after the type with a `^`.
            case Retains =>
              val refs = List.of(retained(annotation.tpe).map(captureRef(_)))
              functionSyntax(tpe, refs, false).or(Capturing(apply(tpe), refs))

            case RetainsCap =>
              functionSyntax(tpe, Nil, true).or(Capturing(apply(tpe), Unset))

            case Reach | ReadOnly | Only =>
              captureRef(repr)

            // Every other annotation, including the markers the capture checker leaves on
            // inferred and declared types, is invisible in the type's source form.
            case _ =>
              apply(tpe)

        case OrType(left, right) =>
          Infix(apply(left), "|", apply(right))

        case AndType(left, right) =>
          Infix(apply(left), "&", apply(right))

        // An impure by-name type, `=> T`, carries a `@retainsByName` annotation; without it the
        // type is the pure by-name type, `-> T`.
        case ByNameType(tpe) =>
          tpe.absolve match
            case AnnotatedType(tpe, annotation)
            if annotation.tpe.typeSymbol.fullName == RetainsByName =>
              Prefix("=>", apply(tpe))

            case _ =>
              Prefix(if bindings.pureFuns then "->" else "=>", apply(tpe))

        case FlexibleType(tpe) =>
          Suffix(apply(tpe), "?")

        case typ@AppliedType(base, arguments0) =>
          if typ.isFunctionType then
            functionSyntax(typ, Nil, false).or(Primitive("<unknown>"))
          else if typ.typeSymbol == defn.RepeatedParamClass
          then
            Suffix(apply(arguments0.head), " *")
          else if arguments0.length == 2 && repr.typeSymbol.flags.is(Flags.Infix)
          then
            Application(apply(base), List.of(arguments0.map(apply(_))), true)
          else if defn.isTupleClass(base.typeSymbol)
          then
            Sequence('(', List.of(arguments0.map(apply(_))))
          else if base <:< TypeRepr.of[NamedTuple.NamedTuple]
          then
            arguments0(0).absolve match
              case AppliedType(_, names) => apply(arguments0(1)).absolve match
                case Sequence(_, elements) =>
                  Sequence
                    ( '(',
                      List.of:
                        names.zip(elements.stdlib).map:
                          _.absolve match
                            case (ConstantType(StringConstant(name)), element) =>
                              Named(false, name.tt, element) )

              case ref@TypeRef(prefix, name) =>
                apply(ref)

          else
            Application(apply(base), List.of(arguments0.map(apply(_))), false)

        case ConstantType(constant) =>
          constant.absolve match
            case ByteConstant(byte)        => Primitive(s"$byte.toByte")
            case ShortConstant(short)      => Primitive(s"$short.toShort")
            case IntConstant(int)          => Primitive(int.toString.tt)
            case LongConstant(long)        => Primitive(s"${long}L")
            case BooleanConstant(true)     => Primitive("true")
            case BooleanConstant(false)    => Primitive("false")
            case StringConstant(string)    => Primitive(s"\"$string\"")
            case CharConstant(char)        => Primitive(s"'$char'")
            case DoubleConstant(double)    => Primitive(s"${double.toString}")
            case FloatConstant(float)      => Primitive(s"${float.toString}F")
            case UnitConstant()            => Primitive("()")
            case NullConstant()            => Primitive("null")

            case ClassOfConstant(cls) =>
              Application(Primitive("classOf"), List(apply(cls)), false)

        // A dependent function type. Its purity is carried by the refined function class, not
        // by the method type, so the two have to be rendered together.
        case typ@Refinement(base, "apply", member) =>
          functionSyntax(typ, Nil, false).or(apply(member))

        case Refinement(base, name, member) =>
          if name == "Self" then Infix(apply(member), "is", apply(base))
          else infixAlias(base, name, member).or:
            val refined: Structural = apply(base) match
              case refined@Structural(base, members, defs) => refined

              case other =>
                Structural(other, Ledger(), Ledger())

            signature(name, member) match
              case signature@Declaration(method, _, _) =>
                if method then refined.copy(terms = refined.terms.updated(name, signature))
                else refined.copy(types = refined.types.updated(name, signature))

        case TypeBounds(lower, upper) =>
          typeBounds(Symbolic("?"), lower, upper)

        case method: MethodType =>
          methodSyntax(method, Nil, !bindings.pureFuns)

        case typ@PolyType(arguments0, types, result) =>
          val arguments = arguments0.zip(types).map:
            case (name, TypeBounds(lower, upper)) =>
              typeBounds(symbolic(name), lower, upper)

          Infix(Sequence('[', List.of(arguments)), "=>", apply(result))

        case TypeLambda(arguments0, bounds, tpe) =>
          val arguments = arguments0.zip(bounds).map:
            case (argument, TypeBounds(lower, upper)) =>
              typeBounds(symbolic(argument), lower, upper)

          Infix(Sequence('[', List.of(arguments)), "=>>", apply(tpe))

        case ParamRef(binder, n) =>
          binder match
            case TypeLambda(params, _, _) => symbolic(params(n))
            case MethodType(params, _, _) => symbolic(params(n))
            case PolyType(params, _, _)   => symbolic(params(n))
            case other                    => Primitive("ParamRef")

        case rt@RecursiveType(body) =>
          bindings.nameFor(rt, candidateName(rt))
          apply(body)

        case RecursiveThis(binder) =>
          Symbolic(bindings.nameFor(binder, candidateName(binder)))

        case MatchType(_, scrutinee, cases) =>
          def renderCase(case0: TypeRepr): Syntax = case0.absolve match
            case TypeLambda(_, _, body) => renderCase(body)
            case other                  => apply(other)

          Syntax.Match(apply(scrutinee), List.of(cases.map(renderCase)))

        case MatchCase(pattern, rhs) =>
          Prefix("case", Infix(apply(pattern), "=>", apply(rhs)))

        case SuperType(_, _) =>
          Symbolic("super")

        case NoPrefix() =>
          Primitive("")

        case repr =>
          if retry then apply(repr.typeSymbol.typeRef, false) else Primitive("<unknown>")

enum Syntax:
  case Simple(designator: Designator)
  case Symbolic(text: Text)
  case Primitive(text: Text)
  case Projection(base: Syntax, text: Text)
  case Structural(syntax: Syntax, types: Ledger[Text, Syntax], terms: Ledger[Text, Syntax])
  case Infix(left: Syntax, middle: Text, right: Syntax)
  case Prefix(middle: Text, right: Syntax)
  case Suffix(left: Syntax, suffix: Text)
  case Application(left: Syntax, elements: List[Syntax], infix: Boolean)
  case Selection(left: Syntax, right: Text)
  case Named(isUsing: Boolean, name: Text, syntax: Syntax)
  case Sequence(style: '(' | '[' | '{', syntaxes: List[Syntax])
  case Declaration(method: Boolean, syntaxes: List[Syntax], result: Syntax)
  case Value(designator: Designator)
  case Compound(syntaxes: List[Syntax])
  case Match(scrutinee: Syntax, cases: List[Syntax])

  // A capture set of `Unset` is the universal one, written `T^`, as distinct from the empty
  // one, written `T^{}`.
  case Capturing(base: Syntax, refs: Optional[List[Syntax]])

  // A function type, whose arrow records both whether the function is pure and, if it has been
  // given one, its capture set.
  case Function
    ( parameters: Syntax,
      contextual: Boolean,
      impure:     Boolean,
      refs:       List[Syntax],
      result:     Syntax )

  def precedence: Int = this match
    case Structural(_, _, _)     => 0
    case Prefix(_, _)            => 0
    case Named(_, _, _)          => 0
    case Suffix(_, _)            => 0
    case Function(_, _, _, _, _) => 4
    case Capturing(_, _)         => 9
    case Match(_, _)             => 10
    case Infix(_, "<:", _)       => 10
    case Infix(_, ">:", _)       => 10
    case Projection(_, _)        => 9
    case Compound(_)             => 10
    case Simple(_)               => 10
    case Symbolic(_)             => 10
    case Primitive(_)            => 10
    case Application(_, _, _)    => 10
    case Selection(_, _)         => 10
    case Sequence(_, _)          => 10
    case Declaration(_, _, _)    => 10
    case Value(_)                => 10

    case Infix(_, middle, _) =>
      middle.s.head match
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

  def qualified: Text = text(using Imports.empty)

  def text(using imports: Imports): Text = this match
    case Simple(designator)      => designator.text
    case Symbolic(text)          => text
    case Projection(base, text)  => s"${base.text}#$text".tt
    case Primitive(text)         => text
    case Selection(left, right)  => s"${left.text}.${right}"
    case Prefix(prefix, base)    => s"$prefix ${base.text}".tt
    case Suffix(base, suffix)    => s"${base.text}$suffix".tt
    case Sequence('(', elements) => s"(${elements.map(_.text).mkString(", ")})".tt
    case Sequence('[', elements) => s"[${elements.map(_.text).mkString(", ")}]".tt
    case Sequence('{', elements) => s"{${elements.map(_.text).mkString(", ")}}".tt
    case Value(designator)       => s"${designator.text}.type".tt
    case Compound(syntaxes)      => syntaxes.map(_.text).mkString.tt

    case Capturing(base, refs) =>
      val base2 = if base.precedence < precedence then Sequence('(', List(base)) else base

      refs.lay(s"${base2.text}^".tt): refs =>
        s"${base2.text}^{${refs.map(_.text).mkString(", ")}}".tt

    case Function(parameters, contextual, impure, refs, result) =>
      // Function types are right-associative, so a function to the left of the arrow needs
      // parentheses where one to the right does not.
      val wrap = parameters.precedence <= precedence
      val left = if wrap then Sequence('(', List(parameters)) else parameters
      val right = if result.precedence < precedence then Sequence('(', List(result)) else result
      val set = if refs.nil then "" else refs.map(_.text).mkString("{", ", ", "}")
      val arrow = s"${if contextual then "?" else ""}${if impure then "=>" else "->"}$set"

      s"${left.text} $arrow ${right.text}".tt

    case Match(scrutinee, cases) =>
      s"${scrutinee.text} match { ${cases.map(_.text).mkString("; ")} }".tt

    case Declaration(method, syntaxes, result) =>
      s"${syntaxes.map(_.text).mkString}${if method then ": " else ""}${result.text}".tt

    case Application(left, elements, infix) =>
      left match
        case Simple(Designator.Type(parent, name)) if infix && imports.has(parent) =>
          elements.stdlib match
            case scala.List(first, second) => Infix(first, name, second).text
            case _ => left.text+elements.map(_.text).mkString("[", ", ", "]").tt

        case _ =>
          left.text+elements.map(_.text).mkString("[", ", ", "]").tt

    case Structural(base, members, defs) =>
      val members2 = members.stdlib.map: (name, syntax) => s"type $name = ${syntax.text}".tt
      val defs2 = defs.stdlib.map: (name, syntax) => s"def $name${syntax.text}".tt
      s"${base.text} { ${(members2 ++ defs2).mkString("; ")} }".tt

    case Infix(left: Syntax, middle, right: Syntax) =>
      val left2 = if left.precedence < precedence then Sequence('(', List(left)) else left
      val right2 = if right.precedence < precedence then Sequence('(', List(right)) else right
      s"${left2.text} $middle ${right2.text}".tt

    case Named(isUsing, name, syntax) =>
      if isUsing then s"using $name: ${syntax.text}".tt else s"$name: ${syntax.text}".tt
