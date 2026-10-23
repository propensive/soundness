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

package degustation

import scala.collection.immutable.Set

import anticipation.*
import gossamer.*
import murmuration.*
import denominative.dysasymptotics.linearSize
import denominative.{nil, size}
import rudiments.{all, at, each, prim}
import vacuous.*

import Pickled.*

object Erasure:
  enum Erased:
    case Ref(name: Text)
    case Array(element: Erased)

  case class Unresolved(detail: Text) extends RuntimeException(detail.s)

  private val Object: Text = t"java.lang.Object"

  private val universal: Set[Text] =
    Set(t"scala.Any", t"scala.AnyVal", t"scala.Matchable", t"scala.Singleton")

  private val specialErasure: Map[Text, Text] = Map(
    t"scala.Any"            -> Object,
    t"scala.Matchable"      -> Object,
    t"scala.caps.Pure"      -> Object,
    t"scala.AnyVal"         -> Object,
    t"scala.Singleton"      -> Object,
    t"scala.Tuple"          -> t"scala.Product",
    t"scala.NonEmptyTuple"  -> t"scala.Product",
    t"scala.*:"             -> Object,
    t"scala.Unit"           -> t"scala.runtime.BoxedUnit")

  private val boxed: Map[Text, Text] = Map(
    t"scala.Boolean" -> t"java.lang.Boolean",
    t"scala.Byte"    -> t"java.lang.Byte",
    t"scala.Short"   -> t"java.lang.Short",
    t"scala.Char"    -> t"java.lang.Character",
    t"scala.Int"     -> t"java.lang.Integer",
    t"scala.Long"    -> t"java.lang.Long",
    t"scala.Float"   -> t"java.lang.Float",
    t"scala.Double"  -> t"java.lang.Double",
    t"scala.Unit"    -> t"scala.runtime.BoxedUnit")

  private def functionClass(arity: Int): Text =
    if arity > 22 then t"scala.runtime.FunctionXXL" else t"scala.Function$arity"

// Scala 3's type erasure, as far as the spelling of an erased signature needs it: the rules of the
// compiler's `TypeErasure` and `Signature` (for Scala 3 source, value classes semi-erased, in
// signature-name mode) over the `Pickled` model, with the universe answering what every name is.
// A member's key disambiguator is `(paramSigs)resultSig`, each a fully-qualified erased class
// name or a type-parameter count — the spelling a consumer's `SIGNED` reference carries, so that
// the two sides of a reference agree by construction.
class Erasure(universe: Universe):
  import Erasure.*

  // The erased signature of a definition: for a method, its clauses' parameters and its result;
  // for anything else — a value, a parameterless method — the signature the compiler gives a
  // non-method, `()`.
  def signature(tree: Tree, scope: Optional[Scope], owner: Optional[ClassInfo]): Text = tree match
    case Tree.DefDef(_, name, clauses, returns, _, _) =>
      def typeClause(clause: List[Tree]): Boolean = clause.prim match
        case Tree.TypeParam(_, _, _, _) => true
        case _                          => false

      // An erased parameter — by modifier, or by having an `erased class` as its type — has no
      // place in the signature.
      val parts = clauses.stdlib.flatMap: clause =>
        if typeClause(clause) then scala.List(clause.size.toString.tt)
        else clause.stdlib.collect:
          case Tree.Param(_, _, tpt, modifiers)
              if !modifiers.has(Flag.Erased) && !erasedParam(Universe.typeOf(tpt), scope) =>
            sigName(Universe.typeOf(tpt), scope)

      // A method with no clause at all has an expression type, whose signature names no result;
      // one with type parameters only is a polymorphic type over its result, which does.
      val result =
        if clauses.nil then t""
        else if name.text == t"<init>" then
          // A constructor's result is its class applied to its own type parameters, which erases
          // as the class does — to its underlying type for a value class, to `Object` for `Array`.
          owner.lay(t"scala.Unit"): info =>
            // A class read from TASTy is referred to by its address; `classRef` spells a binary name.
            val reference =
              if info.address >= 0 then Type.TypeRefDirect(info.address) else Universe.classRef(info.fullName)

            val self =
              if info.typeParamAddresses.nil then reference
              else Type.Applied(reference, info.typeParamAddresses.map(Type.TypeRefDirect(_)))

            render(erase(self, info.scope, vc = true))
        else resultSigName(Universe.typeOf(returns), scope)

      t"(${parts.to(List).join(t",")})$result"

    case _ => t"()"

  // The signature of a pickled methodic type — a refinement's method, say — spelled as a
  // declaration's would be.
  def signatureOf(tpe: Type, scope: Optional[Scope]): Text =
    def parts(tpe: Type): (scala.List[Text], Type) = tpe match
      case Type.Poly(_, names, _, result) =>
        val (rest, last) = parts(result)
        (names.size.toString.tt :: rest, last)

      case Type.Method(_, _, params, result, _) =>
        val (rest, last) = parts(result)
        (params.filter(!erasedParam(_, scope)).map(sigName(_, scope)).stdlib ++ rest, last)

      case other => (scala.Nil, other)

    tpe match
      case Type.Poly(_, _, _, _) | Type.Method(_, _, _, _, _) =>
        val (names, result) = parts(tpe)
        t"(${names.to(List).join(t",")})${resultSigName(result, scope)}"

      case _ => t"()"

  // The signature of a member found by lookup: a Java member's is read off its descriptor; a
  // Scala member's from its declaration in its class's file.
  def signature(owner: ClassInfo, member: Member): Text =
    member.erased.or:
      owner.scope.let: scope => scope.definition(member.address).let(signature(_, scope, owner))
      . or(t"()")

  // A Java member's signature from its descriptor types, which are already erased.
  def javaSignature(params: List[Type], result: Type): Text =
    def render(tpe: Type): Text = tpe match
      case Type.Applied(_, arguments) if arguments.size == 1 => t"${render(arguments.stdlib.head)}[]"
      case other                                             => Universe.className(other).or(t"java.lang.Object")

    t"(${params.map(render).join(t",")})${render(result)}"

  def sigName(tpe: Type, scope: Optional[Scope]): Text = render(erase(tpe, scope, vc = true))

  // A result type erases as a parameter does, except that `Unit` stays `scala.Unit`.
  def resultSigName(tpe: Type, scope: Optional[Scope]): Text =
    universe.classOf(tpe, scope) match
      case info: ClassInfo if info.fullName == t"scala.Unit" => t"scala.Unit"
      case _                                                 => sigName(tpe, scope)

  def render(erased: Erased): Text = erased match
    case Erased.Ref(name)      => name
    case Erased.Array(element) => t"${render(element)}[]"

  // The erasure |T| of a type. `vc` is the compiler's `semiEraseVCs`: whether a value class
  // erases to its underlying type (as a parameter does) or stays itself (as an array element
  // or the underlying of another value class does).
  def erase(tpe: Type, scope: Optional[Scope], vc: Boolean): Erased = tpe match
    case Type.Applied(constructor, arguments) =>
      // Resolution has pinned the arguments to this file and expanded any alias, so the class
      // and the arguments it is really applied to are the resolution's.
      // An applied alias erases as its expansion does — the expansion whole, so a refinement
      // (a polymorphic function type) is not reduced to its parent class on the way.
      val aliased: Optional[Erased] = universe.resolve(constructor, scope) match
        case Resolution.Alias(arity, apply, aliasScope, _, false) if arity == arguments.size =>
          erase(apply(Universe.pin(arguments, scope)), aliasScope.or(scope), vc)

        case _ => Unset

      aliased.or(universe.resolve(tpe, scope) match
        case Resolution.Class(info, arguments, _) => info.fullName.s match
          case "scala.Array" =>
            arguments.prim.lay(Erased.Ref(Object)): element =>
              if isGenericArrayElement(element, scope) then Erased.Ref(Object)
              else Erased.Array(erase(element, scope, vc = false))

          case "scala.*:"         => Erased.Ref(tupleErasure(tpe, scope))
          case "scala.<repeated>" => Erased.Ref(t"scala.collection.immutable.Seq")

          case "scala.&" => arguments.stdlib match
            case scala.List(left, right) => erase(Type.And(left, right), scope, vc)
            case _                       => Erased.Ref(Object)

          case "scala.|" => arguments.stdlib match
            case scala.List(left, right) => erase(Type.Or(left, right), scope, vc)
            case _                       => Erased.Ref(Object)

          case _ => classErasure(info, arguments, vc)

        case Resolution.Alias(_, apply, aliasScope, _, _) => erase(apply(List()), aliasScope, vc)
        case Resolution.Abstract(high, boundScope)        => erase(high, boundScope, vc)
        case Resolution.Term(underlying, termScope)       => erase(underlying, termScope, vc)
        case Resolution.Package(path)                     => throw Unresolved(t"package $path applied")
        case Resolution.Unknown(reason)                   => throw Unresolved(reason))

    case Type.ByName(_)                => Erased.Ref(t"scala.Function0")
    case Type.Annotated(underlying, _) => erase(underlying, scope, vc)
    case Type.Flexible(underlying)     => erase(underlying, scope, vc)
    case Type.Super(_, underlying)     => erase(underlying, scope, vc)
    case Type.Recursive(_, parent)     => erase(parent, scope, vc)

    case Type.Match(bound, scrutinee, cases) =>
      erase(reduceMatch(bound, scrutinee, cases, scope), scope, vc)

    case Type.Bounds(low, high, _)         => erase(high.or(low), scope, vc)
    case Type.Bind(_, _, bounds)           => erase(Universe.upper(bounds), scope, vc)
    case Type.Opaque(alias, _)             => erase(alias, scope, vc)
    case Type.Foreign(path, inner)         => erase(inner, universe.scope(path), vc)
    case Type.Poly(_, _, _, result)        => erase(result, scope, vc)
    case Type.Method(_, _, _, result, _)   => erase(result, scope, vc)
    case Type.Lambda(_, _, _, result)      => erase(result, scope, vc)
    case Type.Abstraction(_, _, _, result) => erase(result, scope, vc)

    case Type.RecursiveThis(address) =>
      scope.let(_.binder(address)).lay(Erased.Ref(Object)):
        case Type.Recursive(_, parent) => erase(parent, scope, vc)
        case _                         => Erased.Ref(Object)

    case Type.And(left, right) =>
      if isPure(left, scope) then erase(right, scope, vc)
      else if isPure(right, scope) then erase(left, scope, vc)
      else erasedGlb(erase(left, scope, vc), erase(right, scope, vc))

    case Type.Or(left, right) =>
      erasedLub(erase(left, scope, vc), erase(right, scope, vc))

    case Type.Refined(parent, name, info) =>
      val polyFunction = universe.classOf(parent, scope).lay(false)(_.fullName == t"scala.PolyFunction")
      if polyFunction && name == t"apply" then Erased.Ref(functionClass(arity(info, scope)))
      else erase(parent, scope, vc)

    case Type.Literal(constant) =>
      universe.classOf(constant.className).lay(Erased.Ref(constant.className)):
        classErasure(_, List(), vc)

    case other => universe.resolve(other, scope) match
      case Resolution.Class(info, arguments, _) => classErasure(info, arguments, vc)

      case Resolution.Alias(arity, apply, aliasScope, _, _) =>
        // An unapplied alias of a type lambda erases as its body does, with its parameters
        // standing for themselves.
        erase(apply(List.fill(arity)(Type.TypeRef(t"Any", Type.TermRefPkg(t"scala")))), aliasScope, vc)

      case Resolution.Abstract(high, boundScope)  => erase(high, boundScope, vc)
      case Resolution.Term(underlying, termScope) => erase(underlying, termScope, vc)
      case Resolution.Package(path)               => throw Unresolved(t"package $path as a type")
      case Resolution.Unknown(reason)             => throw Unresolved(reason)

  // The erasure of a reference to a class: the special classes that erase to `Object` or
  // `Product`, the synthetic function classes, `Unit` as a parameter, a value class to its
  // underlying type, and every other class to itself.
  private def classErasure(info: ClassInfo, arguments: List[Type], vc: Boolean): Erased =
    val name = info.fullName

    specialErasure.at(name).let(Erased.Ref(_)).or:
      Universe.functionArity(name.s) match
        case arity: Int if arity >= 0 => Erased.Ref(functionClass(arity))

        case _ =>
          if vc && info.isValueClass then valueClassErasure(info, arguments)
          else Erased.Ref(name)

  // A derived value class erases to the erasure of its underlying type, with the class's type
  // arguments substituted; a primitive underlying that the class declares generically is boxed.
  private def valueClassErasure(info: ClassInfo, arguments: List[Type]): Erased =
    info.valueParam.lay(Erased.Ref(info.fullName)): underlying =>
      val substituted =
        Universe.replace(underlying, info.typeParamAddresses.stdlib.zip(arguments.stdlib))

      val erased = erase(substituted, info.scope, vc = false)

      val genericPrimitive = universe.classOf(underlying, info.scope).lay(false): cls =>
        Universe.primitives.contains(cls.fullName)

      val genericArray = universe.classOf(underlying, info.scope).lay(false): cls =>
        cls.fullName == t"scala.Array"

      erased match
        case Erased.Ref(name) if Universe.primitives.contains(name) && !genericPrimitive =>
          Erased.Ref(boxed.at(name).or(name))

        case _ if genericArray => erase(underlying, info.scope, vc = false)
        case other             => other

  // Whether an array of `element` erases to `Object`: an abstract element type whose bound admits
  // both primitives and references cannot be stored in any one JVM array.
  private def isGenericArrayElement(element: Type, scope: Optional[Scope]): Boolean =
    // `scala.&` and `scala.|` applied as type constructors are the intersection and union they
    // spell (the compiler's `AndType`/`OrType` have no other pickled form through an alias).
    def andOr(tpe: Type, scope: Optional[Scope]): Optional[Type] = tpe match
      case Type.Applied(head, arguments) => arguments.stdlib match
        case scala.List(left, right) => universe.resolve(head, scope) match
          case Resolution.Class(info, _, _) if info.fullName == t"scala.&" => Type.And(left, right)
          case Resolution.Class(info, _, _) if info.fullName == t"scala.|" => Type.Or(left, right)
          case _                                                           => Unset

        case _ => Unset

      case _ => Unset

    def arrayUpperBound(tpe: Type, scope: Optional[Scope]): Optional[Text] = andOr(tpe, scope).lay(arrayUpperBound0(tpe, scope)):
      arrayUpperBound0(_, scope)

    def arrayUpperBound0(tpe: Type, scope: Optional[Scope]): Optional[Text] = tpe match
      case Type.Foreign(path, inner) => arrayUpperBound(inner, universe.scope(path))

      case Type.And(left, right) =>
        val first = arrayUpperBound(left, scope)
        val second = arrayUpperBound(right, scope)
        if first == second then first else first.or(second)

      case Type.Or(left, right) =>
        val first = arrayUpperBound(left, scope)
        val second = arrayUpperBound(right, scope)
        if first == second then first else Unset

      case Type.Annotated(underlying, _) => arrayUpperBound(underlying, scope)
      case Type.Flexible(underlying)     => arrayUpperBound(underlying, scope)

      // An application is judged by its constructor: an applied alias, opaque or not, by its
      // expansion (the compiler's `translucentSuperType`), never by what the expansion resolves to.
      case Type.Applied(constructor, arguments) => universe.resolve(constructor, scope) match
        case Resolution.Alias(arity, apply, aliasScope, _, _) if arity == arguments.size =>
          arrayUpperBound(apply(Universe.pin(arguments, scope)), aliasScope)

        case Resolution.Abstract(high, boundScope) => arrayUpperBound(high, boundScope)
        case Resolution.Class(info, _, _)          => classBound(info)
        case _                                     => Unset

      case other => universe.resolve(other, scope) match
        case Resolution.Class(info, _, _) => classBound(info)

        case Resolution.Alias(arity, apply, aliasScope, _, _) =>
          val arguments = List.fill(arity)(Type.TypeRef(t"Any", Type.TermRefPkg(t"scala")))
          arrayUpperBound(apply(arguments), aliasScope)

        case Resolution.Abstract(high, boundScope)  => arrayUpperBound(high, boundScope)
        case Resolution.Term(underlying, termScope) => arrayUpperBound(underlying, termScope)
        case _                                      => Unset

    def classBound(info: ClassInfo): Optional[Text] =
      if universal.contains(info.fullName) then Unset
      else if Universe.primitives.contains(info.fullName) then info.fullName
      else Object

    def abstractElement(tpe: Type, scope: Optional[Scope]): Boolean =
      andOr(tpe, scope).lay(abstractElement0(tpe, scope))(abstractElement0(_, scope))

    def abstractElement0(tpe: Type, scope: Optional[Scope]): Boolean = tpe match
      case Type.Foreign(path, inner)     => abstractElement(inner, universe.scope(path))
      case Type.Annotated(underlying, _) => abstractElement(underlying, scope)
      case Type.Flexible(underlying)     => abstractElement(underlying, scope)
      case Type.And(left, right)         => abstractElement(left, scope) || abstractElement(right, scope)
      case Type.Or(left, right)          => abstractElement(left, scope) || abstractElement(right, scope)
      case Type.Bounds(_, _, _)          => true
      case Type.Param(_, _)              => true

      // An applied opaque type constructor is a higher-kinded abstraction to the compiler
      // (`isLambdaSub`), generic unless its expansion fits a JVM array; an applied transparent
      // alias is judged by its expansion, and an applied abstract constructor is abstract.
      case Type.Applied(constructor, arguments) => universe.resolve(constructor, scope) match
        case Resolution.Alias(arity, _, _, _, true) if arity == arguments.size => true

        case Resolution.Alias(arity, apply, aliasScope, _, _) if arity == arguments.size =>
          abstractElement(apply(Universe.pin(arguments, scope)), aliasScope)

        case Resolution.Abstract(_, _) => true
        case _                         => false

      case other => universe.resolve(other, scope) match
        case Resolution.Class(_, _, _) => false

        case Resolution.Alias(arity, apply, aliasScope, _, _) =>
          val arguments = List.fill(arity)(Type.TypeRef(t"Any", Type.TermRefPkg(t"scala")))
          abstractElement(apply(arguments), aliasScope)

        case Resolution.Abstract(_, _)              => true
        case Resolution.Term(underlying, termScope) => abstractElement(underlying, termScope)
        case _                                      => false

    abstractElement(element, scope) && arrayUpperBound(element, scope).absent

  // `*:` chains ending in `EmptyTuple` erase to the tuple class of their arity; other chains to
  // `Product`.
  private def tupleErasure(tpe: Type, scope: Optional[Scope]): Text =
    def arity(tpe: Type, count: Int): Int = tpe match
      case Type.Applied(constructor, List(_, tail)) =>
        universe.classOf(constructor, scope) match
          case info: ClassInfo if info.fullName == t"scala.*:" => arity(tail, count + 1)
          case _                                               => -1

      case other =>
        universe.classOf(other, scope) match
          case info: ClassInfo if info.fullName == t"scala.EmptyTuple$$" => count
          case info: ClassInfo if info.fullName == t"scala.EmptyTuple"   => count
          case _                                                         => -1

    arity(tpe, 0) match
      case -1                   => t"scala.Product"
      case count if count <= 22 => t"scala.Tuple$count"
      case _                    => t"scala.runtime.TupleXXL"

  // The compiler normalizes a match type before erasing it. Full reduction needs subtyping; this
  // reduces the nominal case — a scrutinee that is a class type, matched against class patterns
  // by inheritance — and otherwise falls back to the bound, as the compiler does for an
  // irreducible match.
  private def reduceMatch(bound: Type, scrutinee: Type, cases: List[Type], scope: Optional[Scope])
  :   Type =

    def pattern(matchCase: Type): Optional[(Type, Type)] = matchCase match
      case Type.MatchCase(pattern, rhs)      => (pattern, rhs)
      case Type.Lambda(_, _, _, result)      => pattern(result)
      case Type.Abstraction(_, _, _, result) => pattern(result)
      case _                                 => Unset

    def classOf(tpe: Type): Optional[ClassInfo] = tpe match
      case Type.Bind(_, _, bounds)       => classOf(Universe.upper(bounds))
      case Type.Param(_, _)              => Unset
      case Type.Annotated(underlying, _) => classOf(underlying)
      case other                         => universe.classOf(other, scope)

    universe.classOf(scrutinee, scope).lay(bound): subject =>
      def isAbstract(tpe: Type): Boolean = universe.owner(tpe, scope) match
        case Resolution.Class(_, _, _) => false
        case _                         => true

      val reduced = cases.stdlib.view.map(pattern).flatMap(_.lay(scala.List())(scala.List(_))).map:
        (pattern, rhs) =>
          val stripped = pattern match
            case Type.Bind(_, _, bounds) => Universe.upper(bounds)
            case other                   => other

          // A case matches when its pattern's class is one the scrutinee's class derives from, and
          // is skipped when the two are unrelated classes; anything else — a pattern that is a
          // subclass of the scrutinee, or an abstract one — ends the attempt, since whether it
          // matches cannot be decided here.
          classOf(stripped) match
            case candidate: ClassInfo if !isAbstract(stripped) =>
              if universe.derivesFrom(subject, candidate.fullName) then Some(rhs)
              else if universe.derivesFrom(candidate, subject.fullName) then Some(Unset)
              else None

            case _ => Some(Unset)

      . collectFirst { case Some(outcome) => outcome }

      reduced match
        case Some(rhs: Type) => rhs
        case _               => bound

  private def isPure(tpe: Type, scope: Optional[Scope]): Boolean =
    universe.classOf(tpe, scope).lay(false)(_.fullName == t"scala.caps.Pure")

  // The parameter count of a pickled method type, without its erased parameters.
  private def arity(methodic: Type, scope: Optional[Scope]): Int = methodic match
    case Type.Poly(_, _, _, result)      => arity(result, scope)
    case Type.Method(_, _, params, _, _) => params.stdlib.count(!erasedParam(_, scope))
    case _                               => 0

  // Whether a parameter of this type is erased: the compiler marks one by annotating its type
  // `@ErasedParam`, and a parameter whose type is an `erased class` is erased by that alone.
  private def erasedParam(tpe: Type, scope: Optional[Scope]): Boolean = tpe match
    case Type.Annotated(underlying, annotation) =>
      val erasedAnnotation = Universe.className(Universe.parentType(annotation)).lay(false): name =>
        name == t"scala.annotation.internal.ErasedParam"

      erasedAnnotation || erasedParam(underlying, scope)

    case other =>
      // An erased class: declared `erased`, or extending the `scala.compiletime.Erased` marker.
      universe.classOf(other, scope).lay(false): info =>
        info.flags.contains(Flag.Erased) || universe.derivesFrom(info, t"scala.compiletime.Erased")

  // --- the erased lattice -----------------------------------------------------------------
  private def isTrait(name: Text): Boolean = universe.classOf(name).lay(false)(_.isTrait)

  private def isSubClass(name: Text, base: Text): Boolean =
    name == base || universe.classOf(name).lay(false)(universe.derivesFrom(_, base))

  private def baseClasses(name: Text): List[Text] =
    universe.classOf(name).lay(List(name))(universe.linearization)

  // The erased greatest lower bound picks one of its two arguments by the compiler's total order:
  // arrays before non-arrays, primitives before others, classes before traits, subclasses before
  // superclasses, then by name.
  def erasedGlb(left: Erased, right: Erased): Erased =
    if compareGlb(left, right) <= 0 then left else right

  private def compareGlb(left: Erased, right: Erased): Int = (left, right) match
    case (Erased.Array(first), Erased.Array(second)) => compareGlb(first, second)
    case (Erased.Array(_), _)                        => -1
    case (_, Erased.Array(_))                        => 1

    case (Erased.Ref(first), Erased.Ref(second)) =>
      def compareClasses: Int =
        if isSubClass(first, second) then -1
        else if isSubClass(second, first) then 1
        else first.s.compareTo(second.s)

      val primitive1 = Universe.primitives.contains(first)
      val primitive2 = Universe.primitives.contains(second)

      if primitive1 && primitive2 then compareClasses
      else if primitive1 then -1
      else if primitive2 then 1
      else
        val real1 = !isTrait(first)
        val real2 = !isTrait(second)
        if real1 && real2 then compareClasses
        else if real1 then -1
        else if real2 then 1
        else compareClasses

  // The erased least upper bound: `Nothing` and `Null` vanish against reference types, arrays
  // join elementwise, and two classes join at the last minimal common base class, preferring
  // classes over traits — or `Object`.
  def erasedLub(left: Erased, right: Erased): Erased = (left, right) match
    case (Erased.Ref(first), other) if first == t"scala.Nothing"   => other
    case (other, Erased.Ref(second)) if second == t"scala.Nothing" => other
    // `Null | C` is `C` only for a `C` that derives from `Object`: a universal trait does not.
    case (Erased.Ref(first), other) if first == t"scala.Null" && derivesFromObject(other)   => other
    case (other, Erased.Ref(second)) if second == t"scala.Null" && derivesFromObject(other) => other

    case (Erased.Array(first), Erased.Array(second)) =>
      (first, second) match
        case (Erased.Ref(a), Erased.Ref(b)) if Universe.primitives.contains(a) || Universe.primitives.contains(b) =>
          if a == b then Erased.Array(first) else Erased.Ref(Object)

        case _ => Erased.Array(erasedLub(first, second))

    case (Erased.Array(_), _) | (_, Erased.Array(_)) => Erased.Ref(Object)

    case (Erased.Ref(first), Erased.Ref(second)) =>
      val superclasses =
        baseClasses(first).filter(_ != t"scala.caps.Pure").filter(isSubClass(second, _))

      // The candidates are the leading classes and the first trait: the compiler's `takeUntil`
      // keeps going only through non-traits, and stops after the first trait it meets.
      def takeUntil(remaining: scala.List[Text], taken: scala.List[Text]): scala.List[Text] =
        remaining.headOption match
          case Some(head) =>
            if isTrait(head) then (head :: taken).reverse else takeUntil(remaining.tail, head :: taken)

          case None => taken.reverse

      val candidates = takeUntil(superclasses.stdlib, scala.List())

      val minimums = candidates.filter: candidate =>
        candidate != t"scala.*:" && candidates.forall: x => !isSubClass(x, candidate) || x == candidate

      minimums.lastOption match
        case Some(lub) => universe.classOf(lub).lay(Erased.Ref(lub))(classErasure(_, List(), vc = true))
        case None      => Erased.Ref(Object)

  private def derivesFromObject(erased: Erased): Boolean = erased match
    case Erased.Array(_)  => true

    case Erased.Ref(name) =>
      isReference(erased) && (name == Object || baseClasses(name).stdlib.contains(Object))

  private def isReference(erased: Erased): Boolean = erased match
    case Erased.Array(_)  => true
    case Erased.Ref(name) => !Universe.primitives.contains(name) && name != t"scala.Nothing" && name != t"scala.Null"

