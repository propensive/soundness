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
import scala.collection.mutable

import anticipation.*
import gossamer.*
import murmuration.*
import denominative.dysasymptotics.linearSize
import denominative.{nil, size}
import rudiments.{at, each, prim}
import vacuous.*

import Pickled.*

object ClassSource:
  // The first source that has an entry answers for it.
  def apply(sources: ClassSource*): ClassSource = new ClassSource:
    private val all = sources.to(List)

    def read(path: Text): Optional[Data] =
      all.stdlib.view.map(_.read(path)).find(_.present).getOrElse(Unset)

    def list(directory: Text): List[Text] =
      all.stdlib.flatMap(_.list(directory).stdlib).distinct.to(List)


// Where a universe finds its classes: a classpath reduced to two questions, so that the same
// semantic layer runs wherever the bytes can be fetched from — directories and jars on the JVM,
// a bundle in a browser, a linked-in archive in a native binary. Paths are slash-separated and
// relative, as classfiles and `.tasty` files are named: `scala/collection/immutable/List.tasty`.
trait ClassSource:
  def read(path: Text): Optional[Data]
  def list(directory: Text): List[Text]

// A loaded `.tasty` file: its trees; the fully-qualified name of every definition in it that a
// reference from outside a term body can reach, by address; the definitions inside term bodies,
// which are local; and every definition's owner — the address of the definition enclosing it, or
// the name of its package at the top.
case class Scope
  ( path:    Text,
    file:    Unpickler.File,
    names:   Map[Int, Text],
    locals:  Set[Int],
    owners:  Map[Int, Int | Text] ):

  def definition(address: Int): Optional[Tree] = file.definitions.at(address)
  def binder(address: Int): Optional[Type] = file.binders.at(address)

  // Whether the definition at `address` is enclosed, at any depth, by the one at `enclosing`.
  def within(address: Int, enclosing: Int): Boolean = owners.at(address).lay(false):
    case owner: Int => owner == enclosing || within(owner, enclosing)
    case _          => false

  // The nearest enclosing class of a definition, by address, if it is inside one.
  def enclosingClass(address: Int): Optional[Int] = owners.at(address).lay(Unset):
    case owner: Int =>
      definition(owner) match
        case Tree.TypeDef(_, _, _: Tree.Template, _) => owner
        case _                                       => enclosingClass(owner)

    case _ => Unset

enum Kind:
  case Val, Def, Type, Class

// One member of a class, by declaration: its name, kind and declared type — for a method, the
// methodic type its clauses spell — and, for a Java member, the erased signature its descriptor
// already is. `address` finds the declaring tree in the class's scope.
case class Member
  ( name:    Text,
    kind:    Kind,
    tpe:     Type,
    flags:   Set[Flag],
    address: Int,
    erased:  Optional[Text] )

// A class as the universe knows it, from its TASTy or its classfile.
case class ClassInfo
  ( fullName:   Text,
    flags:      Set[Flag],
    typeParams: List[(Text, Type)],
    // The addresses of the type parameters' definitions, which the class's own types refer to.
    typeParamAddresses: List[Int],
    parents:    List[Type],
    members:    List[Member],
    valueParam: Optional[Type],
    scope:      Optional[Scope],
    java:       Boolean,
    // The address of the class's definition in its scope; -1 for a class without TASTy.
    address:    Int = -1 ):

  // The package the class is in, as its scope records it: the owner of a top-level class, or of
  // the top-level class enclosing a nested one.
  def pkg: Text = scope.lay(fullName.s.lastIndexOf('.') match
    case -1    => t""

    case index => fullName.s.substring(0, index).nn.tt): scope =>
      def owner(address: Int): Text = scope.owners.at(address) match
        case pkg: Text   => pkg
        case parent: Int => owner(parent)
        case _           => t""

      owner(address)

  def isTrait: Boolean = flags.contains(Flag.Trait)
  def isObject: Boolean = flags.contains(Flag.Object)

  // The value class rule the compiler applies: a class extending `AnyVal` that is not one of the
  // primitives (nor `AnyVal` or `Null` themselves).
  def isValueClass: Boolean = valueParam.present && !Universe.primitives.contains(fullName)

// What a type reference turns out to name.
enum Resolution:
  // A class, with the type arguments it was applied to, if any.
  case Class(info: ClassInfo, arguments: List[Type], scope: Optional[Scope])
  // A type alias (opaque or not): `apply` substitutes arguments into its right-hand side, and
  // `body` is that right-hand side as declared (a lambda, when parameterized).
  case Alias
    ( arity:  Int,
      apply:  List[Type] => Type,
      scope:  Optional[Scope],
      body:   Optional[Type] = Unset,
      opaque: Boolean = false )

  // An abstract type or type parameter, known only by its upper bound.
  case Abstract(high: Type, scope: Optional[Scope])
  // A term, known by its declared type.
  case Term(tpe: Type, scope: Optional[Scope])
  case Package(path: Text)
  case Unknown(reason: Text)

// The classes a module's TASTy can reach, loaded lazily from a `ClassSource`, and the semantic
// questions over them that erasure and member lookup ask: what a name refers to, what a class's
// parents and members are, how it linearizes. Nothing here subtypes, infers or substitutes beyond
// an alias's parameters — the two questions the `tasty/1` discipline cannot read off the bytes
// need no more than this.
class Universe(source: ClassSource):
  private val scopes = mutable.HashMap[Text, Optional[Scope]]()
  private val classes = mutable.HashMap[Text, Optional[ClassInfo]]()
  private val linearizations = mutable.HashMap[Text, List[Text]]()
  private val linearizing = mutable.HashSet[Text]()

  lazy val erasure: Erasure = Erasure(this)

  // --- loading ------------------------------------------------------------------------------

  // The scope of the `.tasty` file at `path`, loaded once.
  def scope(path: Text): Optional[Scope] =
    scopes.getOrElseUpdate(path, source.read(path).let { data => Universe.index(path, Unpickler(data)) })

  // A `.tasty` file supplied directly — the module being atomized — rather than found on the
  // classpath; its classes become resolvable like any other.
  def enter(path: Text, data: Data): Scope =
    val scope = Universe.index(path, Unpickler(data))
    scopes(path) = scope
    scope

  // The class named `fullName`, from the first of its TASTy or its classfile that exists. A nested
  // class lives in its top-level class's TASTy, so every split of the name between package and
  // class segments is tried, longest package first.
  def classOf(fullName: Text): Optional[ClassInfo] =
    classes.getOrElseUpdate(fullName, Universe.synthetic(fullName).or(load(fullName)))

  private def load(fullName: Text): Optional[ClassInfo] =
    // A class's simple name may itself contain dots (a top-level definitions' carrier is named
    // after its source file, `rudiments.Scribe$package`), so every way of reading the name as
    // package segments, a top-level class name and nested class names is tried, longest package
    // first; a module class `X$` lives in `X.tasty`.
    val segments = fullName.cut(t".").stdlib
    var result: Optional[ClassInfo] = Unset
    var split = segments.length - 1

    while result.absent && split >= 0 do
      val directory = segments.take(split).to(List).join(t"/")
      val rest = segments.drop(split)
      var width = rest.length
      var scalaCompiled = false

      while result.absent && width >= 1 do
        val top = rest.take(width).to(List).join(t".")
        val nested = rest.drop(width)
        val stripped = if top.s.endsWith("$") then top.s.dropRight(1).nn.tt else top
        val file = Universe.encode(stripped)
        val tastyPath = if directory == t"" then t"$file.tasty" else t"$directory/$file.tasty"
        val found = scope(tastyPath)
        if found.present then scalaCompiled = true
        result = found.let(classIn(_, fullName, top :: nested))
        width -= 1

      // A classfile is Java's only where no TASTy covers the name: a Scala object's static
      // forwarders are a classfile too, and must not pass for a Java class.
      if result.absent && !scalaCompiled then
        // A nested Java class is named `Outer$.Inner` when static (its owner is the module
        // class); its classfile is `Outer$Inner` either way.
        val encoded = rest.map(Universe.encode)
        val flat = (encoded.init.map(_.s.stripSuffix("$")) :+ encoded.last.s).mkString("$").tt
        val classPath = if directory == t"" then t"$flat.class" else t"$directory/$flat.class"
        result = source.read(classPath).let: data => fromClassfile(Classfile(data))

      split -= 1

    result

  // The class at `path` (the class names from the top-level class inwards) in a loaded file.
  private def classIn(scope: Scope, fullName: Text, path: scala.List[Text]): Optional[ClassInfo] =
    def search(statements: scala.List[Tree], remaining: scala.List[Text]): Optional[ClassInfo] =
      remaining.headOption match
        case None => Unset

        case Some(name) =>
          val rest = remaining.tail

          statements.collectFirst:
            case tree @ Tree.TypeDef(address, `name`, template: Tree.Template, _) =>
              if rest.isEmpty then classInfo(scope, fullName, tree, template)
              else search(template.body.stdlib, rest)

          . getOrElse(Unset)

    def topLevel(statements: scala.List[Tree]): scala.List[Tree] = statements.flatMap:
      case Tree.Package(_, inner) => topLevel(inner.stdlib)
      case other                  => scala.List(other)

    search(topLevel(scope.file.statements.stdlib), path)

  private def classInfo(scope: Scope, fullName: Text, typeDef: Tree.TypeDef, template: Tree.Template)
  :   ClassInfo =

    val typeParams = template.typeParams.stdlib.collect:
      case Tree.TypeParam(_, name, bounds, _) => (name, Universe.typeOf(bounds))

    . to(List)

    val typeParamAddresses = template.typeParams.stdlib.collect:
      case Tree.TypeParam(address, _, _, _) => address

    . to(List)

    val parents = template.parents.map(Universe.parentType)

    // The class's declarations: its type parameters and constructor parameters — which the
    // compiler enters as members too — and the definitions of its body.
    val declarations =
      template.typeParams.stdlib ++ template.params.stdlib ++ template.body.stdlib

    val members = declarations.flatMap:
      case Tree.TypeParam(address, name, bounds, modifiers) =>
        scala.List(Member(name, Kind.Type, Universe.typeOf(bounds), modifiers.flags, address, Unset))

      case Tree.Param(address, name, tpt, modifiers) =>
        scala.List(Member(name.spelled, Kind.Val, Universe.typeOf(tpt), modifiers.flags + Flag.ParamAccessor,
            address, Unset))

      // A term without a right-hand side is abstract (`Deferred`).
      case Tree.ValDef(address, name, tpt, rhs, modifiers) =>
        val flags = if rhs.absent then modifiers.flags + Flag.Abstract else modifiers.flags
        scala.List(Member(name.spelled, Kind.Val, Universe.typeOf(tpt), flags, address, Unset))

      case Tree.DefDef(address, name, clauses, returns, rhs, modifiers) =>
        val tpe = Universe.methodType(address, clauses, returns)
        val abstractMethod = rhs.absent && name.text != t"<init>"
        val flags = if abstractMethod then modifiers.flags + Flag.Abstract else modifiers.flags
        scala.List(Member(name.spelled, Kind.Def, tpe, flags, address, Unset))

      case Tree.TypeDef(address, name, _: Tree.Template, modifiers) =>
        scala.List(Member(name, Kind.Class, Type.TypeRefDirect(address), modifiers.flags, address, Unset))

      case Tree.TypeDef(address, name, rhs, modifiers) =>
        scala.List(Member(name, Kind.Type, Universe.typeOf(rhs), modifiers.flags, address, Unset))

      case _ => scala.List()

    . to(List)

    val valueParam: Optional[Type] =
      val extendsAnyVal = parents.prim.let(Universe.className).lay(false)(_ == t"scala.AnyVal")

      if extendsAnyVal then template.params.prim.let:
        case Tree.Param(_, _, tpt, _) => Universe.typeOf(tpt)
        case _                        => Unset

      else Unset

    ClassInfo(fullName, typeDef.modifiers.flags, typeParams, typeParamAddresses, parents, members,
        valueParam, scope, java = false, typeDef.address)

  private def fromClassfile(classfile: Classfile.Class): ClassInfo =
    val flags = if classfile.isInterface then Set(Flag.Trait) else Set[Flag]()

    // `java.lang.Object` is the compiler's `AnyRef`: it extends `Any` and carries the members the
    // compiler gives every reference type.
    val isObject = classfile.name == t"java.lang.Object"

    // A Java interface derives from `Object`, as the compiler's classfile parser reads its
    // superclass entry (unlike a Scala trait), which is what lets `Interface | Null` erase to the
    // interface — except `Comparable`, `Cloneable` and `Serializable`, which the parser treats
    // as universal traits.
    val universal = Set(t"java.lang.Comparable", t"java.lang.Cloneable", t"java.io.Serializable")
    val any = Type.TypeRef(t"Any", Type.TermRefPkg(t"scala"))

    val parents =
      if isObject then List(any)
      else
        val superclass =
          if universal.contains(classfile.name) then scala.List(any)
          else classfile.superclass.let(Universe.classRef).lay(scala.List())(scala.List(_))

        (superclass ++ classfile.interfaces.map(Universe.classRef).stdlib).to(List)

    // A static member belongs to the class's module class in the compiler's model.
    def flagsOf(access: Int): Set[Flag] =
      (if (access & 0x0008) != 0 then Set(Flag.Static) else Set[Flag]())
      ++ (if (access & 0x0400) != 0 then Set(Flag.Abstract) else Set[Flag]())

    val fields = classfile.fields.map: field =>
      val tpe = Classfile.Descriptor.field(field.descriptor)
      Member(field.name, Kind.Val, tpe, flagsOf(field.flags), -1, t"()")

    // Bridges and synthetic methods (mixin forwarders in a Scala 2 trait's classfile, say) are
    // not members the compiler sees.
    val methods = classfile.methods.filter { method => (method.flags & 0x1040) == 0 }.map: method =>
      val params = Classfile.Descriptor.parameters(method.descriptor)
      val result = Classfile.Descriptor.result(method.descriptor)
      val tpe = Type.Method(-1, params.map { _ => t"" }, params, result, Clause.Plain)
      // A constructor's signature names its class as the result, as a Scala constructor's does.
      val signatureResult = if method.name == t"<init>" then Universe.classRef(classfile.name) else result
      val signature = erasure.javaSignature(params, signatureResult)
      Member(method.name, Kind.Def, tpe, flagsOf(method.flags), -1, signature)

    // The compiler gives `java.lang.String` a `+` of its own (`String_+`).
    val stringPlus =
      if classfile.name == t"java.lang.String" then
        val tpe = Type.Method(-1, List(t"x"), List(Universe.classRef(t"scala.Any")),
            Universe.classRef(t"java.lang.String"), Clause.Plain)

        scala.List(Member(t"+", Kind.Def, tpe, Set(), -1, t"(java.lang.Object)java.lang.String"))

      else scala.List()

    val builtins = (if isObject then Universe.objectMembers.stdlib else scala.List()) ++ stringPlus
    val members = (builtins ++ fields.stdlib ++ methods.stdlib).to(List)

    // A nested class belongs to its outer class, or to the outer class's module class when it
    // is static, as the compiler's classfile parser places it.
    val fullName = classfile.outer.lay(classfile.name): outer =>
      val simple = classfile.name.s.substring(outer.s.length + 1).nn.tt
      val outerName = classOf(outer).let(_.fullName).or(outer)
      if classfile.static then t"$outerName$$.$simple" else t"$outerName.$simple"

    ClassInfo(fullName, flags, List(), List(), parents, members, Unset, Unset, java = true)

  // --- resolution ---------------------------------------------------------------------------

  // What `tpe` refers to, following local references in `scope`.
  def resolve(tpe: Type, scope: Optional[Scope]): Resolution = tpe match
    case Type.TypeRefPkg(name) =>
      classOf(name).lay(Resolution.Package(name))(Resolution.Class(_, List(), Unset))

    case Type.TermRefPkg(name) => Resolution.Package(name)

    case Type.TypeRef(name, prefix)      => typeMember(prefix, name, scope)
    case Type.TypeRefIn(name, prefix, _) => typeMember(prefix, name, scope)

    case Type.TermRef(name, prefix)      => termMember(prefix, name, scope)
    case Type.TermRefIn(name, prefix, _) => termMember(prefix, name, scope)

    case Type.TypeRefDirect(address)   => local(address, scope)
    case Type.TypeRefLocal(address, _) => local(address, scope)
    case Type.TermRefDirect(address)   => local(address, scope)
    case Type.TermRefLocal(address, _) => local(address, scope)

    case Type.This(cls) => resolve(cls, scope)

    // A type application of a method reference resolves as the instantiated method does.
    case Type.Applied(Type.TermRef(_, _) | Type.TermRefIn(_, _, _) | Type.TermRefDirect(_)
        | Type.TermRefLocal(_, _) | Type.AppliedTerms(_, _), _) =>
      val (applied, appliedScope) = applicationType(tpe, scope)
      if applied == tpe then Resolution.Unknown(t"an unresolved type application") else resolve(applied, appliedScope)

    case Type.Applied(constructor, arguments) =>
      // The arguments leave this file with the class or alias they are applied to, so they are
      // pinned to it; an applied alias resolves to whatever its expansion does.
      val pinned = Universe.pin(arguments, scope)

      resolve(constructor, scope) match
        case Resolution.Class(info, _, classScope) => Resolution.Class(info, pinned, classScope)

        case Resolution.Alias(_, apply, aliasScope, _, _) =>
          // An expansion that is not itself nominal — a union, say — is handed back as an alias
          // of itself, for the caller to take apart.
          val expansion = apply(pinned)

          resolve(expansion, aliasScope) match
            case Resolution.Unknown(_) => Resolution.Alias(0, _ => expansion, aliasScope)
            case other                 => other

        case other => other

    case Type.Foreign(path, inner)     => resolve(inner, this.scope(path))

    // An application of a method reference resolves as its result does, with the arguments in
    // place of the parameters.
    case Type.AppliedTerms(_, _) =>
      val (applied, appliedScope) = applicationType(tpe, scope)
      if applied == tpe then Resolution.Unknown(t"an unresolved term application")
      else resolve(applied, appliedScope)

    case Type.Annotated(underlying, _) => resolve(underlying, scope)
    case Type.Flexible(underlying)     => resolve(underlying, scope)
    case Type.ByName(underlying)       => resolve(underlying, scope)
    // A methodic type as a term's type: what matters of it is what applying it yields.
    case Type.Method(_, _, _, result, _) => resolve(result, scope)
    case Type.Poly(_, _, _, result)      => resolve(result, scope)
    case Type.Opaque(alias, _)           => resolve(alias, scope)
    case Type.Bounds(low, high, _)       => resolve(high.or(low), scope)
    case Type.Match(bound, _, _)         => resolve(bound, scope)
    case Type.Bind(_, _, bounds)         => resolve(Universe.upper(bounds), scope)
    case Type.Lambda(_, _, _, _)         => aliasOrAbstract(tpe, scope)
    case Type.Abstraction(_, _, _, _)    => aliasOrAbstract(tpe, scope)
    case Type.Recursive(_, parent)       => resolve(parent, scope)
    case Type.Super(_, underlying)       => resolve(underlying, scope)
    case Type.Refined(parent, _, _)      => resolve(parent, scope)

    case Type.Param(binder, number) =>
      scope.let(_.binder(binder)).lay(Resolution.Unknown(t"unknown binder $binder")):
        case Type.Poly(_, _, bounds, _)   => bound(bounds, number, scope)
        case Type.Lambda(_, _, bounds, _) => bound(bounds, number, scope)

        case Type.Method(_, _, params, _, _) =>
          Universe.nth(params, number).lay(Resolution.Unknown(t"no parameter $number")):
            Resolution.Term(_, scope)

        case _ => Resolution.Unknown(t"binder $binder is not methodic")

    case Type.Literal(constant) =>
      classOf(constant.className).lay(Resolution.Unknown(constant.className)):
        Resolution.Class(_, List(), Unset)

    case other => Resolution.Unknown(t"unresolvable ${other.getClass.getSimpleName.nn}")

  private def bound(bounds: List[Type], number: Int, scope: Optional[Scope]): Resolution =
    Universe.nth(bounds, number).lay(Resolution.Unknown(t"no bound $number")): bound =>
      Resolution.Abstract(Universe.upper(bound), scope)

  // A definition in the current file, by address.
  private def local(address: Int, scope: Optional[Scope]): Resolution =
    scope.let: scope =>
      scope.definition(address).let:
        case Tree.TypeDef(_, name, _: Tree.Template, _) =>
          scope.names.at(address).let(classOf(_)).lay(Resolution.Unknown(t"local class $name")):
            Resolution.Class(_, List(), scope)

        case Tree.TypeDef(_, _, rhs, modifiers) =>
          opaqueIf(aliasOrAbstract(Universe.typeOf(rhs), scope), modifiers.has(Flag.Opaque))

        case Tree.TypeParam(_, _, bounds, _) => Resolution.Abstract(Universe.upper(Universe.typeOf(bounds)), scope)
        case Tree.ValDef(_, _, tpt, _, _)    => Resolution.Term(Universe.typeOf(tpt), scope)
        case Tree.Param(_, _, tpt, _)        => Resolution.Term(Universe.typeOf(tpt), scope)

        case Tree.DefDef(address, _, clauses, returns, _, _) =>
          Resolution.Term(Universe.methodType(address, clauses, returns), scope)

        case Tree.Bind(_, _, tpe, _, _) => Resolution.Term(tpe, scope)
        case other                      => Resolution.Unknown(t"local ${other.getClass.getSimpleName.nn}")

      . or(Resolution.Unknown(t"no definition at $address"))

    . or(Resolution.Unknown(t"no scope for local $address"))

  // A type member's declared type as a resolution: an alias of its right-hand side, possibly
  // parameterized, or an abstract type known by its upper bound.
  private def aliasOrAbstract(tpe: Type, scope: Optional[Scope]): Resolution = tpe match
    // Bounds with no upper bound are an alias of the lower, which may itself be a lambda.
    case Type.Bounds(low, Unset, _) => aliasOrAbstract(low, scope)

    case Type.Bounds(_, high, _) =>
      high.lay(Resolution.Unknown(t"bounds without bounds")): high =>
        Resolution.Abstract(high, scope)

    // A parameterized opaque type is a lambda over an opaque body.
    case Type.Lambda(address, names, bounds, result) =>
      Resolution.Alias(names.size, arguments => Universe.substitute(result, address, arguments),
          scope, tpe, result.isInstanceOf[Type.Opaque])

    case Type.Abstraction(params, _, _, result) =>
      Resolution.Alias(params.size, arguments =>
          Universe.replace(result, params.stdlib.zip(arguments.stdlib)), scope, tpe,
          result.isInstanceOf[Type.Opaque])

    case Type.Opaque(alias, _) =>
      aliasOrAbstract(alias, scope) match
        case resolution: Resolution.Alias => resolution.copy(opaque = true)
        case other                        => other

    case other => Resolution.Alias(0, _ => other, scope)

  // A prefix that is an intersection, spelled as such or as an application of `scala.&`, has the
  // members of its parts; the first part that has the member provides it. A union is taken the
  // same way, for want of the compiler's join.
  def parts(prefix: Type, scope: Optional[Scope]): Optional[(Type, Type)] = prefix match
    case Type.And(left, right) => (left, right)
    case Type.Or(left, right)  => (left, right)
    // Resolved as a whole, so that an alias of `scala.&` or `scala.|` (proscenium's) counts too;
    // the resolution's arguments are the parts, in the class's own order.
    case Type.Applied(_, arguments) if arguments.size == 2 => resolve(prefix, scope) match
      case Resolution.Class(info, resolved, _) if info.fullName == t"scala.&" || info.fullName == t"scala.|" =>
        resolved.stdlib match
          case scala.List(left, right) => (left, right)
          case _                       => (arguments.stdlib(0), arguments.stdlib(1))

      case _ => Unset

    case _ => Unset

  private def typeMember(prefix: Type, name: Text, scope: Optional[Scope]): Resolution =
    parts(prefix, scope).let: (left, right) =>
      typeMember(left, name, scope) match
        case Resolution.Unknown(_) => typeMember(right, name, scope)
        case found                 => found

    . or:
      typeMember0(prefix, name, scope)

  // The addresses of the parameters of the clause of the definition at `address` whose names are
  // `names`, or none when the binder is not a definition's (a synthetic member's, say).
  def clauseAddresses(address: Int, names: List[Text], scope: Optional[Scope], typeClause: Boolean)
  :   scala.List[Int] =

    scope.let(_.definition(address)) match
      case Tree.DefDef(_, _, clauses, _, _, _) =>
        def nameOf(tree: Tree): Text = tree match
          case Tree.Param(_, name, _, _)     => name.text
          case Tree.TypeParam(_, name, _, _) => name
          case _                             => t""

        def addressOf(tree: Tree): Int = tree match
          case Tree.Param(address, _, _, _)     => address
          case Tree.TypeParam(address, _, _, _) => address
          case _                                => -1

        // A type clause and a term clause may spell the same names (`def f[key](key: key)`), so
        // only clauses of the binder's own kind are candidates.
        def isTypeClause(clause: List[Tree]): Boolean = clause.prim match
          case Tree.TypeParam(_, _, _, _) => true
          case _                          => false

        clauses.stdlib.find: clause =>
          isTypeClause(clause) == typeClause && clause.map(nameOf) == names

        . map(_.stdlib.map(addressOf)).getOrElse(scala.Nil)

      case _ => scala.Nil

  // The type of a (type or term) application of a method reference: the method's type with the
  // arguments substituted for its parameters — by address for a definition's, by position for a
  // synthetic member's — which is where a dependent result comes to refer to an argument. A
  // reference that is no application resolves to its declared type.
  def applicationType(tpe: Type, scope: Optional[Scope]): (Type, Optional[Scope]) = tpe match
    case Type.Applied(function @ (Type.TermRef(_, _) | Type.TermRefIn(_, _, _) | Type.TermRefDirect(_)
        | Type.TermRefLocal(_, _) | Type.AppliedTerms(_, _)), arguments) =>
      val (functionType, functionScope) = applicationType(function, scope)

      functionType match
        case Type.Poly(address, names, _, result) =>
          val instantiated = clauseAddresses(address, names, functionScope, typeClause = true) match
            case scala.Nil => Universe.substitute(result, address, Universe.pin(arguments, scope))
            case addresses => Universe.replace(result, addresses.zip(Universe.pin(arguments, scope).stdlib))

          (instantiated, functionScope)

        case other => (other, functionScope)

    case Type.AppliedTerms(function, arguments) =>
      val (functionType, functionScope) = applicationType(function, scope)

      functionType match
        case Type.Method(address, names, _, result, _) =>
          val instantiated = clauseAddresses(address, names, functionScope, typeClause = false) match
            case scala.Nil => result
            case addresses => Universe.replace(result, addresses.zip(Universe.pin(arguments, scope).stdlib))

          (instantiated, functionScope)

        case Type.ByName(underlying) => (underlying, functionScope)
        case other                   => (other, functionScope)

    case Type.TermRef(_, _) | Type.TermRefIn(_, _, _) | Type.TermRefDirect(_) | Type.TermRefLocal(_, _) =>
      resolve(tpe, scope) match
        case Resolution.Term(declared, termScope) => (declared, termScope.or(scope))
        case _                                    => (tpe, scope)

    case other => (other, scope)

  // The expansion of an alias reference or application, with the scope it is spelled in.
  def expansionOf(tpe: Type, scope: Optional[Scope]): Optional[(Type, Optional[Scope])] = tpe match
    case Type.Applied(constructor, arguments) => resolve(constructor, scope) match
      case Resolution.Alias(arity, apply, aliasScope, _, false) if arity == arguments.size =>
        (apply(Universe.pin(arguments, scope)), aliasScope.or(scope))

      case _ => Unset

    case Type.TypeRef(_, _) | Type.TypeRefIn(_, _, _) | Type.TypeRefDirect(_) | Type.TypeRefLocal(_, _) =>
      resolve(tpe, scope) match
        case Resolution.Alias(0, apply, aliasScope, _, false) => (apply(List()), aliasScope.or(scope))
        case _                                                => Unset

    case _ => Unset

  // A type member a refinement of the prefix's type declares — `x.Self` with `x: T is Tc`, which
  // is `Tc { type Self = T }` — as the compiler's `lookupRefined`/`findMember` sees it, before the
  // class behind the refinement is asked.
  private def refinedMember(tpe: Type, name: Text, scope: Optional[Scope], depth: Int): Optional[Resolution] =
    if depth > 12 then Unset else tpe match
      case Type.Refined(parent, refined, info) =>
        if refined == name then info match
          case Type.Bounds(low, high, _) if high == low => Resolution.Alias(0, _ => low, scope)
          case Type.Bounds(low, high, _)                => Resolution.Abstract(high.or(low), scope)
          case alias                                    => Resolution.Alias(0, _ => alias, scope)

        else refinedMember(parent, name, scope, depth + 1)

      case Type.Annotated(underlying, _) => refinedMember(underlying, name, scope, depth)
      case Type.Foreign(path, inner)     => refinedMember(inner, name, this.scope(path), depth)

      case Type.TermRef(_, _) | Type.TermRefIn(_, _, _) | Type.TermRefDirect(_) | Type.TermRefLocal(_, _) =>
        resolve(tpe, scope) match
          case Resolution.Term(declared, termScope) => refinedMember(declared, name, termScope.or(scope), depth + 1)
          case _                                    => Unset

      // An applied method reference — a given instance's method, say — has the instantiated
      // method's result as its type.
      case Type.Applied(Type.TermRef(_, _) | Type.TermRefIn(_, _, _) | Type.TermRefDirect(_)
          | Type.TermRefLocal(_, _) | Type.AppliedTerms(_, _), _) | Type.AppliedTerms(_, _) =>
        val (applied, appliedScope) = applicationType(tpe, scope)
        if applied == tpe then Unset else refinedMember(applied, name, appliedScope, depth + 1)

      case Type.Applied(_, _) | Type.TypeRef(_, _) | Type.TypeRefIn(_, _, _) | Type.TypeRefDirect(_)
          | Type.TypeRefLocal(_, _) =>
        expansionOf(tpe, scope).let((expansion, expansionScope) => refinedMember(expansion, name, expansionScope, depth + 1))

      case _ => Unset

  private def typeMember0(prefix: Type, name: Text, scope: Optional[Scope]): Resolution =
    refinedMember(prefix, name, scope, 0).or:
      typeMember1(prefix, name, scope) match
        // A type member selected on `C.this` may belong to `C`'s declared self type.
        case unknown @ Resolution.Unknown(_) => selfTypeOf(prefix, scope) match
          case self: Type => typeMember(self, name, scope)
          case _          => unknown

        case found => found

  private def typeMember1(prefix: Type, name: Text, scope: Optional[Scope]): Resolution =
    owner(prefix, scope) match
      case Resolution.Package(path) =>
        val fullName = Universe.qualify(path, name)

        // `scala.AnyRef` is the compiler's own alias of `java.lang.Object`, in no file.
        if fullName == t"scala.AnyRef" then
          Resolution.Alias(0, _ => Type.TypeRef(t"Object", Type.TermRefPkg(t"java.lang")), Unset)

        else Universe.impureFunction(fullName).or:
         classOf(fullName).let(Resolution.Class(_, List(), Unset)).or:
          // A Java class's statics belong to its module class in the compiler's model, which has
          // no class of its own; a type alias at package level lives in the package object.
          val static =
            if name.s.endsWith("$") then classOf(fullName.s.dropRight(1).nn.tt) else Unset

          static.let(Resolution.Class(_, List(), Unset)).or:
            packageObjectMember(path, name, Set(Kind.Type, Kind.Class))
              . lay(Resolution.Unknown(t"no class $fullName")): (owner, member) =>
                member.kind match
                  case Kind.Class =>
                    classOf(t"${owner.fullName}.$name").lay(Resolution.Unknown(t"nested $name")):
                      Resolution.Class(_, List(), owner.scope)

                  case _ =>
                    opaqueIf(aliasOrAbstract(member.tpe, owner.scope), member.flags.contains(Flag.Opaque))

      case Resolution.Class(info, _, classScope) =>
        // A Java class's nested classes are not among its classfile's members.
        def javaNested: Optional[Resolution] =
          if info.java then classOf(t"${info.fullName}$$$name").let(Resolution.Class(_, List(), Unset))
          else Unset

        memberIn(info, name, Set(Kind.Type, Kind.Class)).lay(javaNested.or(Resolution.Unknown(t"no type $name in ${info.fullName}"))):
          (owner, member) =>
            member.kind match
              case Kind.Class =>
                classOf(t"${owner.fullName}.$name").lay(Resolution.Unknown(t"nested $name")):
                  Resolution.Class(_, List(), owner.scope)

              case _ =>
                opaqueIf(aliasOrAbstract(member.tpe, owner.scope), member.flags.contains(Flag.Opaque))

      case other => other

  // An opaque type's alias is marked as such: erasure sees through it, but nothing else does.
  private def opaqueIf(resolution: Resolution, opaque: Boolean): Resolution = resolution match
    case alias: Resolution.Alias if opaque => alias.copy(opaque = true)
    case other                             => other

  private def termMember(prefix: Type, name: Text, scope: Optional[Scope]): Resolution =
    termMember(prefix, Name.Simple(name), scope)

  private def termMember(prefix: Type, name: Name, scope: Optional[Scope]): Resolution =
    termMember(prefix, name, scope, prefix)

  // `path` is the stable prefix the member is selected on, kept while `prefix` is widened to the
  // type whose members are searched, for the member's type to be seen from it.
  private def termMember(prefix: Type, name: Name, scope: Optional[Scope], path: Type): Resolution =
    prefix match
      // A refinement declaring the member — a dependent function type's `apply` — gives its type
      // (the compiler's `findMember` joins the refinement with the parent's member in its favour).
      case Type.Refined(parent, refined, info) if refined == name.unsigned.text => info match
        case Type.Method(_, _, _, _, _) | Type.Poly(_, _, _, _) | Type.ByName(_) =>
          Resolution.Term(info, scope)

        case _ => termMember(parent, name, scope, path)

      case Type.Refined(parent, _, _)    => termMember(parent, name, scope, path)
      case Type.Annotated(underlying, _) => termMember(underlying, name, scope, path)

      // A term's members are its declared type's, refinements and all.
      case Type.TermRef(_, _) | Type.TermRefIn(_, _, _) | Type.TermRefDirect(_)
          | Type.TermRefLocal(_, _) =>
        resolve(prefix, scope) match
          case Resolution.Term(declared, termScope) =>
            termMember(declared, name, termScope.or(scope), path)

          case _ => termMember1(prefix, name, scope, path)

      case _ => termMember1(prefix, name, scope, path)

  private def termMember1(prefix: Type, name: Name, scope: Optional[Scope], path: Type): Resolution =
    parts(prefix, scope).let: (left, right) =>
      termMember(left, name, scope, path) match
        case Resolution.Unknown(_) => termMember(right, name, scope, path)
        case found                 => found

    . or:
      termMember0(prefix, name, scope, path) match
        // A member selected on `C.this` may belong to `C`'s declared self type.
        case unknown @ Resolution.Unknown(_) => selfTypeOf(prefix, scope) match
          case self: Type => termMember(self, name, scope)
          case _          => unknown

        case found => found

  // The declared self type of the class a `this` type names, if it has one.
  private def selfTypeOf(prefix: Type, scope: Optional[Scope]): Optional[Type] = prefix match
    case Type.This(cls) => classOf(cls, scope).let(selfType(_))
    case _              => Unset

  // Among the members of a name along a class's linearization, the one a reference denotes: the
  // one whose signature the reference carries, if it does; and a concrete one over an abstract
  // redeclaration (`findMember` joins them in the concrete one's favour).
  def choose(info: ClassInfo, name: Name): Optional[(ClassInfo, Member)] =
    val candidates = termsIn(info, name.unsigned.spelled).stdlib

    // An unsigned reference to an overloaded name denotes the parameterless alternative (the
    // pickler signs every other kind of method).
    def parameterless(member: Member): Boolean = member.tpe match
      case Type.Method(_, _, _, _, _) | Type.Poly(_, _, _, _) => false
      case _                                                  => true

    val matching = name.signature.lay(candidates.filter((_, m) => parameterless(m)) match
      case scala.Nil => candidates
      case found     => found

    ): signature =>
      candidates.filter((owner, member) => erasure.signature(owner, member) == signature) match
        case scala.Nil => candidates
        case found     => found

    matching.find((_, member) => !member.flags.contains(Flag.Abstract))
      . orElse(matching.headOption).getOrElse(Unset)

  // The type arguments with which `info`, applied to `arguments`, instantiates its base class
  // `owner`: the arguments themselves for the class itself, else those of the parent clause that
  // leads to `owner`, with the class's own parameters substituted.
  def baseArguments(info: ClassInfo, arguments: List[Type], owner: ClassInfo): Optional[List[Type]] =
    if info.fullName == owner.fullName then arguments
    else
      def substituted(parent: Type): Type =
        Universe.replace(parent, info.typeParamAddresses.stdlib.zip(arguments.stdlib))

      info.parents.stdlib.view.flatMap: parent =>
        val parentArguments = parent match
          case Type.Applied(_, parentArguments) => Universe.pin(parentArguments.map(substituted), info.scope)
          case _                                => List()

        classOf(parent, info.scope).let: parentClass =>
          if derivesFrom(parentClass, owner.fullName) || parentClass.fullName == owner.fullName
          then baseArguments(parentClass, parentArguments, owner)
          else Unset

        . lay(scala.List())(scala.List(_))

      . headOption.getOrElse(Unset)

  private def termMember0(prefix: Type, name: Name, scope: Optional[Scope], path: Type): Resolution =
    val text = name.unsigned.spelled

    owner(prefix, scope) match
      case Resolution.Package(path) =>
        // A package-level term is an object, whose class is the module class, named with a `$`;
        // or a Java class, whose static members are its own; or a package, when a package path
        // is pickled as a selection rather than a `TERMREFpkg`.
        val fullName = Universe.qualify(path, text)

        classOf(t"$fullName$$").let(Resolution.Class(_, List(), Unset)).or:
          classOf(fullName).let: info =>
            if info.java then Resolution.Class(info, List(), Unset) else Resolution.Package(fullName)

          . or:
            packageObjectMember(path, text, Set(Kind.Val, Kind.Def))
              . lay(Resolution.Package(fullName)): (owner, member) =>
                Resolution.Term(member.tpe, owner.scope)

      case Resolution.Class(info, arguments, _) =>
        choose(info, name).lay(Resolution.Unknown(t"$text")):
          (owner, member) =>
            // An object member is a term whose type is its module class.
            if member.flags.contains(Flag.Object)
            then
              classOf(t"${owner.fullName}.$text$$").lay(Resolution.Unknown(t"object $text")):
                Resolution.Class(_, List(), owner.scope)
            else
              // The member's type as seen from the prefix: the declaring class's type parameters
              // stand for the arguments the prefix applies it to.
              val seen =
                if arguments.nil then member.tpe
                else baseArguments(info, arguments, owner).lay(member.tpe): ownerArguments =>
                  Universe.replace(member.tpe, owner.typeParamAddresses.stdlib.zip(ownerArguments.stdlib))

              Resolution.Term(seenFrom(seen, path, owner, scope), owner.scope)

      case other => other

  // A member's type as seen from a stable prefix (the compiler's `asSeenFrom`): the declaring
  // class's `this`, and the type members selected on it, come to refer to the prefix, so that a
  // refinement of the prefix's type (`x.Transport` with `x: Schematic { type Transport = T }`)
  // answers for them. The prefix is pinned to the scope it is spelled in, since the member's type
  // is read in the declaring class's.
  private def seenFrom(tpe: Type, prefix: Type, owner: ClassInfo, scope: Optional[Scope]): Type =
    val stable = prefix match
      case Type.TermRef(_, _) | Type.TermRefIn(_, _, _) | Type.TermRefDirect(_)
          | Type.TermRefLocal(_, _) | Type.This(_) => true

      case _ => false

    if !stable then tpe else
      val pinned = Universe.pin(List(prefix), scope).prim.or(prefix)

      def ownerThis(cls: Type): Boolean = classOf(cls, owner.scope).lay(false): info =>
        info.fullName == owner.fullName || derivesFrom(owner, info.fullName)

      def nameAt(address: Int): Optional[Text] = owner.scope.let(_.definition(address)).let:
        case Tree.TypeDef(_, name, _, _) => name
        case _                           => Unset

      Universe.transform(tpe):
        case Type.TypeRefLocal(address, Type.This(cls)) if ownerThis(cls) =>
          nameAt(address).lay(Type.TypeRefLocal(address, Type.This(cls))):
            Type.TypeRefIn(_, pinned, cls)

        case Type.TypeRef(name, Type.This(cls)) if ownerThis(cls) => Type.TypeRefIn(name, pinned, cls)

  // A member of a package's package object or of one of its top-level-definition carriers (the
  // `<file>$package` objects), which is where a reference pickled against the package prefix
  // finds a package-level type alias or definition.
  def packageObjectMember(pkg: Text, name: Text, kinds: Set[Kind])

  :   Optional[(ClassInfo, Member)] =
    val packageObject = classOf(Universe.qualify(pkg, t"package$$")).let(memberIn(_, name, kinds))

    packageObject.or:
      carriers(pkg).stdlib.view.flatMap: carrier =>
        classOf(carrier).let(memberIn(_, name, kinds)).lay(scala.List())(scala.List(_))

      . headOption.getOrElse(Unset)

  private val carrierCache = mutable.HashMap[Text, List[Text]]()

  // The module classes of the top-level-definition carriers in a package, by full name.
  private def carriers(pkg: Text): List[Text] = carrierCache.getOrElseUpdate(pkg, {
    val directory = if pkg == t"" || pkg == t"<empty>" then t"" else pkg.s.replace('.', '/').nn.tt

    source.list(directory).filter(_.s.endsWith("$package.tasty")).map: entry =>
      val stem = entry.s.dropRight(".tasty".length).nn.tt
      Universe.qualify(pkg, t"${Universe.decode(stem)}$$")

  })

  // The class (or package) a prefix denotes: a package path, a class's `this`, a term's class.
  def owner(prefix: Type, scope: Optional[Scope]): Resolution = resolve(prefix, scope) match
    case Resolution.Term(tpe, termScope)              => owner(tpe, termScope)
    case Resolution.Alias(0, apply, aliasScope, _, _) => owner(apply(List()), aliasScope)
    case Resolution.Abstract(high, boundScope)        => owner(high, boundScope)
    case other                                        => other

  // The class a type is a reference to, through aliases, bounds and singletons.
  // The self type a class declares, if any.
  def selfType(info: ClassInfo): Optional[Type] =
    info.scope.let: scope =>
      scope.definition(info.address) match
        case Tree.TypeDef(_, _, Tree.Template(_, _, _, self, _), _) =>
          self.let: pair => Universe.typeOf(pair(1))

        case _ => Unset

  def classOf(tpe: Type, scope: Optional[Scope]): Optional[ClassInfo] = owner(tpe, scope) match
    case Resolution.Class(info, _, _) => info
    case _                            => Unset

  // The member called `name` of one of the given kinds, in `info` or the first of its
  // linearization that declares one, with its declaring class.
  def memberIn(info: ClassInfo, name: Text, kinds: Set[Kind]): Optional[(ClassInfo, Member)] =
    // The class's own declarations first, which also keeps a parent clause that names a member
    // of the class itself from needing the linearization it is part of computing.
    info.members.stdlib.find { m => m.name == name && kinds.contains(m.kind) } match
      case Some(member) => (info, member)

      case None =>
        linearization(info).stdlib.view.flatMap: className =>
          classOf(className).lay(scala.List()): owner =>
            owner.members.filter { m => m.name == name && kinds.contains(m.kind) }.map((owner, _)).stdlib

        . headOption.getOrElse(Unset)

  // Every term member called `name` along the linearization, declaring class first.
  def termsIn(info: ClassInfo, name: Text): List[(ClassInfo, Member)] =
    linearization(info).stdlib.flatMap: className =>
      classOf(className).lay(scala.List()): owner =>
        owner.members.filter: m => m.name == name && (m.kind == Kind.Def || m.kind == Kind.Val)
          . map((owner, _)).stdlib

    . to(List)

  // The class's linearization by full name: itself, then its parents' linearizations from the
  // last parent to the first, each contributing only the classes not yet seen.
  def linearization(info: ClassInfo): List[Text] =
    linearizations.get(info.fullName) match
      case Some(result) => result

      case None =>
        // A class asked for while its own linearization is being computed — a parent clause
        // that reaches back into it — contributes only itself.
        if linearizing.contains(info.fullName) then List(info.fullName)
        else
          linearizing += info.fullName
          val result = computeLinearization(info)
          linearizing -= info.fullName
          linearizations(info.fullName) = result
          result

  // `L(C) = C, L(Pn) +: … +: L(P1)`, where the concatenation keeps the *last* occurrence of a
  // class that appears more than once, so a class's superclass's linearization is a suffix.
  private def computeLinearization(info: ClassInfo): List[Text] =
    val concatenated = info.parents.stdlib.reverse.flatMap: parent =>
      classOf(parent, info.scope).lay(scala.List())(linearization(_).stdlib)

    val merged = concatenated.reverse.distinct.reverse.filter(_ != info.fullName)
    (info.fullName :: merged).to(List)

  def derivesFrom(info: ClassInfo, base: Text): Boolean = linearization(info).has(base)

object Universe:

  // A `scala.annotation.internal` marker annotation, as the compiler attaches to a parameter's
  // type for a modifier that is not otherwise representable in a method type.
  def marker(name: Text): Tree =
    Tree.New(Tree.Path(Type.TypeRef(name, Type.TermRefPkg(t"scala.annotation.internal"))))

  // The classes the compiler defines without a classfile or TASTy behind them: the top and
  // bottom of the type lattice, the repeated-parameter marker, and the function classes it
  // makes up by arity.
  def synthetic(fullName: Text): Optional[ClassInfo] =
    def make(parents: List[Text], flags: Set[Flag] = Set(Flag.Trait)): ClassInfo =
      ClassInfo(fullName, flags, List(), List(), parents.map(classRef), List(), Unset, Unset, java = false)

    fullName.s match
      case "scala.Any"           => make(List(), Set(Flag.Abstract)).copy(members = anyMembers)
      case "scala.AnyKind"       => make(List(), Set(Flag.Abstract))
      case "scala.Matchable"     => make(List(t"scala.Any"))
      case "scala.Nothing"       => make(List(t"scala.Any"), Set(Flag.Final, Flag.Abstract))
      case "scala.Null"          => make(List(t"scala.Any"), Set(Flag.Final, Flag.Abstract))
      case "scala.Singleton"     => make(List(t"scala.Any"))
      case "scala.<repeated>"    => make(List(t"scala.collection.immutable.Seq"))
      case "scala.&" | "scala.|" => make(List(t"scala.Any"))

      // The impure function types are aliases, not classes: see `impureFunction`.
      case name if name.startsWith("scala.Impure") => Unset

      case name =>
        val function = functionArity(name)

        if function >= 0 then
          val params = List.fill(function)(t"java.lang.Object").join(t",")
          val apply = builtin(t"apply", t"($params)java.lang.Object")
          make(List(t"java.lang.Object")).copy(members = List(apply))

        else Unset

  // `scala.ImpureFunctionN` and `scala.ImpureContextFunctionN` are the compiler's own aliases, in
  // no file (`Definitions.newFunctionNType`): `[-T1, …, -TN, +R] =>> FunctionN[T1, …, TN, R]^`.
  // The body binds its parameters to a binder no file's address could be.
  def impureFunction(fullName: Text): Optional[Resolution] =
    val name = fullName.s
    val arity = if name.startsWith("scala.Impure") then functionArity(name) else -1

    if arity < 0 then Unset else
      val binder = -7
      val count = arity + 1
      val underlying = classRef(t"scala.${name.substring("scala.Impure".length).nn}")
      val params = (0 until count).map(Type.Param(binder, _)).to(List)
      val retainsCap = classRef(t"scala.annotation.retainsCap")

      val annotation =
        Tree.Apply(Tree.Select(Name.Simple(t"<init>"), Tree.New(Tree.Path(retainsCap))), List())

      val result = Type.Annotated(Type.Applied(underlying, params), annotation)
      val bounds = Type.Bounds(classRef(t"scala.Nothing"), classRef(t"scala.Any"), List())
      val names = (0 until count).map { index => t"T$index" }.to(List)
      val lambda = Type.Lambda(binder, names, List.fill(count)(bounds), result)
      Resolution.Alias(count, arguments => substitute(result, binder, arguments), Unset, lambda)

  // The members the compiler gives `Any` and `AnyRef` beyond what `java.lang.Object` declares,
  // with the signatures it assigns them (`getClass` and `synchronized` take a type parameter).
  private def builtin(name: Text, signature: Text): Member =
    Member(name, Kind.Def, Type.TypeRef(t"Any", Type.TermRefPkg(t"scala")), Set(), -1, signature)

  val anyMembers: List[Member] = List(
    builtin(t"==", t"(java.lang.Object)scala.Boolean"),
    builtin(t"!=", t"(java.lang.Object)scala.Boolean"),
    builtin(t"equals", t"(java.lang.Object)scala.Boolean"),
    builtin(t"##", t"()scala.Int"),
    builtin(t"hashCode", t"()scala.Int"),
    builtin(t"toString", t"()java.lang.String"),
    builtin(t"getClass", t"(1)java.lang.Class"),
    builtin(t"isInstanceOf", t"(1)scala.Boolean"),
    cast(t"asInstanceOf"),
    builtin(t"$$isInstanceOf$$", t"(1)scala.Boolean"),
    cast(t"$$asInstanceOf$$"))

  // `asInstanceOf[T]: T`: a polymorphic member whose result is its type argument.
  private def cast(name: Text): Member =
    val binder = -9
    val bounds = Type.Bounds(classRef(t"scala.Nothing"), classRef(t"scala.Any"), List())
    val tpe = Type.Poly(binder, List(t"T"), List(bounds), Type.Param(binder, 0))
    Member(name, Kind.Def, tpe, Set(), -1, t"(1)java.lang.Object")

  val objectMembers: List[Member] = List(
    builtin(t"eq", t"(java.lang.Object)scala.Boolean"),
    builtin(t"ne", t"(java.lang.Object)scala.Boolean"),
    builtin(t"synchronized", t"(1,java.lang.Object)java.lang.Object"),
    builtin(t"getClass", t"(1)java.lang.Class"))

  // The arity of a function class the compiler synthesizes — `FunctionN` beyond the implemented
  // 22, `ContextFunctionN`, `ImpureFunctionN`, `ImpureContextFunctionN` — or -1.
  def functionArity(name: String): Int =
    def digits(prefix: String): Int =
      if name.startsWith(prefix) && name.length > prefix.length &&
          name.substring(prefix.length).nn.forall(_.isDigit)

      then name.substring(prefix.length).nn.toInt else -1

    val plain = digits("scala.Function")
    val context = digits("scala.ContextFunction")
    val impure = digits("scala.ImpureFunction")
    val impureContext = digits("scala.ImpureContextFunction")

    if context >= 0 then context
    else if impure >= 0 then impure
    else if impureContext >= 0 then impureContext
    else if plain > 22 then plain
    else -1

  // A class name as it is spelled in a classfile or TASTy file name: operator characters by
  // their `NameTransformer` names (`*:` is `$times$colon`), and any other character that cannot
  // appear in a JVM name — a dot in a carrier's name, say — as `$uXXXX`.
  def encode(name: Text): Text =
    val builder = java.lang.StringBuilder()

    name.s.foreach: char =>
      val encoded = char match
        case '~'   => "$tilde"
        case '='   => "$eq"
        case '<'   => "$less"
        case '>'   => "$greater"
        case '!'   => "$bang"
        case '#'   => "$hash"
        case '%'   => "$percent"
        case '^'   => "$up"
        case '&'   => "$amp"
        case '|'   => "$bar"
        case '*'   => "$times"
        case '/'   => "$div"
        case '+'   => "$plus"
        case '-'   => "$minus"
        case ':'   => "$colon"
        case '\\'  => "$bslash"
        case '?'   => "$qmark"
        case '@'   => "$at"
        case '.'   => "$u002E"
        case other => other.toString

      builder.append(encoded)

    builder.toString.nn.tt

  // Types leaving the file they were read in — arguments substituted into another file's alias
  // or value class — pinned to it, so their local references still resolve.
  def pin(types: List[Type], scope: Optional[Scope]): List[Type] =
    scope.lay(types): scope => types.map(Type.Foreign(scope.path, _))

  // The inverse of `encode`, for a class name read from a file name.
  def decode(name: Text): Text =
    name.s.replace("$u002E", ".").nn.replace("$times", "*").nn.replace("$colon", ":").nn
      .replace("$eq", "=").nn.replace("$less", "<").nn.replace("$greater", ">").nn
      .replace("$bang", "!").nn.replace("$hash", "#").nn.replace("$percent", "%").nn
      .replace("$up", "^").nn.replace("$amp", "&").nn.replace("$bar", "|").nn
      .replace("$div", "/").nn.replace("$plus", "+").nn.replace("$minus", "-").nn
      .replace("$bslash", "\\").nn.replace("$qmark", "?").nn.replace("$at", "@").nn
      .replace("$tilde", "~").nn.tt

  // A package member's full name.
  def qualify(pkg: Text, name: Text): Text =
    if pkg == t"" || pkg == t"<empty>" || pkg == t"_root_" then name else t"$pkg.$name"

  // The `index`th element of a list, if it has one.
  def nth[element](list: List[element], index: Int): Optional[element] =
    if index < 0 then Unset else list.stdlib.lift(index).getOrElse(Unset)

  val primitives: Set[Text] = Set(t"scala.Unit", t"scala.Boolean", t"scala.Byte", t"scala.Short",
      t"scala.Char", t"scala.Int", t"scala.Long", t"scala.Float", t"scala.Double")

  // A reference to a class by fully-qualified name, as a Java descriptor or a package-member
  // reference spells it. A Java nested class's binary `$` is a `.` in its name.
  def classRef(fullName: Text): Type =
    val dotted = fullName.s.replaceAll("\\$(?=[^$])", ".").nn.tt
    val index = dotted.s.lastIndexOf('.')
    if index < 0 then Type.TypeRef(dotted, Type.TermRefPkg(t"<empty>"))
    else Type.TypeRef(dotted.s.substring(index + 1).nn.tt, Type.TermRefPkg(dotted.s.substring(0, index).nn.tt))

  // The fully-qualified name a class type names, when it can be read off the type alone.
  def className(tpe: Type): Optional[Text] = tpe match
    case Type.TypeRefPkg(name)           => name
    case Type.TypeRef(name, prefix)      => prefixName(prefix).let(p => t"$p.$name")
    case Type.TypeRefIn(name, prefix, _) => prefixName(prefix).let(p => t"$p.$name")
    case Type.Applied(constructor, _)    => className(constructor)
    case Type.Annotated(underlying, _)   => className(underlying)
    case _                               => Unset

  private def prefixName(prefix: Type): Optional[Text] = prefix match
    case Type.TermRefPkg(name)     => if name == t"<empty>" then Unset else name
    case Type.TypeRefPkg(name)     => name
    case Type.This(cls)            => className(cls)
    case Type.TermRef(name, inner) => prefixName(inner).let(p => t"$p.${name.unsigned.text}$$")
    case _                         => Unset

  // The upper bound a bounds type imposes, or the type itself when it is not bounds.
  def upper(tpe: Type): Type = tpe match
    case Type.Bounds(low, high, _) => high.or(low)
    case other                     => other

  // The type a type tree denotes.
  def typeOf(tree: Tree): Type = tree match
    case Tree.Path(tpe)                  => tpe
    case Tree.IdentTpt(_, tpe)           => tpe
    case Tree.SelectTpt(name, qualifier) => Type.TypeRef(name, termType(qualifier))
    case Tree.SingletonTpt(reference)    => termType(reference)
    // An application of `scala.&` or `scala.|` written as a type tree is the intersection or
    // union itself to the compiler (`processAppliedType`); pickled as a type, it stays as written.
    case Tree.AppliedTpt(constructor, arguments) =>
      val applied = arguments.map(typeOf)

      (typeOf(constructor), applied.stdlib) match
        case (Type.TypeRef(name, Type.TermRefPkg(pkg)), scala.List(left, right))
            if pkg == t"scala" && name == t"&" => Type.And(left, right)

        case (Type.TypeRef(name, Type.TermRefPkg(pkg)), scala.List(left, right))
            if pkg == t"scala" && name == t"|" => Type.Or(left, right)

        case (head, _) => Type.Applied(head, applied)

    case Tree.ByNameTpt(underlying)                => Type.ByName(typeOf(underlying))
    case Tree.AnnotatedTpt(underlying, annotation) => Type.Annotated(typeOf(underlying), annotation)
    case Tree.ExplicitTpt(tpt)                     => typeOf(tpt)
    case Tree.Block(_, expression)                 => typeOf(expression)

    case Tree.BoundsTpt(low, high, alias) =>
      alias.lay(Type.Bounds(typeOf(low), high.let(typeOf), List())): alias =>
        Type.Opaque(typeOf(alias), Type.Bounds(typeOf(low), high.let(typeOf), List()))

    case Tree.LambdaTpt(typeParams, body) =>
      val addresses = typeParams.stdlib.collect { case Tree.TypeParam(address, _, _, _) => address }
      val names = typeParams.stdlib.collect { case Tree.TypeParam(_, name, _, _) => name }
      val bounds = typeParams.stdlib.collect { case Tree.TypeParam(_, _, bounds, _) => typeOf(bounds) }

      def abstraction(result: Type): Type =
        Type.Abstraction(addresses.to(List), names.to(List), bounds.to(List), result)

      // The compiler distributes a lambda over bounds (`HKTypeLambda.boundsFromParams`): a
      // higher-kinded type `F[_] <: Hi` is bounded by `Nothing` and `[T] =>> Hi`, a lower bound of
      // `Nothing` staying as it is.
      typeOf(body) match
        case Type.Bounds(low, high, variances) if high.present =>
          val nothing = className(low).lay(false)(_ == t"scala.Nothing")
          Type.Bounds(if nothing then low else abstraction(low), high.let(abstraction), variances)

        case result => abstraction(result)

    case Tree.RefinedTpt(underlying, refinements) =>
      refinements.stdlib.foldLeft(typeOf(underlying)): (parent, refinement) =>
        refinement match
          case Tree.ValDef(_, name, tpt, _, _)   => Type.Refined(parent, name.text, typeOf(tpt))
          // A parameterless method's info is an expression type, as a declaration's is.
          case Tree.DefDef(_, name, clauses, returns, _, _) if clauses.nil =>
            Type.Refined(parent, name.text, Type.ByName(typeOf(returns)))

          case Tree.DefDef(address, name, clauses, returns, _, _) =>
            Type.Refined(parent, name.text, methodType(address, clauses, returns))

          case Tree.TypeDef(_, name, rhs, _) => Type.Refined(parent, name, memberInfo(typeOf(rhs)))
          case _                             => parent

    case Tree.MatchTpt(bound, scrutinee, cases) =>
      val caseTypes = cases.map:
        case Tree.CaseDef(pattern, _, rhs) =>
          // A case's binders — `case left *: right` — are the parameters of a lambda over the
          // case, in order of appearance, each bounded as its binding declares.
          var binders: scala.List[(Int, Text, Type)] = scala.Nil

          val patternType = transform(typeOf(pattern)):
            case Type.Bind(address, name, bounds) =>
              binders = binders :+ (address, name, bounds)
              Type.TypeRefDirect(address)

          val matchCase = Type.MatchCase(patternType, typeOf(rhs))

          if binders.isEmpty then matchCase
          else Type.Abstraction(binders.map(_(0)).to(List), binders.map(_(1)).to(List),
              binders.map(_(2)).to(List), matchCase)

        case other => typeOf(other)

      Type.Match(bound.let(typeOf).or(Type.TypeRef(t"Any", Type.TermRefPkg(t"scala"))),
          typeOf(scrutinee), caseTypes)

    case Tree.Typed(_, tpt)                  => typeOf(tpt)
    case Tree.Bind(address, name, tpe, _, _) => Type.Bind(address, name, tpe)
    case other                               => termType(other)

  // The type of a term tree used as a type's prefix, or of a parent constructor call.
  def termType(tree: Tree): Type = tree match
    case Tree.Path(tpe)                        => tpe
    case Tree.Ident(_, tpe)                    => tpe
    case Tree.Select(name, qualifier)          => Type.TermRef(name, termType(qualifier))
    case Tree.SelectIn(name, qualifier, owner) => Type.TermRefIn(name, termType(qualifier), owner)
    case Tree.QualifiedThis(qualifier)         => Type.This(typeOf(qualifier))
    case Tree.New(tpt)                         => typeOf(tpt)
    // Applying a method type yields its result; applying a reference whose type is not yet at hand
    // is recorded, for the result to depend on the arguments once it is.
    case Tree.Apply(function, arguments) => termType(function) match
      case methodic @ (Type.Method(_, _, _, _, _) | Type.ByName(_) | Type.Poly(_, _, _, _)) => applied(methodic)
      case reference                                                                        => Type.AppliedTerms(reference, arguments.map(termType))

    // A type application of a method reference whose polymorphic type is not at hand is kept as
    // an application of the reference, for the atomizer to instantiate once it has resolved it.
    case Tree.TypeApply(function, arguments) => termType(function) match
      case poly @ Type.Poly(_, _, _, _) => instantiated(poly, arguments.map(typeOf))
      case reference                    => Type.Applied(reference, arguments.map(typeOf))

    case Tree.Typed(expression, _)      => termType(expression)
    case Tree.Inlined(expression, _, _) => termType(expression)
    case Tree.Block(_, expression)      => termType(expression)
    case other if other.isTypeTree      => typeOf(other)
    case other                          => Type.TypeRef(t"Nothing", Type.TermRefPkg(t"scala"))

  // A type member's info as the compiler keeps it: an alias is bounds that coincide.
  def memberInfo(tpe: Type): Type = tpe match
    case Type.Bounds(low, Unset, variances) => Type.Bounds(low, low, variances)
    case bounds: Type.Bounds                => bounds
    case Type.Opaque(_, bounds)             => bounds
    case alias                              => Type.Bounds(alias, alias, List())

  // The type of an application: the method type's result, through any by-name wrapper.
  private def applied(tpe: Type): Type = tpe match
    case Type.Method(_, _, _, result, _) => result
    case Type.ByName(underlying)         => applied(underlying)
    case Type.Poly(_, _, _, result)      => applied(result)
    case other                           => other

  // The type of a type application: the polymorphic type's result, with the arguments in place
  // of its parameters.
  private def instantiated(tpe: Type, arguments: List[Type]): Type = tpe match
    case Type.Poly(address, _, _, result) => substitute(result, address, arguments)
    case other                            => other

  // The type an annotation tree constructs: the annotation class applied to the type arguments
  // of its constructor call, `new Child[T]()` being `Apply(TypeApply(Select(New(Child), <init>),
  // List(T)), Nil)`.
  def annotationType(tree: Tree): Type = tree match
    case Tree.Apply(function, _)                   => annotationType(function)

    case Tree.TypeApply(function, arguments)       =>
      annotationType(function) match
        case Type.Applied(_, _) => annotationType(function)
        case constructor        => Type.Applied(constructor, arguments.map(typeOf))

    case Tree.Select(_, Tree.New(tpt))      => typeOf(tpt)
    case Tree.SelectIn(_, Tree.New(tpt), _) => typeOf(tpt)
    case Tree.New(tpt)                      => typeOf(tpt)
    case Tree.Block(_, expression)          => annotationType(expression)
    case other                              => typeOf(other)

  // The class type a parent clause names: the type under the constructor call, if there is one.
  def parentType(tree: Tree): Type = tree match
    case Tree.Apply(function, _)              => parentType(function)

    // `new C[T](…)` with the type arguments inferred names the bare constructor in its `New` and
    // applies the type arguments to the constructor call; the parent is the class so applied.
    case Tree.TypeApply(function, arguments) =>
      val applied = arguments.map(typeOf)

      parentType(function) match
        case Type.Lambda(address, _, _, result)     => substitute(result, address, applied)
        case Type.Abstraction(params, _, _, result) => replace(result, params.stdlib.zip(applied.stdlib))
        case Type.Applied(constructor, _)           => Type.Applied(constructor, applied)
        case constructor                            => Type.Applied(constructor, applied)

    case Tree.Select(_, Tree.New(tpt))      => typeOf(tpt)
    case Tree.SelectIn(_, Tree.New(tpt), _) => typeOf(tpt)
    case Tree.New(tpt)                      => typeOf(tpt)
    case Tree.Block(_, expression)          => parentType(expression)
    case other                              => typeOf(other)

  // The methodic type a definition's clauses and result spell, with the definition's own
  // address as every clause's binder. Parameters are referenced by address in the trees, so
  // their types are left to refer to them that way.
  def methodType(address: Int, clauses: List[List[Tree]], returns: Tree): Type =
    clauses.stdlib.foldRight(typeOf(returns)): (clause, result) =>
      val trees = clause.stdlib

      trees.headOption match
        case Some(Tree.TypeParam(_, _, _, _)) =>
          val names = trees.collect { case Tree.TypeParam(_, name, _, _) => name }
          val bounds = trees.collect { case Tree.TypeParam(_, _, bounds, _) => typeOf(bounds) }
          Type.Poly(address, names.to(List), bounds.to(List), result)

        case _ =>
          val names = trees.collect { case Tree.Param(_, name, _, _) => name.text }

          // The compiler's `MethodType.fromSymbols` records an `erased` or `inline` parameter by
          // annotating its type, which is how a refinement's `apply` keeps its erased parameters
          // out of its function arity.
          val params = trees.collect:
            case Tree.Param(_, _, tpt, modifiers) =>
              val tpe = typeOf(tpt)
              if modifiers.has(Flag.Erased) then Type.Annotated(tpe, Universe.marker(t"ErasedParam"))
              else if modifiers.has(Flag.Inline) then Type.Annotated(tpe, Universe.marker(t"InlineParam"))
              else tpe

          def flagged(flag: Flag): Boolean = trees.exists:
            case Tree.Param(_, _, _, modifiers) => modifiers.has(flag)
            case _                              => false

          val kind =
            if flagged(Flag.Given) then Clause.Contextual
            else if flagged(Flag.Implicit) then Clause.Implicit
            else Clause.Plain

          Type.Method(address, names.to(List), params.to(List), result, kind)

  // Substitutes the parameters of the lambda at `binder` in `tpe`.
  def substitute(tpe: Type, binder: Int, arguments: List[Type]): Type =
    transform(tpe):
      case Type.Param(`binder`, number) => Universe.nth(arguments, number).or(Type.Param(binder, number))

  // Replaces local references by address: type parameters in a type, or term parameters whose
  // singleton types a dependent method type mentions.
  def replace(tpe: Type, params: scala.List[(Int, Type)]): Type =
    transform(tpe):
      case Type.TypeRefDirect(address) if params.exists(_(0) == address) =>
        params.find(_(0) == address).get(1)

      case Type.TypeRefLocal(address, _) if params.exists(_(0) == address) =>
        params.find(_(0) == address).get(1)

      case Type.TermRefDirect(address) if params.exists(_(0) == address) =>
        params.find(_(0) == address).get(1)

      case Type.TermRefLocal(address, _) if params.exists(_(0) == address) =>
        params.find(_(0) == address).get(1)

  // Rewrites every subtype of `tpe` for which `partial` is defined, bottom-up.
  def transform(tpe: Type)(partial: PartialFunction[Type, Type]): Type =
    def recur(tpe: Type): Type =
      val mapped = mapChildren(tpe)(recur)
      if partial.isDefinedAt(mapped) then partial(mapped) else mapped

    recur(tpe)

  // Rebuilds `tpe` with `recur` applied to each of its immediate component types.
  def mapChildren(tpe: Type)(recur: Type => Type): Type =
    tpe match
      case Type.TermRefLocal(address, prefix)     => Type.TermRefLocal(address, recur(prefix))
      case Type.TypeRefLocal(address, prefix)     => Type.TypeRefLocal(address, recur(prefix))
      case Type.TermRef(name, prefix)             => Type.TermRef(name, recur(prefix))
      case Type.TermRefIn(name, prefix, owner)    => Type.TermRefIn(name, recur(prefix), recur(owner))
      case Type.TypeRef(name, prefix)             => Type.TypeRef(name, recur(prefix))
      case Type.TypeRefIn(name, prefix, space)    => Type.TypeRefIn(name, recur(prefix), recur(space))
      case Type.This(cls)                         => Type.This(recur(cls))
      case Type.Super(thisType, underlying)       => Type.Super(recur(thisType), recur(underlying))
      case Type.Recursive(address, parent)        => Type.Recursive(address, recur(parent))
      case Type.Refined(parent, name, info)       => Type.Refined(recur(parent), name, recur(info))
      case Type.Applied(constructor, arguments)   => Type.Applied(recur(constructor), arguments.map(recur))
      case Type.AppliedTerms(function, arguments) => Type.AppliedTerms(recur(function), arguments.map(recur))
      case Type.Bounds(low, high, variances)      => Type.Bounds(recur(low), high.let(recur), variances)
      case Type.Annotated(underlying, annotation) => Type.Annotated(recur(underlying), annotation)
      case Type.And(left, right)                  => Type.And(recur(left), recur(right))
      case Type.Or(left, right)                   => Type.Or(recur(left), recur(right))
      case Type.Match(bound, scrutinee, cases)    => Type.Match(recur(bound), recur(scrutinee), cases.map(recur))
      case Type.MatchCase(pattern, rhs)           => Type.MatchCase(recur(pattern), recur(rhs))
      case Type.Flexible(underlying)              => Type.Flexible(recur(underlying))
      case Type.ByName(underlying)                => Type.ByName(recur(underlying))
      case Type.Opaque(alias, bounds)             => Type.Opaque(recur(alias), recur(bounds))

      case Type.Abstraction(params, names, bounds, result) =>
        Type.Abstraction(params, names, bounds.map(recur), recur(result))

      case Type.Foreign(path, inner) => Type.Foreign(path, recur(inner))

      case Type.Method(address, names, params, result, clause) =>
        Type.Method(address, names, params.map(recur), recur(result), clause)

      case Type.Poly(address, names, bounds, result) =>
        Type.Poly(address, names, bounds.map(recur), recur(result))

      case Type.Lambda(address, names, bounds, result) =>
        Type.Lambda(address, names, bounds.map(recur), recur(result))

      case other => other

  // Indexes a file: the fully-qualified name of every definition reachable from outside a term
  // body, and the set of those that are local to one.
  def index(path: Text, file: Unpickler.File): Scope =
    val names = mutable.HashMap[Int, Text]()
    val locals = mutable.HashSet[Int]()
    val owners = mutable.HashMap[Int, Int | Text]()

    def qualify(owner: Text, name: Text): Text =
      if owner == t"" || owner == t"<empty>" then name else t"$owner.$name"

    // A definition's owner: the enclosing definition's address, or its package.
    type Owner = Int | Text

    // The naming context: the full name of the nearest enclosing class (or package), and the
    // compiler's `_$` filler for each term — method or value — between it and the definition,
    // which is how `fullName` spells a method's parameters: `pkg.Class._$param`.
    case class Naming(enclosing: Text, filler: Text):
      def name(simple: Text): Text = qualify(enclosing, t"$filler$simple")
      def inClass(fullName: Text): Naming = Naming(fullName, t"")
      def inTerm: Naming = Naming(enclosing, t"${filler}_$$")

    // A local *type* — a polymorphic lambda's type parameter, a method-local alias — keeps its
    // qualified name too: a reference to it in an inline body spells the compiler's `fullName`,
    // `Owner._$_$field`, where a local term is referred to by position.
    def enter(address: Int, owner: Owner, qualified: Text, local: Boolean, isType: Boolean = false): Unit =
      owners(address) = owner
      if local then locals += address
      if !local || isType then names(address) = qualified

    // Walks statements as members of `owner`; `local` marks everything inside a term body.
    def member(tree: Tree, owner: Owner, naming: Naming, local: Boolean): Unit = tree match
      case Tree.Package(pathType, statements) =>
        val pkg = pathType match
          case Type.TermRefPkg(name) => name
          case _                     => t""

        statements.foreach(member(_, pkg, Naming(pkg, t""), local))

      case Tree.TypeDef(address, name, rhs, modifiers) =>
        val fullName = naming.name(name)
        enter(address, owner, fullName, local, isType = true)

        rhs match
          case Tree.Template(typeParams, params, parents, _, body) =>
            val inner = naming.inClass(fullName)
            typeParams.foreach(member(_, address, inner, local))
            params.foreach(member(_, address, inner, local))
            parents.foreach(terms(_, address, inner))
            body.foreach(member(_, address, inner, local))

          case other => terms(other, address, naming.inTerm)

        modifiers.annotations.foreach: a => terms(a.tree, address, naming.inTerm)

      case Tree.ValDef(address, name, tpt, rhs, modifiers) =>
        val fullName = naming.name(name.text)
        enter(address, owner, fullName, local)
        terms(tpt, address, naming.inTerm)
        rhs.let(body(_, address, naming.inTerm))
        modifiers.annotations.foreach: a => terms(a.tree, address, naming.inTerm)

      case Tree.DefDef(address, name, clauses, returns, rhs, modifiers) =>
        val fullName = naming.name(name.spelled)
        enter(address, owner, fullName, local)
        clauses.foreach(_.foreach(member(_, address, naming.inTerm, local)))
        terms(returns, address, naming.inTerm)
        rhs.let(body(_, address, naming.inTerm))
        modifiers.annotations.foreach: a => terms(a.tree, address, naming.inTerm)

      case Tree.TypeParam(address, name, bounds, _) =>
        val fullName = naming.name(name)
        enter(address, owner, fullName, local, isType = true)
        terms(bounds, address, naming.inTerm)

      case Tree.Param(address, name, tpt, _) =>
        val fullName = naming.name(name.text)
        enter(address, owner, fullName, local)
        terms(tpt, address, naming.inTerm)

      case other => terms(other, owner, naming)

    // Inside a term body every definition is local.
    def body(tree: Tree, owner: Owner, naming: Naming): Unit = tree match
      case Tree.TypeDef(_, _, _, _) | Tree.ValDef(_, _, _, _, _) | Tree.DefDef(_, _, _, _, _, _)
          | Tree.TypeParam(_, _, _, _) | Tree.Param(_, _, _, _) =>
        member(tree, owner, naming, local = true)

      // A binder is local, but a type binder is named like a method-local type, and a term
      // binder's name is harmless.
      case Tree.Bind(address, name, _, pattern, _) =>
        locals += address
        names(address) = naming.name(name)
        owners(address) = owner
        body(pattern, owner, naming)

      case other => children(other).foreach(body(_, owner, naming))

    // Non-definition trees (type trees, parents, annotations) whose nested definitions — a
    // refinement's members, a lambda type's parameters — are reachable, not local.
    def terms(tree: Tree, owner: Owner, naming: Naming): Unit = tree match
      case Tree.TypeDef(_, _, _, _) | Tree.ValDef(_, _, _, _, _) | Tree.DefDef(_, _, _, _, _, _)
          | Tree.TypeParam(_, _, _, _) | Tree.Param(_, _, _, _) =>
        member(tree, owner, naming, local = false)

      case Tree.Bind(address, name, _, pattern, _) =>
        locals += address
        names(address) = naming.name(name)
        owners(address) = owner
        body(pattern, owner, naming)

      case Tree.Lambda(_, _) | Tree.Block(_, _) | Tree.Inlined(_, _, _) | Tree.Match(_, _, _)
          | Tree.CaseDef(_, _, _) =>
        body(tree, owner, naming)

      case other => children(other).foreach(terms(_, owner, naming))

    file.statements.foreach(member(_, t"", Naming(t"", t""), local = false))
    Scope(path, file, names.to(Map), locals.to(Set), owners.to(Map))

  // The immediate subtrees of a tree.
  def children(tree: Tree): scala.List[Tree] = tree match
    case Tree.Package(_, statements)                 => statements.stdlib
    case Tree.TypeDef(_, _, rhs, _)                  => scala.List(rhs)
    case Tree.ValDef(_, _, tpt, rhs, _)              => tpt :: rhs.lay(scala.List())(scala.List(_))
    case Tree.DefDef(_, _, clauses, returns, rhs, _) => clauses.stdlib.flatMap(_.stdlib) ++ (returns :: rhs.lay(scala.List())(scala.List(_)))
    case Tree.TypeParam(_, _, bounds, _)             => scala.List(bounds)
    case Tree.Param(_, _, tpt, _)                    => scala.List(tpt)

    case Tree.Template(typeParams, params, parents, self, body) =>
      typeParams.stdlib ++ params.stdlib ++ parents.stdlib ++ self.lay(scala.List()): s => scala.List(s(1))
      ++ body.stdlib

    case Tree.Import(qualifier, _)                                => scala.List(qualifier)
    case Tree.Export(qualifier, _)                                => scala.List(qualifier)
    case Tree.Select(_, qualifier)                                => scala.List(qualifier)
    case Tree.SelectIn(_, qualifier, _)                           => scala.List(qualifier)
    case Tree.QualifiedThis(qualifier)                            => scala.List(qualifier)
    case Tree.New(tpt)                                            => scala.List(tpt)
    case Tree.Throw(expression)                                   => scala.List(expression)
    case Tree.NamedArg(_, argument)                               => scala.List(argument)
    case Tree.Apply(function, arguments)                          => function :: arguments.stdlib
    case Tree.ApplySigPoly(function, _, arguments)                => function :: arguments.stdlib
    case Tree.TypeApply(function, arguments)                      => function :: arguments.stdlib
    case Tree.SuperRef(qualifier, mixin)                          => qualifier :: mixin.lay(scala.List())(scala.List(_))
    case Tree.Typed(expression, tpt)                              => scala.List(expression, tpt)
    case Tree.Assign(lhs, rhs)                                    => scala.List(lhs, rhs)
    case Tree.Block(statements, expression)                       => statements.stdlib :+ expression
    case Tree.Inlined(expression, call, bindings)                 => bindings.stdlib ++ call.lay(scala.List())(scala.List(_)) :+ expression
    case Tree.Lambda(method, target)                              => method :: target.lay(scala.List())(scala.List(_))
    case Tree.If(_, condition, positive, negative)                => scala.List(condition, positive, negative)
    case Tree.Match(_, scrutinee, cases)                          => scrutinee.lay(scala.List())(scala.List(_)) ++ cases.stdlib
    case Tree.Try(expression, cases, finalizer)                   => (expression :: cases.stdlib) ++ finalizer.lay(scala.List())(scala.List(_))
    case Tree.Return(_, expression)                               => expression.lay(scala.List())(scala.List(_))
    case Tree.While(condition, body)                              => scala.List(condition, body)
    case Tree.Repeated(elements, tpt)                             => elements.stdlib :+ tpt
    case Tree.SelectOuter(_, qualifier, _)                        => scala.List(qualifier)
    case Tree.Quote(body, _)                                      => scala.List(body)
    case Tree.Splice(expression, _)                               => scala.List(expression)
    case Tree.SplicePattern(pattern, _, typeArguments, arguments) => pattern :: (typeArguments.stdlib ++ arguments.stdlib)
    case Tree.Bind(_, _, _, pattern, _)                           => scala.List(pattern)
    case Tree.Alternative(patterns)                               => patterns.stdlib
    case Tree.Unapply(function, implicits, _, patterns)           => (function :: implicits.stdlib) ++ patterns.stdlib
    case Tree.QuotePattern(bindings, body, quotes, _)             => bindings.stdlib ++ scala.List(body, quotes)
    case Tree.CaseDef(pattern, guard, rhs)                        => pattern :: (guard.lay(scala.List())(scala.List(_)) :+ rhs)
    case Tree.Hole(_, _, _, arguments)                            => arguments.stdlib
    case Tree.SelectTpt(_, qualifier)                             => scala.List(qualifier)
    case Tree.SingletonTpt(reference)                             => scala.List(reference)
    case Tree.RefinedTpt(underlying, refinements)                 => underlying :: refinements.stdlib
    case Tree.AppliedTpt(constructor, arguments)                  => constructor :: arguments.stdlib
    case Tree.LambdaTpt(typeParams, body)                         => typeParams.stdlib :+ body
    case Tree.BoundsTpt(low, high, alias)                         => low :: (high.lay(scala.List())(scala.List(_)) ++ alias.lay(scala.List())(scala.List(_)))
    case Tree.AnnotatedTpt(underlying, annotation)                => scala.List(underlying, annotation)
    case Tree.MatchTpt(bound, scrutinee, cases)                   => bound.lay(scala.List())(scala.List(_)) ++ (scrutinee :: cases.stdlib)
    case Tree.ByNameTpt(underlying)                               => scala.List(underlying)
    case Tree.ExplicitTpt(tpt)                                    => scala.List(tpt)
    case _                                                        => scala.List()
