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
import rudiments.{Scribe, at, collect, each, prim}
import vacuous.*

import Pickled.*

// The atomization rules of the Scala discipline, applied over the `Pickled` model of a module's
// TASTy with a `Universe` answering the two questions the bytes cannot: what a reference names,
// and what a declaration's parameters erase to. The folding principle (LIRA §10.3) governs every
// decision here, as it did when these rules ran over the compiler's reflection: declarations whose
// addition cannot break consumers stand alone as atoms; fragments whose addition can break
// consumers fold into their enclosing atom's value, so the entire compatibility check stays set
// arithmetic.
//
// Canonicalization is structural, never printed: symbols appear as fully-qualified names, types
// as a tag-length-value fold over the type constructors, type-lambda parameters as de Bruijn
// indices, and members in sorted key order. Positions, tool versions and sharing never enter the
// encoding. A construct outside the encoder's vocabulary is a hard error, so vocabulary drift in
// the toolchain is detected rather than silently absorbed.
object Degustation:
  class Unencodable(val construct: Text) extends RuntimeException(construct.s)

  // Atomizes the `.tasty` files of one module — each named by its classpath-relative path — against
  // the classpath `dependencies`, which must carry everything the files reference, including the
  // Scala library and the JDK's classes.
  def atomize(files: List[(Text, Data)], dependencies: ClassSource): List[ScalaAtom] =
    val own = files.stdlib.toMap

    val module = new ClassSource:
      def read(path: Text): Optional[Data] = own.get(path).getOrElse(Unset)

      def list(directory: Text): List[Text] =
        own.keys.filter(_.s.startsWith(if directory == t"" then "" else directory.s + "/"))
          . map(_.s.substring(if directory == t"" then 0 else directory.s.length + 1).nn.tt)
          . filter(_.s.indexOf('/') < 0).to(List)

    val universe = Universe(ClassSource(module, dependencies))
    val session = Session(universe)

    files.each: (path, _) =>
      universe.scope(path).let(session.file(_))

    session.result

  private class Session(universe: Universe):
    private val erasure = universe.erasure
    private val atoms = mutable.LinkedHashMap[String, ScalaAtom]()

    // --- canonical binary encoding -----------------------------------------------------------
    private def uvarint(out: Scribe[Byte], value0: Long): Unit =
      var value = value0

      while value >= 0x80L do
        out.append(((value & 0x7f) | 0x80).toByte)
        value >>>= 7

      out.append(value.toByte)

    // The compiler renders a module class's qualified name through `qualToString`, which
    // sanitizes `<` and `>` to `$` across the whole string (`scala.<:<$` is `scala.$:$$`); a
    // member of a module class carries the sanitized owner. Other qualified names are untouched.
    private def spelled(name: String): String =
      val cut = name.lastIndexOf("$.") match
        case -1    => if name.endsWith("$") then name.length else -1
        case index => index + 1

      if cut < 0 then name
      else name.substring(0, cut).nn.replace('<', '$').nn.replace('>', '$').nn + name.substring(cut).nn

    private def utf8(out: Scribe[Byte], text: String): Unit =
      val bytes = text.getBytes("UTF-8").nn
      uvarint(out, bytes.length.toLong)
      out.append(Data.build(bytes.length) { array => java.lang.System.arraycopy(bytes, 0, array, 0, bytes.length) })

    private def tag(out: Scribe[Byte], char: Char): Unit = out.append(char.toByte)

    // --- symbols ----------------------------------------------------------------------------

    // What this layer knows of a definition the module's trees refer to: its fully-qualified
    // name, the key of the class or package that declares it, whether it is a type, whether it is
    // inline or a macro, its erased signature, and — for a definition in one of the module's own
    // files — its address and scope, so that locality can be decided.
    private case class Symbol
      ( fullName:  Text,
        ownerKey:  Text,
        isType:    Boolean,
        inline:    Boolean,
        signature: Text,
        address:   Optional[Int],
        scope:     Optional[Scope],
        isPackage: Boolean = false )

    // A package as a symbol: the compiler gives every package the signature of a non-method.
    private def packageSymbol(path: Text): Symbol =
      Symbol(path, path, false, false, t"()", Unset, Unset, isPackage = true)

    private def symbolOf(tree: Tree, scope: Scope): Symbol = tree match
      case Tree.Ident(name, tpe) =>
        try symbolOfType(tpe, name.unsigned.text, scope)
        catch case error: Unencodable =>
          throw Unencodable(t"${error.getMessage.nn} [identifier ${name.text}: ${tpe.toString.take(240).nn}]")

      case Tree.IdentTpt(name, tpe) => symbolOfType(tpe, name, scope)
      case Tree.Path(tpe)           => symbolOfType(tpe, t"", scope)

      case Tree.Select(name, qualifier) =>
        val prefix = Universe.termType(qualifier)
        try select(name, prefix, scope)
        catch case error: Unencodable =>
          throw Unencodable(t"${error.getMessage.nn} [selection ${name.text} on ${prefix.toString.take(900).nn}]")

      case Tree.SelectTpt(name, qualifier)       => selectType(name, Universe.termType(qualifier), scope)
      case Tree.SelectIn(name, qualifier, owner) => selectIn(name, owner, scope)
      case Tree.QualifiedThis(qualifier)         => symbolOf(qualifier, scope)
      case Tree.Typed(expression, _)             => symbolOf(expression, scope)
      case Tree.Inlined(expression, _, _)        => symbolOf(expression, scope)
      case Tree.Apply(function, _)               => symbolOf(function, scope)
      case Tree.TypeApply(function, _)           => symbolOf(function, scope)
      case other                                 => throw Unencodable(t"symbol of ${other.getClass.getSimpleName.nn}")

    private def symbolOfType(tpe: Type, name: Text, scope: Scope): Symbol = tpe match
      case Type.TermRefDirect(address)     => local(address, scope)
      case Type.TermRefLocal(address, _)   => local(address, scope)
      case Type.TypeRefDirect(address)     => local(address, scope)
      case Type.TypeRefLocal(address, _)   => local(address, scope)
      case Type.TermRef(name, prefix)      => select(name, prefix, scope)
      case Type.TermRefIn(name, _, owner)  => selectIn(name, owner, scope)
      case Type.TypeRef(name, prefix)      => selectType(name, prefix, scope)
      case Type.TypeRefIn(name, prefix, _) => selectType(name, prefix, scope)
      case Type.TermRefPkg(name)           => packageSymbol(name)
      case Type.TypeRefPkg(name)           => packageSymbol(name).copy(isType = true)
      // The compiler gives a type symbol the signature of a non-method.
      case Type.This(cls)                => symbolOfType(cls, name, scope)
      case Type.Annotated(underlying, _) => symbolOfType(underlying, name, scope)
      case Type.Applied(constructor, _)  => symbolOfType(constructor, name, scope)

      case Type.Foreign(path, inner) =>
        universe.scope(path).lay(symbolOfType(inner, name, scope))(symbolOfType(inner, name, _))

      // A type lambda or bounds standing for a type (an export forwarder's alias, say) names
      // what its body or bound does.
      case Type.Lambda(_, _, _, result)      => symbolOfType(result, name, scope)
      case Type.Abstraction(_, _, _, result) => symbolOfType(result, name, scope)
      case Type.Opaque(alias, _)             => symbolOfType(alias, name, scope)
      case Type.Bounds(low, high, _)         => symbolOfType(high.or(low), name, scope)
      case other                             => throw Unencodable(t"symbol of type ${other.getClass.getSimpleName.nn}")

    // A definition in one of the module's files, by address.
    private def local(address: Int, scope: Scope): Symbol =
      val tree = scope.definition(address).or(throw Unencodable(t"no definition at $address"))

      val fullName = scope.names.at(address).or:
        // A local definition is named for diagnostics only.
        tree match
          case Tree.ValDef(_, name, _, _, _)    => name.text
          case Tree.DefDef(_, name, _, _, _, _) => name.text
          case Tree.TypeDef(_, name, _, _)      => name
          case Tree.TypeParam(_, name, _, _)    => name
          case Tree.Param(_, name, _, _)        => name.text
          case Tree.Bind(_, name, _, _, _)      => name
          case _                                => t"<local>"

      val ownerKey = scope.enclosingClass(address).let(ownerKeyOf(_, scope)).or:
        scope.owners.at(address) match
          case pkg: Text => pkg
          case _         => t""

      val (isType, inline) = tree match
        case Tree.TypeDef(_, _, _, _) | Tree.TypeParam(_, _, _, _) => (true, false)

        case Tree.DefDef(_, _, _, _, _, modifiers) =>
          (false, modifiers.has(Flag.Inline) || modifiers.has(Flag.Macro))

        case Tree.ValDef(_, _, _, _, modifiers) =>
          (false, modifiers.has(Flag.Inline) || modifiers.has(Flag.Macro))

        case _ => (false, false)

      val owner = scope.enclosingClass(address).let(classAt(_, scope))
      val signature = if isType then t"()" else erasure.signature(tree, scope, owner)
      Symbol(fullName, ownerKey, isType, inline, signature, address, scope)

    private def classAt(address: Int, scope: Scope): Optional[ClassInfo] =
      scope.names.at(address).let(universe.classOf(_))

    // The owner key of the class at `address`: the package for a top-level-definition carrier,
    // the class's full name otherwise.
    private def ownerKeyOf(address: Int, scope: Scope): Text =
      val name = scope.names.at(address).or(t"")

      if isCarrier(name) then
        scope.owners.at(address) match
          case pkg: Text => pkg
          case _         => name

      else name

    private def ownerKeyOf(info: ClassInfo): Text =
      if isCarrier(info.fullName) then info.pkg else info.fullName

    // A top-level-definition carrier, by its own name: a class nested in one (an opaque type's
    // companion) is not one.
    private def isCarrier(fullName: Text): Boolean =
      fullName.s.endsWith("$package") || fullName.s.endsWith("$package$")

    // A term member selected from a prefix: the declaring class, by name and signature. A member
    // of a refinement — a polymorphic function's `apply` — belongs to the refined class, with the
    // refinement's own type.
    private def select(name: Name, prefix: Type, scope: Scope): Symbol = prefix match
      case Type.Refined(parent, refined, _) if refined == name.unsigned.text =>
        // The compiler joins the refinement with the parent's member of that name, whose symbol
        // the selection then carries; a refinement of a name the parent lacks — a polymorphic
        // function's `apply` — has no symbol at all.
        val owner = universe.classOf(parent, scope).or(throw Unencodable(t"refined class of $refined"))

        if universe.termsIn(owner, refined).nil
        then Symbol(t"<none>", t"", false, false, t"()", Unset, Unset)
        else memberSymbol(owner, name)

      case Type.Annotated(underlying, _) => select(name, underlying, scope)

      case Type.Foreign(path, inner) =>
        universe.scope(path).lay(select(name, inner, scope))(select(name, inner, _))

      // An applied method reference — `x.asInstanceOf[T]`, `head(xs)(using t)` — has the method's
      // result with the type and term arguments in place of its parameters, which is where a
      // dependent result (`t.Operand`) comes to refer to the argument.
      case Type.Applied(Type.TermRef(_, _) | Type.TermRefIn(_, _, _) | Type.TermRefDirect(_)
          | Type.TermRefLocal(_, _) | Type.AppliedTerms(_, _), _) | Type.AppliedTerms(_, _) =>
        val (applied, appliedScope) = universe.applicationType(prefix, scope)
        if applied == prefix then throw Unencodable(t"application of ${name.text}'s prefix")
        select(name, applied, appliedScope.or(scope))

      // A selection on `C.this` may name a member of `C`'s declared self type.
      case Type.This(cls) =>
        try select0(name, prefix, scope)
        catch case error: Unencodable =>
          universe.classOf(cls, scope).let(universe.selfType(_)) match
            case self: Type => select(name, self, scope)
            case _          => throw error

      case _ => universe.parts(prefix, scope) match
        // A member of an intersection comes from whichever part has it.
        case (left: Type, right: Type) =>
          try select(name, left, scope) catch case _: Unencodable => select(name, right, scope)

        case _ => select0(name, prefix, scope)

    private def select0(name: Name, prefix: Type, scope: Scope): Symbol =
      universe.resolve(prefix, scope) match
        // A term's members are those of its declared type, refinements and all.
        case Resolution.Term(declared, termScope)         => select(name, declared, termScope.or(scope))
        case Resolution.Alias(0, apply, aliasScope, _, _) => select(name, apply(List()), aliasScope.or(scope))
        case Resolution.Abstract(high, boundScope)        => select(name, high, boundScope.or(scope))
        case Resolution.Class(info, _, _)                 => memberSymbol(info, name)

        case Resolution.Package(path) =>
          // A term in a package is an object, a top-level definition in a carrier, or a package.
          val fullName = Universe.qualify(path, name.unsigned.text)

          universe.classOf(t"$fullName$$") match
            case module: ClassInfo => Symbol(fullName, path, false, false, t"()", Unset, Unset)
            // A Java class named as a term holds its statics.
            case _ if universe.classOf(fullName).lay(false)(_.java) =>
              Symbol(fullName, path, false, false, t"()", Unset, Unset)

            case _ =>
              val carried: Optional[(ClassInfo, Member)] =
                universe.packageObjectMember(path, name.unsigned.text, Set(Kind.Val, Kind.Def))

              carried.let { pair => memberSymbol(pair(0), name) }.or:
                universe.resolve(Type.TermRef(name, prefix), scope) match
                  case Resolution.Package(sub) => packageSymbol(sub)
                  case other                   => throw Unencodable(t"member ${name.text} of package $path: $other")

        case other => throw Unencodable(t"prefix of ${name.text}: $other")

    private def selectIn(name: Name, owner: Type, scope: Scope): Symbol =
      universe.owner(owner, scope) match
        case Resolution.Class(info, _, _) => memberSymbol(info, name)
        case other                        => throw Unencodable(t"owner of ${name.text}: $other")

    private def memberSymbol(info: ClassInfo, name: Name): Symbol =
      universe.choose(info, name).lay(throw Unencodable(t"no member ${name.text} in ${info.fullName}")): pair =>
        val (owner, member) = pair
        // A Java class's statics belong to its module class.
        val static = owner.java && member.flags.contains(Flag.Static)
        val ownerName = if static then t"${owner.fullName}$$" else owner.fullName
        val ownerKey = if static then ownerName else ownerKeyOf(owner)
        val inline = member.flags.contains(Flag.Inline) || member.flags.contains(Flag.Macro)
        val address: Optional[Int] = if member.address < 0 then Unset else member.address

        Symbol(t"$ownerName.${member.name}", ownerKey, false, inline, erasure.signature(owner, member),
            address, owner.scope)

    private def selectType(name: Text, prefix: Type, scope: Scope): Symbol =
      def typeMember(owner: ClassInfo, member: Member): Symbol =
        Symbol(qualifiedMember(owner, name), ownerKeyOf(owner), true, false, t"()", Unset, owner.scope)

      def noSymbol: Symbol = Symbol(t"<none>", t"", true, false, t"()", Unset, Unset)

      def general(): Symbol = universe.parts(prefix, scope) match
        // A member of an intersection comes from whichever part has it.
        case (left: Type, right: Type) =>
          try selectType(name, left, scope) catch case _: Unencodable => selectType(name, right, scope)

        case _ => universe.resolve(prefix, scope) match
          // A term's type members are its declared type's, refinements and all.
          case Resolution.Term(declared, termScope) => selectType(name, declared, termScope.or(scope))

          case _                                    => dealias(prefix, scope) match
            // An alias's members are its expansion's, a refinement's in particular.
            case expansion: Type => selectType(name, expansion, scope)
            case _               => selectType0(name, prefix, scope)

      prefix match
        // A refinement's type member has no symbol unless the parent declares it too.
        case Type.Refined(parent, refined, _) if refined == name =>
          universe.classOf(parent, scope).let(universe.memberIn(_, name, Set(Kind.Type, Kind.Class)))
            . lay(noSymbol): pair => typeMember(pair(0), pair(1))

        case Type.Refined(parent, _, _)    => selectType(name, parent, scope)
        case Type.Annotated(underlying, _) => selectType(name, underlying, scope)

        case Type.Foreign(path, inner) =>
          universe.scope(path).lay(selectType(name, inner, scope))(selectType(name, inner, _))

        // A selection on `C.this` may name a type member of `C`'s declared self type.
        case Type.This(cls) =>
          try general()
          catch case error: Unencodable =>
            universe.classOf(cls, scope).let(universe.selfType(_)) match
              case self: Type => selectType(name, self, scope)
              case _          => throw error

        case _ => general()

    private def selectType0(name: Text, prefix: Type, scope: Scope): Symbol =
      def typeMember(owner: ClassInfo, member: Member): Symbol =
        Symbol(qualifiedMember(owner, name), ownerKeyOf(owner), true, false, t"()", Unset, owner.scope)

      universe.owner(prefix, scope) match
        case Resolution.Class(info, _, _) =>
          // A Java class's nested classes are not among its classfile's members.
          def javaNested: Optional[Symbol] =
            if !info.java then Unset
            else universe.classOf(t"${info.fullName}$$$name").let: nested =>
              Symbol(nested.fullName, ownerKeyOf(nested), true, false, t"()", Unset, Unset)

          universe.memberIn(info, name, Set(Kind.Type, Kind.Class))
            . lay(javaNested.or(throw Unencodable(t"no type $name in ${info.fullName} [prefix ${prefix.toString.take(200).nn}]"))): pair =>
              val (owner, member) = pair
              typeMember(owner, member)

        case Resolution.Package(path) =>
          val fullName = Universe.qualify(path, name)

          // `scala.AnyRef` is the compiler's own alias of `java.lang.Object`, in no file; a
          // reference to it names the alias.
          if fullName == t"scala.AnyRef" || Universe.impureFunction(fullName).present then
            Symbol(fullName, path, true, false, t"()", Unset, Unset)

          else universe.classOf(fullName) match
            case info: ClassInfo => Symbol(info.fullName, ownerKeyOf(info), true, false, t"()", Unset, info.scope)

            case _ =>
              val carried: Optional[(ClassInfo, Member)] =
                universe.packageObjectMember(path, name, Set(Kind.Type, Kind.Class))

              carried.let: pair => typeMember(pair(0), pair(1))
                . or(throw Unencodable(t"no type $fullName"))

        case other => throw Unencodable(t"prefix of type $name: $other")

    // A member's full name as the compiler spells it: its declaring class's full name, carrier
    // objects included, then the member's name.
    private def qualifiedMember(owner: ClassInfo, name: Text): Text = t"${owner.fullName}.$name"

    // Whether the definition `symbol` lies inside the definition at `enclosing` in `scope`.
    // Whether the definition `symbol` lies inside the definition at `enclosing` in `scope`, or is
    // that definition: a recursive reference to the inlined method is local to its body too.
    private def isWithin(symbol: Symbol, enclosing: Int, scope: Scope): Boolean =
      symbol.address.lay(false): address =>
        symbol.scope.lay(false)(_.path == scope.path) &&
        (address == enclosing || scope.within(address, enclosing))

    // --- types --------------------------------------------------------------------------------

    // A binder in scope while encoding a type: a methodic type or lambda, by the address the
    // `PARAMtype` references name it by, and the addresses of parameters it binds when it was
    // spelled as definitions.
    private case class Binder(address: Int, params: scala.List[Int])

    private def denylisted(fullName: Text): Boolean =
      val diagnostic = Set(
        t"scala.deprecated", t"scala.deprecatedInheritance", t"scala.deprecatedOverriding",
        t"scala.annotation.deprecatedName", t"scala.annotation.implicitNotFound",
        t"scala.annotation.implicitAmbiguous", t"scala.annotation.migration",
        t"scala.annotation.nowarn", t"scala.annotation.unused")

      val interop = scala.List(
        "scala.scalajs.js.annotation.", "scala.scalanative.unsafe.",
        "scala.annotation.internal.", "scala.annotation.unchecked.")

      diagnostic.contains(fullName) || interop.exists: prefix => fullName.s.startsWith(prefix)

    // Capture annotations are capture checking's. A compiler reading the TASTy without it keeps
    // them in the types of trees, but every type map it runs over a type drops them
    // (`RetainingAnnotation.mapWith` is `EmptyAnnotation` unless capture checking is enabled
    // somewhere), and the unpickler maps the declared types of most definitions on the way to
    // their symbols' infos. Which map ran decides the info the atomizer saw:
    //
    //  - a parameter's type (`suppressInto`/`revealInto`, alias-following maps): capture
    //    annotations stripped, and an alias application whose expansion the stripping changes is
    //    replaced by the expansion — `Scanner^ ?=> T`, pickled as `ImpureContextFunction1[Scanner^,
    //    T]`, becomes `ContextFunction1[Scanner, T]`, while `Diagnostics ?=> T` stays as written;
    //  - any other member's type, if the member is not synthetic (`avoidPrivateLeaks`), or a
    //    method's type when any of its clauses is non-empty (`MethodType.fromSymbols`), or a type
    //    member's (`checkNonCyclic`): stripped, with no alias following;
    //  - a synthetic value's type, or a synthetic method's with only empty clauses: untouched.
    private def retainsLike(annotation: Tree, scope: Scope): Boolean =
      val name = annotationName(annotation, scope)

      name == t"scala.annotation.retains" || name == t"scala.annotation.retainsCap" ||
      name == t"scala.annotation.retainsByName"

    // A type with its capture annotations stripped, as a plain type map leaves it.
    private def strip(tpe: Type, scope: Scope): Type =
      Universe.transform(tpe):
        case Type.Annotated(underlying, annotation) if retainsLike(annotation, scope) => underlying

    // A type as an alias-following map (`FollowAliasesMap`) leaves it: capture annotations
    // stripped, and each alias application or reference expanded when the expansion, stripped,
    // differs from the expansion as it stands — the map returns the original when following
    // changes nothing under it. Match types are left as they are, as the maps leave them.
    private def follow(tpe: Type, scope: Scope): Type =
      def recur(tpe: Type): Type = tpe match
        case Type.Match(_, _, _) => tpe

        // `dealias` looks through an annotation to the alias under it, so an alias directly under
        // a capture annotation — `(a is B)^`, `=> T` with its `@retainsByName` — is expanded once
        // the annotation is stripped: the expansion then differs from what was dealiased. Under
        // any other annotation, the expansion only stands if following it changes it.
        case Type.Annotated(underlying, annotation) =>
          val retains = retainsLike(annotation, scope)

          dealias(underlying, scope) match
            case dealiased: Type =>
              val inner = recur(dealiased)
              if retains then inner
              else if inner == dealiased then tpe
              else Type.Annotated(inner, annotation)

            case _ =>
              if retains then recur(underlying) else Type.Annotated(recur(underlying), annotation)

        case Type.Foreign(path, inner) =>
          universe.scope(path).lay(Type.Foreign(path, recur(inner))): inner2 =>
            Type.Foreign(path, follow(inner, inner2))

        case Type.Applied(_, _) | Type.TypeRef(_, _) | Type.TypeRefIn(_, _, _) | Type.TypeRefPkg(_)
            | Type.TypeRefDirect(_) | Type.TypeRefLocal(_, _) =>
          dealias(tpe, scope) match
            case dealiased: Type =>
              val followed = recur(dealiased)
              if followed == dealiased then tpe else followed

            case _ => tpe match
              case Type.Applied(constructor, arguments) => Type.Applied(constructor, arguments.map(recur))
              case _                                    => tpe

        case other => Universe.mapChildren(other)(recur)

      recur(tpe)

    // The compiler's `dealiasKeepAnnotsAndOpaques`: the full expansion of an alias reference or
    // application, through annotations, or `Unset` when there is nothing to expand. The alias's
    // own body comes stripped of capture annotations — a type member's info was mapped when the
    // alias was read (`checkNonCyclic`), and instantiating the compiler's synthetic aliases maps
    // their bodies (`substParams`) — but the arguments are substituted as they stand.
    private def dealias(tpe: Type, scope: Scope): Optional[Type] =
      def pinned(expansion: Type, aliasScope: Optional[Scope]): Type =
        aliasScope.lay(expansion): aliasScope => Type.Foreign(aliasScope.path, expansion)

      def step(tpe: Type, scope: Scope): Optional[Type] = tpe match
        case Type.Applied(constructor, arguments) =>
          universe.resolve(constructor, scope) match
            case Resolution.Alias(arity, apply, aliasScope, body, false)
                if arity > 0 && arity == arguments.size =>
              val pinnedArguments = Universe.pin(arguments, scope)
              val bodyScope = aliasScope.or(scope)

              val expansion = body match
                case Type.Lambda(address, _, _, result) =>
                  Universe.substitute(strip(result, bodyScope), address, pinnedArguments)

                case Type.Abstraction(params, _, _, result) =>
                  Universe.replace(strip(result, bodyScope), params.stdlib.zip(pinnedArguments.stdlib))

                case _ => strip(apply(pinnedArguments), bodyScope)

              pinned(foldAndOr(expansion, bodyScope), aliasScope)

            case _ => Unset

        case Type.TypeRef(_, _) | Type.TypeRefIn(_, _, _) | Type.TypeRefDirect(_) | Type.TypeRefLocal(_, _) =>
          universe.resolve(tpe, scope) match
            case Resolution.Alias(0, apply, aliasScope, _, false) =>
              pinned(strip(apply(List()), aliasScope.or(scope)), aliasScope)

            case _ => Unset

        case Type.Annotated(underlying, annotation) =>
          step(underlying, scope).let(Type.Annotated(_, annotation))

        case Type.Foreign(path, inner) =>
          universe.scope(path).lay(step(inner, scope))(step(inner, _)).let(Type.Foreign(path, _))

        case _ => Unset

      def chain(tpe: Type): Type = step(tpe, scope).lay(tpe)(chain)

      step(tpe, scope).let(chain)

    // `avoidPrivateLeaks` runs over every non-synthetic member's info and dealiases any
    // unqualified-private type alias it refers to, which only a definition in the alias's own
    // class can (so the reference is by address); the expansion is itself checked.
    private def privateLeaks(tpe: Type, scope: Scope): Type =
      def hidden(address: Int): Boolean = scope.definition(address) match
        case Tree.TypeDef(_, _, rhs, modifiers) =>
          !rhs.isInstanceOf[Tree.Template] && modifiers.has(Flag.Private) && !modifiers.has(Flag.Opaque)

        case _ => false

      def expand(reference: Type, address: Int, arguments: List[Type]): Type =
        universe.resolve(reference, scope) match
          case Resolution.Alias(arity, apply, aliasScope, _, false) if arity == arguments.size =>
            val expansion = foldAndOr(apply(Universe.pin(arguments, scope)), aliasScope.or(scope))
            privateLeaks(aliasScope.lay(expansion) { aliasScope => Type.Foreign(aliasScope.path, expansion) }, scope)

          case _ => if arguments.nil then reference else Type.Applied(reference, arguments)

      Universe.transform(tpe):
        case Type.Applied(reference @ Type.TypeRefDirect(address), arguments) if hidden(address) =>
          expand(reference, address, arguments)

        case Type.Applied(reference @ Type.TypeRefLocal(address, _), arguments) if hidden(address) =>
          expand(reference, address, arguments)

        case reference @ Type.TypeRefDirect(address) if hidden(address)   => expand(reference, address, List())
        case reference @ Type.TypeRefLocal(address, _) if hidden(address) => expand(reference, address, List())

    // A method parameter's type as `MethodType.fromSymbols` adapts it: followed, a repeated
    // parameter's `@Repeated Seq[T]` made `<repeated>[T]`, and a `consume` annotation on the
    // parameter copied onto its type.
    private def parameterInfo(param: Tree.Param, scope: Scope): Type =
      val followed = follow(Universe.typeOf(param.tpt), scope)

      // `revealInto`: a covariant `T @$into` — the suppressed form the compiler pickles for a
      // synthetic member's parameter — is `into[T]` again, by its external name.
      def reveal(tpe: Type): Type = tpe match
        case Type.Annotated(underlying, annotation)
            if annotationName(annotation, scope) == t"scala.annotation.internal.$$into" =>
          val into = Type.TypeRef(t"into", Type.TermRef(Name.Simple(t"Conversion"), Type.TermRefPkg(t"scala")))
          Type.Applied(into, List(reveal(underlying)))

        case Type.Annotated(underlying, annotation) => Type.Annotated(reveal(underlying), annotation)
        case Type.ByName(underlying)                => Type.ByName(reveal(underlying))
        case other                                  => other

      val repeated = reveal(followed) match
        case Type.Annotated(Type.Applied(_, arguments), annotation)
            if annotationName(annotation, scope) == t"scala.annotation.internal.Repeated" =>
          Type.Applied(Type.TypeRef(t"<repeated>", Type.TermRefPkg(t"scala")), arguments)

        case other => other

      def annotate(tpe: Type, annotation: Tree): Type = tpe match
        case Type.ByName(underlying) => Type.ByName(annotate(underlying, annotation))
        case other                   => Type.Annotated(other, annotation)

      param.modifiers.annotations.stdlib
        . find: annotation => annotationName(annotation.tree, scope) == t"scala.caps.internal$$.consume"
        . fold(repeated): consume => annotate(repeated, consume.tree)

    // The class of an annotation: its tree may name the class through an alias (`proscenium`
    // re-exports `targetName`), which the compiler's `annotClass` sees through.
    private def annotationName(annotation: Tree, scope: Scope): Text =
      def classOf(tpe: Type, scope: Scope): Text =
        val head = tpe match
          case Type.Applied(constructor, _) => constructor
          case other                        => other

        universe.resolve(head, scope) match
          case Resolution.Alias(0, apply, aliasScope, _, false) => classOf(apply(List()), aliasScope.or(scope))
          case Resolution.Class(info, _, _)                     => info.fullName
          case _                                                => symbolOfType(head, t"", scope).fullName

      classOf(Universe.annotationType(annotation), scope)

    // The addresses of the parameters of the clause of the definition at `address` whose names
    // are `names`, or none when the binder is not a definition's.
    private def clauseAddresses(address: Int, names: List[Text], scope: Scope): scala.List[Int] =
      universe.clauseAddresses(address, names, scope, typeClause = false)

    private def clauseAddresses(address: Int, names: List[Text], scope: Scope, typeClause: Boolean)

    :   scala.List[Int] =
      universe.clauseAddresses(address, names, scope, typeClause)

    // The compiler's `typeSymbol` of a `this` type is its class's self type's: the class itself
    // without a declared self type, else whatever the declared self type names — nothing, for an
    // intersection.
    private def thisSymbol(cls: Type, scope: Scope): Text =
      val info = universe.classOf(cls, scope)

      val self: Optional[Tree] = info.let: info =>
        if info.isObject || info.address < 0 then Unset
        else info.scope.let(_.definition(info.address)).let:
          case Tree.TypeDef(_, _, Tree.Template(_, _, _, self, _), _) => self.let(_(1))
          case _                                                      => Unset

      // The class's self type with a declared one is `givenSelf & C` (`ClassInfo.selfType`), an
      // intersection whatever is declared, and an intersection names no symbol.
      self.lay(symbolOfType(cls, t"", scope).fullName): _ => t"<none>"

    // Structural type encoding. `binders` tracks enclosing lambda-like binders outermost-last,
    // so parameter references encode as (de Bruijn depth, index) and binder names vanish.
    private def encodeType
      ( out: Scribe[Byte], tpe: Type, binders: scala.List[Binder], scope: Scope )

    :   Unit =

      def recur(inner: Type, inner2: scala.List[Binder] = binders): Unit =
        encodeType(out, inner, inner2, scope)

      // A reference to a parameter of an enclosing binder, by address.
      def paramOf(address: Int): Option[(Int, Int)] =
        binders.zipWithIndex.collectFirst:
          case (binder, depth) if binder.params.contains(address) =>
            (depth, binder.params.indexOf(address))

      tpe match
        case Type.TypeRefDirect(address) =>
          paramOf(address) match
            case Some((depth, index)) =>
              tag(out, 'p')
              uvarint(out, depth.toLong)
              uvarint(out, index.toLong)

            case None =>
              tag(out, 'R')
              utf8(out, spelled(symbolOfType(tpe, t"", scope).fullName.s))

        case Type.TypeRefLocal(address, _) =>
          paramOf(address) match
            case Some((depth, index)) =>
              tag(out, 'p')
              uvarint(out, depth.toLong)
              uvarint(out, index.toLong)

            case None =>
              tag(out, 'R')
              utf8(out, spelled(symbolOfType(tpe, t"", scope).fullName.s))

        case Type.TermRefDirect(address) =>
          paramOf(address) match
            case Some((depth, index)) =>
              tag(out, 'p')
              uvarint(out, depth.toLong)
              uvarint(out, index.toLong)

            case None =>
              tag(out, 'S')
              utf8(out, spelled(termTypeSymbol(tpe, scope).s))

        case Type.TypeRef(_, _) | Type.TypeRefIn(_, _, _) | Type.TypeRefPkg(_) =>
          tag(out, 'R')
          utf8(out, spelled(symbolOfType(tpe, t"", scope).fullName.s))

        case Type.TermRef(_, _) | Type.TermRefIn(_, _, _) | Type.TermRefLocal(_, _)
            | Type.TermRefPkg(_) =>
          tag(out, 'S')
          utf8(out, spelled(termTypeSymbol(tpe, scope).s))

        case Type.Applied(constructor, arguments) =>
          reduceApplication(tpe, scope) match
            case reduced: Type => recur(reduced)

            case _ =>
              tag(out, 'A')
              recur(constructor)
              uvarint(out, arguments.size.toLong)
              arguments.each(recur(_))

        case Type.And(left, right) =>
          tag(out, '&')
          recur(left)
          recur(right)

        case Type.Or(left, right) =>
          tag(out, '|')
          recur(left)
          recur(right)

        case Type.ByName(underlying) =>
          tag(out, 'N')
          recur(underlying)

        case Type.Annotated(underlying, annotation) =>
          // Interop and diagnostic annotations are denylisted (they never alter what Scala
          // consumers can depend on); every other retained annotation folds.
          val name = annotationName(annotation, scope)

          if denylisted(name) then recur(underlying)
          else
            tag(out, '@')
            utf8(out, name.s)
            recur(underlying)

        case Type.Literal(constant) =>
          tag(out, 'C')
          utf8(out, constantText(constant, scope))

        case Type.Lambda(address, names, bounds, result) =>
          tag(out, 'L')
          uvarint(out, names.size.toLong)
          val inner = Binder(address, scala.List()) :: binders
          bounds.each(recur(_, inner))
          recur(result, inner)

        case Type.Abstraction(params, names, bounds, result) =>
          tag(out, 'L')
          uvarint(out, names.size.toLong)
          val inner = Binder(-1, params.stdlib) :: binders
          bounds.each(recur(_, inner))
          recur(result, inner)

        // A methodic type spelled by a definition's clauses (a refinement's method, say) binds
        // the parameters of the clause with its names; a dependent result refers to them by
        // address.
        case Type.Method(address, names, params, result, clause) =>
          tag(out, 'M')
          if clause != Clause.Plain then tag(out, 'i')
          uvarint(out, names.size.toLong)
          val inner = Binder(address, clauseAddresses(address, names, scope, typeClause = false)) :: binders
          params.each(recur(_, inner))
          recur(result, inner)

        case Type.Poly(address, names, bounds, result) =>
          tag(out, 'P')
          uvarint(out, names.size.toLong)
          val inner = Binder(address, clauseAddresses(address, names, scope, typeClause = true)) :: binders
          bounds.each(recur(_, inner))
          recur(result, inner)

        case Type.Param(binder, number) =>
          tag(out, 'p')
          val depth = binders.indexWhere(_.address == binder)
          uvarint(out, (if depth < 0 then binders.length else depth).toLong)
          uvarint(out, number.toLong)

        case Type.This(cls) =>
          tag(out, 'H')
          utf8(out, spelled(thisSymbol(cls, scope).s))

        case Type.Super(thisType, underlying) =>
          tag(out, 'U')
          recur(thisType)
          recur(underlying)

        case Type.Refined(parent, name, info) =>
          tag(out, 'F')
          recur(parent)
          utf8(out, name.s)
          recur(info)

        case Type.Bounds(low, high, _) =>
          tag(out, 'B')
          recur(low)
          recur(high.or(low))

        case Type.Opaque(_, bounds) => recur(bounds)

        case Type.Recursive(address, parent) =>
          tag(out, 'r')
          recur(parent, Binder(address, scala.List()) :: binders)

        case Type.RecursiveThis(address) =>
          tag(out, 'h')
          uvarint(out, binders.indexWhere(_.address == address).max(0).toLong)

        case Type.Flexible(underlying) => recur(underlying)

        case Type.Match(bound, scrutinee, cases) =>
          tag(out, 'X')
          recur(bound)
          recur(scrutinee)
          uvarint(out, cases.size.toLong)
          cases.each(recur(_))

        case Type.MatchCase(pattern, rhs) =>
          tag(out, 'x')
          recur(pattern)
          recur(rhs)

        // A type pattern's binder is a type symbol of its own, named like a method-local type.
        case Type.Bind(address, _, _) => recur(Type.TypeRefDirect(address))

        case Type.Foreign(path, inner) =>
          universe.scope(path).lay(recur(inner))(encodeType(out, inner, binders, _))

    // The compiler reduces an application of a type alias as it reads it (`appliedTo`): an
    // application of a type lambda itself is instantiated, and so is one of an alias whose body is
    // an eta-expansion — `type F[A, B] = G[A, B]` — which becomes `G[args]`; any other alias
    // application stays as written. Returns the reduced type, pinned to the alias's file, or
    // `Unset` when the application stands.
    private def reduceApplication(tpe: Type, scope: Scope): Optional[Type] = tpe match
      case Type.Applied(constructor, arguments) =>
        val pinned = Universe.pin(arguments, scope)

        constructor match
          case Type.Lambda(address, _, _, result) =>
            Universe.substitute(result, address, pinned)

          case Type.Abstraction(params, _, _, result) =>
            Universe.replace(result, params.stdlib.zip(pinned.stdlib))

          // An application with a wildcard argument reduces (the compiler's `Reducer`) when every
          // occurrence of the parameters can take the wildcard: as a type argument of a class, or
          // as the alias of a refinement, which becomes the wildcard's bounds.
          case _ if arguments.exists(wildcardArgument) =>
            universe.resolve(constructor, scope) match
              case Resolution.Alias(arity, apply, aliasScope, _, false) if arity == arguments.size =>
                reduceWildcards(apply(pinned)).let: reduced =>
                  aliasScope.lay(reduced): aliasScope => Type.Foreign(aliasScope.path, reduced)

              case _ => Unset

          case _ =>
            universe.resolve(constructor, scope) match
              // An alias whose body applies a `scala.compiletime.ops` type reduces like an
              // eta-expansion does.
              case Resolution.Alias(arity, apply, aliasScope, body, false)
                  if arity > 0 && (etaExpansion(body) || compiletimeApplication(body, aliasScope.or(scope))) =>
                val folded = foldAndOr(apply(pinned), aliasScope.or(scope))
                aliasScope.lay(folded): aliasScope => Type.Foreign(aliasScope.path, folded)

              case _ => Unset

      case _ => Unset

    // The compiler's `&` and `|` are its own aliases, whose applications a reduction produces as
    // the intersection and union types themselves (`processAppliedType`); written directly in a
    // pickled type, `scala.&[A, B]` stays an application.
    private def foldAndOr(expansion: Type, scope: Scope): Type = expansion match
      case Type.Applied(head, arguments) if arguments.size == 2 =>
        val name = universe.classOf(head, scope).let(_.fullName)

        arguments match
          case List(left, right) =>
            if name == t"scala.&" then Type.And(left, right)
            else if name == t"scala.|" then Type.Or(left, right)
            else expansion

          case _ => expansion

      case _ => expansion

    private def wildcardArgument(tpe: Type): Boolean = tpe match
      case Type.Bounds(_, _, _)   => true
      case Type.Foreign(_, inner) => wildcardArgument(inner)
      case _                      => false

    private def reduceWildcards(expansion: Type): Optional[Type] =
      // A refinement aliasing a wildcard — `type Self = A` with `A := ? >: lo <: hi` — is the
      // bounds themselves.
      def wildcard(tpe: Type): Boolean = tpe match
        case Type.Bounds(_, _, _)   => true
        case Type.Foreign(_, inner) => wildcard(inner)
        case _                      => false

      val normalized = Universe.transform(expansion):
        case Type.Refined(parent, name, Type.Bounds(low, high, _)) if high == low && wildcard(low) =>
          Type.Refined(parent, name, low)

      var replaced = true

      def check(tpe: Type, argument: Boolean): Unit = tpe match
        case Type.Bounds(_, _, _)     => if !argument then replaced = false
        case Type.Foreign(_, inner)   => check(inner, argument)
        case Type.Applied(head, args) => check(head, false); args.each(check(_, true))

        case Type.Refined(parent, _, info) =>
          check(parent, false)

          def bounds(info: Type): Unit = info match
            case Type.Bounds(low, high, _) => check(low, false); high.let(check(_, false))
            case Type.Foreign(_, inner)    => bounds(inner)
            case other                     => check(other, false)

          bounds(info)

        case other => Universe.mapChildren(other) { child => check(child, false); child }

      check(normalized, false)
      if replaced then normalized else Unset

    private def compiletimeApplication(body: Optional[Type], scope: Scope): Boolean =
      def compiletime(head: Type): Boolean =
        Universe.className(head).lay(false)(_.s.startsWith("scala.compiletime.ops."))

      body match
        case Type.Lambda(_, _, _, Type.Applied(head, _))      => compiletime(head)
        case Type.Abstraction(_, _, _, Type.Applied(head, _)) => compiletime(head)
        case _                                                => false

    // Whether a parameterized alias's body applies some constructor to exactly its own parameters,
    // in order.
    private def etaExpansion(body: Optional[Type]): Boolean = body match
      case Type.Lambda(address, names, _, Type.Applied(_, arguments)) =>
        arguments.size == names.size &&
        arguments.stdlib.zipWithIndex.forall:
          case (Type.Param(binder, number), index) => binder == address && number == index
          case _                                   => false

      case Type.Abstraction(params, _, _, Type.Applied(_, arguments)) =>
        arguments.size == params.size &&
        arguments.stdlib.zip(params.stdlib).forall:
          case (Type.TypeRefDirect(address), param)   => address == param
          case (Type.TypeRefLocal(address, _), param) => address == param
          case _                                      => false

      case _ => false

    // The compiler's `typeSymbol` of a term reference: the symbol its declared type names — an
    // alias as much as a class, undealiased — through applications and wrappers; an application
    // reduces as the compiler reduces it, and a union or intersection names no symbol.
    private def termTypeSymbol(tpe: Type, scope: Scope): Text =
      def symbol(tpe: Type, scope: Scope): Text = tpe match
        case Type.TypeRef(_, _) | Type.TypeRefIn(_, _, _) | Type.TypeRefPkg(_)
            | Type.TypeRefDirect(_) | Type.TypeRefLocal(_, _) =>
          symbolOfType(tpe, t"", scope).fullName

        case Type.Applied(constructor, _) =>
          reduceApplication(tpe, scope) match
            case reduced: Type => symbol(reduced, scope)

            case _ =>
              universe.resolve(constructor, scope) match
                case Resolution.Class(info, _, _) => info.fullName
                case _                            => symbol(constructor, scope)

        case Type.And(_, _) | Type.Or(_, _) => t"<none>"
        case Type.Annotated(underlying, _)  => symbol(underlying, scope)
        case Type.Flexible(underlying)      => symbol(underlying, scope)
        case Type.ByName(underlying)        => symbol(underlying, scope)
        case Type.This(cls)                 => symbol(cls, scope)
        case Type.Opaque(_, bounds)         => symbol(bounds, scope)
        case Type.Bounds(_, high, _)        => symbol(high.or(tpe), scope)
        case Type.Refined(parent, _, _)     => symbol(parent, scope)

        case Type.Foreign(path, inner) =>
          universe.scope(path).lay(symbol(inner, scope))(symbol(inner, _))

        case other =>
          universe.resolve(other, scope) match
            case Resolution.Term(declared, termScope) =>
              symbol(declared, termScope.or(scope))

            case Resolution.Class(info, _, _) => info.fullName
            case _                            => throw Unencodable(t"type symbol of ${other.getClass.getSimpleName.nn}")

      symbol(tpe, scope)

    // --- constants ----------------------------------------------------------------------------

    // The compiler's own rendering, class name and all, of a constant: what the compiler-driven
    // atomizer wrote, kept so that the two agree byte for byte.
    private def constantText(constant: Constant, scope: Scope): String =
      "dotty.tools.dotc.core.Constants$Constant:" + constantShow(constant, scope)

    private def constantShow(constant: Constant, scope: Scope): String = constant match
      case Constant.UnitValue          => "()"
      case Constant.NullValue          => "null"
      case Constant.TrueValue          => "true"
      case Constant.FalseValue         => "false"
      case Constant.ByteValue(value)   => value.toString
      case Constant.ShortValue(value)  => value.toString
      case Constant.IntValue(value)    => value.toString
      case Constant.LongValue(value)   => value.toString + "L"
      case Constant.FloatValue(value)  => value.toString + "f"
      case Constant.DoubleValue(value) => value.toString + "d"
      case Constant.CharValue(value)   => escapedChar(value)
      case Constant.StringValue(value) => escapedString(value.s)
      case Constant.ClassValue(tpe)    => "classOf[" + typeShow(tpe, scope) + "]"

    // The compiler's printer omits the `scala`, `java.lang` and `Predef` prefixes of a class.
    private def typeShow(tpe: Type, scope: Scope): String =
      val full = symbolOfType(tpe, t"", scope).fullName.s

      scala.List("java.lang.", "scala.Predef$.", "scala.").find(full.startsWith) match
        case Some(prefix) => full.substring(prefix.length).nn
        case None         => full

    private def requiresFormat(char: Char): Boolean = char match
      case '\b' | '\t' | '\n' | '\f' | '\r' | '"' | '\'' | '\\' => true
      case _ => char < ' ' || char == '\u007f' || Character.isISOControl(char)

    private def escaped(builder: java.lang.StringBuilder, char: Char): Unit =
      val replacement = char match
        case '\b'                      => "\\b"
        case '\t'                      => "\\t"
        case '\n'                      => "\\n"
        case '\f'                      => "\\f"
        case '\r'                      => "\\r"
        case '"'                       => "\\\""
        case '\''                      => "\\'"
        case '\\'                      => "\\\\"
        case _ if requiresFormat(char) => "\\u%04x".format(char.toInt)
        case _                         => char.toString

      builder.append(replacement)

    private def escapedChar(char: Char): String =
      val builder = java.lang.StringBuilder()
      builder.append('\'')
      escaped(builder, char)
      builder.append('\'')
      builder.toString.nn

    private def escapedString(text: String): String =
      val builder = java.lang.StringBuilder()
      builder.append('"')
      text.foreach(escaped(builder, _))
      builder.append('"')
      builder.toString.nn

    // --- flags and keys -----------------------------------------------------------------------

    // The subset of modifier flags that consumers can depend on, in a fixed bit order. The
    // `exported` flag is deliberately absent (`tasty.md` §9): an `export` forwarder atomizes
    // identically to the equivalent hand-written forwarder, so converting between them is a
    // non-event.
    private def flagBits(modifiers: Modifiers, deferred: Boolean, module: Boolean = false,
        moduleValue: Boolean = false): Long =
      val flags = modifiers.flags

      val bits = scala.List(
        flags.contains(Flag.Abstract) || flags.contains(Flag.AbsOverride),
        flags.contains(Flag.Case), deferred, flags.contains(Flag.Enum),
        flags.contains(Flag.Erased), flags.contains(Flag.Final) || module || moduleValue,
        flags.contains(Flag.Given), flags.contains(Flag.Implicit), flags.contains(Flag.Infix),
        flags.contains(Flag.Inline), flags.contains(Flag.Lazy) || moduleValue,
        flags.contains(Flag.Macro), flags.contains(Flag.Object),
        flags.contains(Flag.Mutable), flags.contains(Flag.Opaque), flags.contains(Flag.Open),
        flags.contains(Flag.Protected), flags.contains(Flag.Sealed), flags.contains(Flag.Trait),
        flags.contains(Flag.Transparent))

      bits.zipWithIndex.foldLeft(0L): (acc, pair) =>
        if pair(0) then acc | (1L << pair(1)) else acc

    // Whether a definition is deferred: a term with no right-hand side (a parameter is not, nor
    // a constructor), or a type known only by bounds.
    private def deferred(tree: Tree): Boolean = tree match
      // A parameter accessor or setter lacks a body without being deferred (`normalizeFlags`).
      case Tree.ValDef(_, _, _, rhs, modifiers) => rhs.absent && !modifiers.has(Flag.ParamSetter)

      case Tree.DefDef(_, name, _, _, rhs, modifiers) =>
        rhs.absent && name.text != t"<init>" && !modifiers.has(Flag.ParamSetter)

      case Tree.TypeDef(_, _, rhs, modifiers) =>
        // An opaque type is abstract to everything outside its scope, the TASTy's readers
        // included; a higher-kinded abstract type is bounds under a lambda.
        def abstractRhs(rhs: Tree): Boolean = rhs match
          case Tree.BoundsTpt(_, _, alias)        => alias.absent
          case Tree.Path(Type.Bounds(_, high, _)) => high.present
          case Tree.LambdaTpt(_, body)            => abstractRhs(body)
          case _                                  => false

        modifiers.has(Flag.Opaque) || abstractRhs(rhs)

      case Tree.TypeParam(_, _, _, _) => true
      case _                          => false

    private def modifiersOf(tree: Tree): Modifiers = tree match
      case Tree.ValDef(_, _, _, _, modifiers)    => modifiers
      case Tree.DefDef(_, _, _, _, _, modifiers) => modifiers
      case Tree.TypeDef(_, _, _, modifiers)      => modifiers
      case Tree.TypeParam(_, _, _, modifiers)    => modifiers
      case Tree.Param(_, _, _, modifiers)        => modifiers
      case _                                     => Modifiers.empty

    private def nameOf(tree: Tree): Text = tree match
      case Tree.ValDef(_, name, _, _, _)    => name.text
      case Tree.DefDef(_, name, _, _, _, _) => name.text
      case Tree.TypeDef(_, name, _, _)      => name
      case Tree.TypeParam(_, name, _, _)    => name
      case Tree.Param(_, name, _, _)        => name.text
      case _                                => t""

    private def addressOf(tree: Tree): Int = tree match
      case Tree.ValDef(address, _, _, _, _)    => address
      case Tree.DefDef(address, _, _, _, _, _) => address
      case Tree.TypeDef(address, _, _, _)      => address
      case Tree.TypeParam(address, _, _, _)    => address
      case Tree.Param(address, _, _, _)        => address
      case _                                   => -1

    // The erased-signature disambiguator of a term member's key: overloads differ here, and the
    // spelling matches what consumers' TASTy references carry in their SIGNED names.
    private def signatureText(tree: Tree, scope: Scope, owner: Optional[ClassInfo]): Text =
      erasure.signature(tree, scope, owner)

    private def keyOf(ownerKey: Text, tree: Tree, scope: Scope, owner: Optional[ClassInfo]): String =
      s"${spelled(ownerKey.s)}.${nameOf(tree)}${signatureText(tree, scope, owner)}"

    private def excluded(tree: Tree): Boolean =
      val modifiers = modifiersOf(tree)
      val flags = modifiers.flags

      // Qualified-private (`private[scope]`) members are included, conservatively (`tasty.md`
      // §5): macros and derivation can reach them from dependency TASTy, so only unqualified
      // `private` and `private[this]` are outside the API.
      val qualified = modifiers.privateWithin.present
      val hidden = (flags.contains(Flag.Private) && !qualified) || flags.contains(Flag.Local)
      hidden || flags.contains(Flag.Artifact)

    private def record
      ( key:         String,
        replaceable: Boolean,
        data:        Data,
        references:  scala.List[String] = scala.Nil )

    :   Unit =

      if atoms.contains(key) then throw Unencodable(t"duplicate key $key")

      val listed = references.map { reference => ScalaReference.Own(reference.tt) }.to(List)
      atoms(key) = ScalaAtom(key.tt, replaceable, data, listed)

    // --- members ------------------------------------------------------------------------------

    // The declared type of a member, as the compiler's `info` presents it: a parameterless
    // method is an expression type, a method with clauses a methodic type, a type member its
    // bounds (an alias being bounds that coincide).
    private def infoOf(tree: Tree): Type = tree match
      case Tree.DefDef(address, _, clauses, returns, _, _) =>
        if clauses.nil then Type.ByName(Universe.typeOf(returns))
        else Universe.methodType(address, clauses, returns)

      case Tree.ValDef(_, _, tpt, _, _)    => Universe.typeOf(tpt)
      case Tree.Param(_, _, tpt, _)        => Universe.typeOf(tpt)
      case Tree.TypeParam(_, _, bounds, _) => Universe.typeOf(bounds)

      // A type alias's info is bounds that coincide; an opaque type's, its declared bounds, or
      // the unbounded ones when it declares none. A higher-kinded abstract or opaque type is
      // bounded below by `Nothing` and above by a lambda to its upper bound.
      case Tree.TypeDef(_, _, rhs, modifiers) => Universe.typeOf(rhs) match
        case Type.Bounds(low, Unset, variances) => Type.Bounds(low, low, variances)
        case bounds: Type.Bounds                => bounds
        case Type.Opaque(_, bounds)             => bounds

        case Type.Abstraction(params, names, bounds, Type.Opaque(_, Type.Bounds(_, high, _))) =>
          val any = Type.TypeRef(t"Any", Type.TermRefPkg(t"scala"))

          Type.Bounds(Type.TypeRef(t"Nothing", Type.TermRefPkg(t"scala")),
              Type.Abstraction(params, names, bounds, high.or(any)), List())

        case Type.Abstraction(params, names, bounds, _) if modifiers.has(Flag.Opaque) =>
          val any = Type.TypeRef(t"Any", Type.TermRefPkg(t"scala"))

          Type.Bounds(Type.TypeRef(t"Nothing", Type.TermRefPkg(t"scala")),
              Type.Abstraction(params, names, bounds, any), List())

        case _ if modifiers.has(Flag.Opaque) => unbounded
        case alias                           => Type.Bounds(alias, alias, List())

      case _                              => throw Unencodable(t"info of a non-member")

    private val unbounded: Type =
      Type.Bounds(Type.TypeRef(t"Nothing", Type.TermRefPkg(t"scala")),
          Type.TypeRef(t"Any", Type.TermRefPkg(t"scala")), List())

    // A constructor's clauses as the compiler normalizes them: an empty clause is prepended
    // before a leading `implicit` clause, and appended after clauses that are all `using`.
    private def normalizedClauses(tree: Tree): scala.List[List[Tree]] = tree match
      case Tree.DefDef(_, name, clauses, _, _, _) if name.text == t"<init>" =>
        val all = clauses.stdlib

        def isTypeClause(clause: List[Tree]): Boolean = clause.prim match
          case Tree.TypeParam(_, _, _, _) => true
          case _                          => false

        def leads(clause: List[Tree], flag: Flag): Boolean = clause.prim match
          case Tree.Param(_, _, _, modifiers) => modifiers.has(flag)
          case _                              => false

        val (typeClauses, termClauses) = all.span(isTypeClause)

        val normalizedTerms =
          if termClauses.headOption.exists(leads(_, Flag.Implicit)) then List[Tree]() :: termClauses
          else if termClauses.forall { clause => leads(clause, Flag.Given) } then termClauses :+ List[Tree]()
          else termClauses

        typeClauses ++ normalizedTerms

      case Tree.DefDef(_, _, clauses, _, _, _) => clauses.stdlib
      case _                                   => scala.Nil

    // The binders a definition's own clauses introduce, for the parameter references in its
    // type: each clause of a method is a binder over its parameters' addresses.
    private def clauseBinders(tree: Tree): scala.List[Binder] = tree match
      case Tree.DefDef(address, _, clauses, _, _, _) =>
        clauses.stdlib.map: clause =>
          Binder(address, clause.stdlib.map(addressOf))

      case _ => scala.Nil

    private def termAtom(ownerKey: Text, tree: Tree, scope: Scope, owner: Optional[ClassInfo]): Unit =
      val modifiers = modifiersOf(tree)

      // The value of an object is final and lazy to the compiler, whatever its definition says.
      val moduleValue = tree.isInstanceOf[Tree.ValDef] && modifiers.has(Flag.Object)

      val encoded = Array.collect[Byte](): out =>
        tag(out, if tree.isInstanceOf[Tree.DefDef] then 'd' else 'v')
        uvarint(out, flagBits(modifiers, deferred(tree), moduleValue = moduleValue))

        // Parameter names are not part of the key, but they are named-argument surface, so they
        // fold into the value; `HASDEFAULT` existence travels through the flag bits of the
        // default getters, which are ordinary (synthetic) members.
        normalizedClauses(tree).foreach: clause =>
          uvarint(out, clause.size.toLong)
          clause.each: param => utf8(out, nameOf(param).s)

        encodeInfo(out, tree, scope)
        annotations(out, modifiers, scope)

      record(keyOf(ownerKey, tree, scope, owner), replaceable = false, encoded)

      if modifiers.has(Flag.Inline) || modifiers.has(Flag.Macro) then
        inlineAtom(ownerKey, tree, scope, owner)

    // Encodes a member's declared type. A method's clauses bind its parameters, so references to
    // them inside the type encode as parameter references, as the compiler's method types have
    // them; the clauses themselves are encoded as nested methodic types, innermost last.
    private def encodeInfo(out: Scribe[Byte], tree: Tree, scope: Scope): Unit = tree match
      case Tree.DefDef(address, name, clauses, returns, _, modifiers) if !clauses.nil =>
        def encodeClauses(remaining: scala.List[List[Tree]], binders: scala.List[Binder]): Unit =
          remaining.headOption match
            case Some(clause) =>
              val rest = remaining.tail
              val trees = clause.stdlib
              val binder = Binder(address, trees.map(addressOf))
              val inner = binder :: binders

              trees.headOption match
                case Some(Tree.TypeParam(_, _, _, _)) =>
                  tag(out, 'P')
                  uvarint(out, trees.length.toLong)

                  trees.foreach:
                    case Tree.TypeParam(_, _, bounds, _) =>
                      encodeType(out, strip(Universe.typeOf(bounds), scope), inner, scope)

                    case _ => ()

                  encodeClauses(rest, inner)

                case _ =>
                  tag(out, 'M')

                  val implicitClause = trees.exists:
                    case Tree.Param(_, _, _, modifiers) =>
                      modifiers.has(Flag.Implicit) || modifiers.has(Flag.Given)

                    case _ => false

                  if implicitClause then tag(out, 'i')
                  uvarint(out, trees.length.toLong)

                  trees.foreach:
                    case param: Tree.Param =>
                      val info = selections(parameterInfo(param, scope))
                      val leaked = if modifiers.has(Flag.Synthetic) then info else privateLeaks(info, scope)
                      encodeType(out, leaked, inner, scope)

                    case _ => ()

                  encodeClauses(rest, inner)

            case None =>
              val untouched = modifiers.has(Flag.Synthetic) && clauses.stdlib.forall(_.nil)

              val mapped =
                if untouched then resultType
                else if modifiers.has(Flag.Synthetic) then selections(strip(resultType, scope))
                else privateLeaks(selections(strip(resultType, scope)), scope)

              encodeType(out, mapped, binders, scope)

        // Integrating the parameters into the method type maps every reference to them, and a
        // type selected on a parameter — `instantiable.Self` — then reduces (`derivedSelect`,
        // `lookupRefined`) to the alias the parameter's type refines it with, if any.
        def paramAddresses = clauses.stdlib.flatMap(_.stdlib).collect { case Tree.Param(a, _, _, _) => a }

        def selections(tpe: Type): Type = Universe.transform(tpe):
          case selected @ Type.TypeRef(member, Type.TermRefDirect(address)) if paramAddresses.contains(address) =>
            refinedAlias(address, member).or(selected)

          case selected @ Type.TypeRef(member, Type.TermRefLocal(address, _)) if paramAddresses.contains(address) =>
            refinedAlias(address, member).or(selected)

        def refinedAlias(address: Int, member: Text): Optional[Type] =
          def loop(tpe: Type, depth: Int): Optional[Type] =
            if depth > 8 then Unset else tpe match
              case Type.Refined(parent, refined, info) =>
                if refined == member then info match
                  case Type.Bounds(low, high, _) if high == low => low
                  case Type.Bounds(_, _, _)                     => loop(parent, depth + 1)
                  case alias                                    => alias

                else loop(parent, depth + 1)

              case Type.Annotated(underlying, _) => loop(underlying, depth)
              case Type.Foreign(_, inner)        => loop(inner, depth)
              case other                         => dealias(other, scope).let(loop(_, depth + 1))

          scope.definition(address).let:
            case Tree.Param(_, _, tpt, _) => loop(Universe.typeOf(tpt), 0)
            case _                        => Unset

        // A constructor's result is its class, applied to the constructor's own type parameters;
        // every tracked parameter (of a constructor, or of a given method) refines the result with
        // its singleton type.
        def resultType: Type =
          val plain =
            if name.text == t"<init>" then
              val cls = scope.enclosingClass(address).or(throw Unencodable(t"constructor's class"))

              val typeParams = clauses.stdlib.headOption.toList.flatMap: clause =>
                clause.stdlib.collect { case Tree.TypeParam(address, _, _, _) => Type.TypeRefDirect(address) }

              if typeParams.isEmpty then Type.TypeRefDirect(cls)
              else Type.Applied(Type.TypeRefDirect(cls), typeParams.to(List))

            else Universe.typeOf(returns)

          // A given method's tracked parameters would refine its result too, but only when the
          // reader has the modularity feature enabled, which the atomizer's compiler never did.
          val tracked =
            if name.text != t"<init>" then scala.Nil
            else clauses.stdlib.flatMap(_.stdlib).collect:
              case Tree.Param(address, paramName, _, modifiers) if modifiers.has(Flag.Tracked) =>
                (paramName.text, address)

          tracked.foldLeft(plain): (result, param) =>
            Type.Refined(result, param(0), Type.TermRefDirect(param(1)))

        encodeClauses(normalizedClauses(tree), scala.Nil)

      // A class parameter's accessor is mapped like a parameter; a synthetic value's declared type
      // reaches its symbol as it stands.
      case Tree.Param(_, _, tpt, _) => encodeType(out, follow(Universe.typeOf(tpt), scope), scala.Nil, scope)

      case other =>
        val synthetic = modifiersOf(other).has(Flag.Synthetic)
        val untouched = isTerm(other) && synthetic

        val mapped =
          if untouched then infoOf(other)
          else if isTerm(other) then privateLeaks(strip(infoOf(other), scope), scope)
          else strip(infoOf(other), scope)

        encodeType(out, mapped, scala.Nil, scope)

    private def inlineAtom(ownerKey: Text, tree: Tree, scope: Scope, owner: Optional[ClassInfo]): Unit =
      // The body of an inline or macro definition is copied into consumers at their compile
      // time: a replaceable atom (LIRA §10.2), keyed by the declaration, whose value is the
      // canonical encoding of the body tree — local names alpha-normalized, every outward
      // reference fully qualified — and whose reference list names everything the body splices
      // into consumers, for used-set closure (LIRA §13.4).
      val references = mutable.TreeSet[String]()
      val definitionAddress = addressOf(tree)

      val encoded = Array.collect[Byte](): out =>
        tag(out, 'i')

        val locals = mutable.HashMap[Int, Int]()

        def localIndex(address: Int): Int = locals.getOrElseUpdate(address, locals.size)

        def reference(symbol: Symbol): Unit =
          if symbol.isPackage || symbol.fullName == t"<none>" then ()
          else if symbol.isType then references += spelled(symbol.fullName.s)
          else
            // The symbol's own name is what follows its owner's key, which is not the last dotted
            // segment when the symbol is a carrier of a dotted file name.
            val full = symbol.fullName.s
            val prefix = symbol.ownerKey.s + "."

            val simple =
              if isCarrier(symbol.fullName) && symbol.ownerKey.s.nonEmpty && full.startsWith(prefix)
              then full.substring(prefix.length).nn
              else full.substring(full.lastIndexOf('.') + 1).nn

            // A reference is keyed as the atom it refers to is: a constructor's default getter by
            // its member name, not by its `fullName` spelling.
            val keyed = simple.replace("<init>$default$", "$lessinit$greater$default$").nn
            val key = s"${spelled(symbol.ownerKey.s)}.$keyed${symbol.signature}"
            references += key

            // Closure through nested inlining: using an inline member copies its body too.
            if symbol.inline then references += key + "[inline]"

        def name(symbol: Symbol): Unit =
          symbol.address match
            case address: Int if isWithin(symbol, definitionAddress, scope) =>
              tag(out, 'l')
              uvarint(out, localIndex(address).toLong)

            case _ =>
              tag(out, 'g')
              utf8(out, spelled(symbol.fullName.s) + symbol.signature.s)
              reference(symbol)

        def encodeTypeHere(tpe: Type): Unit = encodeType(out, tpe, scala.Nil, scope)

        def isTermPattern(tree: Tree): Boolean = tree match
          case Tree.Bind(_, _, _, _, _) | Tree.Unapply(_, _, _, _) | Tree.Alternative(_) => false
          case _                                                                         => true

        def term(tree: Tree): Unit = tree match
          case Tree.Inlined(body, _, bindings) =>
            tag(out, 'I')
            uvarint(out, bindings.size.toLong)
            bindings.each(term)
            term(body)

          // A wildcard pattern is an identifier to the compiler's reflection, whose symbol is
          // whatever its type names — a term, or the type it is ascribed — or none at all.
          case Tree.Ident(wildcard, tpe) if wildcard.unsigned.text == t"_" =>
            tag(out, 'x')

            // Only a named type (or a `this`) has a denotation to take a symbol from; the
            // compiler reads the symbol off the type with its annotations stripped.
            def stripAnnotations(tpe: Type): Type = tpe match
              case Type.Annotated(underlying, _) => stripAnnotations(underlying)
              case other                         => other

            val stripped = stripAnnotations(tpe)

            val named = stripped match
              case Type.TypeRef(_, _) | Type.TypeRefIn(_, _, _) | Type.TypeRefPkg(_)
                  | Type.TypeRefDirect(_) | Type.TypeRefLocal(_, _) | Type.TermRef(_, _)
                  | Type.TermRefIn(_, _, _) | Type.TermRefPkg(_) | Type.TermRefDirect(_)
                  | Type.TermRefLocal(_, _) | Type.This(_) => true

              case _ => false

            val symbol =
              if !named then Unset
              else
                try symbolOfType(stripped, t"_", scope)
                catch case _: Unencodable => Unset

            symbol.lay({ tag(out, 'g'); utf8(out, "<none>()") })(name(_))

          case Tree.Ident(_, _) =>
            tag(out, 'x')
            name(symbolOf(tree, scope))

          case Tree.Path(tpe) => path(tpe)

          case Tree.Select(_, qualifier) =>
            tag(out, '.')
            term(qualifier)
            name(symbolOf(tree, scope))

          case Tree.SelectIn(_, qualifier, _) =>
            tag(out, '.')
            term(qualifier)
            name(symbolOf(tree, scope))

          case Tree.New(tpt) =>
            tag(out, 'n')
            encodeTypeHere(Universe.typeOf(tpt))

          case Tree.Apply(function, arguments) =>
            if arguments.nil && spurious(function) then term(function)
            else
              tag(out, 'a')
              term(function)
              uvarint(out, arguments.size.toLong)
              arguments.each(term)

          case Tree.TypeApply(function, arguments) =>
            tag(out, 'y')
            term(function)
            uvarint(out, arguments.size.toLong)
            arguments.each: argument => encodeTypeHere(Universe.typeOf(argument))

          case Tree.Typed(expression, tpt) =>
            tag(out, if isTermPattern(expression) then ':' else 'o')
            term(expression)
            encodeTypeHere(Universe.typeOf(tpt))

          case Tree.Block(statements, expression) =>
            tag(out, '{')

            // Imports and exports among a body's statements are purely lexical: every
            // reference this encoding emits is already fully-qualified, so they carry no
            // semantic content and fold into nothing, like positions.
            val retained = statements.filter:
              case Tree.Import(_, _) => false
              case Tree.Export(_, _) => false
              case _                 => true

            uvarint(out, retained.size.toLong)
            retained.each(term)
            term(expression)

          case Tree.If(_, condition, positive, negative) =>
            tag(out, '?')
            term(condition)
            term(positive)
            term(negative)

          case Tree.Match(Tree.MatchKind.Implicit, _, cases) =>
            tag(out, 'f')
            uvarint(out, cases.size.toLong)
            cases.each(term)

          case Tree.Match(kind, scrutinee, cases) =>
            tag(out, if kind == Tree.MatchKind.Inline then 'M' else 'm')
            scrutinee.let(term(_))
            uvarint(out, cases.size.toLong)
            cases.each(term)

          case Tree.CaseDef(pattern, guard, rhs) =>
            tag(out, 'e')
            term(pattern)
            guard.lay(tag(out, '0'))(term(_))
            term(rhs)

          case Tree.Bind(address, _, _, pattern, _) =>
            tag(out, 'b')
            uvarint(out, localIndex(address).toLong)
            term(pattern)

          case Tree.Unapply(function, implicits, _, patterns) =>
            tag(out, 'u')
            term(function)
            uvarint(out, implicits.size.toLong)
            implicits.each(term)
            uvarint(out, patterns.size.toLong)
            patterns.each(term)

          case Tree.Alternative(patterns) =>
            tag(out, '/')
            uvarint(out, patterns.size.toLong)
            patterns.each(term)

          case Tree.While(condition, body) =>
            tag(out, 'w')
            term(condition)
            term(body)

          case Tree.Assign(lhs, rhs) =>
            tag(out, '=')
            term(lhs)
            term(rhs)

          case Tree.Return(_, expression) =>
            tag(out, 'j')
            expression.lay(tag(out, '0'))(term(_))

          case Tree.Try(expression, cases, finalizer) =>
            tag(out, 'q')
            term(expression)
            uvarint(out, cases.size.toLong)
            cases.each(term)
            finalizer.lay(tag(out, '0'))(term(_))

          case Tree.Repeated(elements, tpt) =>
            tag(out, '*')
            uvarint(out, elements.size.toLong)
            elements.each(term)
            encodeTypeHere(Universe.typeOf(tpt))

          case Tree.Lambda(method, _) =>
            tag(out, '\\')
            term(method)

          case Tree.NamedArg(argName, argument) =>
            tag(out, '$')
            utf8(out, argName.s)
            term(argument)

          case Tree.ValDef(address, _, tpt, rhs, _) =>
            tag(out, 'V')
            uvarint(out, localIndex(address).toLong)
            encodeTypeHere(Universe.typeOf(tpt))
            rhs.lay(tag(out, '0'))(term(_))

          case Tree.DefDef(address, _, clauses, returns, rhs, _) =>
            tag(out, 'D')
            uvarint(out, localIndex(address).toLong)

            clauses.each: clause =>
              clause.each:
                case Tree.Param(address, _, _, _) => uvarint(out, localIndex(address).toLong)
                case _                            => ()

            encodeTypeHere(Universe.typeOf(returns))
            rhs.lay(tag(out, '0'))(term(_))

          case Tree.QualifiedThis(qualifier) =>
            tag(out, 'z')
            utf8(out, spelled(symbolOf(qualifier, scope).fullName.s))

          case Tree.Elided(tpe) =>
            tag(out, 'T')
            encodeTypeHere(tpe)

          // The compiler's reflection presents a quote or a splice as the application of the
          // quotation runtime's `quote`/`splice` to its body, with the quoted type as type
          // argument; this encoding keeps that shape.
          case Tree.Quote(body, tpe) =>
            quotation(t"quote", tpe, body)

          case Tree.Splice(expression, tpe) =>
            quotation(t"splice", tpe, expression)

          case Tree.SplicePattern(pattern, tpe, typeArguments, arguments) =>
            tag(out, 'Y')
            term(pattern)
            encodeTypeHere(tpe)
            uvarint(out, typeArguments.size.toLong)
            typeArguments.each: argument => encodeTypeHere(Universe.typeOf(argument))
            uvarint(out, arguments.size.toLong)
            arguments.each(term)

          case Tree.QuotePattern(bindings, body, quotes, tpe) =>
            tag(out, 'W')
            uvarint(out, bindings.size.toLong)
            bindings.each(term)
            term(body)
            term(quotes)
            encodeTypeHere(tpe)

          case Tree.Hole(_, index, tpe, arguments) =>
            tag(out, 'H')
            uvarint(out, index.toLong)
            encodeTypeHere(tpe)
            uvarint(out, arguments.size.toLong)
            arguments.each(term)

          case other if other.isTypeTree =>
            tag(out, 'T')
            encodeTypeHere(Universe.typeOf(other))

          case other => throw Unencodable(t"term ${other.getClass.getSimpleName.nn}")

        def quotation(method: Text, tpe: Type, argument: Tree): Unit =
          tag(out, 'a')
          tag(out, 'y')
          tag(out, 'x')
          val runtime = Type.TermRef(Name.Simple(t"Expr"), Type.TermRefPkg(t"scala.quoted.runtime"))
          name(select(Name.Simple(method), runtime, scope))
          uvarint(out, 1L)
          encodeTypeHere(tpe)
          uvarint(out, 1L)
          term(argument)

        // A path or constant in term position is a reference tree or a literal to the compiler.
        def path(tpe: Type): Unit = tpe match
          case Type.Literal(constant) =>
            tag(out, 'k')
            utf8(out, constantText(constant, scope))

          case Type.This(cls) =>
            tag(out, 'z')
            utf8(out, spelled(symbolOfType(cls, t"", scope).fullName.s))

          case Type.TermRef(_, prefix) =>
            val symbol = symbolOfType(tpe, t"", scope)

            if isStatic(symbol) || elidable(prefix, symbol) then
              tag(out, 'x')
              name(symbol)

            else
              tag(out, '.')
              path(prefix)
              name(symbol)

          case Type.TermRefIn(_, prefix, _) =>
            val symbol = symbolOfType(tpe, t"", scope)

            if isStatic(symbol) || elidable(prefix, symbol) then
              tag(out, 'x')
              name(symbol)

            else
              tag(out, '.')
              path(prefix)
              name(symbol)

          case Type.TermRefDirect(_) | Type.TermRefPkg(_) =>
            tag(out, 'x')
            name(symbolOfType(tpe, t"", scope))

          case Type.TermRefLocal(_, prefix) =>
            val symbol = symbolOfType(tpe, t"", scope)

            if isStatic(symbol) || isWithin(symbol, definitionAddress, scope) || elidable(prefix, symbol) then
              tag(out, 'x')
              name(symbol)

            else
              tag(out, '.')
              path(prefix)
              name(symbol)

          case Type.Annotated(underlying, _) => path(underlying)

          case other =>
            tag(out, 'T')
            encodeTypeHere(other)

        // Whether a term's owner chain is all packages and objects, which is when the compiler
        // refers to it by a bare identifier rather than a selection.
        // The compiler reads a `C.this.x` reference as a bare identifier (`prefixIsElidable`)
        // when `C` is a static owner — an object nested only in objects and packages — or when
        // `x` is a parameter accessor of `C` itself and `C` is not a trait.
        def elidable(prefix: Type, symbol: Symbol): Boolean = prefix match
          case Type.This(cls) =>
            universe.classOf(cls, scope).lay(false): info =>
              val member = symbol.fullName.s.substring(symbol.fullName.s.lastIndexOf('.') + 1).nn.tt

              val accessor = info.members.exists: m =>
                m.name == member && m.flags.contains(Flag.ParamAccessor)

              isStaticOwner(info) || (!info.isTrait && accessor && symbol.ownerKey == info.fullName)

          // A prefix that is a static object: `Variable.generic`, whichever class declares
          // `generic`.
          case Type.TermRef(_, _) | Type.TermRefIn(_, _, _) | Type.TermRefLocal(_, _)
              | Type.TermRefDirect(_) =>
            val module = try symbolOfType(prefix, t"", scope).fullName catch case _: Unencodable => t""
            universe.classOf(t"$module$$").lay(false)(isStaticOwner)

          case _ => false

        def isStaticOwner(info: ClassInfo): Boolean =
          info.isObject && {
            val full = info.fullName.s
            val index = full.lastIndexOf('.')
            index < 0 || universe.classOf(full.substring(0, index).nn.tt).lay(true)(isStaticOwner)

          }

        def isStatic(symbol: Symbol): Boolean =
          symbol.ownerKey == t"" || symbol.ownerKey.s.indexOf('.') < 0 ||
            universe.classOf(symbol.ownerKey).lay(true)(_.isObject) ||
            universe.classOf(t"${symbol.ownerKey}$$").present && universe.classOf(symbol.ownerKey).absent

        // The compiler drops an empty argument list applied to a parameterless method that
        // overrides a Java or `Any` member (`x.toString()`).
        def spurious(function: Tree): Boolean =
          def parameterless: Boolean =
            try
              val symbol = symbolOf(function, scope)

              symbol.address.lay(false): address =>
                symbol.scope.let(_.definition(address)).lay(false):
                  case Tree.DefDef(_, _, clauses, _, _, _) => clauses.nil
                  case _                                   => false

            catch case _: Unencodable => false

          function match
            case Tree.Select(name, _)      => spuriousName(name.unsigned.text) && parameterless
            case Tree.SelectIn(name, _, _) => spuriousName(name.unsigned.text) && parameterless
            case Tree.Ident(name, _)       => spuriousName(name.unsigned.text) && parameterless
            case _                         => false

        def spuriousName(name: Text): Boolean = name == t"toString" || name == t"hashCode"

        tree match
          case Tree.DefDef(_, _, clauses, _, rhs, _) =>
            clauses.each: clause =>
              clause.each:
                case Tree.Param(address, _, _, _) => uvarint(out, localIndex(address).toLong)
                case _                            => ()

            rhs.lay(tag(out, '0'))(term(_))

          case Tree.ValDef(_, _, _, rhs, _) => rhs.lay(tag(out, '0'))(term(_))
          case _                            => tag(out, '0')

      record(keyOf(ownerKey, tree, scope, owner) + "[inline]", replaceable = true, encoded,
          references.toList)

    private def typeMemberAtom(ownerKey: Text, tree: Tree, scope: Scope): Unit =
      val modifiers = modifiersOf(tree)

      val encoded = Array.collect[Byte](): out =>
        tag(out, 't')
        uvarint(out, flagBits(modifiers, deferred(tree)))
        encodeType(out, strip(infoOf(tree), scope), scala.Nil, scope)
        annotations(out, modifiers, scope)

      record(s"${spelled(ownerKey.s)}.${nameOf(tree)}", replaceable = false, encoded)

    private def annotations(out: Scribe[Byte], modifiers: Modifiers, scope: Scope): Unit =
      val names = modifiers.annotations.stdlib
        . map: annotation => annotationName(annotation.tree, scope)
        . filter: name => !denylisted(name)
        . map(_.s)
        . sorted

      uvarint(out, names.length.toLong)
      names.foreach: name => utf8(out, name)

    // --- templates ----------------------------------------------------------------------------
    private def classAtoms(tree: Tree.TypeDef, template: Tree.Template, scope: Scope): Unit =
      if !excluded(tree) then
        val fullName = scope.names.at(tree.address).or(throw Unencodable(t"unnamed class"))
        val isCarrier = this.isCarrier(tree.name)
        val ownerKey = ownerKeyOf(tree.address, scope)
        val info = universe.classOf(fullName)

        if !isCarrier then templateAtom(tree, template, fullName, scope)

        val declarations =
          template.typeParams.stdlib ++ template.params.stdlib ++ template.body.stdlib

        // The compiler's reflection counts neither a `case` value — an enum case, a case object's
        // value — as a value definition nor a `case` type as a type definition, so neither is
        // atomized.
        constructorProxies(tree, template, scope, info)

        declarations.foreach: member =>
          if !excluded(member) && !modifiersOf(member).has(Flag.Case) then member match
            case nested @ Tree.TypeDef(_, _, inner: Tree.Template, _)  => classAtoms(nested, inner, scope)
            case Tree.TypeDef(_, _, _, _) | Tree.TypeParam(_, _, _, _) => typeMemberAtom(ownerKey, member, scope)

            case Tree.DefDef(_, _, _, _, _, _) | Tree.ValDef(_, _, _, _, _) | Tree.Param(_, _, _, _) =>
              termAtom(ownerKey, member, scope, info)

            case _ => ()

          else if !excluded(member) then member match
            case nested @ Tree.TypeDef(_, _, inner: Tree.Template, _) => classAtoms(nested, inner, scope)
            case Tree.DefDef(_, _, _, _, _, _)                        => termAtom(ownerKey, member, scope, info)
            case _                                                    => ()

        // A nested class with constructor proxies to offer but no companion, and no term of its
        // name in the enclosing class or its bases, gets a synthetic companion from the compiler
        // (`addConstructorProxies`), entered among the enclosing class's declarations: a module
        // class with `Any` as its parent, its value, and the proxies.
        template.body.each:
          case nested @ Tree.TypeDef(_, nestedName, _: Tree.Template, nestedModifiers)
              if !nestedName.s.endsWith("$") && proxiesNeeded(nestedModifiers) =>
            val companionName = t"$nestedName$$"

            val hasCompanion = template.body.exists:
              case Tree.TypeDef(_, `companionName`, _: Tree.Template, _) => true
              case _                                                     => false

            val hasTerm = info.lay(false)(!universe.termsIn(_, nestedName).nil)
            if !hasCompanion && !hasTerm then syntheticCompanion(nested, scope)

          case _ => ()

        typeConstructorCompanions(template, ownerKey, info, scope)

    // An exported type alias of a class with a synthetic companion gets a forwarder to that
    // companion (`typeConstructorCompanion`): a parameterless, synthetic method of the companion's
    // type, unless the owner already has a term of the name.
    private def typeConstructorCompanions
      ( template: Tree.Template, ownerKey: Text, info: Optional[ClassInfo], scope: Scope )

    :   Unit =

      template.body.each:
        case Tree.TypeDef(_, name, rhs, modifiers)
            if !rhs.isInstanceOf[Tree.Template] && modifiers.has(Flag.Exported) =>
          underlyingClass(Universe.typeOf(rhs), scope).let: cls =>
            val proxies = !cls.java && proxiesNeeded(Modifiers(cls.flags, Unset, List()))
            val phantom = universe.classOf(t"${cls.fullName}$$").absent
            val companionName = t"$name$$"

            val hasTerm =
              info.lay(false)(!universe.termsIn(_, name).nil) ||
              template.body.exists:
                case Tree.TypeDef(_, `companionName`, _: Tree.Template, _) => true
                case _                                                     => false

            if proxies && phantom && !hasTerm then
              val encoded = Array.collect[Byte](): out =>
                tag(out, 'd')
                uvarint(out, 0L)
                tag(out, 'N')
                tag(out, 'R')
                utf8(out, spelled(s"${cls.fullName}$$"))
                uvarint(out, 0L)

              record(s"${spelled(ownerKey.s)}.$name()", replaceable = false, encoded)

        case _ => ()

    // The class an alias ultimately names (`underlyingClassRef`): through eta-expansions,
    // annotations, applications and further aliases, but not refinements, bounds or opaque types.
    private def underlyingClass(tpe: Type, scope: Scope): Optional[ClassInfo] = tpe match
      case Type.Abstraction(_, _, _, Type.Applied(head, _)) if etaExpansion(tpe) => underlyingClass(head, scope)
      case Type.Lambda(_, _, _, Type.Applied(head, _)) if etaExpansion(tpe)      => underlyingClass(head, scope)
      case Type.Annotated(underlying, _)                                         => underlyingClass(underlying, scope)
      case Type.Applied(head, _)                                                 => underlyingClass(head, scope)
      case Type.Bounds(low, Unset, _)                                            => underlyingClass(low, scope)

      case Type.Foreign(path, inner) =>
        universe.scope(path).lay(underlyingClass(inner, scope))(underlyingClass(inner, _))

      case Type.TypeRef(_, _) | Type.TypeRefIn(_, _, _) | Type.TypeRefPkg(_) | Type.TypeRefDirect(_)
          | Type.TypeRefLocal(_, _) =>
        universe.resolve(tpe, scope) match
          case Resolution.Class(info, _, _) => info

          case Resolution.Alias(0, apply, aliasScope, _, false) =>
            underlyingClass(apply(List()), aliasScope.or(scope))

          case _ => Unset

      case _ => Unset

    private def syntheticCompanion(nested: Tree.TypeDef, scope: Scope): Unit =
      val className = scope.names.at(nested.address).or(throw Unencodable(t"unnamed class"))
      val moduleName = t"$className$$"
      val modifiers = Modifiers(Set(Flag.Object, Flag.Synthetic), Unset, List())

      val moduleClass = Array.collect[Byte](): out =>
        tag(out, 'c')
        uvarint(out, flagBits(modifiers, deferred = false, module = true))
        uvarint(out, 0L)
        uvarint(out, 1L)
        tag(out, 'R')
        utf8(out, "scala.Any")
        tag(out, 's')
        tag(out, 'S')
        utf8(out, spelled(moduleName.s))
        uvarint(out, 0L)
        uvarint(out, 0L)
        uvarint(out, 0L)

      record(spelled(moduleName.s), replaceable = false, moduleClass)

      val moduleValue = Array.collect[Byte](): out =>
        tag(out, 'v')
        uvarint(out, flagBits(modifiers, deferred = false, moduleValue = true))
        tag(out, 'R')
        utf8(out, spelled(moduleName.s))
        uvarint(out, 0L)

      record(s"$className()", replaceable = false, moduleValue)
      constructorProxies(nested, moduleName, scope)

    // The compiler adds to the companion of every concrete, non-case class — unless it or a base
    // class defines `apply` — an inline `apply` per constructor, with the constructor's type and no
    // body (so it is deferred); classes with no companion get a synthetic one, which the walk
    // never reaches. These proxies are declarations of the companion to the compiler's reflection,
    // and so atoms.
    private def constructorProxies
      ( module: Tree.TypeDef, template: Tree.Template, scope: Scope, info: Optional[ClassInfo] )

    :   Unit =

      if module.modifiers.has(Flag.Object) then
        val companionName = module.name.s.dropRight(1).nn.tt

        val companion = scope.owners.at(module.address) match
          case owner: Int =>
            scope.definition(owner).let:
              case Tree.TypeDef(_, _, outer: Tree.Template, _) =>
                outer.body.stdlib.collectFirst:
                  case candidate @ Tree.TypeDef(_, `companionName`, _: Tree.Template, _) => candidate

                . getOrElse(Unset)

              case _ => Unset

            . or(Unset)

          case _ =>
            def topLevel(statements: scala.List[Tree]): scala.List[Tree] = statements.flatMap:
              case Tree.Package(_, inner) => topLevel(inner.stdlib)
              case other                  => scala.List(other)

            topLevel(scope.file.statements.stdlib).collectFirst:
              case candidate @ Tree.TypeDef(_, `companionName`, _: Tree.Template, _) => candidate

            . getOrElse(Unset)

        companion.let: companion =>
          val hasApply = info.lay(false): info =>
            !universe.termsIn(info, t"apply").nil

          if proxiesNeeded(companion.modifiers) && !hasApply then
            constructorProxies(companion, scope.names.at(module.address).or(t""), scope)

    // Whether a class gets constructor proxies: concrete, not a case class, not synthetic.
    private def proxiesNeeded(modifiers: Modifiers): Boolean =
      val flags = modifiers.flags

      !flags.contains(Flag.Abstract) && !flags.contains(Flag.Trait) && !flags.contains(Flag.Case) &&
      !flags.contains(Flag.Synthetic) && !flags.contains(Flag.Object) &&
      !flags.contains(Flag.Invisible)

    // The `apply` proxies for each constructor of `companion`, as members of the module `ownerKey`.
    private def constructorProxies(companion: Tree.TypeDef, ownerKey: Text, scope: Scope): Unit =
      companion.rhs match
        case classTemplate: Tree.Template =>
          val companionInfo = scope.names.at(companion.address).let(universe.classOf(_))

          classTemplate.body.each:
            case constructor @ Tree.DefDef(_, name, clauses, returns, _, modifiers)
                if name.text == t"<init>" && !excluded(constructor) =>
              val proxyModifiers = Modifiers(
                Set(Flag.Synthetic, Flag.Inline) ++ modifiers.flags.filter(f => f == Flag.Private ||
                    f == Flag.Protected || f == Flag.Local),

                modifiers.privateWithin, List())

              val encoded = Array.collect[Byte](): out =>
                tag(out, 'd')
                uvarint(out, flagBits(proxyModifiers, deferred = false))

                normalizedClauses(constructor).foreach: clause =>
                  uvarint(out, clause.size.toLong)
                  clause.each: param => utf8(out, nameOf(param).s)

                encodeInfo(out, constructor, scope)
                annotations(out, proxyModifiers, scope)

              val signature = erasure.signature(constructor, scope, companionInfo)
              val key = s"${spelled(ownerKey.s)}.apply$signature"
              record(key, replaceable = false, encoded)

              val body = Array.collect[Byte](): out =>
                tag(out, 'i')
                var index = 0

                clauses.each: clause =>
                  clause.each:
                    case Tree.Param(_, _, _, _) =>
                      uvarint(out, index.toLong)
                      index += 1

                    case _ => ()

                tag(out, '0')

              record(key + "[inline]", replaceable = true, body)

            case _ => ()

        case _ => ()

    private def templateAtom(tree: Tree.TypeDef, template: Tree.Template, fullName: Text, scope: Scope)
    :   Unit =

      val modifiers = tree.modifiers
      val info = universe.classOf(fullName)

      val encoded = Array.collect[Byte](): out =>
        tag(out, 'c')
        // A module class is final, whether or not its definition says so.
        uvarint(out, flagBits(modifiers, deferred = false, module = modifiers.has(Flag.Object)))

        // Type parameters: positional, variance and bounds folded; names alpha-normalized away.
        val typeParams = template.typeParams.stdlib
        uvarint(out, typeParams.length.toLong)

        typeParams.foreach:
          case Tree.TypeParam(_, _, bounds, paramModifiers) =>
            val variance =
              if paramModifiers.has(Flag.Covariant) then 1L
              else if paramModifiers.has(Flag.Contravariant) then 2L else 0L

            uvarint(out, variance)
            encodeType(out, strip(Universe.typeOf(bounds), scope), scala.Nil, scope)

          case _ => ()

        // Parents, in linearization-relevant declaration order. A parent that is a type tree
        // stands as written, alias and all; a constructor call's type is the constructed class —
        // `new AnyRef()` is a `java.lang.Object` — so an alias it names dealiases.
        val parents = template.parents.map: parent =>
          val tpe = Universe.parentType(parent)

          parent match
            case Tree.Apply(_, _) | Tree.TypeApply(_, _) | Tree.Block(_, _) =>
              val bare = tpe match
                case Type.TypeRef(_, _) | Type.TypeRefIn(_, _, _) | Type.TypeRefDirect(_)
                    | Type.TypeRefLocal(_, _) => dealias(tpe, scope).or(tpe)

                case _ => tpe

              strip(trackedRefinements(parent, bare, scope), scope)

            case _ => strip(tpe, scope)

        uvarint(out, parents.size.toLong)
        parents.each: parent => encodeType(out, parent, scala.Nil, scope)

        // The self type, when declared.
        template.self.lay(tag(out, '-')): (_, tpt) =>
          tag(out, 's')
          encodeType(out, Universe.typeOf(tpt), scala.Nil, scope)

        // Abstract members fold into an *open* template (adding one breaks external subclasses);
        // on sealed or final templates they are pure additions and do not fold.
        val open =
          !modifiers.has(Flag.Final) && !modifiers.has(Flag.Sealed) && !modifiers.has(Flag.Object)

        if open then
          val members = template.typeParams.stdlib ++ template.params.stdlib ++ template.body.stdlib

          val deferredKeys = members
            . filter: member => deferred(member) && !excluded(member)
            . map: member => keyOf(fullName, member, scope, info)
            . sorted

          uvarint(out, deferredKeys.length.toLong)
          deferredKeys.foreach: key => utf8(out, key)

        else
          uvarint(out, 0L)

        // The sealed/enum child list folds in declaration order: ordinals are behavior.
        if modifiers.has(Flag.Sealed) || modifiers.has(Flag.Enum) then
          val children = modifiers.annotations.stdlib.flatMap: annotation =>
            Universe.annotationType(annotation.tree) match
              case Type.Applied(constructor, List(child))
                  if Universe.className(constructor).lay(false)(_ == t"scala.annotation.internal.Child") =>
                scala.List(symbolOfType(child, t"", scope).fullName.s)

              case _ => scala.Nil

          . reverse

          uvarint(out, children.length.toLong)
          children.foreach: child => utf8(out, spelled(child))

        else
          uvarint(out, 0L)

        annotations(out, modifiers, scope)

      record(spelled(fullName.s), replaceable = false, encoded)

    // A constructor call's type carries a refinement for each tracked parameter of the
    // constructor (`effectiveResultType`), binding it to the argument passed.
    private def trackedRefinements(call: Tree, constructed: Type, scope: Scope): Type =
      def unwind(tree: Tree): (Tree, scala.List[List[Tree]]) = tree match
        case Tree.Apply(function, arguments) =>
          val (head, clauses) = unwind(function)
          (head, clauses :+ arguments)

        case Tree.TypeApply(function, _) => unwind(function)
        case Tree.Block(_, expression)   => unwind(expression)
        case other                       => (other, scala.Nil)

      val (head, argumentClauses) = unwind(call)

      val tracked: scala.List[(Text, Type)] = head match
        case Tree.SelectIn(name, Tree.New(_), owner) if name.unsigned.text == t"<init>" =>
          universe.classOf(owner, scope).lay(scala.Nil): info =>
            val constructors = info.scope.let(_.definition(info.address)).lay(scala.Nil):
              case Tree.TypeDef(_, _, Tree.Template(_, _, _, _, body), _) =>
                body.stdlib.collect:
                  case constructor @ Tree.DefDef(_, name, _, _, _, _) if name.text == t"<init>" => constructor

              case _ => scala.Nil

            def termClauses(constructor: Tree.DefDef): scala.List[List[Tree]] =
              constructor.clauses.stdlib.filter: clause =>
                clause.prim match
                  case Tree.Param(_, _, _, _) => true
                  case _                      => false

            constructors.find: c => termClauses(c).map(_.size) == argumentClauses.map(_.size)
              . map: constructor =>
                termClauses(constructor).zip(argumentClauses).flatMap: (params, arguments) =>
                  params.stdlib.zip(arguments.stdlib).collect:
                    case (Tree.Param(_, paramName, _, modifiers), argument) if modifiers.has(Flag.Tracked) =>
                      (paramName.text, Universe.termType(argument))

              . getOrElse(scala.Nil)

        case _ => scala.Nil

      tracked.foldLeft(constructed): (result, refinement) =>
        Type.Refined(result, refinement(0), refinement(1))

    private def isTerm(tree: Tree): Boolean = tree match
      case Tree.DefDef(_, _, _, _, _, _) | Tree.ValDef(_, _, _, _, _) | Tree.Param(_, _, _, _) => true
      case _                                                                                   => false

    // --- roots --------------------------------------------------------------------------------
    def file(scope: Scope): Unit =
      def walk(tree: Tree): Unit = tree match
        case Tree.Package(_, statements)                              => statements.each(walk)
        case typeDef @ Tree.TypeDef(_, _, template: Tree.Template, _) => classAtoms(typeDef, template, scope)
        case _                                                        => ()

      scope.file.statements.each(walk)

    // References were collected as bare keys; those matching an atom of this module are `Own`,
    // the rest `Foreign`, resolved against dependencies at assembly time.
    def result: List[ScalaAtom] =
      val keys = atoms.keySet.toSet

      atoms.values.toList.map: atom =>
        val classified = atom.references.map:
          case ScalaReference.Own(key) if !keys.contains(key.s) => ScalaReference.Foreign(key)
          case reference                                        => reference

        atom.copy(references = classified)

      . to(List)
