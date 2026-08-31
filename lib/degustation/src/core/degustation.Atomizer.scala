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

import scala.quoted.Quotes

import anticipation.*
import murmuration.map

// The atomization rules of `tasty/1`, applied over the compiler's reflection of unpickled
// TASTy. The folding principle (LIRA §10.3) governs every decision here: declarations whose
// addition cannot break consumers stand alone as atoms; fragments whose addition can break
// consumers fold into their enclosing atom's value, so the entire compatibility check stays set
// arithmetic.
//
// Canonicalization is structural, never printed: symbols appear as fully-qualified names, types
// as a tag-length-value fold over the type constructors, type-lambda parameters as de Bruijn
// indices, and members in sorted key order. Positions, tool versions and compiler-internal
// sharing never enter the encoding. A construct outside the encoder's vocabulary is a hard
// error, so vocabulary drift in the toolchain is detected rather than silently absorbed.
object Atomizer:

  private class Unencodable(val construct: String) extends RuntimeException(construct)

  def atomize(using quotes: Quotes)(roots: scala.List[quotes.reflect.Tree])
  :   scala.List[ScalaAtom] =

    import quotes.reflect.*

    val atoms = scala.collection.mutable.LinkedHashMap[String, ScalaAtom]()

    // --- canonical binary encoding -----------------------------------------------------------

    def uvarint(out: java.io.ByteArrayOutputStream, value0: Long): Unit =
      var value = value0

      while value >= 0x80L do
        out.write(((value & 0x7f) | 0x80).toInt)
        value >>>= 7

      out.write(value.toInt)

    def utf8(out: java.io.ByteArrayOutputStream, text: String): Unit =
      val bytes = text.getBytes("UTF-8").nn
      uvarint(out, bytes.length.toLong)
      out.write(bytes)

    def tag(out: java.io.ByteArrayOutputStream, char: Char): Unit = out.write(char.toInt)

    // The subset of modifier flags that consumers can depend on, in a fixed bit order. The
    // `exported` flag is deliberately absent (`tasty.md` §9): an `export` forwarder atomizes
    // identically to the equivalent hand-written forwarder, so converting between them is a
    // non-event.
    def flagBits(symbol: Symbol): Long =
      val flags = symbol.flags

      scala.List(
        Flags.Abstract, Flags.Case, Flags.Deferred, Flags.Enum, Flags.Erased,
        Flags.Final, Flags.Given, Flags.Implicit, Flags.Infix, Flags.Inline, Flags.Lazy,
        Flags.Macro, Flags.Module, Flags.Mutable, Flags.Opaque, Flags.Open, Flags.Protected,
        Flags.Sealed, Flags.Trait, Flags.Transparent)

      . zipWithIndex
      . foldLeft(0L): (bits, pair) => if flags.is(pair(0)) then bits | (1L << pair(1)) else bits

    // Structural type encoding. `binders` tracks enclosing lambda-like binders outermost-last,
    // so parameter references encode as (de Bruijn depth, index) and binder names vanish.
    def encodeType
      ( out: java.io.ByteArrayOutputStream, tpe: TypeRepr, binders: scala.List[Any] )
    :   Unit =

      tpe match
        case tpe: NamedType =>
          tpe match
            case _: TypeRef => tag(out, 'R')
            case _          => tag(out, 'S')

          utf8(out, tpe.typeSymbol.fullName)

        case AppliedType(constructor, arguments) =>
          tag(out, 'A')
          encodeType(out, constructor, binders)
          uvarint(out, arguments.length.toLong)
          arguments.foreach: argument => encodeType(out, argument, binders)

        case AndType(left, right) =>
          tag(out, '&')
          encodeType(out, left, binders)
          encodeType(out, right, binders)

        case OrType(left, right) =>
          tag(out, '|')
          encodeType(out, left, binders)
          encodeType(out, right, binders)

        case ByNameType(underlying) =>
          tag(out, 'N')
          encodeType(out, underlying, binders)

        case AnnotatedType(underlying, annotation) =>
          // Interop and diagnostic annotations are denylisted (they never alter what Scala
          // consumers can depend on); every other retained annotation folds.
          if denylisted(annotation.tpe.typeSymbol.fullName) then
            encodeType(out, underlying, binders)
          else
            tag(out, '@')
            utf8(out, annotation.tpe.typeSymbol.fullName)
            encodeType(out, underlying, binders)

        case ConstantType(constant) =>
          tag(out, 'C')
          utf8(out, constant.getClass.getName.nn + ":" + constant.show)

        case tpe: TypeLambda =>
          tag(out, 'L')
          uvarint(out, tpe.paramNames.length.toLong)
          tpe.paramBounds.foreach: bounds => encodeType(out, bounds, tpe :: binders)
          encodeType(out, tpe.resType, tpe :: binders)

        case tpe: MethodType =>
          tag(out, 'M')
          if tpe.isImplicit then tag(out, 'i')
          uvarint(out, tpe.paramNames.length.toLong)
          tpe.paramTypes.foreach: param => encodeType(out, param, tpe :: binders)
          encodeType(out, tpe.resType, tpe :: binders)

        case tpe: PolyType =>
          tag(out, 'P')
          uvarint(out, tpe.paramNames.length.toLong)
          tpe.paramBounds.foreach: bounds => encodeType(out, bounds, tpe :: binders)
          encodeType(out, tpe.resType, tpe :: binders)

        case tpe: ParamRef =>
          tag(out, 'p')
          val depth = binders.indexOf(tpe.binder)
          uvarint(out, (if depth < 0 then binders.length else depth).toLong)
          uvarint(out, tpe.paramNum.toLong)

        case tpe: ThisType =>
          tag(out, 'H')
          utf8(out, tpe.typeSymbol.fullName)

        case tpe: SuperType =>
          tag(out, 'U')
          encodeType(out, tpe.thistpe, binders)
          encodeType(out, tpe.supertpe, binders)

        case Refinement(parent, name, info) =>
          tag(out, 'F')
          encodeType(out, parent, binders)
          utf8(out, name)
          encodeType(out, info, binders)

        case TypeBounds(low, high) =>
          tag(out, 'B')
          encodeType(out, low, binders)
          encodeType(out, high, binders)

        case tpe: RecursiveType =>
          tag(out, 'r')
          encodeType(out, tpe.underlying, tpe :: binders)

        case tpe: RecursiveThis =>
          tag(out, 'h')
          uvarint(out, binders.indexOf(tpe.binder).max(0).toLong)

        case tpe: FlexibleType =>
          encodeType(out, tpe.underlying, binders)

        case tpe: MatchType =>
          tag(out, 'X')
          encodeType(out, tpe.bound, binders)
          encodeType(out, tpe.scrutinee, binders)
          uvarint(out, tpe.cases.length.toLong)
          tpe.cases.foreach: matchCase => encodeType(out, matchCase, binders)

        case tpe: MatchCase =>
          tag(out, 'x')
          encodeType(out, tpe.pattern, binders)
          encodeType(out, tpe.rhs, binders)

        case _ =>
          // dotc's lazy placeholder for cyclically-referenced types (`LazyRef`) leaks
          // through the reflection layer with no reflect-level extractor: dereference it
          // and encode the underlying type, which is what consumers observe. The immediate
          // underlying is always a proper type (typically a named reference), so this
          // cannot recurse unboundedly.
          if tpe.getClass.getName.nn.endsWith("LazyRef") then
            val context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
            val lazyRef = tpe.asInstanceOf[dotty.tools.dotc.core.Types.LazyRef]
            encodeType(out, lazyRef.ref(using context).asInstanceOf[TypeRepr], binders)
          else
            throw Unencodable(s"type ${tpe.getClass.getName}")

    def denylisted(fullName: String): Boolean =
      val diagnostic = scala.collection.immutable.Set(
        "scala.deprecated", "scala.deprecatedInheritance", "scala.deprecatedOverriding",
        "scala.annotation.deprecatedName", "scala.annotation.implicitNotFound",
        "scala.annotation.implicitAmbiguous", "scala.annotation.migration",
        "scala.annotation.nowarn", "scala.annotation.unused")

      val interop = scala.List(
        "scala.scalajs.js.annotation.", "scala.scalanative.unsafe.",
        "scala.annotation.internal.", "scala.annotation.unchecked.")

      diagnostic.contains(fullName) || interop.exists: prefix => fullName.startsWith(prefix)

    // --- keys ---------------------------------------------------------------------------------

    // The erased-signature disambiguator of a term member's key: overloads differ here, and the
    // spelling matches what consumers' TASTy references carry in their SIGNED names.
    def signatureText(symbol: Symbol): String =
      try
        val signature = symbol.signature
        val params = signature.paramSigs.map(_.toString).mkString(",")
        s"($params)${signature.resultSig}"
      catch case _: Exception => ""

    def keyOf(owner: String, symbol: Symbol): String =
      s"$owner.${symbol.name}${signatureText(symbol)}"

    // --- membership ---------------------------------------------------------------------------

    def carrier(symbol: Symbol): Boolean = symbol.name.contains("$package")

    // The owner component of a member's key: a top-level definition's `$package` carrier is
    // folded away, so the key names the package directly.
    def ownerKeyOf(classSymbol: Symbol): String =
      if carrier(classSymbol) then classSymbol.owner.fullName.stripSuffix("$")
      else classSymbol.fullName

    def excluded(symbol: Symbol): Boolean =
      val flags = symbol.flags

      // Qualified-private (`private[scope]`) members are included, conservatively (`tasty.md`
      // §5): macros and derivation can reach them from dependency TASTy, so only unqualified
      // `private` and `private[this]` are outside the API.
      val qualified = symbol.privateWithin.isDefined
      val hidden = (flags.is(Flags.Private) && !qualified) || flags.is(Flags.PrivateLocal)
      val internal = flags.is(Flags.Artifact) || symbol.isLocalDummy || symbol.isNoSymbol
      hidden || internal

    def record
      ( key:         String,
        replaceable: Boolean,
        encoding:    java.io.ByteArrayOutputStream,
        references:  scala.List[String] = scala.Nil )
    :   Unit =

      if atoms.contains(key) then throw Unencodable(s"duplicate key $key")
      val data = Array.unsafeFrozen(encoding.toByteArray.nn)

      val listed =
        references.map: reference => ScalaReference.Own(Text(reference))
        . to(List)

      atoms(key) = ScalaAtom(Text(key), replaceable, data, listed)

    // --- members ------------------------------------------------------------------------------

    def termAtom(ownerKey: String, symbol: Symbol): Unit =
      val out = java.io.ByteArrayOutputStream()
      tag(out, if symbol.isDefDef then 'd' else 'v')
      uvarint(out, flagBits(symbol))

      // Parameter names are not part of the key, but they are named-argument surface, so they
      // fold into the value; `HASDEFAULT` existence travels through the flag bits of the
      // default getters, which are ordinary (synthetic) members.
      symbol.paramSymss.foreach: clause =>
        uvarint(out, clause.length.toLong)
        clause.foreach: param => utf8(out, param.name)

      encodeType(out, symbol.info, scala.Nil)
      annotations(out, symbol)
      record(keyOf(ownerKey, symbol), replaceable = false, out)

      if symbol.flags.is(Flags.Inline) || symbol.flags.is(Flags.Macro) then inlineAtom(ownerKey,
          symbol)

    def inlineAtom(ownerKey: String, symbol: Symbol): Unit =
      // The body of an inline or macro definition is copied into consumers at their compile
      // time: a replaceable atom (LIRA §10.2), keyed by the declaration, whose value is the
      // canonical encoding of the body tree — local names alpha-normalized, every outward
      // reference fully qualified — and whose reference list names everything the body splices
      // into consumers, for used-set closure (LIRA §13.4).
      val out = java.io.ByteArrayOutputStream()
      tag(out, 'i')

      val locals = scala.collection.mutable.HashMap[Symbol, Int]()
      val references = scala.collection.mutable.TreeSet[String]()

      def local(member: Symbol): Boolean =
        var current = member
        var found = false

        while !found && !current.isNoSymbol do
          if current == symbol then found = true
          current = if current.isNoSymbol then current else current.owner

        found

      def reference(member: Symbol): Unit =
        if !member.isNoSymbol && !member.isPackageDef && !member.owner.isNoSymbol
        then
          if member.isClassDef || member.isTypeDef then references += member.fullName else
            references += keyOf(ownerKeyOf(member.owner), member)

            // Closure through nested inlining: using an inline member copies its body too.
            if member.flags.is(Flags.Inline) || member.flags.is(Flags.Macro)
            then references += keyOf(ownerKeyOf(member.owner), member) + "[inline]"

      def name(member: Symbol): Unit =
        if local(member) then
          tag(out, 'l')
          uvarint(out, locals.getOrElseUpdate(member, locals.size).toLong)
        else
          tag(out, 'g')
          utf8(out, member.fullName + signatureText(member))
          reference(member)

      def term(tree: Tree): Unit = tree match
        case Inlined(_, bindings, body) =>
          tag(out, 'I')
          uvarint(out, bindings.length.toLong)
          bindings.foreach(term)
          term(body)

        case Ident(_) =>
          tag(out, 'x')
          name(tree.symbol)

        case Select(qualifier, _) =>
          tag(out, '.')
          term(qualifier)
          name(tree.symbol)

        case Literal(constant) =>
          tag(out, 'k')
          utf8(out, constant.getClass.getName.nn + ":" + constant.show)

        case This(_) =>
          tag(out, 'z')
          utf8(out, tree.symbol.fullName)

        case New(tpt) =>
          tag(out, 'n')
          encodeType(out, tpt.tpe, scala.Nil)

        case Apply(fun, arguments) =>
          tag(out, 'a')
          term(fun)
          uvarint(out, arguments.length.toLong)
          arguments.foreach(term)

        case TypeApply(fun, arguments) =>
          tag(out, 'y')
          term(fun)
          uvarint(out, arguments.length.toLong)
          arguments.foreach: argument => encodeType(out, argument.tpe, scala.Nil)

        case Typed(expression, tpt) =>
          tag(out, ':')
          term(expression)
          encodeType(out, tpt.tpe, scala.Nil)

        case Block(statements, expression) =>
          tag(out, '{')

          // Imports and exports among a body's statements are purely lexical: every
          // reference this encoding emits is already fully-qualified, so they carry no
          // semantic content and fold into nothing, like positions.
          val retained = statements.filter:
            case _: Import => false
            case _: Export => false
            case _         => true

          uvarint(out, retained.length.toLong)
          retained.foreach(term)
          term(expression)

        case If(condition, positive, negative) =>
          tag(out, '?')
          term(condition)
          term(positive)
          term(negative)

        case matched @ Match(scrutinee, cases) =>
          tag(out, if matched.isInline then 'M' else 'm')
          term(scrutinee)
          uvarint(out, cases.length.toLong)
          cases.foreach(term)

        case SummonFrom(cases) =>
          tag(out, 'f')
          uvarint(out, cases.length.toLong)
          cases.foreach(term)

        case CaseDef(pattern, guard, rhs) =>
          tag(out, 'e')
          term(pattern)

          guard match
            case scala.Some(condition) => term(condition)
            case scala.None            => tag(out, '0')

          term(rhs)

        case Bind(_, pattern) =>
          tag(out, 'b')
          uvarint(out, locals.getOrElseUpdate(tree.symbol, locals.size).toLong)
          term(pattern)

        case Unapply(fun, implicits, patterns) =>
          tag(out, 'u')
          term(fun)
          uvarint(out, implicits.length.toLong)
          implicits.foreach(term)
          uvarint(out, patterns.length.toLong)
          patterns.foreach(term)

        case Alternatives(patterns) =>
          tag(out, '/')
          uvarint(out, patterns.length.toLong)
          patterns.foreach(term)

        case Wildcard() =>
          tag(out, '_')

        case TypedOrTest(inner, tpt) =>
          tag(out, 'o')
          term(inner)
          encodeType(out, tpt.tpe, scala.Nil)

        case While(condition, body) =>
          tag(out, 'w')
          term(condition)
          term(body)

        case Assign(lhs, rhs) =>
          tag(out, '=')
          term(lhs)
          term(rhs)

        case Return(expression, _) =>
          tag(out, 'j')
          term(expression)

        case Try(expression, cases, finalizer) =>
          tag(out, 'q')
          term(expression)
          uvarint(out, cases.length.toLong)
          cases.foreach(term)

          finalizer match
            case scala.Some(effect) => term(effect)
            case scala.None         => tag(out, '0')

        case Repeated(elements, tpt) =>
          tag(out, '*')
          uvarint(out, elements.length.toLong)
          elements.foreach(term)
          encodeType(out, tpt.tpe, scala.Nil)

        case Closure(target, _) =>
          tag(out, '\\')
          term(target)

        case NamedArg(argName, argument) =>
          tag(out, '$')
          utf8(out, argName)
          term(argument)

        case valDef: ValDef =>
          tag(out, 'V')
          uvarint(out, locals.getOrElseUpdate(valDef.symbol, locals.size).toLong)
          encodeType(out, valDef.tpt.tpe, scala.Nil)

          valDef.rhs match
            case scala.Some(rhs) => term(rhs)
            case scala.None      => tag(out, '0')

        case defDef: DefDef =>
          tag(out, 'D')
          uvarint(out, locals.getOrElseUpdate(defDef.symbol, locals.size).toLong)

          defDef.termParamss.foreach: clause =>
            clause.params.foreach: param =>
              uvarint(out, locals.getOrElseUpdate(param.symbol, locals.size).toLong)

          encodeType(out, defDef.returnTpt.tpe, scala.Nil)

          defDef.rhs match
            case scala.Some(rhs) => term(rhs)
            case scala.None      => tag(out, '0')

        case tpt: TypeTree =>
          tag(out, 'T')
          encodeType(out, tpt.tpe, scala.Nil)

        case _ =>
          throw Unencodable(s"term ${tree.getClass.getName}")

      symbol.tree match
        case defDef: DefDef =>
          defDef.termParamss.foreach: clause =>
            clause.params.foreach: param =>
              uvarint(out, locals.getOrElseUpdate(param.symbol, locals.size).toLong)

          defDef.rhs match
            case scala.Some(body) => term(body)
            case scala.None       => tag(out, '0')

        case valDef: ValDef => valDef.rhs match
          case scala.Some(body) => term(body)
          case scala.None       => tag(out, '0')

        case _ => tag(out, '0')

      record(keyOf(ownerKey, symbol) + "[inline]", replaceable = true, out, references.toList)

    def typeMemberAtom(ownerKey: String, symbol: Symbol): Unit =
      val out = java.io.ByteArrayOutputStream()
      tag(out, 't')
      uvarint(out, flagBits(symbol))
      encodeType(out, symbol.info, scala.Nil)
      annotations(out, symbol)
      record(s"$ownerKey.${symbol.name}", replaceable = false, out)

    def annotations(out: java.io.ByteArrayOutputStream, symbol: Symbol): Unit =
      val names = symbol.annotations
        . map(_.tpe.typeSymbol.fullName)
        . filter: name => !denylisted(name)
        . sorted

      uvarint(out, names.length.toLong)
      names.foreach: name => utf8(out, name)

    // --- templates ----------------------------------------------------------------------------

    def classAtoms(symbol: Symbol): Unit =
      if !excluded(symbol) then
        val isCarrier = carrier(symbol)
        val ownerKey = ownerKeyOf(symbol)

        if !isCarrier then templateAtom(symbol)

        symbol.declarations.foreach: member =>
          if !excluded(member) then
            if member.isClassDef then classAtoms(member)
            else if member.isTypeDef then typeMemberAtom(ownerKey, member)
            else if member.isDefDef || member.isValDef then termAtom(ownerKey, member)

    def templateAtom(symbol: Symbol): Unit =
      val out = java.io.ByteArrayOutputStream()
      tag(out, 'c')
      uvarint(out, flagBits(symbol))

      // Type parameters: positional, variance and bounds folded; names alpha-normalized away.
      val typeParams = symbol.typeMembers.filter(_.isTypeParam)
      uvarint(out, typeParams.length.toLong)

      typeParams.foreach: param =>
        val variance =
          if param.flags.is(Flags.Covariant) then 1L
          else if param.flags.is(Flags.Contravariant) then 2L else 0L

        uvarint(out, variance)
        encodeType(out, param.info, scala.Nil)

      // Parents, in linearization-relevant declaration order.
      val parents = symbol.typeRef.baseClasses match
        case _ =>
          symbol.tree match
            case classDef: ClassDef =>
              classDef.parents.map:
                case tpt: TypeTree => tpt.tpe
                case parent        => parent.asInstanceOf[Term].tpe

            case _ => scala.Nil

      uvarint(out, parents.length.toLong)
      parents.foreach: parent => encodeType(out, parent, scala.Nil)

      // The self type, when declared.
      symbol.tree match
        case classDef: ClassDef => classDef.self match
          case scala.Some(self) =>
            tag(out, 's')
            encodeType(out, self.tpt.tpe, scala.Nil)

          case scala.None => tag(out, '-')

        case _ => tag(out, '-')

      // Abstract members fold into an *open* template (adding one breaks external subclasses);
      // on sealed or final templates they are pure additions and do not fold.
      val open =
        !symbol.flags.is(Flags.Final) && !symbol.flags.is(Flags.Sealed) &&
          !symbol.flags.is(Flags.Module)

      if open then
        val deferred = symbol.declarations
          . filter: member => member.flags.is(Flags.Deferred) && !excluded(member)
          . map: member => keyOf(symbol.fullName, member)
          . sorted

        uvarint(out, deferred.length.toLong)
        deferred.foreach: key => utf8(out, key)
      else
        uvarint(out, 0L)

      // The sealed/enum child list folds in declaration order: ordinals are behavior.
      if symbol.flags.is(Flags.Sealed) || symbol.flags.is(Flags.Enum) then
        val children = symbol.children.map(_.fullName)
        uvarint(out, children.length.toLong)
        children.foreach: child => utf8(out, child)
      else
        uvarint(out, 0L)

      annotations(out, symbol)
      record(symbol.fullName, replaceable = false, out)

    // --- roots --------------------------------------------------------------------------------

    def walk(tree: Tree): Unit = tree match
      case PackageClause(_, statements) => statements.foreach(walk)
      case classDef: ClassDef           => classAtoms(classDef.symbol)
      case _                            => ()

    roots.foreach(walk)

    // References were collected as bare keys; those matching an atom of this module are `Own`,
    // the rest `Foreign`, resolved against dependencies at assembly time.
    val keys = atoms.keySet.toSet

    atoms.values.toList.map: atom =>
      val classified =
        atom.references.map:
          case ScalaReference.Own(key) if !keys.contains(key.s) => ScalaReference.Foreign(key)
          case reference                                        => reference

      atom.copy(references = classified)
