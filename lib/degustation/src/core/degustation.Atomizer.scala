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

// The atomization rules of `scala-tasty/1`, applied over the compiler's reflection of unpickled
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

    // The subset of modifier flags that consumers can depend on, in a fixed bit order.
    def flagBits(symbol: Symbol): Long =
      val flags = symbol.flags

      scala.List(
        Flags.Abstract, Flags.Case, Flags.Deferred, Flags.Enum, Flags.Erased, Flags.Exported,
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

    def excluded(symbol: Symbol): Boolean =
      val flags = symbol.flags

      val hidden = flags.is(Flags.Private) || flags.is(Flags.PrivateLocal)
      val internal = flags.is(Flags.Artifact) || symbol.isLocalDummy || symbol.isNoSymbol
      hidden || internal

    def record(key: String, replaceable: Boolean, encoding: java.io.ByteArrayOutputStream)
    :   Unit =

      if atoms.contains(key) then throw Unencodable(s"duplicate key $key")
      val data = Array.unsafeFrozen(encoding.toByteArray.nn)
      atoms(key) = ScalaAtom(Text(key), replaceable, data)

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
      // time: a replaceable atom (LIRA §10.2), keyed by the declaration. The canonical body
      // encoding and reference collection follow in a later change; until then the value is the
      // declaration's own encoding, marked replaceable.
      val out = java.io.ByteArrayOutputStream()
      tag(out, 'i')
      encodeType(out, symbol.info, scala.Nil)
      record(keyOf(ownerKey, symbol) + "[inline]", replaceable = true, out)

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

        val ownerKey =
          if isCarrier then symbol.owner.fullName.stripSuffix("$") else symbol.fullName

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
    atoms.values.toList
