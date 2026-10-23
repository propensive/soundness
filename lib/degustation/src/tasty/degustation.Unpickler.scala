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

import java.nio.charset.StandardCharsets

import scala.collection.immutable.{IndexedSeq, Set}
import scala.collection.mutable

import dotty.tools.tasty.{TastyBuffer, TastyFormat, TastyHeaderUnpickler, TastyReader,
    UnpickleException}

import anticipation.*
import rudiments.at
import vacuous.*

import Pickled.*

// Reads a `.tasty` file into the `Pickled` model: the header is checked, the name table decoded,
// and the ASTs section unpickled into trees; the other sections (positions, comments,
// attributes) are skipped, since nothing in them is API. Every shared subtree is resolved to the
// tree it shares, memoized by address, so the result is a plain tree in which only the
// identity-bearing nodes keep their addresses.
//
// The grammar followed is the one documented in `TastyFormat`; where the compiler's
// `TreeUnpickler` departs from it to build symbols or re-type nodes, this reader keeps what the
// file says, which is all a structural reading needs.
object Unpickler:
  // The ASTs of one file: its top-level statements, every definition node by address, so a local
  // reference (`TERMREFdirect` and friends) can be followed to the definition it names, and every
  // binder — a methodic or recursive type — by address, for `PARAMtype` and `RECthis` references.
  case class File(statements: List[Tree], definitions: Map[Int, Tree], binders: Map[Int, Type])

  def apply(data: Data): File =
    val bytes = new scala.Array[Byte](data.length)
    System.arraycopy(Array.unsafeJvm(data), 0, bytes, 0, data.length)
    val reader = TastyReader(bytes)
    TastyHeaderUnpickler(reader).readFullHeader()
    val names = readNames(reader)
    val sections = readSections(reader, names)

    sections.at(TastyFormat.ASTsSection.tt).lay(File(List(), Map(), Map())): section =>
      Trees(section, names).readFile()

  // The name table, decoded into structured names; entries may refer back to earlier entries.
  private def readNames(reader: TastyReader): IndexedSeq[Name] =
    import reader.*
    import TastyFormat.NameTags.*
    val entries = mutable.ArrayBuffer[Name]()

    def name(): Name = entries(readNameRef().index)
    def text(): Text = name().text

    def paramSig(): ParamSig =
      val ref = readInt()

      if ref < 0 then ParamSig.TypeParams(-ref)
      else ParamSig.Erased(entries(TastyBuffer.NameRef(ref).index).text)

    def entry(): Name =
      val tag = readByte()
      val length = readNat()
      val start = currentAddr
      val end = start + length

      val result = tag match
        case UTF8 =>
          goto(end)
          Name.Simple(String(bytes, start.index, length, StandardCharsets.UTF_8).tt)

        case QUALIFIED    => Name.Qualified(name(), text())
        case EXPANDED     => Name.Expanded(name(), text())
        case EXPANDPREFIX => Name.ExpandPrefix(name(), text())

        case UNIQUE =>
          val separator = text()
          val number = readNat()
          val originals = until(end)(name())
          Name.Unique(separator, number, originals.headOption.getOrElse(Unset))

        case DEFAULTGETTER => Name.DefaultGetter(name(), readNat())

        case SIGNED =>
          val original = name()
          val result = text()
          Name.Signed(original, Unset, result, until(end)(paramSig()).to(List))

        case TARGETSIGNED =>
          val original = name()
          val target = name()
          val result = text()
          Name.Signed(original, target, result, until(end)(paramSig()).to(List))

        case SUPERACCESSOR  => Name.SuperAccessor(name())
        case INLINEACCESSOR => Name.InlineAccessor(name())
        case BODYRETAINER   => Name.BodyRetainer(name())
        case OBJECTCLASS    => Name.ObjectClass(name())
        case other          => throw UnpickleException(s"unknown name tag $other")

      if currentAddr != end then throw UnpickleException(s"name of tag $tag has the wrong length")
      result

    until(readEnd())(entries += entry())
    entries.toIndexedSeq

  private def readSections(reader: TastyReader, names: IndexedSeq[Name]): Map[Text, TastyReader] =
    import reader.*

    collectWhile(!isAtEnd):
      val name = names(readNameRef().index).text
      val end = readEnd()
      val section = TastyReader(bytes, currentAddr.index, end.index, currentAddr.index)
      goto(end)
      name -> section

    . to(Map)

  // The ASTs section reader. Addresses are the section-relative addresses the format uses for
  // every `ASTRef`, so a reader positioned anywhere in the section can follow one with `goto`.
  private class Trees(reader: TastyReader, names: IndexedSeq[Name]):
    import reader.*
    import TastyFormat.*

    private val types = mutable.HashMap[Int, Type]()
    private val trees = mutable.HashMap[Int, Tree]()
    private val definitions = mutable.LinkedHashMap[Int, Tree]()

    def readFile(): File =
      val statements = until(endAddr)(readTopLevel())

      val binders = types.collect:
        case (address, binder @ (Type.Method(_, _, _, _, _) | Type.Poly(_, _, _, _)
            | Type.Lambda(_, _, _, _) | Type.Recursive(_, _))) =>
          address -> binder

      File(statements.to(List), definitions.to(Map), binders.to(Map))

    private def name(): Name = names(readNameRef().index)
    private def text(): Text = name().text

    // Addresses in the model are the section-relative indices every `ASTRef` uses.
    private def jump(address: Int): scala.Unit = goto(TastyBuffer.Addr(address))

    // A length-tagged node must have been read in full: anything else means the grammar above has
    // drifted from the file's, which is an error here rather than a silent misreading later.
    private def finish(tag: Int, end: TastyBuffer.Addr): scala.Unit =
      if currentAddr != end then
        throw UnpickleException:
          s"${astTagToString(tag)} read to ${currentAddr.index}, not its end ${end.index}"

    // Reads whatever subtree starts at `address`, leaving the reader where it was.
    private def at[result](address: Int)(read: => result): result =
      val here = currentAddr
      jump(address)
      val result = read
      goto(here)
      result

    // --- types --------------------------------------------------------------------------------

    def readType(): Type =
      val start = currentAddr.index
      val tag = readByte()

      def lengthType(): Type =
        val end = readEnd()

        // The result type comes first in the file, but a binder's parameters must be known for
        // `PARAMtype` references inside the result to resolve, so the result is read after a
        // placeholder is registered at the binder's address.
        def binder(make: (List[Text], List[Type], Type, Clause) => Type): Type =
          val resultReader = currentAddr
          skipTree()
          val names = mutable.ListBuffer[Text]()
          val params = mutable.ListBuffer[Type]()

          while currentAddr != end && !isModifierTag(nextByte) do
            params += readType()
            names += text()

          var clause = Clause.Plain

          while currentAddr != end do
            readByte() match
              case IMPLICIT => clause = Clause.Implicit
              case GIVEN    => clause = Clause.Contextual
              case _        => ()

          val result = at(resultReader.index)(readType())
          make(names.to(List), params.to(List), result, clause)

        val result = tag match
          case TERMREFin =>
            val name = this.name()
            val prefix = readType()
            Type.TermRefIn(name, prefix, readType())

          case TYPEREFin =>
            val name = text()
            val prefix = readType()
            Type.TypeRefIn(name, prefix, readType())

          case REFINEDtype =>
            val name = text()
            val parent = readType()
            Type.Refined(parent, name, readType())

          case APPLIEDtype =>
            val constructor = readType()
            Type.Applied(constructor, until(end)(readType()).to(List))

          case TYPEBOUNDS =>
            val low = readType()
            val high = if currentAddr == end || isModifierTag(nextByte) then Unset else readType()

            val variances = until(end):
              readByte() match
                case COVARIANT     => Variance.Covariant
                case CONTRAVARIANT => Variance.Contravariant
                case _             => Variance.Invariant

            Type.Bounds(low, high, variances.to(List))

          case ANNOTATEDtype =>
            val underlying = readType()
            Type.Annotated(underlying, readTree())

          case ANDtype   => Type.And(readType(), readType())
          case ORtype    => Type.Or(readType(), readType())
          case SUPERtype => Type.Super(readType(), readType())

          case MATCHtype =>
            val bound = readType()
            val scrutinee = readType()
            Type.Match(bound, scrutinee, until(end)(readType()).to(List))

          case MATCHCASEtype => Type.MatchCase(readType(), readType())

          case POLYtype =>
            binder: (names, bounds, result, _) => Type.Poly(start, names, bounds, result)

          case METHODtype =>
            binder: (names, params, result, clause) =>
              Type.Method(start, names, params, result, clause)

          case TYPELAMBDAtype =>
            binder: (names, bounds, result, _) => Type.Lambda(start, names, bounds, result)

          case PARAMtype =>
            val binderAddress = readAddr().index
            Type.Param(binderAddress, readNat())

          case FLEXIBLEtype => Type.Flexible(readType())

          case BIND =>
            val name = text()
            val bounds = readType()
            goto(end) // modifiers, which the format allows and nothing here reads
            Type.Bind(start, name, bounds)

          case other => throw UnpickleException(s"unknown type tag ${astTagToString(other)}")

        finish(tag, end)
        result

      def simpleType(): Type = tag match
        case TERMREFdirect => Type.TermRefDirect(readAddr().index)
        case TYPEREFdirect => Type.TypeRefDirect(readAddr().index)

        case TERMREFsymbol =>
          val address = readAddr().index
          Type.TermRefLocal(address, readType())

        case TYPEREFsymbol =>
          val address = readAddr().index
          Type.TypeRefLocal(address, readType())

        case TERMREFpkg => Type.TermRefPkg(text())
        case TYPEREFpkg => Type.TypeRefPkg(text())

        case TERMREF =>
          val name = this.name()
          Type.TermRef(name, readType())

        case TYPEREF =>
          val name = text()
          Type.TypeRef(name, readType())

        case THIS => Type.This(readType())

        case RECtype =>
          types.get(start) match
            case Some(tpe) =>
              skipTree(tag)
              tpe

            case None =>
              val recursive = Type.Recursive(start, readType())
              types(start) = recursive
              recursive

        case RECthis => Type.RecursiveThis(readAddr().index)

        case SHAREDtype =>
          val address = readAddr().index
          types.getOrElseUpdate(address, at(address)(readType()))

        case BYNAMEtype => Type.ByName(readType())
        case _          => Type.Literal(readConstant(tag))

      val result = if tag < firstLengthTreeTag then simpleType() else lengthType()
      types(start) = result
      result

    private def readConstant(tag: Int): Constant = tag match
      case UNITconst   => Constant.UnitValue
      case FALSEconst  => Constant.FalseValue
      case TRUEconst   => Constant.TrueValue
      case BYTEconst   => Constant.ByteValue(readInt().toByte)
      case SHORTconst  => Constant.ShortValue(readInt().toShort)
      case CHARconst   => Constant.CharValue(readNat().toChar)
      case INTconst    => Constant.IntValue(readInt())
      case LONGconst   => Constant.LongValue(readLongInt())
      case FLOATconst  => Constant.FloatValue(java.lang.Float.intBitsToFloat(readInt()))
      case DOUBLEconst => Constant.DoubleValue(java.lang.Double.longBitsToDouble(readLongInt()))
      case STRINGconst => Constant.StringValue(text())
      case NULLconst   => Constant.NullValue
      case CLASSconst  => Constant.ClassValue(readType())
      case other       => throw UnpickleException(s"unknown constant tag ${astTagToString(other)}")

    // --- modifiers ----------------------------------------------------------------------------

    private def readModifiers(end: TastyBuffer.Addr): Modifiers =
      val flags = mutable.Set[Flag]()
      var privateWithin: Optional[Type] = Unset
      val annotations = mutable.ListBuffer[Annotation]()

      while currentAddr != end do
        val tag = readByte()

        tag match
          case PRIVATE          => flags += Flag.Private
          case PROTECTED        => flags += Flag.Protected

          case ABSTRACT =>
            if nextByte == OVERRIDE then
              readByte()
              flags += Flag.AbsOverride
            else
              flags += Flag.Abstract

          case FINAL            => flags += Flag.Final
          case SEALED           => flags += Flag.Sealed
          case CASE             => flags += Flag.Case
          case IMPLICIT         => flags += Flag.Implicit
          case ERASED           => flags += Flag.Erased
          case LAZY             => flags += Flag.Lazy
          case OVERRIDE         => flags += Flag.Override
          case INLINE           => flags += Flag.Inline
          case INLINEPROXY      => flags += Flag.InlineProxy
          case MACRO            => flags += Flag.Macro
          case OPAQUE           => flags += Flag.Opaque
          case STATIC           => flags += Flag.Static
          case OBJECT           => flags += Flag.Object
          case TRAIT            => flags += Flag.Trait
          case ENUM             => flags += Flag.Enum
          case LOCAL            => flags += Flag.Local
          case SYNTHETIC        => flags += Flag.Synthetic
          case ARTIFACT         => flags += Flag.Artifact
          case MUTABLE          => flags += Flag.Mutable
          case FIELDaccessor    => flags += Flag.FieldAccessor
          case CASEaccessor     => flags += Flag.CaseAccessor
          case COVARIANT        => flags += Flag.Covariant
          case CONTRAVARIANT    => flags += Flag.Contravariant
          case HASDEFAULT       => flags += Flag.HasDefault
          case STABLE           => flags += Flag.Stable
          case EXTENSION        => flags += Flag.Extension
          case GIVEN            => flags += Flag.Given
          case PARAMsetter      => flags += Flag.ParamSetter
          case PARAMalias       => flags += Flag.ParamAlias
          case EXPORTED         => flags += Flag.Exported
          case OPEN             => flags += Flag.Open
          case INVISIBLE        => flags += Flag.Invisible
          case TRANSPARENT      => flags += Flag.Transparent
          case INFIX            => flags += Flag.Infix
          case TRACKED          => flags += Flag.Tracked
          case INTO             => flags += Flag.Into
          case PRIVATEqualified => privateWithin = readType()

          case PROTECTEDqualified =>
            flags += Flag.Protected
            privateWithin = readType()

          case ANNOTATION =>
            val annotationEnd = readEnd()
            val tpe = readType()

            val tree =
              if isCompactAnnotationType(nextByte) then Tree.Path(readType()) else readTree()

            finish(ANNOTATION, annotationEnd)
            annotations += Annotation(tpe, tree)

          case other =>
            throw UnpickleException(s"unknown modifier tag ${astTagToString(other)}")

      Modifiers(flags.to(Set), privateWithin, annotations.to(List))

    // A compact annotation pickles its type where a full annotation tree would otherwise go.
    private def isCompactAnnotationType(tag: Int): Boolean = tag match
      case APPLIEDtype | SHAREDtype | TYPEREF | TYPEREFdirect | TYPEREFsymbol | TYPEREFin  => true
      case _                                                                                => false

    private def nothingButModifiers(end: TastyBuffer.Addr): Boolean =
      currentAddr == end || isModifierTag(nextByte)

    // --- definitions --------------------------------------------------------------------------

    private def readTopLevel(): Tree =
      if nextByte == PACKAGE then
        readByte()
        val end = readEnd()
        val path = readType()
        Tree.Package(path, until(end)(readTopLevel()).to(List))
      else
        readStatement()

    private def readStatement(): Tree = nextByte match
      case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM => readDefinition()

      case IMPORT | EXPORT =>
        val tag = readByte()
        val end = readEnd()
        val qualifier = readTree()
        val selectors = until(end)(readSelector()).to(List)

        if tag == IMPORT then Tree.Import(qualifier, selectors)
        else Tree.Export(qualifier, selectors)

      case _ => readTree()

    private def readSelector(): Selector = readByte() match
      case IMPORTED => Selector.Imported(text())
      case RENAMED  => Selector.Renamed(text())
      case BOUNDED  => Selector.Bounded(readTree())
      case other    => throw UnpickleException(s"unknown selector tag ${astTagToString(other)}")

    private def readDefinition(): Tree =
      val start = currentAddr.index

      trees.get(start) match
        case Some(tree) =>
          skipTree()
          tree

        case None =>
          val tag = readByte()
          val end = readEnd()
          val name = this.name()

          val tree = tag match
            case DEFDEF =>
              val clauses = readClauses()
              val returns = readTpt()
              val rhs = if nothingButModifiers(end) then Unset else readTree()
              Tree.DefDef(start, name, clauses, returns, rhs, readModifiers(end))

            case VALDEF =>
              val tpt = readTpt()
              val rhs = if nothingButModifiers(end) then Unset else readTree()
              Tree.ValDef(start, name, tpt, rhs, readModifiers(end))

            case TYPEDEF =>
              val rhs = if nextByte == TEMPLATE then readTemplate() else readTpt()
              Tree.TypeDef(start, name.text, rhs, readModifiers(end))

            case TYPEPARAM =>
              val bounds = readTpt()
              Tree.TypeParam(start, name.text, bounds, readModifiers(end))

            case PARAM =>
              val tpt = readTpt()
              Tree.Param(start, name, tpt, readModifiers(end))

            case other =>
              throw UnpickleException(s"unknown definition tag ${astTagToString(other)}")

          finish(tag, end)
          trees(start) = tree
          definitions(start) = tree
          tree

    // Parameter clauses: runs of type parameters or term parameters, an `EMPTYCLAUSE` for `()`,
    // and a `SPLITCLAUSE` between two clauses of the same kind.
    private def readClauses(): List[List[Tree]] =
      val clauses = mutable.ListBuffer[List[Tree]]()
      var more = true

      while more do
        nextByte match
          case PARAM | TYPEPARAM =>
            val kind = nextByte
            val clause = mutable.ListBuffer[Tree]()
            while nextByte == kind do clause += readDefinition()
            clauses += clause.to(List)
            if nextByte == SPLITCLAUSE then readByte()

          case EMPTYCLAUSE =>
            readByte()
            clauses += List()
            if nextByte == SPLITCLAUSE then readByte()

          case _ => more = false

      clauses.to(List)

    private def readTemplate(): Tree =
      readByte() // TEMPLATE
      val end = readEnd()
      val typeParams = mutable.ListBuffer[Tree]()
      while nextByte == TYPEPARAM do typeParams += readDefinition()
      val params = mutable.ListBuffer[Tree]()
      while nextByte == PARAM do params += readDefinition()

      // Parents run until the self definition, the primary constructor or an explicit end.
      val parents = mutable.ListBuffer[Tree]()

      def moreParents: Boolean =
        currentAddr != end && nextByte != SELFDEF && nextByte != DEFDEF && nextByte != SPLITCLAUSE

      while moreParents do parents += readTree()

      val self: Optional[(Text, Tree)] =
        if nextByte == SELFDEF then
          readByte()
          val name = text()
          (name, readTpt())
        else
          Unset

      if nextByte == SPLITCLAUSE then readByte()
      val body = until(end)(readStatement()).to(List)
      finish(TEMPLATE, end)
      Tree.Template(typeParams.to(List), params.to(List), parents.to(List), self, body)

    // --- trees --------------------------------------------------------------------------------

    def readTpt(): Tree = nextByte match
      case SHAREDterm =>
        readByte()
        val address = readAddr().index
        trees.getOrElseUpdate(address, at(address)(readTpt()))

      case BLOCK =>
        // A type tree may be preceded by the type aliases it needs, as a block.
        readByte()
        val end = readEnd()
        val expression = at(currentAddr.index)(readTpt())
        skipTree()
        val statements = until(end)(readStatement()).to(List)
        Tree.Block(statements, expression)

      case HOLE =>
        readByte()
        val end = readEnd()
        readHole(end, term = false)

      case tag if isTypeTreeTag(tag) => readTree()
      case _                         => Tree.Path(readType())

    private def readHole(end: TastyBuffer.Addr, term: Boolean): Tree =
      val index = readNat()
      val tpe = readType()
      Tree.Hole(term, index, tpe, until(end)(readTree()).to(List))

    private def readCases(end: TastyBuffer.Addr): List[Tree] =
      val cases = mutable.ListBuffer[Tree]()

      while currentAddr != end && nextUnsharedTag == CASEDEF do
        if nextByte == SHAREDterm then
          readByte()
          val address = readAddr().index
          cases += at(address)(readCase())
        else
          cases += readCase()

      cases.to(List)

    private def nextUnsharedTag: Int =
      if nextByte == SHAREDterm then
        val here = currentAddr
        readByte()
        val address = readAddr().index
        goto(here)
        at(address)(nextByte)
      else
        nextByte

    private def readCase(): Tree =
      readByte() // CASEDEF
      val end = readEnd()
      val pattern = readTree()
      val rhs = readTree()
      val guard = if currentAddr == end then Unset else readTree()
      finish(CASEDEF, end)
      Tree.CaseDef(pattern, guard, rhs)

    def readTree(): Tree =
      val start = currentAddr.index
      val tag = readByte()

      def simpleTree(): Tree = tag match
        case SHAREDterm =>
          val address = readAddr().index
          trees.getOrElseUpdate(address, at(address)(readTree()))

        case IDENT =>
          val name = this.name()
          Tree.Ident(name, readType())

        case IDENTtpt =>
          val name = text()
          Tree.IdentTpt(name, readType())

        case SELECT =>
          val name = this.name()
          Tree.Select(name, readTree())

        case SELECTtpt =>
          val name = text()
          Tree.SelectTpt(name, readTree())

        case QUALTHIS     => Tree.QualifiedThis(readTree())
        case NEW          => Tree.New(readTpt())
        case ELIDED       => Tree.Elided(readType())
        case THROW        => Tree.Throw(readTree())
        case SINGLETONtpt => Tree.SingletonTpt(readTree())
        case BYNAMEtpt    => Tree.ByNameTpt(readTpt())
        case EXPLICITtpt  => Tree.ExplicitTpt(readTpt())

        case NAMEDARG =>
          val name = text()
          Tree.NamedArg(name, readTree())

        case _ =>
          jump(start)
          Tree.Path(readType())

      def lengthTree(): Tree =
        val end = readEnd()

        val result = tag match
          case SUPER =>
            val qualifier = readTree()
            val mixin = if currentAddr == end then Unset else readTree()
            Tree.SuperRef(qualifier, mixin)

          case APPLY =>
            val function = readTree()
            Tree.Apply(function, until(end)(readTree()).to(List))

          case TYPEAPPLY =>
            val function = readTree()
            Tree.TypeApply(function, until(end)(readTpt()).to(List))

          case APPLYsigpoly =>
            val function = readTree()
            val method = readType()
            Tree.ApplySigPoly(function, method, until(end)(readTree()).to(List))

          case TYPED =>
            val expression = readTree()
            Tree.Typed(expression, readTpt())

          case ASSIGN => Tree.Assign(readTree(), readTree())

          case BLOCK =>
            val expression = at(currentAddr.index)(readTree())
            skipTree()
            Tree.Block(until(end)(readStatement()).to(List), expression)

          case INLINED =>
            val expression = at(currentAddr.index)(readTree())
            skipTree()

            val call: Optional[Tree] =
              if currentAddr == end then Unset
              else nextUnsharedTag match
                case VALDEF | DEFDEF => Unset
                case _               => readTree()

            Tree.Inlined(expression, call, until(end)(readStatement()).to(List))

          case IF =>
            val inline = nextByte == INLINE
            if inline then readByte()
            Tree.If(inline, readTree(), readTree(), readTree())

          case LAMBDA =>
            val method = readTree()
            val target = if currentAddr == end then Unset else readTpt()
            Tree.Lambda(method, target)

          case MATCH =>
            if nextByte == IMPLICIT then
              readByte()
              Tree.Match(Tree.MatchKind.Implicit, Unset, readCases(end))
            else if nextByte == INLINE then
              readByte()
              val scrutinee = readTree()
              Tree.Match(Tree.MatchKind.Inline, scrutinee, readCases(end))
            else if nextByte == SUBMATCH then
              readByte()
              val scrutinee = readTree()
              Tree.Match(Tree.MatchKind.Sub, scrutinee, readCases(end))
            else
              val scrutinee = readTree()
              Tree.Match(Tree.MatchKind.Plain, scrutinee, readCases(end))

          case RETURN =>
            val method = readAddr().index
            val expression = if currentAddr == end then Unset else readTree()
            Tree.Return(method, expression)

          case WHILE => Tree.While(readTree(), readTree())

          case TRY =>
            val expression = readTree()
            val cases = readCases(end)
            val finalizer = if currentAddr == end then Unset else readTree()
            Tree.Try(expression, cases, finalizer)

          case SELECTouter =>
            val levels = readNat()
            val qualifier = readTree()
            Tree.SelectOuter(levels, qualifier, readType())

          case SELECTin =>
            val name = this.name()
            val qualifier = readTree()
            Tree.SelectIn(name, qualifier, readType())

          case REPEATED =>
            val tpt = readTpt()
            Tree.Repeated(until(end)(readTree()).to(List), tpt)

          case BIND =>
            // A pattern binding defines a symbol that `TERMREFdirect`s elsewhere refer to.
            val name = text()
            val tpe = readType()
            val pattern = readTree()
            val bind = Tree.Bind(start, name, tpe, pattern, readModifiers(end))
            definitions(start) = bind
            bind

          case ALTERNATIVE => Tree.Alternative(until(end)(readTree()).to(List))

          case UNAPPLY =>
            val function = readTree()
            val implicits = mutable.ListBuffer[Tree]()

            while nextByte == IMPLICITarg do
              readByte()
              implicits += readTree()

            val tpe = readType()
            Tree.Unapply(function, implicits.to(List), tpe, until(end)(readTree()).to(List))

          case REFINEDtpt =>
            val underlying = readTpt()
            Tree.RefinedTpt(underlying, until(end)(readStatement()).to(List))

          case APPLIEDtpt =>
            val constructor = readTpt()
            Tree.AppliedTpt(constructor, until(end)(readTpt()).to(List))

          case ANNOTATEDtpt =>
            val underlying = readTpt()
            Tree.AnnotatedTpt(underlying, readTree())

          case LAMBDAtpt =>
            val typeParams = mutable.ListBuffer[Tree]()
            while nextByte == TYPEPARAM do typeParams += readDefinition()
            Tree.LambdaTpt(typeParams.to(List), readTpt())

          case MATCHtpt =>
            val first = readTpt()

            if currentAddr == end || nextUnsharedTag == CASEDEF
            then Tree.MatchTpt(Unset, first, readCases(end))
            else
              val scrutinee = readTpt()
              Tree.MatchTpt(first, scrutinee, readCases(end))

          case TYPEBOUNDStpt =>
            val low = readTpt()
            val high = if currentAddr == end then Unset else readTpt()
            val alias = if currentAddr == end then Unset else readTpt()
            Tree.BoundsTpt(low, high, alias)

          case QUOTE =>
            val body = readTree()
            Tree.Quote(body, readType())

          case SPLICE =>
            val expression = readTree()
            Tree.Splice(expression, readType())

          case QUOTEPATTERN =>
            val body = at(currentAddr.index)(readTree())
            skipTree()
            val quotes = readTree()
            val tpe = readType()
            Tree.QuotePattern(until(end)(readStatement()).to(List), body, quotes, tpe)

          case SPLICEPATTERN =>
            val pattern = readTree()
            val tpe = readType()
            val rest = until(end)(readTree())
            val (typeArguments, arguments) = rest.span(_.isTypeTree)
            Tree.SplicePattern(pattern, tpe, typeArguments.to(List), arguments.to(List))

          case HOLE => readHole(end, term = true)

          case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM =>
            jump(start)
            readDefinition()

          case IMPORT | EXPORT =>
            jump(start)
            readStatement()

          case PACKAGE =>
            jump(start)
            readTopLevel()

          case _ =>
            jump(start)
            Tree.Path(readType())

        finish(tag, end)
        result

      val tree = if tag < firstLengthTreeTag then simpleTree() else lengthTree()
      trees(start) = tree
      tree

    // Skips the subtree at the current address, by its tag's category. A number is stepped over
    // with the widest reader: every variable-length integer occupies bytes up to the first with
    // its high bit set, so this consumes exactly what a narrower reader would, and it also accepts
    // the 64-bit payloads of `LONGconst` and `DOUBLEconst`, which a `Nat` reader rejects.
    private def skipTree(): scala.Unit = skipTree(readByte())

    private def skipTree(tag: Int): scala.Unit =
      if tag >= firstLengthTreeTag then goto(readEnd())
      else if tag >= firstNatASTTreeTag then
        readLongInt()
        skipTree()
      else if tag >= firstASTTreeTag then
        skipTree()
      else if tag >= firstNatTreeTag then
        readLongInt()
