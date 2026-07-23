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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package locomotion

import scala.caps

import scala.collection.immutable.{List, Nil, ::}
import scala.collection.mutable as scm
import scala.quoted.*

import anticipation.*
import contingency.*
import distillate.*
import prepositional.*
import vacuous.*

// The machinery behind `Protobuf.Inlinable`: expansion-time instance
// resolution (through an in-macro staging compiler), the structural
// generators for messages, repeated fields and oneof sums, and the
// `Inlinable.parsable` entry macro.
object stagedInternal:

  // The runtime seam for a field type without an `Inlinable`: its
  // occurrences are gathered as wire values through the record loop and
  // decoded once through the field's `Decodable in Protobuf`, exactly as
  // the AST path decodes `Repeated(values)`. (A custom *nominal*
  // `Protobuf.Parsable` is a whole-message parser; field positions route
  // through `Decodable` — `Parsable.fromDecodable` bridges the other way.)
  // The seam resolves its `Decodable` with a reflection-level implicit
  // search at expansion time, crossing the capability boundary through a
  // single erasing cast — the same maneuver as wisteria's field-instance
  // engine: resolution readily finds honest capability-typed codecs, but
  // their `^{tactic, …}` types do not conform to a bare (or even `^`-typed)
  // evidence query from inside the generated parser's inline context, and a
  // `summonInline` there additionally fails to expand the blanket
  // `summonFrom` given for recursively-derived instances.
  private def seamDecode[fieldType: Type](wire: Expr[Protobuf])(using Quotes)
  :   Expr[fieldType] =

    import quotes.reflect.*

    Implicits.search(TypeRepr.of[fieldType is Decodable in Protobuf]).absolve match
      case success: ImplicitSearchSuccess =>
        success.tree.asExpr.absolve match
          case '{ $found: any } =>
            '{ $found.asInstanceOf[fieldType is Decodable in Protobuf].decoded($wire) }

      case failure: ImplicitSearchFailure =>
        report.errorAndAbort
          (s"locomotion: no Decodable in Protobuf for ${TypeRepr.of[fieldType].show}: "+
            failure.explanation)

  // A nominal `Protobuf.Parsable` for a field type, resolved at expansion
  // time — the recursion tie: a recursive message's own alias given (a lazy
  // val) is found from inside its own definition, so nested occurrences
  // parse through the very instance being generated. Also the composition
  // point for hand-written `Parsable`s in field positions.
  private def parsableFor[fieldType: Type](using Quotes): Option[Expr[Any]] =
    import quotes.reflect.*

    Implicits.search(TypeRepr.of[fieldType is Protobuf.Parsable]) match
      case success: ImplicitSearchSuccess => Some(success.tree.asExpr)
      case _                              => None

  // ── Expansion-time environment ─────────────────────────────────────────

  private def macroClassloader: ClassLoader =
    val contextual = Thread.currentThread.nn.getContextClassLoader
    if contextual != null then contextual else getClass.getClassLoader.nn

  private def currentOutputDirectory(using Quotes): Option[String] =
    try
      val ctx = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
      given dotty.tools.dotc.core.Contexts.Context = ctx
      import dotty.tools.dotc.config.Settings.Setting.value

      Option(ctx.settings.outputDir.value.file).map(_.nn.getCanonicalPath.nn)
    catch case _: Exception => None

  // A symbol defined in the compilation run in progress has no trustworthy
  // classfile (none on a clean build; a stale one from the previous compile
  // on an incremental build), so instance evaluation must refuse it and the
  // caller degrade to a runtime seam.
  private def definedInCurrentRun(using Quotes)(symbol: quotes.reflect.Symbol): Boolean =
    try
      val ctx = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
      val run = ctx.run

      if run == null then false else
        val sources = run.nn.units.map(_.source.file.path).toSet
        val position = symbol.pos
        position.exists { position => sources.contains(position.sourceFile.path) }
    catch case _: Exception => false

  private def innerClasspath(using Quotes): String =
    val separator = java.io.File.pathSeparator.nn

    val full =
      dotty.tools.dotc.util.ClasspathFromClassloader(macroClassloader).nn

    currentOutputDirectory match
      case None =>
        full

      case Some(excluded) =>
        val entries = full.split(separator).nn
        val joined = StringBuilder()
        var index = 0

        while index < entries.length do
          val entry = entries(index).nn

          val retain =
            try java.io.File(entry).getCanonicalPath != excluded
            catch case _: java.io.IOException => true

          if retain then
            if joined.nonEmpty then joined.append(separator)
            joined.append(entry)

          index += 1

        joined.toString

  // ── Type transport into the staging context ────────────────────────────
  // The outer macro's `Type` cannot cross into the inner compiler, so a type
  // travels as a tree of `java.lang.Class`es and is rebuilt inside with
  // `TypeRepr.typeConstructorOf`. This restricts instance evaluation to
  // class-shaped types (plain classes and applications of them) — opaque
  // and structural types degrade to a runtime seam.

  private case class TypeShape(clazz: Class[?], arguments: List[TypeShape])

  private val primitiveClasses: scala.collection.immutable.Map[String, Class[?]] =
    scala.collection.immutable.Map
      ( "scala.Int"     -> classOf[Int],
        "scala.Long"    -> classOf[Long],
        "scala.Double"  -> classOf[Double],
        "scala.Float"   -> classOf[Float],
        "scala.Boolean" -> classOf[Boolean],
        "scala.Short"   -> classOf[Short],
        "scala.Byte"    -> classOf[Byte],
        "scala.Char"    -> classOf[Char],
        "scala.Unit"    -> classOf[Unit] )

  // The binary name of a class symbol: package segments joined with dots,
  // enclosing type segments with dollars.
  private def binaryName(using Quotes)(symbol: quotes.reflect.Symbol): String =
    import quotes.reflect.*

    def build(symbol: Symbol): String =
      val owner = symbol.owner

      if owner.isPackageDef then
        val prefix = owner.fullName
        if prefix == "<empty>" then symbol.name else prefix+"."+symbol.name
      else
        val ownerName = build(if owner.isClassDef then owner else owner.owner)
        ownerName+"$"+symbol.name

    build(symbol)

  private def shapeOf(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[TypeShape] =
    import quotes.reflect.*

    def classFor(tpe: TypeRepr): Option[Class[?]] =
      tpe.classSymbol.flatMap: symbol =>
        if definedInCurrentRun(symbol) then None
        else primitiveClasses.get(symbol.fullName).orElse:
          try Some(Class.forName(binaryName(symbol), false, macroClassloader).nn)
          catch
            case _: ReflectiveOperationException => None
            case _: LinkageError                 => None

    tpe.dealias match
      case AppliedType(constructor, arguments) =>
        for
          clazz <- classFor(constructor)
          shapes <- arguments.foldRight(Option(List.empty[TypeShape])): (argument, list) =>
                      list.flatMap { tail => shapeOf(argument).map(_ :: tail) }
        yield TypeShape(clazz, shapes)

      case other =>
        classFor(other).map(TypeShape(_, Nil))

  // ── Instance evaluation: the staging summon ────────────────────────────
  // One fresh compiler per summon, over the macro classloader with the
  // current output directory excluded from its classpath. The summon is
  // embedded in the compiled snippet (`summonInline`), so it resolves during
  // the inner compiler's inlining phase, composing conditional instances —
  // the whole instance graph arrives live in one run.
  private def summonViaStaging[field: Type](using Quotes): Option[Inlinable] =
    import quotes.reflect.*
    import scala.quoted.staging

    shapeOf(TypeRepr.of[field]).flatMap: shape =>
      try
        given settings: staging.Compiler.Settings =
          staging.Compiler.Settings.make
            (None, List("-experimental", "-classpath", innerClasspath))

        given staging.Compiler = staging.Compiler.make(macroClassloader)
        val started = System.nanoTime

        val result: Any = staging.run:
          val quotes2 = summon[Quotes]
          import quotes2.reflect as r2

          def rebuild(shape: TypeShape): r2.TypeRepr =
            val base = r2.TypeRepr.typeConstructorOf(shape.clazz)
            if shape.arguments.isEmpty then base
            else base.appliedTo(shape.arguments.map(rebuild))

          val target =
            r2.Refinement
              ( r2.TypeRepr.of[Inlinable], "Self",
                r2.TypeBounds(rebuild(shape), rebuild(shape)) )

          target.asType match
            case '[target] => '{ scala.compiletime.summonInline[target] }

        val duration = (System.nanoTime - started)/1000000L

        result match
          case instance: Inlinable =>
            report.info
              ( s"locomotion: staged summon for ${TypeRepr.of[field].show} took "+
                s"${duration}ms" )

            Some(instance)

          case _ =>
            None
      catch
        case _: ReflectiveOperationException => None
        case _: LinkageError                 => None
        case _: AssertionError               => None
        case _: Exception                    => None

  // ── The resolution ladder and field plans ──────────────────────────────
  // Every field of a message compiles to one of four plans: a builtin
  // scalar `Leaf` (read in place, last occurrence wins); a `Nested`
  // instance (a message, sum or custom leaf, parsed within its field
  // window); a `Gather` (a repeated field, appending per occurrence —
  // packed or not — into a builder); or the runtime `Seam` (occurrences
  // gathered as wire values and decoded once through the field's
  // `Decodable in Protobuf`).

  // The per-entry expansion cache: memoized staging summons, and the set of
  // types whose generators are currently on the expansion stack — a
  // recursive occurrence (`Tree` with `children: List[Tree]`) must degrade
  // to the runtime seam rather than expand forever.
  private final class Cache:
    val instances: scm.HashMap[String, Option[Inlinable]] = scm.HashMap()
    val active: scm.Set[String] = scm.Set()

  // Builtin scalar kinds; the natural wire code of each is `naturalCode`.
  private inline val KInt = 0
  private inline val KLong = 1
  private inline val KBoolean = 2
  private inline val KDouble = 3
  private inline val KFloat = 4
  private inline val KText = 5
  private inline val KData = 6

  private def naturalCode(kind: Int): Int = kind match
    case KDouble => 1
    case KFloat  => 5
    case KText   => 2
    case KData   => 2
    case _       => 0

  private def builtinKind(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[Int] =
    import quotes.reflect.*

    if tpe =:= TypeRepr.of[Int] then Some(KInt)
    else if tpe =:= TypeRepr.of[Long] then Some(KLong)
    else if tpe =:= TypeRepr.of[Boolean] then Some(KBoolean)
    else if tpe =:= TypeRepr.of[Double] then Some(KDouble)
    else if tpe =:= TypeRepr.of[Float] then Some(KFloat)
    else if tpe =:= TypeRepr.of[Text] then Some(KText)
    else if tpe =:= TypeRepr.of[Data] then Some(KData)
    else None

  private enum Elem:
    case Scalar(kind: Int)
    case Chunk(kind: Int)
    case Nested(instance: Inlinable, tpe: Any)
    case Runtime(parsable: Any)
    case Seam

  private enum Plan:
    case Leaf(kind: Int)
    case Nested(instance: Inlinable)
    case NestedRuntime(parsable: Any)
    case OptionalLeaf(kind: Int)
    case OptionalNested(instance: Inlinable, innerType: Any)
    case Gather(element: Elem, elementType: Any)
    case Seam

  private def resolve[field: Type](cache: Cache)(using Quotes): Option[Inlinable] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[field].dealias

    if builtinKind(tpe).isDefined then None else
      cache.instances.getOrElseUpdate(tpe.show, summonViaStaging[field].map(unwrap))
      . orElse(structuralFor[field](cache))

  private def structuralFor[field: Type](cache: Cache)(using Quotes): Option[Inlinable] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[field].dealias

    if productSupported(tpe) then Some(Inlinable.ProductInlinable[field]())
    else sumFor[field](cache)

  // A `derives`-clause carrier delegates to a structural instance; the
  // ladder works with the delegate so generator identities stay
  // recognizable.
  private def unwrap(instance: Inlinable): Inlinable = instance match
    case derived: Inlinable.ForProtobuf[?] => derived.delegate.asInstanceOf[Inlinable]
    case other                         => other

  // An `Optional[inner]` field: `inner | Unset.type`. Handled by the
  // generator itself (an absent field reads as `Unset`; a present one as
  // its inner value), so the seam's open implicit search — which cannot
  // resolve the optional instance from inside a nested inline context — is
  // never consulted.
  private def optionalInner(using Quotes)(tpe: quotes.reflect.TypeRepr)
  :   Option[quotes.reflect.TypeRepr] =

    import quotes.reflect.*

    val unset = TypeRepr.of[vacuous.Optional.Unset]

    tpe.dealias match
      case OrType(left, right) if right =:= unset => Some(left.dealias)
      case OrType(left, right) if left =:= unset  => Some(right.dealias)
      case _                                      => None

  private def planFor(using Quotes)(tpe0: quotes.reflect.TypeRepr, cache: Cache): Plan =
    import quotes.reflect.*

    val tpe = tpe0.dealias

    builtinKind(tpe) match
      case Some(kind) =>
        Plan.Leaf(kind)

      case None if optionalInner(tpe).isDefined =>
        val inner = optionalInner(tpe).get

        builtinKind(inner) match
          case Some(kind) =>
            Plan.OptionalLeaf(kind)

          case None =>
            val innerIterable = inner <:< TypeRepr.of[Iterable[Any]]

            if innerIterable then Plan.Seam else inner.asType match
              case '[innerType] =>
                resolve[innerType](cache) match
                  case Some(instance)
                      if !instance.isInstanceOf[Inlinable.IterableInlinable[?]]
                      && !cache.active.contains(inner.show) =>
                    Plan.OptionalNested(instance, inner)

                  case _ =>
                    Plan.Seam

      case None =>
        val isMap = tpe.derivesFrom(Symbol.requiredClass("scala.collection.Map"))

        // A `Map` is an `Iterable` of pairs, but its wire form is a
        // repeated entry message — it stays on the seam (`mapDecodable`).
        if !isMap && tpe <:< TypeRepr.of[Iterable[Any]] then tpe match
          case AppliedType(_, arguments) =>
            val elementType = arguments.last.dealias

            builtinKind(elementType) match
              case Some(kind) if kind <= KFloat =>
                Plan.Gather(Elem.Scalar(kind), elementType)

              case Some(kind) =>
                Plan.Gather(Elem.Chunk(kind), elementType)

              case None =>
                elementType.asType match
                  case '[element] =>
                    resolve[element](cache) match
                      case Some(instance)
                          if !instance.isInstanceOf[Inlinable.IterableInlinable[?]]
                          && !cache.active.contains(elementType.show) =>
                        Plan.Gather(Elem.Nested(instance, elementType), elementType)

                      // A recursive element would expand forever, and other
                      // shapes have no generator: each occurrence decodes
                      // through the *element's* `Decodable`, exactly the AST
                      // `listDecodable`'s per-occurrence semantics. (A
                      // custom `Packable` element would diverge here; only
                      // builtin scalars are packable today.)
                      case _ =>
                        parsableFor[element] match
                          case Some(parsable) =>
                            Plan.Gather(Elem.Runtime(parsable), elementType)

                          case None =>
                            Plan.Gather(Elem.Seam, elementType)

          case _ =>
            Plan.Seam
        else tpe.asType match
          case '[field] =>
            resolve[field](cache) match
              case Some(instance: Inlinable.IterableInlinable[?]) => Plan.Seam

              case Some(instance) if !cache.active.contains(tpe.show) =>
                Plan.Nested(instance)

              // A recursive message field would expand forever, and an
              // unresolvable one has no generator: a nominal `Parsable`
              // (the recursion tie, or a hand-written instance) parses the
              // field's window in place; otherwise the seam.
              case _ =>
                parsableFor[field] match
                  case Some(parsable) => Plan.NestedRuntime(parsable)
                  case None           => Plan.Seam

  // A sealed sum qualifies when every variant is itself an inlinable case
  // class (resolved through the ladder, so custom variant instances
  // compose). Variants are numbered by declaration order, exactly as the
  // derivations encode them. Anything else — singleton variants, generic
  // sums, an unresolvable variant — degrades to the runtime seam,
  // preserving the derived semantics exactly.
  private def sumFor[field: Type](cache: Cache)(using Quotes): Option[Inlinable] =
    sumVariants(quotes.reflect.TypeRepr.of[field].dealias).flatMap: variants =>
      val resolvable = variants.forall: (_, variantType) =>
        variantType.asType match
          case '[variantType] => resolve[variantType](cache).isDefined

      if resolvable then Some(Inlinable.SumInlinable[field]()) else None

  // The variants of a stageable sealed sum: `(label, type)` per variant, or
  // `None` when the shape is unsupported.
  private def sumVariants(using Quotes)(tpe: quotes.reflect.TypeRepr)
  :   Option[List[(String, quotes.reflect.TypeRepr)]] =

    import quotes.reflect.*

    tpe.classSymbol.flatMap: classSymbol =>
      val applied = tpe match
        case AppliedType(_, _) => true
        case _                 => false

      val children = classSymbol.children

      val supported =
        !applied
        && classSymbol.flags.is(Flags.Sealed)
        && children.nonEmpty
        && children.forall { child => child.isClassDef && child.flags.is(Flags.Case) }

      if supported then Some(children.map { child => (child.name, child.typeRef) }) else None

  // The field numbers of a product's fields: an `@field(n)` annotation with
  // a literal argument, or 1-based declaration order. `None` when any
  // annotation cannot be read statically.
  private def fieldNumbersOf(using Quotes)(classSymbol: quotes.reflect.Symbol)
  :   Option[List[Int]] =

    import quotes.reflect.*

    val params = classSymbol.primaryConstructor.paramSymss.flatten.filterNot(_.isTypeParam)
    val caseFields = classSymbol.caseFields

    def annotationNumber(annotations: List[Term]): Option[Option[Int]] =
      annotations.filter(_.tpe <:< TypeRepr.of[locomotion.field]) match
        case Nil =>
          Some(None)

        case Apply(_, List(Literal(IntConstant(number)))) :: Nil =>
          Some(Some(number))

        case _ =>
          None

    val numbers = List.range(0, caseFields.length).map: index =>
      val annotations =
        caseFields(index).annotations ++ params.lift(index).map(_.annotations).getOrElse(Nil)

      annotationNumber(annotations).map(_.getOrElse(index + 1))

    if numbers.exists(_.isEmpty) then None else
      val resolved = numbers.map(_.get)
      if resolved.distinct.length == resolved.length then Some(resolved) else None

  private def productSupported(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean =
    import quotes.reflect.*

    tpe.classSymbol.exists: classSymbol =>
      classSymbol.flags.is(Flags.Case)
      && !classSymbol.owner.isTerm
      && (tpe match { case AppliedType(_, _) => false case _ => true })
      && classSymbol.primaryConstructor.paramSymss
         . filterNot(_.exists(_.isTypeParam)).length == 1
      && fieldNumbersOf(classSymbol).isDefined

  // ── The product generator ──────────────────────────────────────────────
  // Self-contained monomorphic message parsing, mirroring the AST record
  // decoder's semantics: typed local slots and seen flags, a literal
  // field-number dispatch, last-occurrence-wins scalars, per-occurrence
  // gathering (packed or unpacked) for repeated fields, declared defaults
  // then proto3 absent semantics for missing fields, direct construction.
  private[locomotion] def productBody[product: Type](reader: Expr[ProtobufReader])
    (using Quotes)
  :   Expr[product] =

    productBody[product](reader, Cache())

  private def productBody[product: Type](reader: Expr[ProtobufReader], cache: Cache)
    (using Quotes)
  :   Expr[product] =

    import quotes.reflect.*

    val tpe = TypeRepr.of[product].dealias
    cache.active += tpe.show

    try productBody0[product](reader, cache) finally cache.active -= tpe.show

  private def productBody0[product: Type](reader: Expr[ProtobufReader], cache: Cache)
    (using Quotes)
  :   Expr[product] =

    import quotes.reflect.*

    val tpe = TypeRepr.of[product].dealias

    if !productSupported(tpe) then
      report.errorAndAbort
        (s"locomotion: ${tpe.show} is not an inlinable message (a non-generic, top-level or "+
          "object-nested case class with a single parameter list and distinct, statically "+
          "readable field numbers); use a `Decodable in Protobuf`")

    val classSymbol = tpe.classSymbol.get
    val ctor = classSymbol.primaryConstructor
    val fields = classSymbol.caseFields
    val arity = fields.length
    val fieldTypes: List[TypeRepr] = fields.map { field => tpe.memberType(field).dealias }
    val numbers: List[Int] = fieldNumbersOf(classSymbol).get
    val plans: List[Plan] = fieldTypes.map(planFor(_, cache))

    def body
      ( tactic: Expr[Tactic[ProtobufError]],
        parser: Expr[ProtobufParser] )
    :   Expr[product] =

      val owner = Symbol.spliceOwner

      val slots = List.range(0, arity).map: index =>
        Symbol.newVal(owner, "slot"+index, fieldTypes(index), Flags.Mutable, Symbol.noSymbol)

      val seens = List.range(0, arity).map: index =>
        Symbol.newVal(owner, "seen"+index, TypeRepr.of[Boolean], Flags.Mutable, Symbol.noSymbol)

      def zero(fieldType: TypeRepr): Term =
        if fieldType =:= TypeRepr.of[Int] then Literal(IntConstant(0))
        else if fieldType =:= TypeRepr.of[Long] then Literal(LongConstant(0L))
        else if fieldType =:= TypeRepr.of[Double] then Literal(DoubleConstant(0.0))
        else if fieldType =:= TypeRepr.of[Float] then Literal(FloatConstant(0.0f))
        else if fieldType =:= TypeRepr.of[Boolean] then Literal(BooleanConstant(false))
        else fieldType.asType match
          case '[fieldType] => '{ null.asInstanceOf[fieldType] }.asTerm

      val slotDefs = List.range(0, arity).map: index =>
        ValDef(slots(index), Some(zero(fieldTypes(index))))

      val seenDefs = List.range(0, arity).map: index =>
        ValDef(seens(index), Some(Literal(BooleanConstant(false))))

      val unit = Literal(UnitConstant())

      // The proto3 zero value of a builtin scalar — what an absent field
      // decodes to on the AST path (`decoded(Protobuf.Absent)`).
      def zeroValue(kind: Int): Expr[Any] = kind match
        case KInt     => Expr(0)
        case KLong    => Expr(0L)
        case KBoolean => Expr(false)
        case KDouble  => Expr(0.0)
        case KFloat   => Expr(0.0f)
        case KText    => '{ Text("") }
        case KData    => '{ IArray.empty[Byte] }

      // One scalar field read, dispatching on the tag's wire code — the
      // parser handles the natural fast path and payload-window fallback.
      def scalarRead(kind: Int, code: Expr[Int]): Expr[Any] = kind match
        case KInt     => '{ $parser.directLong($code)(using $tactic).toInt }
        case KLong    => '{ $parser.directLong($code)(using $tactic) }
        case KBoolean => '{ $parser.directLong($code)(using $tactic) != 0L }
        case KDouble  => '{ $parser.directDouble($code)(using $tactic) }
        case KFloat   => '{ $parser.directFloat($code)(using $tactic) }
        case KText    => '{ Text($parser.directString($code)(using $tactic)) }
        case KData    => '{ $parser.directData($code)(using $tactic) }

      // One packed element read, in place within the packed run's window.
      def packedRead(kind: Int): Expr[Any] = kind match
        case KInt     => '{ $parser.directVarint()(using $tactic).toInt }
        case KLong    => '{ $parser.directVarint()(using $tactic) }
        case KBoolean => '{ $parser.directVarint()(using $tactic) != 0L }
        case KDouble  => '{ java.lang.Double.longBitsToDouble($parser.directFixed64()(using $tactic)) }
        case KFloat   => '{ java.lang.Float.intBitsToFloat($parser.directFixed32()(using $tactic)) }

      // Per-field gathering state: a typed builder for `Gather` fields, a
      // wire-value buffer for `Seam` fields.
      val builders: scala.collection.immutable.Map[Int, Symbol] =
        List.range(0, arity).flatMap: index =>
          plans(index) match
            case Plan.Gather(_, elementType0) =>
              val elementType = elementType0.asInstanceOf[TypeRepr]

              val builderType =
                TypeRepr.of[scm.Builder].appliedTo(List(elementType, fieldTypes(index)))

              Some(index -> Symbol.newVal
                (owner, "builder"+index, builderType, Flags.EmptyFlags, Symbol.noSymbol))

            case _ =>
              None
        . toMap

      val buffers: scala.collection.immutable.Map[Int, Symbol] =
        List.range(0, arity).flatMap: index =>
          plans(index) match
            case Plan.Seam =>
              Some(index -> Symbol.newVal
                ( owner, "buffer"+index, TypeRepr.of[scm.ListBuffer[Protobuf]],
                  Flags.EmptyFlags, Symbol.noSymbol ))

            case _ =>
              None
        . toMap

      val builderDefs: List[Statement] = List.range(0, arity).flatMap: index =>
        plans(index) match
          case Plan.Gather(_, elementType0) =>
            val elementType = elementType0.asInstanceOf[TypeRepr]

            (elementType.asType, fieldTypes(index).asType) match
              case ('[element], '[fieldType]) =>
                val rhs: Expr[Any] =
                  '{ infer[scala.collection.Factory[element, fieldType]].newBuilder }

                Some(ValDef(builders(index), Some(rhs.asTerm)))

          case _ =>
            None

      val bufferDefs: List[Statement] = List.range(0, arity).flatMap: index =>
        plans(index) match
          case Plan.Seam =>
            Some(ValDef(buffers(index), Some('{ scm.ListBuffer[Protobuf]() }.asTerm)))

          case _ =>
            None

      // Per-field local defs, shaped for the JIT: each field's read (or
      // per-occurrence append) is emitted once and *called* from its
      // dispatch arm.
      val readDefs: scala.collection.immutable.Map[Int, Symbol] =
        List.range(0, arity).flatMap: index =>
          plans(index) match
            case Plan.Leaf(_) | Plan.Nested(_) | Plan.NestedRuntime(_) | Plan.OptionalLeaf(_)
               | Plan.OptionalNested(_, _) =>
              Some(index -> Symbol.newMethod
                ( owner, "readField"+index,
                  MethodType(List("code"))(_ => List(TypeRepr.of[Int]),
                    _ => fieldTypes(index)) ))

            case Plan.Gather(_, _) =>
              Some(index -> Symbol.newMethod
                ( owner, "readOccurrence"+index,
                  MethodType(List("code"))(_ => List(TypeRepr.of[Int]),
                    _ => TypeRepr.of[Unit]) ))

            case Plan.Seam =>
              None
        . toMap

      val readDefDefs: List[Statement] = List.range(0, arity).flatMap: index =>
        readDefs.get(index).map: method =>
          DefDef(method, params =>
            val code = params.head.head.asInstanceOf[Term].asExprOf[Int]

            val rhs: Expr[Any] = plans(index) match
              case Plan.Leaf(kind) =>
                scalarRead(kind, code)

              case Plan.OptionalLeaf(kind) =>
                scalarRead(kind, code)

              case Plan.Nested(instance) =>
                fieldTypes(index).asType match
                  case '[fieldType] =>
                    '{
                      val saved = $parser.directEnterField($code)(using $tactic)

                      val result: fieldType =
                        ${ instance.asInstanceOf[Inlinable { type Self = fieldType }]
                             . parse(reader) }

                      $parser.directLeaveField(saved)
                      result
                    }

              case Plan.NestedRuntime(parsable) =>
                fieldTypes(index).asType match
                  case '[fieldType] =>
                    val instance = parsable.asInstanceOf[Expr[Any]]

                    '{
                      val saved = $parser.directEnterField($code)(using $tactic)

                      val result: fieldType =
                        Protobuf.Parsable.parseField[fieldType]
                          ($instance.asInstanceOf[AnyRef], $reader.asInstanceOf[AnyRef])

                      $parser.directLeaveField(saved)
                      result
                    }

              case Plan.OptionalNested(instance, innerType0) =>
                innerType0.asInstanceOf[TypeRepr].asType match
                  case '[innerType] =>
                    '{
                      val saved = $parser.directEnterField($code)(using $tactic)

                      val result: innerType =
                        ${ instance.asInstanceOf[Inlinable { type Self = innerType }]
                             . parse(reader) }

                      $parser.directLeaveField(saved)
                      result
                    }

              case Plan.Gather(element, elementType0) =>
                val elementType = elementType0.asInstanceOf[TypeRepr]

                (elementType.asType, fieldTypes(index).asType) match
                  case ('[element], '[fieldType]) =>
                    val builder =
                      Ref(builders(index)).asExprOf[scm.Builder[element, fieldType]]

                    element match
                      case Elem.Scalar(kind) =>
                        val natural = naturalCode(kind)

                        '{
                          if $code == 2 && ${Expr(natural)} != 2 then
                            val saved = $parser.directEnterField(2)(using $tactic)

                            while !$parser.directAtLimit do
                              $builder += ${ packedRead(kind).asExprOf[element] }

                            $parser.directLeaveField(saved)
                          else $builder += ${ scalarRead(kind, code).asExprOf[element] }
                        }

                      case Elem.Chunk(kind) =>
                        '{ $builder += ${ scalarRead(kind, code).asExprOf[element] } }

                      case Elem.Nested(instance0, _) =>
                        val instance =
                          instance0.asInstanceOf[Inlinable { type Self = element }]

                        '{
                          val saved = $parser.directEnterField($code)(using $tactic)
                          val result: element = ${ instance.parse(reader) }
                          $parser.directLeaveField(saved)
                          $builder += result
                        }

                      case Elem.Runtime(parsable) =>
                        val instance = parsable.asInstanceOf[Expr[Any]]

                        '{
                          val saved = $parser.directEnterField($code)(using $tactic)

                          val result: element =
                            Protobuf.Parsable.parseField[element]
                              ($instance.asInstanceOf[AnyRef], $reader.asInstanceOf[AnyRef])

                          $parser.directLeaveField(saved)
                          $builder += result
                        }

                      case Elem.Seam =>
                        val occurrence =
                          seamDecode[element]('{ $parser.directWire($code)(using $tactic) })

                        '{ $builder += $occurrence }

              case Plan.Seam =>
                '{ () }

            Some(rhs.asTerm.changeOwner(method)))

      // The dispatch arms, one per field number. Scalars and nested values
      // overwrite their slot (last occurrence wins, as the AST accessors'
      // `single`); gathered fields append.
      def arms(code: Expr[Int]): List[CaseDef] = List.range(0, arity).map: index =>
        val body: Term = plans(index) match
          case Plan.Leaf(_) | Plan.Nested(_) | Plan.NestedRuntime(_) | Plan.OptionalLeaf(_)
             | Plan.OptionalNested(_, _) =>
            Block
              ( List
                  ( Assign
                      ( Ref(slots(index)),
                        Apply(Ref(readDefs(index)), List(code.asTerm)) ),
                    Assign(Ref(seens(index)), Literal(BooleanConstant(true))) ),
                unit )

          case Plan.Gather(_, _) =>
            Block
              ( List
                  ( Apply(Ref(readDefs(index)), List(code.asTerm)),
                    Assign(Ref(seens(index)), Literal(BooleanConstant(true))) ),
                unit )

          case Plan.Seam =>
            val buffer = Ref(buffers(index)).asExprOf[scm.ListBuffer[Protobuf]]

            Block
              ( List
                  ( '{ $buffer += $parser.directWire($code)(using $tactic) }.asTerm,
                    Assign(Ref(seens(index)), Literal(BooleanConstant(true))) ),
                unit )

        CaseDef(Literal(IntConstant(numbers(index))), None, body)

      def fallthrough(code: Expr[Int]) =
        CaseDef(Wildcard(), None, '{ $parser.directSkipField($code)(using $tactic) }.asTerm)

      // The record loop: one tag per iteration, dispatching on the field
      // number until the window is exhausted.
      val step: Term =
        val tag = Symbol.newVal(owner, "tag", TypeRepr.of[Int], Flags.EmptyFlags, Symbol.noSymbol)
        val code = Symbol.newVal(owner, "code", TypeRepr.of[Int], Flags.EmptyFlags, Symbol.noSymbol)
        val tagRef = Ref(tag).asExprOf[Int]
        val codeRef = Ref(code).asExprOf[Int]

        Block
          ( List
              ( ValDef(tag, Some('{ $parser.directTag()(using $tactic) }.asTerm)),
                ValDef(code, Some('{ $tagRef & 7 }.asTerm)) ),
            Match('{ $tagRef >>> 3 }.asTerm, arms(codeRef) ::: List(fallthrough(codeRef))) )

      val loop: List[Statement] =
        List(While('{ !$parser.directAtLimit }.asTerm, step))

      // The declared-default-then-absent expression per field.
      def defaultOr(index: Int, absent: Expr[Any]): Expr[Any] =
        fieldTypes(index).asType match
          case '[fieldType] =>
            '{
              val declared = wisteria.internal.default[product, fieldType](${Expr(index)})

              if !declared.absent then declared.asInstanceOf[fieldType]
              else ${ absent.asExprOf[fieldType] }
            }

      def planAbsent(index: Int, tactic: Expr[Tactic[ProtobufError]]): Expr[Any] =
        plans(index) match
          case Plan.Leaf(kind) =>
            zeroValue(kind)

          case Plan.OptionalLeaf(_) | Plan.OptionalNested(_, _) =>
            '{ vacuous.Unset }

          case Plan.Nested(instance) =>
            fieldTypes(index).asType match
              case '[fieldType] =>
                instance.asInstanceOf[Inlinable { type Self = fieldType }].absent(tactic)

          // An absent runtime-parsed message reads as an empty message —
          // `decoded(Absent)`'s all-fields-absent record.
          case Plan.NestedRuntime(parsable) =>
            fieldTypes(index).asType match
              case '[fieldType] =>
                val instance = parsable.asInstanceOf[Expr[Any]]

                '{
                  val window = $parser.directWindow(0, 0)

                  val result: fieldType =
                    Protobuf.Parsable.parseField[fieldType]
                      ($instance.asInstanceOf[AnyRef], $reader.asInstanceOf[AnyRef])

                  $parser.directRestore(window)
                  result
                }

          case Plan.Gather(_, _) =>
            fieldTypes(index).asType match
              case '[fieldType] =>
                '{ ${ Ref(builders(index)).asExpr }
                     . asInstanceOf[scm.Builder[?, fieldType]].result() }

          case Plan.Seam =>
            fieldTypes(index).asType match
              case '[fieldType] =>
                seamDecode[fieldType]('{ Protobuf.Absent })

      // Finalize each slot: missing fields take their declared default or
      // proto3 absent value; gathered fields collect their builder; seam
      // fields decode their gathered occurrences exactly as the AST path
      // decodes `Repeated(values)`.
      val finalizers: List[Term] = List.range(0, arity).map: index =>
        val seen = Ref(seens(index)).asExprOf[Boolean]

        plans(index) match
          case Plan.Leaf(_) | Plan.Nested(_) | Plan.NestedRuntime(_) | Plan.OptionalLeaf(_)
             | Plan.OptionalNested(_, _) =>
            If
              ( '{ !$seen }.asTerm,
                Assign
                  ( Ref(slots(index)),
                    defaultOr(index, planAbsent(index, tactic)).asTerm ),
                unit )

          case Plan.Gather(_, _) =>
            fieldTypes(index).asType match
              case '[fieldType] =>
                val collected =
                  '{ ${ Ref(builders(index)).asExpr }
                       . asInstanceOf[scm.Builder[?, fieldType]].result() }

                Assign
                  ( Ref(slots(index)),
                    '{
                      if $seen then $collected
                      else ${ defaultOr(index, collected).asExprOf[fieldType] }
                    }.asTerm )

          case Plan.Seam =>
            fieldTypes(index).asType match
              case '[fieldType] =>
                val buffer = Ref(buffers(index)).asExprOf[scm.ListBuffer[Protobuf]]

                val absent: Expr[fieldType] =
                  defaultOr(index, seamDecode[fieldType]('{ Protobuf.Absent }))
                  . asExprOf[fieldType]

                Assign
                  ( Ref(slots(index)),
                    '{
                      if $seen then
                        ${ seamDecode[fieldType]('{ Protobuf.Repeated(proscenium.List.of($buffer.toList)) }) }
                      else $absent
                    }.asTerm )

      val construct: Term =
        Apply(Select(New(Inferred(tpe)), ctor), slots.map { slot => Ref(slot) })

      Block
        ( slotDefs ::: seenDefs ::: builderDefs ::: bufferDefs ::: readDefDefs ::: loop
            ::: finalizers,
          construct )
      . asExprOf[product]

    '{
      val tactic = infer[Tactic[ProtobufError]]
      val parser = $reader.rawParser.asInstanceOf[ProtobufParser]
      ${ body('tactic, 'parser) }
    }

  // A wholly-absent message — what `decoded(Protobuf.Absent)` produces on
  // the AST path: every field takes its declared default or proto3 absent
  // value.
  private[locomotion] def productAbsent[product: Type]
    (tactic: Expr[Tactic[ProtobufError]])
    (using Quotes)
  :   Expr[product] =

    import quotes.reflect.*

    val cache: Cache = Cache()
    val tpe = TypeRepr.of[product].dealias
    cache.active += tpe.show
    val classSymbol = tpe.classSymbol.get
    val ctor = classSymbol.primaryConstructor
    val fields = classSymbol.caseFields
    val arity = fields.length
    val fieldTypes: List[TypeRepr] = fields.map { field => tpe.memberType(field).dealias }
    val plans: List[Plan] = fieldTypes.map(planFor(_, cache))

    val values: List[Term] = List.range(0, arity).map: index =>
      fieldTypes(index).asType match
        case '[fieldType] =>
          val absent: Expr[Any] = plans(index) match
            case Plan.Leaf(kind) => kind match
              case KInt     => Expr(0)
              case KLong    => Expr(0L)
              case KBoolean => Expr(false)
              case KDouble  => Expr(0.0)
              case KFloat   => Expr(0.0f)
              case KText    => '{ Text("") }
              case KData    => '{ IArray.empty[Byte] }

            case Plan.Nested(instance) =>
              instance.asInstanceOf[Inlinable { type Self = fieldType }].absent(tactic)

            case Plan.OptionalLeaf(_) | Plan.OptionalNested(_, _) =>
              '{ vacuous.Unset }

            case Plan.Gather(_, elementType0) =>
              val elementType = elementType0.asInstanceOf[TypeRepr]

              elementType.asType match
                case '[element] =>
                  '{ infer[scala.collection.Factory[element, fieldType]].newBuilder.result() }

            case Plan.Seam =>
              seamDecode[fieldType]('{ Protobuf.Absent })

          '{
            val declared = wisteria.internal.default[product, fieldType](${Expr(index)})

            if !declared.absent then declared.asInstanceOf[fieldType]
            else ${ absent.asExprOf[fieldType] }
          }.asTerm

    Apply(Select(New(Inferred(tpe)), ctor), values).asExprOf[product]

  // ── The sum generator ──────────────────────────────────────────────────
  // A oneof: the whole window is scanned once — recording, for the lowest
  // variant number present, the extent of its last occurrence — then the
  // chosen occurrence is re-entered and parsed as its variant message,
  // exactly the AST disjunction's `map.contains(index + 1)` scan and
  // `Repeated(...)`'s last-occurrence payload.
  private[locomotion] def sumBody[sum: Type](reader: Expr[ProtobufReader])(using Quotes)
  :   Expr[sum] =

    sumBody[sum](reader, Cache())

  private def sumBody[sum: Type](reader: Expr[ProtobufReader], cache: Cache)(using Quotes)
  :   Expr[sum] =

    import quotes.reflect.*

    cache.active += TypeRepr.of[sum].dealias.show

    try sumBody0[sum](reader, cache)
    finally cache.active -= TypeRepr.of[sum].dealias.show

  private def sumBody0[sum: Type](reader: Expr[ProtobufReader], cache: Cache)(using Quotes)
  :   Expr[sum] =

    import quotes.reflect.*

    val variants = sumVariants(TypeRepr.of[sum].dealias).getOrElse:
      report.errorAndAbort
        (s"locomotion: ${TypeRepr.of[sum].show} is not an inlinable oneof (a non-generic "+
          "sealed type whose variants are all case classes)")

    val arity = variants.length

    def dispatch(index: Int, chosen: Expr[Int], tactic: Expr[Tactic[ProtobufError]])
    :   Expr[sum] =

      if index == arity then
        '{ abort(ProtobufError(ProtobufError.Reason.MissingField(0)))(using $tactic) }
      else variants(index)(1).asType match
        case '[type variantType <: sum; variantType] =>
          val instance = resolve[variantType](cache).getOrElse:
            report.errorAndAbort
              (s"locomotion: no Inlinable for variant ${variants(index)(0)}")
          . asInstanceOf[Inlinable { type Self = variantType }]

          '{
            if $chosen == ${Expr(index + 1)} then
              def parseVariant(): variantType = ${ instance.parse(reader) }
              parseVariant()
            else ${ dispatch(index + 1, chosen, tactic) }
          }

    '{
      val tactic = infer[Tactic[ProtobufError]]
      val parser = $reader.rawParser.asInstanceOf[ProtobufParser]
      var chosen = Int.MaxValue
      var chosenStart = 0
      var chosenEnd = 0

      while !parser.directAtLimit do
        val tag = parser.directTag()(using tactic)
        val number = tag >>> 3
        val saved = parser.directEnterField(tag & 7)(using tactic)

        if number >= 1 && number <= ${Expr(arity)} && number <= chosen then
          chosen = number
          chosenStart = parser.directMark
          chosenEnd = parser.directBoundary

        parser.directLeaveField(saved)

      if chosen == Int.MaxValue
      then abort(ProtobufError(ProtobufError.Reason.MissingField(0)))(using tactic)
      else
        val outer = parser.directWindow(chosenStart, chosenEnd)
        val result: sum = ${ dispatch(0, 'chosen, 'tactic) }
        parser.directRestore(outer)
        result
    }

  // ── The entry macro ────────────────────────────────────────────────────
  def inlinableParsable[value: Type](using Quotes): Expr[value is Protobuf.Parsable] =
    import quotes.reflect.*

    val cache: Cache = Cache()

    val root: Inlinable = resolve[value](cache).getOrElse:
      report.errorAndAbort
        (s"locomotion: no Inlinable instance for ${TypeRepr.of[value].show}, and it is not "+
          "an inlinable message or oneof; use a `Decodable in Protobuf`")

    if root.isInstanceOf[Inlinable.IterableInlinable[?]] then
      report.errorAndAbort
        (s"locomotion: ${TypeRepr.of[value].show} is a collection; a Protobuf message is the "+
          "unit of direct parsing")

    val instance = root.asInstanceOf[Inlinable { type Self = value }]

    '{
      // Sealed per the codec-thunk pattern: the generated body resolves its
      // capabilities where it is spliced.
      caps.unsafe.unsafeAssumePure:
        new Protobuf.Parsable.Direct[value]:
          protected def parseCarrier(reader0: AnyRef): value =
            // The reader is never bound as a typed local: a capability
            // class cannot be quoted into a pure hole, so every use casts
            // from the neutral carrier afresh — the rim-reassertion
            // pattern, at each use.
            ${ instance.parse('{ reader0.asInstanceOf[ProtobufReader] }) }
    }
