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
package jacinta

import scala.collection.mutable as scm
import scala.quoted.*

import anticipation.*
import contingency.*
import prepositional.*
import vacuous.*
import zephyrine.*

// The machinery behind `Json.Inlinable`: expansion-time instance resolution
// (through an in-macro staging compiler), the structural generators for
// products and collections, and the `Inlinable.parsable` entry macro.
object stagedInternal:

  // The runtime seam for a field type without an `Inlinable`: a nominal
  // `Parsable` (a staged sum, a custom instance) is called directly,
  // skipping the `Field.Adapter` the fallback chain would otherwise
  // construct per occurrence. An inline method because `summonFrom` may
  // only live in one; it expands where the generated parser is spliced.
  inline def fieldSeam[fieldType](reader: JsonReader): fieldType =
    scala.compiletime.summonFrom:
      case parsable: (`fieldType` is Json.Parsable) => parsable.parse(reader)
      case _ => scala.compiletime.summonInline[fieldType is Json.Field].parse(reader)

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

  private val primitiveClasses: Map[String, Class[?]] =
    Map
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
              (s"jacinta: staged summon for ${TypeRepr.of[field].show} took ${duration}ms")

            Some(instance)

          case _ =>
            None
      catch
        case _: ReflectiveOperationException => None
        case _: LinkageError                 => None
        case _: AssertionError               => None
        case _: Exception                    => None

  // ── The resolution ladder ──────────────────────────────────────────────
  // builtin → staged summon → structural collection/product → (caller's)
  // runtime seam. The cache spans one entry expansion, so a type's staging
  // summon runs at most once however often it recurs in the graph.

  // The per-entry expansion cache: memoized staging summons, and the set of
  // types whose generators are currently on the expansion stack — a
  // recursive occurrence (`Tree` with `children: List[Tree]`) must degrade
  // to the runtime seam rather than expand forever.
  private final class Cache:
    val instances: scm.HashMap[String, Option[Inlinable]] = scm.HashMap()
    val active: scm.Set[String] = scm.Set()

  private def resolve[field: Type](cache: Cache)(using Quotes): Option[Inlinable] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[field].dealias

    builtinFor(tpe).orElse:
      cache.instances.getOrElseUpdate(tpe.show, summonViaStaging[field].map(unwrap))
      . orElse(structuralFor[field](cache))
      . orElse(runtimeFor[field])

  // A `derives`-clause carrier delegates to a structural instance; the
  // ladder works with the delegate so generator identities (product,
  // collection) stay recognizable.
  private def unwrap(instance: Inlinable): Inlinable = instance match
    case derived: Inlinable.ForJson[?] => derived.delegate.asInstanceOf[Inlinable]
    case other                         => other

  // The runtime tiers, resolved with a reflection-level implicit search at
  // expansion time and called through neutral-carrier helpers (the erasing
  // cast crosses the capability boundary, as wisteria's field-instance
  // engine does): a nominal `Parsable` — the recursion tie, since a
  // recursive record's own alias given (a lazy val) is found from inside
  // its own definition — then the `Json.Field` fallback chain. Neither
  // `summonInline` nor a `using` parameter works for these from inside the
  // generated parser's inline context.
  private def runtimeFor[field: Type](using Quotes): Option[Inlinable] =
    import quotes.reflect.*

    def searched(target: TypeRepr): Option[Expr[Any]] =
      Implicits.search(target) match
        case success: ImplicitSearchSuccess => Some(success.tree.asExpr)
        case _                              => None

    searched(TypeRepr.of[field is Json.Parsable]).map(RuntimeInlinable[field](_)).orElse:
      searched(TypeRepr.of[field is Json.Field]).map(RuntimeInlinable[field](_))

  private final class RuntimeInlinable[value](parsing: Expr[Any]) extends Inlinable:
    type Self = value

    def parse(reader: Expr[JsonReader])(using Quotes, Type[value]): Expr[value] =
      '{
        Json.Parsable.parseField[value]
          ($parsing.asInstanceOf[AnyRef], $reader.asInstanceOf[AnyRef])
      }

    override def absent(tactic: Expr[Tactic[JsonError]])(using Quotes, Type[value])
    :   Expr[value] =

      '{ Json.Parsable.absentField[value]($parsing.asInstanceOf[AnyRef])(using $tactic) }

  // Composition points become local defs rather than textual inlining: a
  // fully-flattened parser exceeds HotSpot's huge-method bytecode limit and
  // runs interpreted (measured: ~6x slower than the staged parser). A local
  // def keeps each generated method JIT-compilable while the call stays
  // monomorphic and direct. Leaf generators stay inline.
  private def nested[fieldType: Type]
    (instance: Inlinable { type Self = fieldType }, reader: Expr[JsonReader])
    (using Quotes)
  :   Expr[fieldType] =

    instance match
      case _: Inlinable.ProductInlinable[?] | _: Inlinable.IterableInlinable[?] =>
        '{
          def parseNested(): fieldType = ${ instance.parse(reader) }
          parseNested()
        }

      case _ =>
        instance.parse(reader)

  private def builtinFor(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[Inlinable] =
    import quotes.reflect.*

    if tpe =:= TypeRepr.of[Int] then Some(Inlinable.int)
    else if tpe =:= TypeRepr.of[Long] then Some(Inlinable.long)
    else if tpe =:= TypeRepr.of[Double] then Some(Inlinable.double)
    else if tpe =:= TypeRepr.of[Float] then Some(Inlinable.float)
    else if tpe =:= TypeRepr.of[Boolean] then Some(Inlinable.boolean)
    else if tpe =:= TypeRepr.of[Text] then Some(Inlinable.text)
    else if tpe =:= TypeRepr.of[String] then Some(Inlinable.string)
    else None

  private def structuralFor[field: Type](cache: Cache)(using Quotes): Option[Inlinable] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[field].dealias

    // A `Map` is an `Iterable` of pairs, but its wire form is a JSON
    // object, not an array — it stays on the runtime seam
    // (`Json.Parsable.dictionary` through the `Field` chain).
    val isMap = tpe.derivesFrom(Symbol.requiredClass("scala.collection.Map"))

    if !isMap && tpe <:< TypeRepr.of[Iterable[Any]] then tpe match
      case AppliedType(_, arguments) =>
        arguments.last.asType match
          case '[element] =>
            resolve[element](cache).map: instance =>
              Inlinable.IterableInlinable[element]
                (instance.asInstanceOf[element is Inlinable])

      case _ =>
        None
    // A recursive occurrence degrades to the runtime seam, whose
    // derivation is lazy.
    else if cache.active.contains(tpe.show) then None
    else if productSupported(tpe) then Some(Inlinable.ProductInlinable[field]())
    else None

  private def productSupported(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean =
    import quotes.reflect.*

    tpe.classSymbol.exists: classSymbol =>
      classSymbol.flags.is(Flags.Case)
      && !classSymbol.owner.isTerm
      && (tpe match { case AppliedType(_, _) => false case _ => true })
      && classSymbol.primaryConstructor.paramSymss
         . filterNot(_.exists(_.isTypeParam)).length == 1
      && !hasRenames(classSymbol)

  // `@name` renames resolve through inline machinery the structural
  // generator does not replicate; annotated records stay on `staged`.
  private def hasRenames(using Quotes)(classSymbol: quotes.reflect.Symbol): Boolean =
    import quotes.reflect.*

    val annotated =
      classSymbol.primaryConstructor.paramSymss.flatten.filterNot(_.isTypeParam)
        . flatMap(_.annotations)
      ++ classSymbol.caseFields.flatMap(_.annotations)

    annotated.exists { annotation => annotation.tpe <:< TypeRepr.of[adversaria.name[?]] }

  // ── The collection generator ───────────────────────────────────────────
  // Mirrors `Json.Parsable.iterable` exactly: openArray, per-element loop,
  // per-index focusing when the read site's `Foci` is active — with the
  // element's generated code inlined into the loop.
  private[jacinta] def iterableBody[collection: Type]
    (reader: Expr[JsonReader], element0: Inlinable)
    (using Quotes)
  :   Expr[collection] =

    import quotes.reflect.*

    TypeRepr.of[collection].dealias match
      case AppliedType(_, arguments) =>
        arguments.last.asType match
          case '[element] =>
            val instance = element0.asInstanceOf[Inlinable { type Self = element }]

            '{
              def parseElement(): element = ${ instance.parse(reader) }
              val factory = infer[scala.collection.Factory[element, collection]]
              val builder = factory.newBuilder
              val foci = infer[Foci[Json.Focus]]
              val focused = foci.active
              val parser = $reader.rawParser.asInstanceOf[Parser]
              val ptactic = $reader.rawTactic.asInstanceOf[Tactic[ParseError]]
              parser.directOpenArray()(using ptactic)
              var index = 0
              var continue = parser.directElementFirst()(using ptactic)

              while continue do
                builder +=
                  ( if focused then
                      Json.Parsable.focusing(foci, index.toString.tt)(parseElement())
                    else parseElement() )

                index += 1
                continue = parser.directElementNext()(using ptactic)

              builder.result()
            }

      case _ =>
        report.errorAndAbort
          ("jacinta: an inlinable collection requires an applied collection type")

  // ── The product generator ──────────────────────────────────────────────
  // Self-contained monomorphic product parsing, mirroring the staged
  // parser's semantics: typed local slots and seen flags, literal
  // packed-word key dispatch (an opaque key resolves through the interned
  // name against literal strings; unknown keys are skipped), first
  // occurrence read per key with focus bookkeeping, declared defaults then
  // absent semantics for missing fields, direct construction. Unlike the
  // staged parser there is no KeyTable and no instances array: every field
  // is either inlined through its `Inlinable` code or spliced as a
  // `summonInline`d `Json.Field` runtime call.
  private[jacinta] def productBody[product: Type](reader: Expr[JsonReader])(using Quotes)
  :   Expr[product] =

    productBody[product](reader, Cache())

  private def productBody[product: Type](reader: Expr[JsonReader], cache: Cache)(using Quotes)
  :   Expr[product] =

    import quotes.reflect.*

    cache.active += TypeRepr.of[product].dealias.show

    try productBody0[product](reader, cache)
    finally cache.active -= TypeRepr.of[product].dealias.show

  private def productBody0[product: Type](reader: Expr[JsonReader], cache: Cache)(using Quotes)
  :   Expr[product] =

    import quotes.reflect.*

    val tpe = TypeRepr.of[product].dealias

    if !productSupported(tpe) then
      report.errorAndAbort
        (s"jacinta: ${tpe.show} is not an inlinable product (a non-generic, top-level or "+
          "object-nested case class with a single parameter list and no `@name` renames); "+
          "use `Json.Parsable.staged` or `derived`")

    val classSymbol = tpe.classSymbol.get
    val ctor = classSymbol.primaryConstructor
    val fields = classSymbol.caseFields
    val arity = fields.length
    val fieldNames: List[String] = fields.map(_.name)
    val fieldTypes: List[TypeRepr] = fields.map { field => tpe.memberType(field).dealias }

    def packedName(index: Int): Option[(Long, Long)] =
      val name = fieldNames(index)
      val length = name.length

      val packs = length > 0 && length <= 16 &&
        name.forall { char => char >= ' ' && char < 127 }

      if !packs then None else
        var low = 0L
        var high = 0L
        var position = 0

        while position < length do
          val byte = name.charAt(position).toLong & 0xFF
          if position < 8 then low |= byte << (position*8)
          else high |= byte << ((position - 8)*8)
          position += 1

        Some((low, high))

    val packedNames: List[Option[(Long, Long)]] = List.range(0, arity).map(packedName)

    def body
      ( foci:    Expr[Foci[Json.Focus]],
        focused: Expr[Boolean],
        tactic:  Expr[Tactic[JsonError]],
        parser:  Expr[Parser],
        ptactic: Expr[Tactic[ParseError]] )
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

      // EXPERIMENT(cc-bypass): builtins read straight off the parser bound
      // once per record, skipping the JsonReader rim entirely.
      def builtinDirect(tpe: TypeRepr): Option[Expr[Any]] =
        if tpe =:= TypeRepr.of[Int] then Some('{ $parser.directLong()(using $ptactic).toInt })
        else if tpe =:= TypeRepr.of[Long] then Some('{ $parser.directLong()(using $ptactic) })
        else if tpe =:= TypeRepr.of[Double] then
          Some('{ $parser.directDouble()(using $ptactic) })
        else if tpe =:= TypeRepr.of[Boolean] then
          Some('{ $parser.directBoolean()(using $ptactic) })
        else if tpe =:= TypeRepr.of[Text] then
          Some('{ $parser.directString()(using $ptactic).tt })
        else if tpe =:= TypeRepr.of[String] then
          Some('{ $parser.directString()(using $ptactic) })
        else None

      // The absent expression per field, through the ladder.
      def fieldAbsent(index: Int): Expr[Any] =
        fieldTypes(index).asType match
          case '[fieldType] =>
            if builtinDirect(fieldTypes(index)).isDefined then
              '{ Json.Parsable.missing[fieldType]()(using $tactic) }
            else resolve[fieldType](cache) match
              case Some(instance0) =>
                instance0.asInstanceOf[Inlinable { type Self = fieldType }].absent(tactic)

              case None =>
                '{
                  scala.compiletime.summonInline[fieldType is Json.Field]
                  . absent()(using $tactic)
                }

      val fieldCode: List[(Expr[Any], Expr[Any])] = List.range(0, arity).map: index =>
        (Expr(0), fieldAbsent(index))

      // Per field, up to three local defs, shaped for the JIT: a nested
      // body (generated exactly once and *called* from both branches), a
      // cold def holding the focusing machinery, and the hot `readField`,
      // which shrinks to a flag test and a call — small enough that the
      // key-step arms always inline it.
      val readDefs = List.range(0, arity).map: index =>
        Symbol.newMethod
          ( owner, "readField"+index,
            MethodType(Nil)(_ => Nil, _ => fieldTypes(index)) )

      val slowDefs = List.range(0, arity).map: index =>
        Symbol.newMethod
          ( owner, "readFieldSlow"+index,
            MethodType(Nil)(_ => Nil, _ => fieldTypes(index)) )

      val readDefDefs: List[Statement] = List.range(0, arity).flatMap: index =>
        fieldTypes(index).asType match
          case '[fieldType] =>
            def rawParse(): Expr[fieldType] =
              builtinDirect(fieldTypes(index)) match
                case Some(direct) =>
                  direct.asExprOf[fieldType]

                case None =>
                  resolve[fieldType](cache) match
                    case Some(instance0) =>
                      instance0.asInstanceOf[Inlinable { type Self = fieldType }]
                      . parse(reader)

                    case None =>
                      '{ stagedInternal.fieldSeam[fieldType]($reader) }

            val builtin = builtinDirect(fieldTypes(index)).isDefined

            // Non-builtin bodies are potentially large: emit once as a def
            // and call it from both temperature branches.
            val nestedDef: Option[(Symbol, Statement)] =
              if builtin then None else
                val symbol =
                  Symbol.newMethod
                    ( owner, "readNested"+index,
                      MethodType(Nil)(_ => Nil, _ => fieldTypes(index)) )

                val rhs = rawParse().asTerm.changeOwner(symbol)
                Some((symbol, DefDef(symbol, _ => Some(rhs))))

            def call(symbol: Symbol): Expr[fieldType] =
              Apply(Ref(symbol), Nil).asExprOf[fieldType]

            def hot(): Expr[fieldType] =
              nestedDef match
                case Some((symbol, _)) => call(symbol)
                case None              => rawParse()

            val slowRhs: Term =
              '{
                Json.Parsable.focusing($foci, ${Expr(fieldNames(index))}.tt)(${ hot() })
              }.asTerm.changeOwner(slowDefs(index))

            val readRhs: Term =
              '{ if $focused then ${ call(slowDefs(index)) } else ${ hot() } }
              . asTerm.changeOwner(readDefs(index))

            nestedDef.map(_(1)).toList
              ::: List
                    ( DefDef(slowDefs(index), _ => Some(slowRhs)),
                      DefDef(readDefs(index), _ => Some(readRhs)) )

      def arms: List[CaseDef] = List.range(0, arity).map: index =>
        val rhs =
          Block
            ( List
                ( Assign(Ref(slots(index)), Apply(Ref(readDefs(index)), Nil)),
                  Assign(Ref(seens(index)), Literal(BooleanConstant(true))) ),
              unit )

        CaseDef(Literal(IntConstant(index)), None, rhs)

      def fallthrough =
        CaseDef(Wildcard(), None, '{ $parser.directSkipValue()(using $ptactic) }.asTerm)

      val run = Symbol.newVal(owner, "run", TypeRepr.of[Boolean], Flags.Mutable, Symbol.noSymbol)

      def packedChainOver(wordRef: Expr[Long], highRef: Expr[Long])(index: Int): Term =
        if index == arity then Literal(IntConstant(-1))
        else packedNames(index) match
          case None => packedChainOver(wordRef, highRef)(index + 1)

          case Some((low, highWord)) =>
            If
              ( '{ $wordRef == ${Expr(low)} && $highRef == ${Expr(highWord)} }.asTerm,
                Literal(IntConstant(index)),
                packedChainOver(wordRef, highRef)(index + 1) )

      def namedChain(name: Expr[String], index: Int): Expr[Int] =
        if index == arity then Expr(-1)
        else
          '{
            if ${Expr(fieldNames(index))} == $name then ${Expr(index)}
            else ${ namedChain(name, index + 1) }
          }

      // One key step: fresh symbols and arm trees per instantiation (trees
      // may not be shared between the unrolled first step and the loop).
      def step(wordStep: Expr[Long]): Term =
        val word =
          Symbol.newVal(owner, "word", TypeRepr.of[Long], Flags.EmptyFlags, Symbol.noSymbol)

        val high =
          Symbol.newVal(owner, "high", TypeRepr.of[Long], Flags.EmptyFlags, Symbol.noSymbol)

        val found =
          Symbol.newVal(owner, "found", TypeRepr.of[Int], Flags.EmptyFlags, Symbol.noSymbol)

        val wordRef = Ref(word).asExprOf[Long]
        val highRef = Ref(high).asExprOf[Long]

        // An opaque key (window edge, escapes, oversized) resolves through
        // the generally-consumed interned name; `null` there is a close
        // brace that became visible only after a refill.
        val opaque: Expr[Int] =
          '{
            val name = $reader.keyName()
            if name == null then -2 else ${ namedChain('{name.nn}, 0) }
          }

        val resolveStep: Term =
          If
            ( '{ $wordRef == JsonReader.KeyOpaque }.asTerm,
              opaque.asTerm,
              Block
                ( List(ValDef(high, Some('{ $parser.directKeyWordHigh }.asTerm))),
                  packedChainOver(wordRef, highRef)(0) ) )

        // The refill-revealed close brace (-2) is one more dispatch case
        // rather than a comparison ahead of every dispatch.
        val ended =
          CaseDef
            ( Literal(IntConstant(-2)), None,
              Assign(Ref(run), Literal(BooleanConstant(false))) )

        Block
          ( List(ValDef(word, Some(wordStep.asTerm))),
            If
              ( '{ $wordRef == JsonReader.KeyEnd }.asTerm,
                Assign(Ref(run), Literal(BooleanConstant(false))),
                Block
                  ( List(ValDef(found, Some(resolveStep))),
                    Match(Ref(found), arms ::: List(ended, fallthrough)) ) ) )

      // The first key step is unrolled ahead of the loop, so the steady-state
      // step consults no per-key seen/comma state.
      val loop: List[Statement] =
        readDefDefs :::
          List
            ( '{ $parser.directOpenObject()(using $ptactic) }.asTerm,
              ValDef(run, Some(Literal(BooleanConstant(true)))),
              step('{ $parser.directKeyWordFirst() }),
              While(Ref(run), step('{ $parser.directKeyWordNext() })) )

      val absents: List[Term] = List.range(0, arity).map: index =>
        fieldTypes(index).asType match
          case '[fieldType] =>
            val keyText: Expr[Text] = '{ ${Expr(fieldNames(index))}.tt }

            val resolveAbsent: Term =
              Assign
                ( Ref(slots(index)),
                  '{
                    val declared =
                      wisteria.internal.default[product, fieldType](${Expr(index)})

                    if !declared.absent then declared.asInstanceOf[fieldType]
                    else Json.Parsable.focusing($foci, $keyText)
                      (${ fieldCode(index)(1).asExprOf[fieldType] })
                  }.asTerm )

            If('{ !${Ref(seens(index)).asExprOf[Boolean]} }.asTerm, resolveAbsent, unit)

      val construct: Term =
        Apply(Select(New(Inferred(tpe)), ctor), slots.map { slot => Ref(slot) })

      Block(slotDefs ::: seenDefs ::: loop ::: absents, construct).asExprOf[product]

    '{
      val foci = infer[Foci[Json.Focus]]
      val focused = foci.active
      val tactic = infer[Tactic[JsonError]]
      val parser = $reader.rawParser.asInstanceOf[Parser]
      val ptactic = $reader.rawTactic.asInstanceOf[Tactic[ParseError]]
      ${ body('foci, 'focused, 'tactic, 'parser, 'ptactic) }
    }

  // ── The entry macro ────────────────────────────────────────────────────
  def inlinableParsable[value: Type](using Quotes): Expr[value is Json.Parsable] =
    import quotes.reflect.*

    val cache: Cache = Cache()

    val root: Inlinable = resolve[value](cache).getOrElse:
      report.errorAndAbort
        (s"jacinta: no Inlinable instance for ${TypeRepr.of[value].show}, and it is not an "+
          "inlinable product; use `Json.Parsable.staged` or `derived`")

    // A runtime-tier root would find the very given under definition — an
    // infinite self-call — or add nothing over the instance it wraps.
    if root.isInstanceOf[RuntimeInlinable[?]] then
      report.errorAndAbort
        (s"jacinta: ${TypeRepr.of[value].show} has no generator of its own; use "+
          "`Json.Parsable.staged` or `derived` rather than `Inlinable.parsable`")

    val instance = root.asInstanceOf[Inlinable { type Self = value }]

    '{
      // Sealed per the codec-thunk pattern, like the staged instances: the
      // generated body resolves its capabilities where it is spliced.
      caps.unsafe.unsafeAssumePure:
        new Json.Parsable.Direct[value]:
          protected def parseCarrier(reader0: AnyRef): value =
            // A capability class cannot be quoted into a pure hole, so
            // every use casts from the neutral carrier afresh.
            ${ instance.parse('{ reader0.asInstanceOf[JsonReader] }) }
    }
