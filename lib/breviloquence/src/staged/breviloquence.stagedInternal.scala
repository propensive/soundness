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
package breviloquence

import scala.collection.mutable as scm
import scala.quoted.*

import anticipation.*
import contingency.*
import distillate.*
import prepositional.*
import vacuous.*

// The machinery behind `Cbor.Inlinable`: expansion-time instance resolution
// (through an in-macro staging compiler), the structural generators for
// products, collections and sums, and the `Inlinable.parsable` entry macro.
object stagedInternal:

  // The runtime seam for a field type without an `Inlinable`: a nominal
  // `Parsable` (a custom instance) is called directly; anything else bridges
  // through its `Decodable in Cbor`, materializing just that field's
  // subtree. An inline method because `summonFrom` may only live in one; it
  // expands where the generated parser is spliced.
  inline def fieldSeam[fieldType](reader: CborReader): fieldType =
    scala.compiletime.summonFrom:
      case parsable: (`fieldType` is Cbor.Parsable) => parsable.parse(reader)

      case _ =>
        Cbor.Parsable
        . fromDecodable(scala.compiletime.summonInline[fieldType is Decodable in Cbor])
        . parse(reader)

  // The seam's absent counterpart, preserving the AST path's semantics for
  // a missing key: `decoded(Cbor(Ast(Unset)))` through the bridge.
  inline def fieldSeamAbsent[fieldType](tactic: Tactic[CborError]): fieldType =
    scala.compiletime.summonFrom:
      case parsable: (`fieldType` is Cbor.Parsable) => parsable.absent()(using tactic)

      case _ =>
        Cbor.Parsable
        . fromDecodable(scala.compiletime.summonInline[fieldType is Decodable in Cbor])
        . absent()(using tactic)

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
              ( s"breviloquence: staged summon for ${TypeRepr.of[field].show} took "+
                s"${duration}ms" )

            Some(instance)

          case _ =>
            None
      catch
        case _: ReflectiveOperationException => None
        case _: LinkageError                 => None
        case _: AssertionError               => None
        case _: Exception                    => None

  // ── The resolution ladder ──────────────────────────────────────────────
  // builtin → staged summon → structural collection/product/sum →
  // (caller's) runtime seam. The cache spans one entry expansion, so a
  // type's staging summon runs at most once however often it recurs in the
  // graph.

  private type Cache = scm.HashMap[String, Option[Inlinable]]

  private def resolve[field: Type](cache: Cache)(using Quotes): Option[Inlinable] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[field].dealias

    builtinFor(tpe).orElse:
      cache.getOrElseUpdate(tpe.show, summonViaStaging[field]).orElse(structuralFor[field](cache))

  // Composition points become local defs rather than textual inlining: a
  // fully-flattened parser exceeds HotSpot's huge-method bytecode limit and
  // runs interpreted. A local def keeps each generated method
  // JIT-compilable while the call stays monomorphic and direct. Leaf
  // generators stay inline.
  private def nested[fieldType: Type]
    (instance: Inlinable { type Self = fieldType }, reader: Expr[CborReader])
    (using Quotes)
  :   Expr[fieldType] =

    instance match
      case _: Inlinable.ProductInlinable[?] | _: Inlinable.IterableInlinable[?]
         | _: Inlinable.SumInlinable[?] =>
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
    else if tpe =:= TypeRepr.of[IArray[Byte]] then Some(Inlinable.byteString)
    else if tpe =:= TypeRepr.of[Cbor] then Some(Inlinable.cbor)
    else None

  private def structuralFor[field: Type](cache: Cache)(using Quotes): Option[Inlinable] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[field].dealias

    // A `Map` is an `Iterable` of pairs, but its wire form is a CBOR map,
    // not an array — it stays on the seam (the AST `mapDecodable`).
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
    else if productSupported(tpe) then Some(Inlinable.ProductInlinable[field]())
    else sumFor[field](cache)

  // A sealed sum qualifies when every variant is itself an inlinable case
  // class (resolved through the ladder, so custom variant instances
  // compose) and its `Discriminable in Cbor` — obtained live through the
  // staging summon — is a `Cbor.DiscriminantKey`, whose dispatch is a
  // scan-ahead of one map entry. Anything else — singleton variants,
  // `@name` renames, an unresolvable variant, a custom discriminator —
  // degrades to the runtime seam, preserving the derived semantics exactly.
  private def sumFor[field: Type](cache: Cache)(using Quotes): Option[Inlinable] =
    sumVariants(quotes.reflect.TypeRepr.of[field].dealias).flatMap: variants =>
      val resolvable = variants.forall: (_, variantType) =>
        variantType.asType match
          case '[variantType] => resolve[variantType](cache).isDefined

      if !resolvable then None
      else discriminantKeyFor[field].map { key => Inlinable.SumInlinable[field](key) }

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
        && children.forall: child =>
             child.isClassDef && child.flags.is(Flags.Case) && !hasRenames(child)

      if supported then Some(children.map { child => (child.name, child.typeRef) }) else None

  // The sum's discriminant key, when its `Discriminable in Cbor` resolves —
  // inside the staging compiler, so the instance is live — to a
  // `Cbor.DiscriminantKey`.
  private def discriminantKeyFor[sum: Type](using Quotes): Option[String] =
    import quotes.reflect.*
    import scala.quoted.staging

    shapeOf(TypeRepr.of[sum]).flatMap: shape =>
      try
        given settings: staging.Compiler.Settings =
          staging.Compiler.Settings.make
            (None, List("-experimental", "-classpath", innerClasspath))

        given staging.Compiler = staging.Compiler.make(macroClassloader)

        val result: Any = staging.run:
          val quotes2 = summon[Quotes]
          import quotes2.reflect as r2

          def rebuild(shape: TypeShape): r2.TypeRepr =
            val base = r2.TypeRepr.typeConstructorOf(shape.clazz)
            if shape.arguments.isEmpty then base
            else base.appliedTo(shape.arguments.map(rebuild))

          val cbor = r2.TypeRepr.of[Cbor]

          val target =
            r2.Refinement
              ( r2.Refinement
                  ( r2.TypeRepr.of[wisteria.Discriminable], "Self",
                    r2.TypeBounds(rebuild(shape), rebuild(shape)) ),
                "Form",
                r2.TypeBounds(cbor, cbor) )

          target.asType match
            case '[target] => '{ scala.compiletime.summonInline[target] }

        result match
          case discriminant: Cbor.DiscriminantKey[?] => Some(discriminant.key.s)
          case _                                     => None
      catch
        case _: ReflectiveOperationException => None
        case _: LinkageError                 => None
        case _: AssertionError               => None
        case _: Exception                    => None

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
  // generator does not replicate; annotated records stay on the AST path.
  private def hasRenames(using Quotes)(classSymbol: quotes.reflect.Symbol): Boolean =
    import quotes.reflect.*

    val annotated =
      classSymbol.primaryConstructor.paramSymss.flatten.filterNot(_.isTypeParam)
        . flatMap(_.annotations)
      ++ classSymbol.caseFields.flatMap(_.annotations)

    annotated.exists { annotation => annotation.tpe <:< TypeRepr.of[adversaria.name[?]] }

  // ── The collection generator ───────────────────────────────────────────
  // Mirrors the AST `collectionDecodable` exactly: a definite array is
  // count-driven, an indefinite one Break-terminated — with the element's
  // generated code inlined into the loop.
  private[breviloquence] def iterableBody[collection: Type]
    (reader: Expr[CborReader], element0: Inlinable)
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
              val tactic = infer[Tactic[CborError]]
              val parser = $reader.rawParser.asInstanceOf[Cbor.Parser]
              var remaining = parser.directOpenArray()(using tactic)
              var run = remaining != 0

              while run do
                if remaining < 0 && parser.directBreak()(using tactic) then run = false
                else
                  builder += parseElement()
                  remaining -= 1
                  if remaining == 0 then run = false

              builder.result()
            }

      case _ =>
        report.errorAndAbort
          ("breviloquence: an inlinable collection requires an applied collection type")

  // ── The product generator ──────────────────────────────────────────────
  // Self-contained monomorphic record parsing, mirroring the AST record
  // decoder's semantics: typed local slots and seen flags, literal
  // packed-word key dispatch (an opaque key resolves through the generally
  // consumed name against literal strings; unknown keys and non-text-keyed
  // entries are skipped), first occurrence read per key, declared defaults
  // then absent semantics for missing fields, direct construction.
  private[breviloquence] def productBody[product: Type](reader: Expr[CborReader])(using Quotes)
  :   Expr[product] =

    productBody[product](reader, scm.HashMap())

  private def productBody[product: Type](reader: Expr[CborReader], cache: Cache)(using Quotes)
  :   Expr[product] =

    import quotes.reflect.*

    val tpe = TypeRepr.of[product].dealias

    if !productSupported(tpe) then
      report.errorAndAbort
        (s"breviloquence: ${tpe.show} is not an inlinable product (a non-generic, top-level "+
          "or object-nested case class with a single parameter list and no `@name` renames); "+
          "use a `Decodable in Cbor`")

    val classSymbol = tpe.classSymbol.get
    val ctor = classSymbol.primaryConstructor
    val fields = classSymbol.caseFields
    val arity = fields.length
    val fieldNames: List[String] = fields.map(_.name)
    val fieldTypes: List[TypeRepr] = fields.map { field => tpe.memberType(field).dealias }

    // Field names pack exactly as `directKeyWord` packs wire keys: 1-16
    // bytes, all 7-bit ASCII, little-endian into a low and high word.
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
      ( tactic: Expr[Tactic[CborError]],
        parser: Expr[Cbor.Parser] )
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

      // Builtins read straight off the parser bound once per record,
      // skipping the CborReader rim entirely.
      def builtinDirect(tpe: TypeRepr): Option[Expr[Any]] =
        if tpe =:= TypeRepr.of[Int] then Some('{ $parser.directLong()(using $tactic).toInt })
        else if tpe =:= TypeRepr.of[Long] then Some('{ $parser.directLong()(using $tactic) })
        else if tpe =:= TypeRepr.of[Double] then
          Some('{ $parser.directDouble()(using $tactic) })
        else if tpe =:= TypeRepr.of[Float] then
          Some('{ $parser.directDouble()(using $tactic).toFloat })
        else if tpe =:= TypeRepr.of[Boolean] then
          Some('{ $parser.directBoolean()(using $tactic) })
        else if tpe =:= TypeRepr.of[Text] then
          Some('{ $parser.directString()(using $tactic).tt })
        else if tpe =:= TypeRepr.of[String] then
          Some('{ $parser.directString()(using $tactic) })
        else if tpe =:= TypeRepr.of[IArray[Byte]] then
          Some('{ $parser.directBytes()(using $tactic) })
        else if tpe =:= TypeRepr.of[Cbor] then
          Some('{ Cbor.ast($parser.value()(using $tactic)) })
        else None

      // The absent expression per field, through the ladder.
      def fieldAbsent(index: Int): Expr[Any] =
        fieldTypes(index).asType match
          case '[fieldType] =>
            if builtinDirect(fieldTypes(index)).isDefined then
              '{ Cbor.Parsable.missing[fieldType]()(using $tactic) }
            else resolve[fieldType](cache) match
              case Some(instance0) =>
                instance0.asInstanceOf[Inlinable { type Self = fieldType }].absent(tactic)

              case None =>
                '{ stagedInternal.fieldSeamAbsent[fieldType]($tactic) }

      // One local def per field, shaped for the JIT: non-builtin bodies are
      // potentially large, so each is emitted once and *called* from its
      // dispatch arm.
      val readDefs = List.range(0, arity).map: index =>
        Symbol.newMethod
          ( owner, "readField"+index,
            MethodType(Nil)(_ => Nil, _ => fieldTypes(index)) )

      val readDefDefs: List[Statement] = List.range(0, arity).map: index =>
        fieldTypes(index).asType match
          case '[fieldType] =>
            val rhs: Expr[fieldType] =
              builtinDirect(fieldTypes(index)) match
                case Some(direct) =>
                  direct.asExprOf[fieldType]

                case None =>
                  resolve[fieldType](cache) match
                    case Some(instance0) =>
                      nested[fieldType]
                        ( instance0.asInstanceOf[Inlinable { type Self = fieldType }],
                          reader )

                    case None =>
                      '{ stagedInternal.fieldSeam[fieldType]($reader) }

            DefDef(readDefs(index), _ => Some(rhs.asTerm.changeOwner(readDefs(index))))

      def arms: List[CaseDef] = List.range(0, arity).map: index =>
        val readAndMark =
          Block
            ( List
                ( Assign(Ref(slots(index)), Apply(Ref(readDefs(index)), Nil)),
                  Assign(Ref(seens(index)), Literal(BooleanConstant(true))) ),
              unit )

        // First occurrence wins, as the AST path's immutable map builder: a
        // repeat key's value is skipped.
        val rhs =
          If
            ( '{ !${Ref(seens(index)).asExprOf[Boolean]} }.asTerm,
              readAndMark,
              '{ $parser.directSkipValue()(using $tactic) }.asTerm )

        CaseDef(Literal(IntConstant(index)), None, rhs)

      def fallthrough =
        CaseDef(Wildcard(), None, '{ $parser.directSkipValue()(using $tactic) }.asTerm)

      val remaining =
        Symbol.newVal(owner, "remaining", TypeRepr.of[Int], Flags.Mutable, Symbol.noSymbol)

      val run = Symbol.newVal(owner, "run", TypeRepr.of[Boolean], Flags.Mutable, Symbol.noSymbol)
      val remainingRef = Ref(remaining).asExprOf[Int]
      val runRef = Ref(run).asExprOf[Boolean]

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

      // One entry step: read the key (packed fast path, general otherwise),
      // dispatch, then count the entry off. A non-text key arrives as a
      // `null` name: its entry is ignored, exactly as the AST record
      // decoder — the key is already consumed, and the unknown arm skips
      // the value.
      def step: Term =
        val word =
          Symbol.newVal(owner, "word", TypeRepr.of[Long], Flags.EmptyFlags, Symbol.noSymbol)

        val high =
          Symbol.newVal(owner, "high", TypeRepr.of[Long], Flags.EmptyFlags, Symbol.noSymbol)

        val found =
          Symbol.newVal(owner, "found", TypeRepr.of[Int], Flags.EmptyFlags, Symbol.noSymbol)

        val wordRef = Ref(word).asExprOf[Long]
        val highRef = Ref(high).asExprOf[Long]

        val opaque: Expr[Int] =
          '{
            val name = $parser.directKeyName()(using $tactic)
            if name == null then -1 else ${ namedChain('{name.nn}, 0) }
          }

        val resolveStep: Term =
          If
            ( '{ $wordRef == CborReader.KeyOpaque }.asTerm,
              opaque.asTerm,
              Block
                ( List(ValDef(high, Some('{ $parser.directKeyHigh }.asTerm))),
                  packedChainOver(wordRef, highRef)(0) ) )

        val entry: Term =
          Block
            ( List
                ( ValDef(word, Some('{ $parser.directKeyWord() }.asTerm)),
                  Block
                    ( List(ValDef(found, Some(resolveStep))),
                      Match(Ref(found), arms ::: List(fallthrough)) ),
                  Assign(Ref(remaining), '{ $remainingRef - 1 }.asTerm) ),
              If
                ( '{ $remainingRef == 0 }.asTerm,
                  Assign(Ref(run), Literal(BooleanConstant(false))),
                  unit ) )

        If
          ( '{ $remainingRef < 0 && $parser.directBreak()(using $tactic) }.asTerm,
            Assign(Ref(run), Literal(BooleanConstant(false))),
            entry )

      val loop: List[Statement] =
        readDefDefs :::
          List
            ( ValDef(remaining, Some('{ $parser.directOpenMap()(using $tactic) }.asTerm)),
              ValDef(run, Some('{ $remainingRef != 0 }.asTerm)),
              While(runRef.asTerm, step) )

      val absents: List[Term] = List.range(0, arity).map: index =>
        fieldTypes(index).asType match
          case '[fieldType] =>
            val resolveAbsent: Term =
              Assign
                ( Ref(slots(index)),
                  '{
                    val declared =
                      wisteria.internal.default[product, fieldType](${Expr(index)})

                    if !declared.absent then declared.asInstanceOf[fieldType]
                    else ${ fieldAbsent(index).asExprOf[fieldType] }
                  }.asTerm )

            If('{ !${Ref(seens(index)).asExprOf[Boolean]} }.asTerm, resolveAbsent, unit)

      val construct: Term =
        Apply(Select(New(Inferred(tpe)), ctor), slots.map { slot => Ref(slot) })

      Block(slotDefs ::: seenDefs ::: loop ::: absents, construct).asExprOf[product]

    '{
      val tactic = infer[Tactic[CborError]]
      val parser = $reader.rawParser.asInstanceOf[Cbor.Parser]
      ${ body('tactic, 'parser) }
    }

  // ── The sum generator ──────────────────────────────────────────────────
  // The variant is chosen by a scan-ahead of the discriminant entry (the
  // parser is left where it started), then parsed from the start of the
  // map as its product — the discriminant entry skips as an unknown key —
  // exactly the AST disjunction's `discriminate` + `delegate` semantics.
  private[breviloquence] def sumBody[sum: Type](reader: Expr[CborReader], key: String)
    (using Quotes)
  :   Expr[sum] =

    sumBody[sum](reader, key, scm.HashMap())

  private def sumBody[sum: Type](reader: Expr[CborReader], key: String, cache: Cache)
    (using Quotes)
  :   Expr[sum] =

    import quotes.reflect.*

    val variants = sumVariants(TypeRepr.of[sum].dealias).getOrElse:
      report.errorAndAbort
        (s"breviloquence: ${TypeRepr.of[sum].show} is not an inlinable sum (a non-generic "+
          "sealed type whose variants are all case classes without `@name` renames)")

    val arity = variants.length

    def dispatch(index: Int, tag: Expr[String]): Expr[sum] =
      if index == arity then
        '{
          provide[Tactic[wisteria.VariantError]]:
            abort(wisteria.VariantError[sum]($tag.tt))
        }
      else variants(index)(1).asType match
        case '[type variantType <: sum; variantType] =>
          val instance = resolve[variantType](cache).getOrElse:
            report.errorAndAbort
              (s"breviloquence: no Inlinable for variant ${variants(index)(0)}")
          . asInstanceOf[Inlinable { type Self = variantType }]

          '{
            if $tag == ${Expr(variants(index)(0))} then
              def parseVariant(): variantType = ${ instance.parse(reader) }
              parseVariant()
            else ${ dispatch(index + 1, tag) }
          }

    '{
      val tactic = infer[Tactic[CborError]]
      val parser = $reader.rawParser.asInstanceOf[Cbor.Parser]
      val tag = parser.directDiscriminant(${Expr(key)})(using tactic)

      if tag == null then abort(CborError(CborError.Reason.Absent))(using tactic)
      else
        val tagValue: String = tag.nn
        ${ dispatch(0, 'tagValue) }
    }

  // ── The entry macro ────────────────────────────────────────────────────
  def inlinableParsable[value: Type](using Quotes): Expr[value is Cbor.Parsable] =
    import quotes.reflect.*

    val cache: Cache = scm.HashMap()

    val root: Inlinable = resolve[value](cache).getOrElse:
      report.errorAndAbort
        (s"breviloquence: no Inlinable instance for ${TypeRepr.of[value].show}, and it is not "+
          "an inlinable product, collection or key-discriminated sum; use a `Decodable in Cbor`")

    val instance = root.asInstanceOf[Inlinable { type Self = value }]

    '{
      // Sealed per the codec-thunk pattern: the generated body resolves its
      // capabilities where it is spliced.
      caps.unsafe.unsafeAssumePure:
        new Cbor.Parsable:
          type Self = value
          def parse(reader: CborReader): value = ${ instance.parse('{reader}) }
    }
