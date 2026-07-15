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
package stratiform

import scala.collection.mutable as scm
import scala.quoted.*

import anticipation.*
import contingency.*
import gigantism.*
import gossamer.*
import prepositional.*
import vacuous.*

// The machinery behind `stratiform.Inlinable`: expansion-time instance
// resolution (through an in-macro staging compiler), the structural
// generators for products and collections, and the `Inlinable.parsable`
// entry macro — jacinta's `stagedInternal`, transposed to TEL's entry-based
// reading (an indent travels with the reader) and to the derived engine's
// gathering semantics for repeatable fields.
object stagedInternal:

  // The runtime seam for a field type without an `Inlinable`: the field's
  // `Tel.Parsing` instance, preferring a nominal `Parsable` in scope (a
  // custom instance, a staged sibling) over the `Tel.Field` fallback chain.
  // The generated record body binds it once per parse, so occurrence reads,
  // repeatability dispatch and absence all consult one instance — exactly
  // the derived engine's per-field instance. An inline method because
  // `summonFrom` may only live in one; it expands where the generated
  // parser is spliced.
  inline def fieldInstance[fieldType]: fieldType is Tel.Parsing =
    scala.compiletime.summonFrom:
      case parsable: (`fieldType` is Tel.Parsable) => parsable
      case _ => scala.compiletime.summonInline[fieldType is Tel.Field]

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
              (s"stratiform: staged summon for ${TypeRepr.of[field].show} took ${duration}ms")

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

  private type Cache = scm.HashMap[String, Option[Inlinable]]

  private def resolve[field: Type](cache: Cache)(using Quotes): Option[Inlinable] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[field].dealias

    builtinFor(tpe).orElse:
      cache.getOrElseUpdate(tpe.show, summonViaStaging[field]).orElse(structuralFor[field](cache))

  private def builtinFor(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[Inlinable] =
    import quotes.reflect.*

    if tpe =:= TypeRepr.of[Int] then Some(Inlinable.int)
    else if tpe =:= TypeRepr.of[Long] then Some(Inlinable.long)
    else if tpe =:= TypeRepr.of[Boolean] then Some(Inlinable.boolean)
    else if tpe =:= TypeRepr.of[Double] then Some(Inlinable.double)
    else if tpe =:= TypeRepr.of[Text] then Some(Inlinable.text)
    else if tpe =:= TypeRepr.of[String] then Some(Inlinable.string)
    else None

  private def structuralFor[field: Type](cache: Cache)(using Quotes): Option[Inlinable] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[field].dealias

    if tpe <:< TypeRepr.of[Iterable[Any]] then tpe match
      case AppliedType(_, arguments) =>
        arguments.last.asType match
          case '[element] =>
            resolve[element](cache).map: instance =>
              Inlinable.IterableInlinable[element]
                (instance.asInstanceOf[element is Inlinable])

      case _ =>
        None
    else if productSupported(tpe) then Some(Inlinable.ProductInlinable[field]())
    else None

  private[stratiform] def productSupported(using Quotes)(tpe: quotes.reflect.TypeRepr)
  :   Boolean =

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
  // A single entry read as a collection: one element — the runtime
  // `Tel.Parsable.iterable`'s single-entry behavior. In field position the
  // product generator never calls this: it gathers each occurrence of the
  // keyword directly (see `fieldLoop`), exactly as the derived engine
  // routes repeatable fields through `parseElement`.
  private[stratiform] def iterableBody[collection: Type]
    (reader: Expr[TelReader], indent: Expr[Int], element0: Inlinable)
    (using Quotes)
  :   Expr[collection] =

    import quotes.reflect.*

    TypeRepr.of[collection].dealias match
      case AppliedType(_, arguments) =>
        arguments.last.asType match
          case '[element] =>
            val instance = element0.asInstanceOf[Inlinable { type Self = element }]

            '{
              def parseElement(): element = ${ instance.parse(reader, indent) }
              val factory = infer[scala.collection.Factory[element, collection]]
              val builder = factory.newBuilder
              builder += parseElement()
              builder.result()
            }

      case _ =>
        report.errorAndAbort
          ("stratiform: an inlinable collection requires an applied collection type")

  // ── The product generator ──────────────────────────────────────────────
  // Self-contained monomorphic entry parsing, mirroring the staged parser's
  // semantics: typed local slots and seen flags, literal packed-word keyword
  // dispatch (an opaque keyword resolves through the interned keyword text
  // against literal strings; unknown keywords are skipped), first occurrence
  // read per keyword with focus bookkeeping, declared defaults then absent
  // semantics for missing fields, direct construction. A collection field
  // gathers every occurrence of its keyword into a typed builder — the
  // derived engine's repeatable semantics, with the element's generated
  // code inlined at the occurrence site. A field with no `Inlinable` binds
  // its runtime `Tel.Parsing` instance once per record and keeps the
  // engine's dynamic repeatability dispatch.
  private[stratiform] def productFields[product: Type]
    (reader: Expr[TelReader], indent: Expr[Int])
    (using Quotes)
  :   Expr[product] =

    productFields[product](reader, indent, scm.HashMap())

  private[stratiform] def productFields[product: Type]
    (reader: Expr[TelReader], indent: Expr[Int], cache: Cache)
    (using Quotes)
  :   Expr[product] =

    '{
      val foci = infer[Foci[Tel.Focus]]
      val focused = foci.active
      val tactic = infer[Tactic[TelError]]
      ${ fieldLoop[product](reader, indent, 'foci, 'focused, 'tactic, cache) }
    }

  // How one field reads, established at expansion: a builtin leaf, a
  // gathered collection (with its element's generator), any other resolved
  // instance (a nested record, a custom leaf), or the runtime seam.
  private enum Plan:
    case Leaf(instance: Inlinable)
    case Gather(element: Inlinable)
    case Nested(instance: Inlinable)
    case Seam

  private def fieldLoop[product: Type]
    ( reader:  Expr[TelReader],
      indent:  Expr[Int],
      foci:    Expr[Foci[Tel.Focus]],
      focused: Expr[Boolean],
      tactic:  Expr[Tactic[TelError]],
      cache:   Cache )
    (using Quotes)
  :   Expr[product] =

    import quotes.reflect.*

    val tpe = TypeRepr.of[product].dealias

    if !productSupported(tpe) then
      report.errorAndAbort
        (s"stratiform: ${tpe.show} is not an inlinable product (a non-generic, top-level or "+
          "object-nested case class with a single parameter list and no `@name` renames); "+
          "use `Tel.Parsable.staged` or `derived`")

    val classSymbol = tpe.classSymbol.get
    val ctor = classSymbol.primaryConstructor
    val fields = classSymbol.caseFields
    val arity = fields.length
    val fieldNames: List[String] = fields.map(_.name)
    val fieldTypes: List[TypeRepr] = fields.map { field => tpe.memberType(field).dealias }
    val wireNames: List[String] = fieldNames.map { name => Tel.camelToKebab(name).s }

    val plans: List[Plan] = fieldTypes.map: fieldType =>
      builtinFor(fieldType) match
        case Some(leaf) =>
          Plan.Leaf(leaf)

        case None =>
          fieldType.asType match
            case '[fieldType] =>
              resolve[fieldType](cache) match
                case Some(iterable: Inlinable.IterableInlinable[?]) => Plan.Gather(iterable.element0)
                case Some(instance)                                 => Plan.Nested(instance)
                case None                                           => Plan.Seam

    // Keywords compile to literal packed-word comparisons using the same
    // camel→kebab mapping and packing as the staged parser; a wire keyword
    // that cannot pack (longer than eight bytes) always arrives as
    // `KeywordOpaque` and resolves through the literal text step, which
    // matches all fields by string.
    def packedKeyword(index: Int): Option[Long] =
      val name = wireNames(index)
      val length = name.length

      val packs = length > 0 && length <= 8 &&
        name.forall { char => char >= '!' && char <= '~' }

      if !packs then None else
        var word = 0L
        var position = 0

        while position < length do
          word |= (name.charAt(position).toLong & 0xFF) << (position*8)
          position += 1

        Some(word)

    val packedKeywords: List[Option[Long]] = List.range(0, arity).map(packedKeyword)

    val owner = Symbol.spliceOwner
    val unit = Literal(UnitConstant())

    val slots = List.range(0, arity).map: index =>
      Symbol.newVal(owner, "slot"+index, fieldTypes(index), Flags.Mutable, Symbol.noSymbol)

    val seens = List.range(0, arity).map: index =>
      Symbol.newVal(owner, "seen"+index, TypeRepr.of[Boolean], Flags.Mutable, Symbol.noSymbol)

    def zero(fieldType: TypeRepr): Term =
      if fieldType =:= TypeRepr.of[Int] then Literal(IntConstant(0))
      else if fieldType =:= TypeRepr.of[Long] then Literal(LongConstant(0L))
      else if fieldType =:= TypeRepr.of[Double] then Literal(DoubleConstant(0.0))
      else if fieldType =:= TypeRepr.of[Boolean] then Literal(BooleanConstant(false))
      else fieldType.asType match
        case '[fieldType] => '{ null.asInstanceOf[fieldType] }.asTerm

    val slotDefs = List.range(0, arity).map: index =>
      ValDef(slots(index), Some(zero(fieldTypes(index))))

    val seenDefs = List.range(0, arity).map: index =>
      ValDef(seens(index), Some(Literal(BooleanConstant(false))))

    def keyText(index: Int): Expr[Text] = '{ ${Expr(wireNames(index))}.tt }

    // ── Gathered collection fields: a typed builder per field, an element
    // def emitted once (composition points become local defs — a fully
    // flattened parser exceeds HotSpot's huge-method limit and runs
    // interpreted), each occurrence appended with focus bookkeeping. ──

    case class GatherState(builder: Symbol, element: Symbol, defs: List[Statement])

    val gathers: List[Option[GatherState]] = List.range(0, arity).map: index =>
      plans(index) match
        case Plan.Gather(element0) =>
          fieldTypes(index).asType match
            case '[fieldType] =>
              fieldTypes(index) match
                case AppliedType(_, arguments) =>
                  arguments.last.asType match
                    case '[element] =>
                      val instance = element0.asInstanceOf[Inlinable { type Self = element }]

                      val builderSymbol =
                        Symbol.newVal
                          ( owner, "gather"+index,
                            TypeRepr.of[scm.Builder[element, fieldType]],
                            Flags.EmptyFlags, Symbol.noSymbol )

                      val builderDef =
                        ValDef
                          ( builderSymbol,
                            Some:
                              '{
                                infer[scala.collection.Factory[element, fieldType]].newBuilder
                              }.asTerm )

                      val elementSymbol =
                        Symbol.newMethod
                          ( owner, "parseElement"+index,
                            MethodType(Nil)(_ => Nil, _ => TypeRepr.of[element]) )

                      val elementRhs =
                        instance.parse(reader, indent).asTerm.changeOwner(elementSymbol)

                      val elementDef = DefDef(elementSymbol, _ => Some(elementRhs))

                      Some(GatherState(builderSymbol, elementSymbol, List(builderDef, elementDef)))

                case _ =>
                  None

        case _ =>
          None

    // ── Seam fields: the runtime instance, bound once per record, with the
    // engine's dynamic repeatability dispatch and occurrence buffer. ──

    case class SeamState(instance: Symbol, repeats: Symbol, buffer: Symbol, defs: List[Statement])

    val bufferType = TypeRepr.of[scm.ListBuffer[Any] | Null]

    val seams: List[Option[SeamState]] = List.range(0, arity).map: index =>
      plans(index) match
        case Plan.Seam =>
          fieldTypes(index).asType match
            case '[fieldType] =>
              val instanceSymbol =
                Symbol.newVal
                  ( owner, "instance"+index, TypeRepr.of[fieldType is Tel.Parsing],
                    Flags.EmptyFlags, Symbol.noSymbol )

              val instanceDef =
                ValDef
                  ( instanceSymbol,
                    Some('{ stagedInternal.fieldInstance[fieldType] }.asTerm) )

              val instanceRef = Ref(instanceSymbol).asExprOf[fieldType is Tel.Parsing]

              val repeatsSymbol =
                Symbol.newVal
                  ( owner, "repeats"+index, TypeRepr.of[Boolean],
                    Flags.EmptyFlags, Symbol.noSymbol )

              val repeatsDef =
                ValDef(repeatsSymbol, Some('{ Tel.Parsable.repeats($instanceRef) }.asTerm))

              val bufferSymbol =
                Symbol.newVal(owner, "buffer"+index, bufferType, Flags.Mutable, Symbol.noSymbol)

              val bufferDef = ValDef(bufferSymbol, Some('{ null }.asTerm))

              Some:
                SeamState
                  ( instanceSymbol, repeatsSymbol, bufferSymbol,
                    List(instanceDef, repeatsDef, bufferDef) )

        case _ =>
          None

    // Nested records (and custom non-leaf instances): the body is emitted
    // once as a local def and called from the dispatch arm, keeping each
    // generated method JIT-compilable.
    val nesteds: List[Option[(Symbol, Statement)]] = List.range(0, arity).map: index =>
      plans(index) match
        case Plan.Nested(instance0) =>
          fieldTypes(index).asType match
            case '[fieldType] =>
              val instance = instance0.asInstanceOf[Inlinable { type Self = fieldType }]

              val symbol =
                Symbol.newMethod
                  ( owner, "parseNested"+index,
                    MethodType(Nil)(_ => Nil, _ => fieldTypes(index)) )

              val rhs = instance.parse(reader, indent).asTerm.changeOwner(symbol)
              Some((symbol, DefDef(symbol, _ => Some(rhs))))

        case _ =>
          None

    // ── Dispatch arms ──────────────────────────────────────────────────────

    def firstWins(index: Int, read: Term): Term =
      If
        ( Ref(seens(index)),
          '{ $reader.skipEntry($indent) }.asTerm,
          Block
            ( List
                ( Assign(Ref(slots(index)), read),
                  Assign(Ref(seens(index)), Literal(BooleanConstant(true))) ),
              unit ) )

    val arms: List[CaseDef] = List.range(0, arity).map: index =>
      val rhs: Term = fieldTypes(index).asType match
        case '[fieldType] =>
          plans(index) match
            case Plan.Leaf(instance0) =>
              val instance = instance0.asInstanceOf[Inlinable { type Self = fieldType }]

              firstWins
                ( index,
                  '{
                    Tel.Parsable.focusing($foci, ${keyText(index)})
                      (${ instance.parse(reader, indent) })
                  }.asTerm )

            case Plan.Nested(_) =>
              val (symbol, _) = nesteds(index).get
              val call = Apply(Ref(symbol), Nil).asExprOf[fieldType]

              firstWins
                ( index,
                  '{ Tel.Parsable.focusing($foci, ${keyText(index)})($call) }.asTerm )

            case Plan.Gather(_) =>
              val gather = gathers(index).get

              fieldTypes(index) match
                case AppliedType(_, arguments) =>
                  arguments.last.asType match
                    case '[element] =>
                      val builderRef =
                        Ref(gather.builder).asExprOf[scm.Builder[element, fieldType]]

                      val call = Apply(Ref(gather.element), Nil).asExprOf[element]

                      '{
                        $builderRef.addOne
                          (Tel.Parsable.focusing($foci, ${keyText(index)})($call))
                      }.asTerm

                case _ =>
                  report.errorAndAbort("stratiform: unreachable gather shape")

            case Plan.Seam =>
              val seam = seams(index).get
              val instanceRef = Ref(seam.instance).asExprOf[fieldType is Tel.Parsing]
              val bufferRef = Ref(seam.buffer).asExprOf[scm.ListBuffer[Any] | Null]

              val ensure: Term =
                If
                  ( '{ $bufferRef == null }.asTerm,
                    Assign(Ref(seam.buffer), '{ scm.ListBuffer.empty[Any] }.asTerm),
                    unit )

              val append: Term =
                '{
                  $bufferRef.asInstanceOf[scm.ListBuffer[Any]].addOne
                    ( Tel.Parsable.focusing($foci, ${keyText(index)}):
                        Tel.Parsable.parseElement($instanceRef, $reader, $indent) )
                }.asTerm

              val read: Term =
                '{
                  Tel.Parsable.focusing($foci, ${keyText(index)})
                    ($instanceRef.parse($reader, $indent))
                }.asTerm

              If
                ( Ref(seam.repeats),
                  Block(List(ensure, append), unit),
                  firstWins(index, read) )

      CaseDef(Literal(IntConstant(index)), None, rhs)

    val fallthrough = CaseDef(Wildcard(), None, '{ $reader.skipEntry($indent) }.asTerm)

    // ── The keyword loop: packed-word dispatch with a literal text step for
    // opaque keywords, mirroring the staged parser's single-step protocol. ──

    val run = Symbol.newVal(owner, "run", TypeRepr.of[Boolean], Flags.Mutable, Symbol.noSymbol)
    val word = Symbol.newVal(owner, "word", TypeRepr.of[Long], Flags.EmptyFlags, Symbol.noSymbol)
    val found = Symbol.newVal(owner, "found", TypeRepr.of[Int], Flags.EmptyFlags, Symbol.noSymbol)
    val wordRef = Ref(word).asExprOf[Long]

    def packedChain(index: Int): Term =
      if index == arity then Literal(IntConstant(-1))
      else packedKeywords(index) match
        case None => packedChain(index + 1)

        case Some(packed) =>
          If
            ( '{ $wordRef == ${Expr(packed)} }.asTerm,
              Literal(IntConstant(index)),
              packedChain(index + 1) )

    val textStep: Term =
      val name =
        Symbol.newVal(owner, "name", TypeRepr.of[String], Flags.EmptyFlags, Symbol.noSymbol)

      val nameRef = Ref(name).asExprOf[String]

      def stringChain(index: Int): Term =
        if index == arity then Literal(IntConstant(-1))
        else
          If
            ( '{ $nameRef == ${Expr(wireNames(index))} }.asTerm,
              Literal(IntConstant(index)),
              stringChain(index + 1) )

      Block
        ( List(ValDef(name, Some('{ $reader.keywordText.s }.asTerm))),
          stringChain(0) )

    val resolveStep: Term =
      If('{ $wordRef == TelReader.KeywordOpaque }.asTerm, textStep, packedChain(0))

    val step: Term =
      Block
        ( List(ValDef(word, Some('{ $reader.keywordWord($indent) }.asTerm))),
          If
            ( '{ $wordRef == TelReader.KeywordEnd }.asTerm,
              Assign(Ref(run), Literal(BooleanConstant(false))),
              Block
                ( List(ValDef(found, Some(resolveStep))),
                  Match(Ref(found), arms :+ fallthrough) ) ) )

    val loop: List[Statement] =
      List(ValDef(run, Some(Literal(BooleanConstant(true)))), While(Ref(run), step))

    // ── Fields whose keywords never arrived — and gathered fields, whose
    // collection is always built from the occurrences (zero occurrences
    // build the empty collection; a repeatable field never consults the
    // declared default), exactly as the derived engine does. ──

    val absents: List[Term] = List.range(0, arity).map: index =>
      fieldTypes(index).asType match
        case '[fieldType] =>
          def resolveAbsent(onAbsent: Expr[fieldType]): Term =
            Assign
              ( Ref(slots(index)),
                '{
                  val declared = wisteria.internal.default[product, fieldType](${Expr(index)})

                  if !declared.absent then declared.asInstanceOf[fieldType]
                  else Tel.Parsable.focusing($foci, ${keyText(index)})($onAbsent)
                }.asTerm )

          def whenUnseen(onAbsent: Expr[fieldType]): Term =
            If
              ( '{ !${Ref(seens(index)).asExprOf[Boolean]} }.asTerm,
                resolveAbsent(onAbsent), unit )

          plans(index) match
            case Plan.Leaf(instance0) =>
              val instance = instance0.asInstanceOf[Inlinable { type Self = fieldType }]
              whenUnseen(instance.absent(tactic))

            case Plan.Nested(instance0) =>
              val instance = instance0.asInstanceOf[Inlinable { type Self = fieldType }]
              whenUnseen(instance.absent(tactic))

            case Plan.Gather(_) =>
              val gather = gathers(index).get

              fieldTypes(index) match
                case AppliedType(_, arguments) =>
                  arguments.last.asType match
                    case '[element] =>
                      val builderRef =
                        Ref(gather.builder).asExprOf[scm.Builder[element, fieldType]]

                      Assign(Ref(slots(index)), '{ $builderRef.result() }.asTerm)

                case _ =>
                  report.errorAndAbort("stratiform: unreachable gather shape")

            case Plan.Seam =>
              val seam = seams(index).get
              val instanceRef = Ref(seam.instance).asExprOf[fieldType is Tel.Parsing]
              val bufferRef = Ref(seam.buffer).asExprOf[scm.ListBuffer[Any] | Null]

              val gatherFinish: Term =
                Assign
                  ( Ref(slots(index)),
                    '{
                      Tel.Parsable.focusing($foci, ${keyText(index)}):
                        Tel.Parsable.gathered[fieldType]
                          ( $instanceRef,
                            $bufferRef match
                              case null   => Nil
                              case buffer => buffer.toList )
                    }.asTerm )

              If
                ( Ref(seam.repeats),
                  gatherFinish,
                  whenUnseen('{ $instanceRef.absent()(using $tactic) }) )

    val construct: Term =
      Apply(Select(New(Inferred(tpe)), ctor), slots.map { slot => Ref(slot) })

    val gatherDefs: List[Statement] = gathers.flatMap(_.toList).flatMap(_.defs)
    val seamDefs: List[Statement] = seams.flatMap(_.toList).flatMap(_.defs)
    val nestedDefs: List[Statement] = nesteds.flatMap(_.toList).map(_(1))

    Block
      ( slotDefs ::: seenDefs ::: gatherDefs ::: seamDefs ::: nestedDefs ::: loop ::: absents,
        construct )
    . asExprOf[product]

  // ── The entry macro ────────────────────────────────────────────────────
  def inlinableParsable[value: Type](using Quotes): Expr[value is Tel.Parsable] =
    import quotes.reflect.*

    val cache: Cache = scm.HashMap()

    if !productSupported(TypeRepr.of[value].dealias) then
      report.errorAndAbort
        (s"stratiform: ${TypeRepr.of[value].show} is not an inlinable product (a non-generic, "+
          "top-level or object-nested case class with a single parameter list and no `@name` "+
          "renames); use `Tel.Parsable.staged` or `derived`")

    '{
      // Sealed per the codec-thunk pattern, like the staged instances: the
      // generated body resolves its capabilities where it is spliced.
      caps.unsafe.unsafeAssumePure:
        new Tel.Parsable:
          type Self = value
          def shape(): Morphology = Morphology.Any

          def parse(reader: TelReader, indent: Int): value =
            reader.finishLine()
            val indent1 = indent + 1
            ${ productFields[value]('reader, 'indent1, cache) }

          override def parse(reader: TelReader): value =
            ${ productFields[value]('reader, '{0}, cache) }
    }
