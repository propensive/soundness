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
import distillate.*
import gossamer.*
import prepositional.*
import vacuous.*

// The machinery behind `BintelInlinable`: expansion-time instance
// resolution (through an in-macro staging compiler), the structural
// generators for structs, repeatable fields and top-level sums, and the
// `BintelInlinable.parsable` entry macro.
//
// BinTEL has no per-subtree bridge back to the AST path (`Bintel.decode` is
// schema-driven top-down), so there is no runtime seam: a shape the
// generator does not support is rejected at expansion with a message
// directing to `Bintel.read`. The exceptions are custom *scalar* leaves,
// which decode through their `Decodable in Text` — exactly the text
// format's blanket case for the atom the wire carries.
object bintelInternal:

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
  // on an incremental build), so instance evaluation must refuse it.
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

  private def summonViaStaging[field: Type](using Quotes): Option[BintelInlinable] =
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
              ( r2.TypeRepr.of[BintelInlinable], "Self",
                r2.TypeBounds(rebuild(shape), rebuild(shape)) )

          target.asType match
            case '[target] => '{ scala.compiletime.summonInline[target] }

        val duration = (System.nanoTime - started)/1000000L

        result match
          case instance: BintelInlinable =>
            report.info
              ( s"stratiform: staged summon for ${TypeRepr.of[field].show} took "+
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
  // BinTEL derivation gives every case-class field one flat slot in
  // declaration order, so a field's wire keyword index is its position.
  // Every field compiles to one of: a builtin `Leaf`; a `Nested` struct; an
  // optional wrapper of either; a `Gather` (a repeatable field, appending
  // per occurrence); or a `SeamText` custom scalar (decoded through its
  // `Decodable in Text`, the wire atom's text semantics). Anything else is
  // rejected — the AST path is the fallback.

  private final class Cache:
    val instances: scm.HashMap[String, Option[BintelInlinable]] = scm.HashMap()
    val active: scm.Set[String] = scm.Set()

  private inline val KInt = 0
  private inline val KLong = 1
  private inline val KBoolean = 2
  private inline val KDouble = 3
  private inline val KText = 4
  private inline val KString = 5

  private def builtinKind(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[Int] =
    import quotes.reflect.*

    if tpe =:= TypeRepr.of[Int] then Some(KInt)
    else if tpe =:= TypeRepr.of[Long] then Some(KLong)
    else if tpe =:= TypeRepr.of[Boolean] then Some(KBoolean)
    else if tpe =:= TypeRepr.of[Double] then Some(KDouble)
    else if tpe =:= TypeRepr.of[Text] then Some(KText)
    else if tpe =:= TypeRepr.of[String] then Some(KString)
    else None

  private enum Elem:
    case Scalar(kind: Int)
    case Nested(instance: BintelInlinable, tpe: Any)
    case SeamText(decoder: Any)

  private enum Plan:
    case Leaf(kind: Int)
    case Nested(instance: BintelInlinable)
    case OptionalLeaf(kind: Int)
    case OptionalNested(instance: BintelInlinable, innerType: Any)
    case Gather(element: Elem, elementType: Any)
    case SeamText(decoder: Any)

  private def resolve[field: Type](cache: Cache)(using Quotes): Option[BintelInlinable] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[field].dealias

    if builtinKind(tpe).isDefined then None else
      cache.instances.getOrElseUpdate(tpe.show, summonViaStaging[field])
      . orElse:
          if productSupported(tpe) && !cache.active.contains(tpe.show)
          then Some(BintelInlinable.ProductInlinable[field]())
          else None

  // A custom scalar leaf: its `Decodable in Text`, resolved at expansion
  // time with a reflection-level search and an erasing cast across the
  // capability boundary (the protobuf port's lesson: neither `summonInline`
  // nor evidence parameters resolve capability-typed codecs from inside the
  // generated parser's inline context).
  private def textDecoder[fieldType: Type](using Quotes): Option[Expr[Any]] =
    import quotes.reflect.*

    Implicits.search(TypeRepr.of[fieldType is Decodable in Text]) match
      case success: ImplicitSearchSuccess => Some(success.tree.asExpr)
      case _                              => None

  private def optionalInner(using Quotes)(tpe: quotes.reflect.TypeRepr)
  :   Option[quotes.reflect.TypeRepr] =

    import quotes.reflect.*

    val unset = TypeRepr.of[vacuous.Optional.Unset]

    tpe.dealias match
      case OrType(left, right) if right =:= unset => Some(left.dealias)
      case OrType(left, right) if left =:= unset  => Some(right.dealias)
      case _                                      => None

  private def planFor[product: Type](using Quotes)
    (fieldName: String, tpe0: quotes.reflect.TypeRepr, cache: Cache)
  :   Plan =

    import quotes.reflect.*

    val tpe = tpe0.dealias

    def reject(shape: String): Nothing =
      report.errorAndAbort
        (s"stratiform: the field `$fieldName` of ${TypeRepr.of[product].show} is $shape, "+
          "which the BinTEL parser generator does not support; use `Bintel.read`")

    builtinKind(tpe) match
      case Some(kind) =>
        Plan.Leaf(kind)

      case None if optionalInner(tpe).isDefined =>
        val inner = optionalInner(tpe).get

        builtinKind(inner) match
          case Some(kind) =>
            Plan.OptionalLeaf(kind)

          case None =>
            if inner <:< TypeRepr.of[Iterable[Any]] then reject("an optional collection")

            inner.asType match
              case '[innerType] =>
                resolve[innerType](cache) match
                  case Some(instance)
                      if !instance.isInstanceOf[BintelInlinable.IterableInlinable[?]] =>
                    Plan.OptionalNested(instance, inner)

                  case _ =>
                    textDecoder[innerType] match
                      case Some(_) => reject("an optional custom scalar")
                      case None    => reject("an optional of an unsupported type")

      case None =>
        val isMap = tpe.derivesFrom(Symbol.requiredClass("scala.collection.Map"))

        if isMap then reject("a Map (a nested `entries` struct on the wire)")
        else if tpe <:< TypeRepr.of[Iterable[Any]] then tpe match
          case AppliedType(_, arguments) =>
            val elementType = arguments.last.dealias

            builtinKind(elementType) match
              case Some(kind) =>
                Plan.Gather(Elem.Scalar(kind), elementType)

              case None =>
                elementType.asType match
                  case '[element] =>
                    resolve[element](cache) match
                      case Some(instance)
                          if !instance.isInstanceOf[BintelInlinable.IterableInlinable[?]] =>
                        Plan.Gather(Elem.Nested(instance, elementType), elementType)

                      case _ =>
                        textDecoder[element] match
                          case Some(decoder) =>
                            Plan.Gather(Elem.SeamText(decoder), elementType)

                          case None =>
                            reject("a collection of an unsupported element")

          case _ =>
            reject("an unapplied collection type")
        else if sumVariants(tpe).isDefined then
          reject("a sum (nested sums derive to an unresolvable schema Reference)")
        else tpe.asType match
          case '[field] =>
            resolve[field](cache) match
              case Some(instance)
                  if !instance.isInstanceOf[BintelInlinable.IterableInlinable[?]] =>
                Plan.Nested(instance)

              case _ =>
                textDecoder[field] match
                  case Some(decoder) => Plan.SeamText(decoder)

                  case None =>
                    if cache.active.contains(tpe.show) then reject("recursive")
                    else reject("of an unsupported type")

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

  private def productSupported(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean =
    import quotes.reflect.*

    tpe.classSymbol.exists: classSymbol =>
      classSymbol.flags.is(Flags.Case)
      && !classSymbol.owner.isTerm
      && (tpe match { case AppliedType(_, _) => false case _ => true })
      && classSymbol.primaryConstructor.paramSymss
         . filterNot(_.exists(_.isTypeParam)).length == 1
      && !hasRenames(classSymbol)

  // `@name` renames change the wire *keyword*, which positional index
  // dispatch never reads — but they mark intent the generator does not
  // otherwise honor for foci paths; renamed records stay on `Bintel.read`.
  private def hasRenames(using Quotes)(classSymbol: quotes.reflect.Symbol): Boolean =
    import quotes.reflect.*

    val annotated =
      classSymbol.primaryConstructor.paramSymss.flatten.filterNot(_.isTypeParam)
        . flatMap(_.annotations)
      ++ classSymbol.caseFields.flatMap(_.annotations)

    annotated.exists { annotation => annotation.tpe <:< TypeRepr.of[adversaria.name[?]] }

  // ── The product generator ──────────────────────────────────────────────
  // One struct body: the child count, then per child a keyword index — the
  // field's declaration position — and the member-typed payload. First
  // occurrence wins for non-repeatable fields (later occurrences are
  // consumed and discarded, exactly as the AST decoder parses every child);
  // repeatable fields gather every occurrence in stream order. Missing
  // fields take their declared default, then the text format's absent
  // semantics, with the same foci bookkeeping as the presented-`Tel`
  // decode.
  private[stratiform] def productBody[product: Type](reader: Expr[BintelReader])(using Quotes)
  :   Expr[product] =

    productBody[product](reader, Cache())

  private def productBody[product: Type](reader: Expr[BintelReader], cache: Cache)
    (using Quotes)
  :   Expr[product] =

    import quotes.reflect.*

    val tpe = TypeRepr.of[product].dealias
    cache.active += tpe.show

    try productBody0[product](reader, cache) finally cache.active -= tpe.show

  private def productBody0[product: Type](reader: Expr[BintelReader], cache: Cache)
    (using Quotes)
  :   Expr[product] =

    import quotes.reflect.*

    val tpe = TypeRepr.of[product].dealias

    if !productSupported(tpe) then
      report.errorAndAbort
        (s"stratiform: ${tpe.show} is not an inlinable BinTEL struct (a non-generic, "+
          "top-level or object-nested case class with a single parameter list and no "+
          "`@name` renames); use `Bintel.read`")

    val classSymbol = tpe.classSymbol.get
    val ctor = classSymbol.primaryConstructor
    val fields = classSymbol.caseFields
    val arity = fields.length
    val fieldNames: List[String] = fields.map(_.name)
    val fieldTypes: List[TypeRepr] = fields.map { field => tpe.memberType(field).dealias }

    val plans: List[Plan] =
      List.range(0, arity).map { i => planFor[product](fieldNames(i), fieldTypes(i), cache) }

    // The wire keyword of each field, for foci paths — kebab-cased as the
    // schema derivation names it.
    val keywords: List[String] =
      fieldNames.map { name => Tel.camelToKebab(name).s }

    def body
      ( tactic:  Expr[Tactic[TelError]],
        foci:    Expr[Foci[Tel.Focus]],
        focused: Expr[Boolean],
        parser:  Expr[BintelParser],
        btactic: Expr[Tactic[BintelError]] )
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
        else if fieldType =:= TypeRepr.of[Boolean] then Literal(BooleanConstant(false))
        else fieldType.asType match
          case '[fieldType] => '{ null.asInstanceOf[fieldType] }.asTerm

      val slotDefs = List.range(0, arity).map: index =>
        ValDef(slots(index), Some(zero(fieldTypes(index))))

      val seenDefs = List.range(0, arity).map: index =>
        ValDef(seens(index), Some(Literal(BooleanConstant(false))))

      val unit = Literal(UnitConstant())

      // One scalar-payload read per builtin kind, with the text format's
      // fault semantics.
      def leafRead(kind: Int): Expr[Any] = kind match
        case KInt =>
          '{
            val atom = Text($parser.directScalar()(using $btactic))

            try atom.s.toInt catch case _: NumberFormatException =>
              raise(TelError(TelError.Reason.NotScalar(atom, t"Int")))(using $tactic)
              0
          }

        case KLong =>
          '{
            val atom = Text($parser.directScalar()(using $btactic))

            try atom.s.toLong catch case _: NumberFormatException =>
              raise(TelError(TelError.Reason.NotScalar(atom, t"Long")))(using $tactic)
              0L
          }

        case KBoolean =>
          '{
            val atom = Text($parser.directScalar()(using $btactic))

            atom.s match
              case "true"  => true
              case "false" => false

              case _ =>
                raise(TelError(TelError.Reason.NotScalar(atom, t"Boolean")))(using $tactic)
                false
          }

        case KDouble =>
          '{
            val atom = Text($parser.directScalar()(using $btactic))

            try atom.s.toDouble catch case _: NumberFormatException =>
              raise(TelError(TelError.Reason.NotScalar(atom, t"Double")))(using $tactic)
              0.0
          }

        case KText   => '{ Text($parser.directScalar()(using $btactic)) }
        case KString => '{ $parser.directScalar()(using $btactic) }

      def leafSentinel(kind: Int): Expr[Any] = kind match
        case KInt     => Expr(0)
        case KLong    => Expr(0L)
        case KBoolean => Expr(false)
        case KDouble  => Expr(0.0)
        case KText    => '{ t"" }
        case KString  => Expr("")

      def seamRead(decoder: Any): Expr[Any] =
        val found = decoder.asInstanceOf[Expr[Any]]
        '{ $found.asInstanceOf[Any is Decodable in Text]
             . decoded(Text($parser.directScalar()(using $btactic))) }

      // Per-field gathering state for repeatable fields.
      val builders: Map[Int, Symbol] =
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

      // One local def per field, shaped for the JIT, wrapped in the same
      // per-field foci bookkeeping as the presented-`Tel` decode.
      val readDefs: List[Symbol] = List.range(0, arity).map: index =>
        val resultType = plans(index) match
          case Plan.Gather(_, _) => TypeRepr.of[Unit]
          case _                 => fieldTypes(index)

        Symbol.newMethod
          (owner, "readField"+index, MethodType(Nil)(_ => Nil, _ => resultType))

      val readDefDefs: List[Statement] = List.range(0, arity).map: index =>
        val keyword: Expr[Text] = '{ ${Expr(keywords(index))}.tt }

        def focusedOver[result: Type](raw: Expr[result]): Expr[result] =
          '{
            if $focused then Tel.Parsable.focusing($foci, $keyword)($raw)
            else $raw
          }

        val rhs: Term = plans(index) match
          case Plan.Leaf(kind) =>
            fieldTypes(index).asType match
              case '[fieldType] =>
                focusedOver[fieldType](leafRead(kind).asExprOf[fieldType]).asTerm

          case Plan.OptionalLeaf(kind) =>
            fieldTypes(index).asType match
              case '[fieldType] =>
                focusedOver[fieldType](leafRead(kind).asExprOf[fieldType]).asTerm

          case Plan.Nested(instance) =>
            fieldTypes(index).asType match
              case '[fieldType] =>
                val raw =
                  instance.asInstanceOf[BintelInlinable { type Self = fieldType }]
                  . parse(reader)

                focusedOver[fieldType](raw).asTerm

          case Plan.OptionalNested(instance, innerType0) =>
            (innerType0.asInstanceOf[TypeRepr].asType, fieldTypes(index).asType) match
              case ('[innerType], '[fieldType]) =>
                val raw =
                  instance.asInstanceOf[BintelInlinable { type Self = innerType }]
                  . parse(reader)

                focusedOver[fieldType]('{ $raw.asInstanceOf[fieldType] }).asTerm

          case Plan.SeamText(decoder) =>
            fieldTypes(index).asType match
              case '[fieldType] =>
                focusedOver[fieldType]
                  ('{ ${seamRead(decoder)}.asInstanceOf[fieldType] }).asTerm

          case Plan.Gather(element, elementType0) =>
            val elementType = elementType0.asInstanceOf[TypeRepr]

            (elementType.asType, fieldTypes(index).asType) match
              case ('[element], '[fieldType]) =>
                val builder =
                  Ref(builders(index)).asExprOf[scm.Builder[element, fieldType]]

                val occurrence: Expr[element] = element match
                  case Elem.Scalar(kind) =>
                    leafRead(kind).asExprOf[element]

                  case Elem.Nested(instance0, _) =>
                    instance0.asInstanceOf[BintelInlinable { type Self = element }]
                    . parse(reader)

                  case Elem.SeamText(decoder) =>
                    '{ ${seamRead(decoder)}.asInstanceOf[element] }

                '{ $builder += ${ focusedOver[element](occurrence) } }.asTerm

        DefDef(readDefs(index), _ => Some(rhs.changeOwner(readDefs(index))))

      def arms: List[CaseDef] = List.range(0, arity).map: index =>
        val body: Term = plans(index) match
          case Plan.Gather(_, _) =>
            Apply(Ref(readDefs(index)), Nil)

          case Plan.Leaf(_) =>
            // A later occurrence of a scalar field is skipped structurally,
            // as the AST decoder consumes it without a typed parse.
            If
              ( '{ !${Ref(seens(index)).asExprOf[Boolean]} }.asTerm,
                Block
                  ( List
                      ( Assign(Ref(slots(index)), Apply(Ref(readDefs(index)), Nil)),
                        Assign(Ref(seens(index)), Literal(BooleanConstant(true))) ),
                    unit ),
                '{ $parser.directSkipScalar()(using $btactic) }.asTerm )

          case _ =>
            // A later occurrence of any other field is consumed and
            // discarded — the AST decoder parses every child regardless.
            If
              ( '{ !${Ref(seens(index)).asExprOf[Boolean]} }.asTerm,
                Block
                  ( List
                      ( Assign(Ref(slots(index)), Apply(Ref(readDefs(index)), Nil)),
                        Assign(Ref(seens(index)), Literal(BooleanConstant(true))) ),
                    unit ),
                Block(List(Apply(Ref(readDefs(index)), Nil)), unit) )

        CaseDef(Literal(IntConstant(index)), None, body)

      def badIndex =
        CaseDef
          ( Wildcard(), None,
            '{ abort(BintelError(BintelError.Reason.BadKeywordIndex))(using $btactic) }.asTerm )

      val total = Symbol.newVal(owner, "total", TypeRepr.of[Int], Flags.EmptyFlags, Symbol.noSymbol)
      val i = Symbol.newVal(owner, "i", TypeRepr.of[Int], Flags.Mutable, Symbol.noSymbol)
      val totalRef = Ref(total).asExprOf[Int]
      val iRef = Ref(i).asExprOf[Int]

      val step: Term =
        val kidx = Symbol.newVal(owner, "kidx", TypeRepr.of[Int], Flags.EmptyFlags, Symbol.noSymbol)

        Block
          ( List
              ( ValDef(kidx, Some('{ $parser.directCount()(using $btactic) }.asTerm)),
                Match(Ref(kidx), arms ::: List(badIndex)) ),
            Assign(Ref(i), '{ $iRef + 1 }.asTerm) )

      val loop: List[Statement] =
        List
          ( ValDef(total, Some('{ $parser.directCount()(using $btactic) }.asTerm)),
            ValDef(i, Some(Literal(IntConstant(0)))),
            While('{ $iRef < $totalRef }.asTerm, step) )

      // Missing fields: declared default, then the text format's absent
      // semantics under the field's focus.
      val absents: List[Term] = List.range(0, arity).map: index =>
        val keyword: Expr[Text] = '{ ${Expr(keywords(index))}.tt }

        fieldTypes(index).asType match
          case '[fieldType] =>
            val absentExpr: Expr[fieldType] = plans(index) match
              case Plan.Leaf(kind) =>
                val sentinel = leafSentinel(kind).asExprOf[fieldType]
                '{ Tel.Parsable.missing[fieldType]($sentinel)(using $tactic) }

              case Plan.OptionalLeaf(_) | Plan.OptionalNested(_, _) =>
                '{ vacuous.Unset.asInstanceOf[fieldType] }

              case Plan.Nested(instance) =>
                instance.asInstanceOf[BintelInlinable { type Self = fieldType }]
                . absent(tactic)

              case Plan.SeamText(_) =>
                '{ abort(TelError(TelError.Reason.Absent))(using $tactic) }

              case Plan.Gather(_, _) =>
                '{ ${ Ref(builders(index)).asExpr }
                     . asInstanceOf[scm.Builder[?, fieldType]].result() }

            val resolveAbsent: Term =
              Assign
                ( Ref(slots(index)),
                  '{
                    val declared =
                      wisteria.internal.default[product, fieldType](${Expr(index)})

                    if !declared.absent then declared.asInstanceOf[fieldType]
                    else Tel.Parsable.focusing($foci, $keyword)($absentExpr)
                  }.asTerm )

            val whenSeen: Term = plans(index) match
              case Plan.Gather(_, _) =>
                Assign
                  ( Ref(slots(index)),
                    '{ ${ Ref(builders(index)).asExpr }
                         . asInstanceOf[scm.Builder[?, fieldType]].result() }.asTerm )

              case _ =>
                unit

            If('{ !${Ref(seens(index)).asExprOf[Boolean]} }.asTerm, resolveAbsent, whenSeen)

      val construct: Term =
        Apply(Select(New(Inferred(tpe)), ctor), slots.map { slot => Ref(slot) })

      Block(slotDefs ::: seenDefs ::: builderDefs ::: readDefDefs ::: loop ::: absents, construct)
      . asExprOf[product]

    '{
      val tactic = infer[Tactic[TelError]]
      val foci = infer[Foci[Tel.Focus]]
      val focused = foci.active
      val parser = $reader.rawParser.asInstanceOf[BintelParser]
      val btactic = $reader.rawTactic.asInstanceOf[Tactic[BintelError]]
      ${ body('tactic, 'foci, 'focused, 'parser, 'btactic) }
    }

  // ── The sum generator ──────────────────────────────────────────────────
  // A top-level sum's schema root is a struct with one `SelectRef` member:
  // one flat slot per variant, in declaration order, so the first child's
  // keyword index chooses the variant directly. Extra children are
  // consumed and discarded, as the AST decoder parses every child.
  private[stratiform] def sumBody[sum: Type](reader: Expr[BintelReader])(using Quotes)
  :   Expr[sum] =

    sumBody[sum](reader, Cache())

  private def sumBody[sum: Type](reader: Expr[BintelReader], cache: Cache)(using Quotes)
  :   Expr[sum] =

    import quotes.reflect.*

    cache.active += TypeRepr.of[sum].dealias.show

    try sumBody0[sum](reader, cache)
    finally cache.active -= TypeRepr.of[sum].dealias.show

  private def sumBody0[sum: Type](reader: Expr[BintelReader], cache: Cache)(using Quotes)
  :   Expr[sum] =

    import quotes.reflect.*

    val variants = sumVariants(TypeRepr.of[sum].dealias).getOrElse:
      report.errorAndAbort
        (s"stratiform: ${TypeRepr.of[sum].show} is not an inlinable BinTEL sum (a "+
          "non-generic sealed type whose variants are all case classes); use `Bintel.read`")

    val arity = variants.length

    def variantInstance(index: Int): (Type[?], BintelInlinable) =
      variants(index)(1).asType match
        case '[type variantType <: sum; variantType] =>
          val instance = resolve[variantType](cache).getOrElse:
            report.errorAndAbort
              (s"stratiform: no BintelInlinable for variant ${variants(index)(0)}")

          (Type.of[variantType], instance)

    def dispatch(index: Int, kidx: Expr[Int], btactic: Expr[Tactic[BintelError]]): Expr[sum] =
      if index == arity then
        '{ abort(BintelError(BintelError.Reason.BadKeywordIndex))(using $btactic) }
      else variants(index)(1).asType match
        case '[type variantType <: sum; variantType] =>
          val instance = resolve[variantType](cache).getOrElse:
            report.errorAndAbort
              (s"stratiform: no BintelInlinable for variant ${variants(index)(0)}")
          . asInstanceOf[BintelInlinable { type Self = variantType }]

          '{
            if $kidx == ${Expr(index)} then
              def parseVariant(): variantType = ${ instance.parse(reader) }
              parseVariant()
            else ${ dispatch(index + 1, kidx, btactic) }
          }

    def discard(index: Int, kidx: Expr[Int], btactic: Expr[Tactic[BintelError]]): Expr[Unit] =
      if index == arity then
        '{ abort(BintelError(BintelError.Reason.BadKeywordIndex))(using $btactic) }
      else variants(index)(1).asType match
        case '[type variantType <: sum; variantType] =>
          val instance = resolve[variantType](cache).getOrElse:
            report.errorAndAbort
              (s"stratiform: no BintelInlinable for variant ${variants(index)(0)}")
          . asInstanceOf[BintelInlinable { type Self = variantType }]

          '{
            if $kidx == ${Expr(index)} then
              def parseExtra(): variantType = ${ instance.parse(reader) }
              parseExtra()
              ()
            else ${ discard(index + 1, kidx, btactic) }
          }

    '{
      val tactic = infer[Tactic[TelError]]
      val parser = $reader.rawParser.asInstanceOf[BintelParser]
      val btactic = $reader.rawTactic.asInstanceOf[Tactic[BintelError]]
      val total = parser.directCount()(using btactic)

      if total == 0 then abort(TelError(TelError.Reason.Absent))(using tactic)
      else
        val kidx = parser.directCount()(using btactic)
        val result: sum = ${ dispatch(0, 'kidx, 'btactic) }
        var i = 1

        while i < total do
          val extra = parser.directCount()(using btactic)
          ${ discard(0, 'extra, 'btactic) }
          i += 1

        result
    }

  // ── The entry macro ────────────────────────────────────────────────────
  def inlinableParsable[value: Type](using Quotes): Expr[value is Bintel.Parsable] =
    import quotes.reflect.*

    val cache: Cache = Cache()
    val tpe = TypeRepr.of[value].dealias

    val instance: BintelInlinable { type Self = value } =
      if productSupported(tpe) then BintelInlinable.ProductInlinable[value]()
      else if sumVariants(tpe).isDefined then BintelInlinable.SumInlinable[value]()
      else
        summonViaStaging[value].getOrElse:
          report.errorAndAbort
            (s"stratiform: ${tpe.show} is not an inlinable BinTEL struct or sum; use "+
              "`Bintel.read`")
        . asInstanceOf[BintelInlinable { type Self = value }]

    '{
      // Sealed per the codec-thunk pattern: the generated body resolves its
      // capabilities where it is spliced.
      caps.unsafe.unsafeAssumePure:
        new Bintel.Parsable.Direct[value]:
          protected def parseCarrier(reader0: AnyRef): value =
            // A capability class cannot be quoted into a pure hole, so
            // every use casts from the neutral carrier afresh.
            ${ instance.parse('{ reader0.asInstanceOf[BintelReader] }) }
    }
