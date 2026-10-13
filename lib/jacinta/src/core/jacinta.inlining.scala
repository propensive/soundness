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
package jacinta

import scala.caps
import scala.collection.immutable.{List, Nil}
import scala.collection.mutable as scm
import scala.quoted.*

import anticipation.*
import contingency.*
import prepositional.*
import vacuous.*
import zephyrine.*

// The machinery behind `Inlinable`: expansion-time instance resolution, the
// structural generators for products and collections, and the entry macros. A
// user-written `Inlinable` can only be evaluated at expansion time by an in-macro
// staging compiler, which `jacinta.staged` contributes as an `Inlinable.Stager`;
// without it, such a field takes the runtime seam.
object inlining:

  // The runtime seam for a field type without an `Inlinable`: a nominal
  // `Parsable` (a staged sum, a custom instance) is called directly,
  // skipping the `Field.Adapter` the fallback chain would otherwise
  // construct per occurrence. An inline method because `summonFrom` may
  // only live in one; it expands where the generated parser is spliced.
  inline def fieldSeam[fieldType](reader: Json.Reader): fieldType =
    scala.compiletime.summonFrom:
      case parsable: (`fieldType` is Json.Parsable) => parsable.parse(reader)

      case _ =>
        scala.compiletime.summonInline[fieldType is Json.Field].parse(reader)

  // ── The resolution ladder ──────────────────────────────────────────────
  // builtin → staged summon → structural collection/product → (caller's)
  // runtime seam. The cache spans one entry expansion, so a type's staging
  // summon runs at most once however often it recurs in the graph.

  // The per-entry expansion cache: memoized staging summons, and the set of
  // types whose generators are currently on the expansion stack — a
  // recursive occurrence (`Tree` with `children: List[Tree]`) must degrade
  // to the runtime seam rather than expand forever.
  private class Cache:
    val instances: scm.HashMap[String, Option[Inlinable]] = scm.HashMap()
    val active: scm.Set[String] = scm.Set()
    val roots: scm.HashMap[String, Expr[Any]] = scm.HashMap()

  // A cache for generating a printer: its runtime tier resolves printers, not parsers.
  private final class PrintingCache extends Cache

  // The cache of the expansion in progress. A structural instance reached through
  // `Inlinable.parse` (or `write`) carries no cache, so it continues this one rather than
  // starting afresh: the active set then spans the whole expansion, and a mutually recursive
  // record ties back to a generator already on the stack instead of expanding forever.
  private val ongoing: scala.util.DynamicVariable[Option[Cache]] =
    scala.util.DynamicVariable(None)

  private def continued(): Cache = ongoing.value.getOrElse(Cache())

  private def within[result](cache: Cache)(generate: => result): result =
    ongoing.withValue(Some(cache))(generate)

  // A recursive occurrence of the root type reads (or writes) through the instance being
  // generated.
  private def rootFor[fieldType: Type](using Quotes): Option[Expr[Any]] =
    import quotes.reflect.*

    val name = TypeRepr.of[fieldType].dealias.show

    ongoing.value.flatMap(_.roots.get(name))

  // Whether an implicit-search result is the `import codecs.direct` fallback given, which a
  // nominal lookup must not take: it would expand this very generator again for the field.
  private def blanket(using Quotes)(tree: quotes.reflect.Tree): Boolean =
    import quotes.reflect.*

    tree match
      case Inlined(_, _, body)    => blanket(body)
      case Apply(function, _)     => blanket(function)
      case TypeApply(function, _) => blanket(function)
      case Block(_, expression)   => blanket(expression)

      case other =>
        val symbol = other.symbol

        val owners =
          List(TypeRepr.of[Json.Parsable2].typeSymbol, TypeRepr.of[Json.Printable2].typeSymbol)

        symbol.name == "generated" && owners.contains(symbol.owner)

  // A type with any `Form` refinement removed: a read of `Foo in Json` infers the instance's
  // type as `Foo { type Form = Json }`, whose codec is `Foo`'s.
  private def formless(using Quotes)(tpe: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    tpe.dealias match
      case Refinement(parent, "Form", _) => formless(parent)
      case other                         => other

  // A record that encodes as text: the derived encoding writes it as a JSON string, ahead of
  // its structure, so it is never printed structurally.
  private def textual(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean =
    import quotes.reflect.*

    tpe.asType.absolve match
      case '[value] =>
        Implicits.search(TypeRepr.of[value is anticipation.Encodable in Text]) match
          case _: ImplicitSearchSuccess => true
          case _                        => false

  private def resolve[field: Type](cache: Cache)(using Quotes): Option[Inlinable] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[field].dealias

    builtinFor(tpe).orElse:
      cache.instances.getOrElseUpdate(tpe.show, Inlinable.Stager.summon[field].map(unwrap))
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
        case success: ImplicitSearchSuccess if !blanket(success.tree) => Some(success.tree.asExpr)
        case _                                                        => None

    ongoing.value match
      case Some(_: PrintingCache) =>
        rootFor[field].orElse(searched(TypeRepr.of[field is Json.Printable]))
        . map(NominalPrinter[field](_))
        . orElse:
            searched(TypeRepr.of[field is anticipation.Encodable in Json])
            . map(EncodedPrinter[field](_))

      case _ =>
        rootFor[field].orElse(searched(TypeRepr.of[field is Json.Parsable]))
        . map(RuntimeInlinable[field](_))
        . orElse(searched(TypeRepr.of[field is Json.Field]).map(RuntimeInlinable[field](_)))

  // The printing counterparts of the runtime tiers: a nominal `Printable` (the recursion tie,
  // or a hand-written one), called in place, and the `Encodable` seam, whose tree is written
  // in place (an element encoding to an absent value is written as the derivation writes it;
  // a record field is handled by `productWrite`, which leaves it out).
  private final class NominalPrinter[value](printable: Expr[Any]) extends Inlinable:
    type Self = value

    def parse(reader: Expr[Json.Reader])(using Quotes, Type[value]): Expr[value] =
      quotes.reflect.report.errorAndAbort("jacinta: a printer cannot parse")

    override def prints: Boolean = true

    override def write(value: Expr[value], out: Expr[AnyRef])(using Quotes, Type[value])
    :   Expr[Unit] =

      '{Json.Printable.writeField[value]($printable.asInstanceOf[AnyRef], $value, $out)}

  private final class EncodedPrinter[value](val encodable: Expr[Any]) extends Inlinable:
    type Self = value

    def parse(reader: Expr[Json.Reader])(using Quotes, Type[value]): Expr[value] =
      quotes.reflect.report.errorAndAbort("jacinta: a printer cannot parse")

    override def prints: Boolean = true

    override def write(value: Expr[value], out: Expr[AnyRef])(using Quotes, Type[value])
    :   Expr[Unit] =

      ' {
          val typed = $encodable.asInstanceOf[value is anticipation.Encodable in Json]
          Json.Printable.writeAst($out, Json.unseal(typed.encoded($value)))
        }

  private final class RuntimeInlinable[value](parsing: Expr[Any]) extends Inlinable:
    type Self = value

    def parse(reader: Expr[Json.Reader])(using Quotes, Type[value]): Expr[value] =
      ' {
          Json.Parsable.parseField[value]
            ( $parsing.asInstanceOf[AnyRef], $reader.asInstanceOf[AnyRef] )
        }

    override def absent(tactic: Expr[Tactic[Json.Error]])(using Quotes, Type[value])
    :   Expr[value] =

      '{Json.Parsable.absentField[value]($parsing.asInstanceOf[AnyRef])(using $tactic)}

  // Composition points become local defs rather than textual inlining: a
  // fully-flattened parser exceeds HotSpot's huge-method bytecode limit and
  // runs interpreted (measured: ~6x slower than the staged parser). A local
  // def keeps each generated method JIT-compilable while the call stays
  // monomorphic and direct. Leaf generators stay inline.
  private def nested[fieldType: Type]
    ( instance: Inlinable { type Self = fieldType }, reader: Expr[Json.Reader] )
    ( using Quotes )
  :   Expr[fieldType] =

    instance match
      case _: Inlinable.ProductInlinable[?] | _: Inlinable.IterableInlinable[?] =>
        ' {
            def parseNested(): fieldType = ${instance.parse(reader)}
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

    // The opaque prelude `List`/`Set`/`Sequence` don't conform to `Iterable`, so recognize
    // them by their type constructors; each is an array exactly like a stdlib collection (the
    // element becomes the recursion point, so `List[Tree]` inside `Tree` degrades `Tree`, not
    // the whole list).
    val isAliasCollection = aliasCollectionUnderlying(tpe).isDefined

    if !isMap && (tpe <:< TypeRepr.of[Iterable[Any]] || isAliasCollection) then tpe match
      case AppliedType(_, arguments) =>
        arguments.last.asType.absolve match
          case '[element] =>
            resolve[element](cache).map: instance =>
              Inlinable.IterableInlinable[element]
                ( instance.asInstanceOf[element is Inlinable] )

      case _ =>
        None
    // A recursive occurrence degrades to the runtime seam, whose
    // derivation is lazy.
    else if cache.active.contains(tpe.show) then None
    else if productSupported(tpe) then Some(Inlinable.ProductInlinable[field]())
    else None

  // The opaque prelude `List`/`Set`/`Sequence` map to their underlying stdlib collection, at
  // which a `Factory` is summoned (their companions expose only a `Conversion`); the result is
  // cast back, a no-op at erasure.
  private def aliasCollectionUnderlying(using Quotes)(tpe: quotes.reflect.TypeRepr)
  :   Option[quotes.reflect.TypeRepr] =

    import quotes.reflect.*

    val listSym     = TypeRepr.of[proscenium.List[Any]].typeSymbol
    val setSym      = TypeRepr.of[proscenium.Set[Any]].typeSymbol
    val sequenceSym = TypeRepr.of[proscenium.Sequence[Any]].typeSymbol

    tpe.dealias match
      case AppliedType(constructor, List(element)) if constructor.typeSymbol == listSym =>
        Some(TypeRepr.of[scala.collection.immutable.List].appliedTo(element))

      case AppliedType(constructor, List(element)) if constructor.typeSymbol == setSym =>
        Some(TypeRepr.of[scala.collection.immutable.Set].appliedTo(element))

      case AppliedType(constructor, List(element)) if constructor.typeSymbol == sequenceSym =>
        Some(TypeRepr.of[scala.collection.immutable.Vector].appliedTo(element))

      case _ =>
        None

  private def productSupported(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean =
    import quotes.reflect.*

    tpe.classSymbol.exists: classSymbol =>
      classSymbol.flags.is(Flags.Case) &&
        !classSymbol.owner.isTerm &&
        (tpe match { case AppliedType(_, _) => false case _ => true }) &&
        classSymbol.primaryConstructor.paramSymss
          . filterNot(_.exists(_.isTypeParam)).length == 1 &&
        !hasRenames(classSymbol)

  // `@name` renames resolve through inline machinery the structural
  // generator does not replicate; annotated records stay on `staged`.
  private def hasRenames(using Quotes)(classSymbol: quotes.reflect.Symbol): Boolean =
    import quotes.reflect.*

    val annotated =
      classSymbol.primaryConstructor.paramSymss.flatten.filterNot(_.isTypeParam)
        . flatMap(_.annotations) ++
        classSymbol.caseFields.flatMap(_.annotations)

    annotated.exists: annotation => annotation.tpe <:< TypeRepr.of[adversaria.name[?]]

  // ── The collection generator ───────────────────────────────────────────
  // Mirrors `Json.Parsable.iterable` exactly: openArray, per-element loop,
  // per-index focusing when the read site's `Foci` is active — with the
  // element's generated code inlined into the loop.
  private[jacinta] def iterableBody[collection: Type]
    ( reader: Expr[Json.Reader], element0: Inlinable )
    ( using Quotes )
  :   Expr[collection] =

    import quotes.reflect.*

    TypeRepr.of[collection].dealias match
      case AppliedType(_, arguments) =>
        val factoryType =
          aliasCollectionUnderlying(TypeRepr.of[collection]).getOrElse(TypeRepr.of[collection])

        (arguments.last.asType, factoryType.asType).absolve match
          case ('[element], '[stdlib]) =>
            val instance = element0.asInstanceOf[Inlinable { type Self = element }]

            ' {
                def parseElement(): element = ${instance.parse(reader)}
                val factory = infer[scala.collection.Factory[element, stdlib]]
                val builder = factory.newBuilder
                val foci = infer[Foci[Json.Focus]]
                val focused = foci.active
                val parser = $reader.rawParser.asInstanceOf[Parser]
                val ptactic = $reader.rawTactic.asInstanceOf[Tactic[Parse.Error]]
                parser.directOpenArray()(using ptactic)
                var index = 0
                var continue = parser.directElementFirst()(using ptactic)

                while continue do
                  builder +=
                    ( if focused then
                        Json.Parsable.focusing(foci, index.toString.tt)(parseElement())
                      else
                        parseElement() )

                  index += 1
                  continue = parser.directElementNext()(using ptactic)

                builder.result().asInstanceOf[collection]
              }

      case _ =>
        report.errorAndAbort
          ( "jacinta: an inlinable collection requires an applied collection type" )

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
  private[jacinta] def productBody[product: Type](reader: Expr[Json.Reader])(using Quotes)
  :   Expr[product] =

    productBody[product](reader, continued())

  private def productBody[product: Type](reader: Expr[Json.Reader], cache: Cache)(using Quotes)
  :   Expr[product] =

    import quotes.reflect.*

    cache.active += TypeRepr.of[product].dealias.show

    try productBody0[product](reader, cache)
    finally cache.active -= TypeRepr.of[product].dealias.show

  private def productBody0[product: Type](reader: Expr[Json.Reader], cache: Cache)(using Quotes)
  :   Expr[product] =

    import quotes.reflect.*

    val tpe = TypeRepr.of[product].dealias

    if !productSupported(tpe) then
      report.errorAndAbort
        ( s"jacinta: ${tpe.show} is not an inlinable product (a non-generic, top-level or " +
          "object-nested case class with a single parameter list and no `@name` renames); " +
          "use `Json.Parsable.staged` or `derived`" )

    val classSymbol = tpe.classSymbol.get
    val ctor = classSymbol.primaryConstructor
    val fields = classSymbol.caseFields
    val arity = fields.length
    val fieldNames: List[String] = fields.map(_.name)
    val fieldTypes: List[TypeRepr] = fields.map: field => tpe.memberType(field).dealias

    def packedName(index: Int): Option[(Long, Long)] =
      val name = fieldNames(index)
      val length = name.length

      val packs = length > 0 && length <= 16 && name.forall: char => char >= ' ' && char < 127

      if !packs then None else
        var low = 0L
        var high = 0L
        var position = 0

        while position < length do
          val byte = name.charAt(position).toLong & 0xFF
          if position < 8 then low |= byte << (position*8) else high |= byte << ((position - 8)*8)
          position += 1

        Some((low, high))

    val packedNames: List[Option[(Long, Long)]] = List.range(0, arity).map(packedName)

    def body
      ( foci:    Expr[Foci[Json.Focus]],
        focused: Expr[Boolean],
        tactic:  Expr[Tactic[Json.Error]],
        parser:  Expr[Parser],
        ptactic: Expr[Tactic[Parse.Error]] )
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
          case '[fieldType] => '{null.asInstanceOf[fieldType]}.asTerm

      val slotDefs = List.range(0, arity).map: index =>
        ValDef(slots(index), Some(zero(fieldTypes(index))))

      val seenDefs = List.range(0, arity).map: index =>
        ValDef(seens(index), Some(Literal(BooleanConstant(false))))

      val unit = Literal(UnitConstant())

      // EXPERIMENT(cc-bypass): builtins read straight off the parser bound
      // once per record, skipping the Json.Reader rim entirely.
      def builtinDirect(tpe: TypeRepr): Option[Expr[Any]] =
        if tpe =:= TypeRepr.of[Int] then Some('{$parser.directLong()(using $ptactic).toInt})
        else if tpe =:= TypeRepr.of[Long] then Some('{$parser.directLong()(using $ptactic)})
        else if tpe =:= TypeRepr.of[Double] then
          Some('{$parser.directDouble()(using $ptactic)})
        else if tpe =:= TypeRepr.of[Boolean] then
          Some('{$parser.directBoolean()(using $ptactic)})
        else if tpe =:= TypeRepr.of[Text] then
          Some('{$parser.directString()(using $ptactic).tt})
        else if tpe =:= TypeRepr.of[String] then
          Some('{$parser.directString()(using $ptactic)})
        else
          None

      // The absent expression per field, through the ladder.
      def fieldAbsent(index: Int): Expr[Any] =
        fieldTypes(index).asType match
          case '[fieldType] =>
            if builtinDirect(fieldTypes(index)).isDefined then
              '{Json.Parsable.missing[fieldType]()(using $tactic)}
            else
              resolve[fieldType](cache) match
                case Some(instance0) =>
                  instance0.asInstanceOf[Inlinable { type Self = fieldType }].absent(tactic)

                case None =>
                  ' {
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
                      '{inlining.fieldSeam[fieldType]($reader)}

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
              ' {
                  Json.Parsable.focusing($foci, ${Expr(fieldNames(index))}.tt)(${hot()})
                }

              . asTerm.changeOwner(slowDefs(index))

            val readRhs: Term =
              '{if $focused then ${call(slowDefs(index))} else ${hot()}}
              . asTerm.changeOwner(readDefs(index))

            nestedDef.map(_(1)).toList :::
              List
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
        CaseDef(Wildcard(), None, '{$parser.directSkipValue()(using $ptactic)}.asTerm)

      val run = Symbol.newVal(owner, "run", TypeRepr.of[Boolean], Flags.Mutable, Symbol.noSymbol)

      def packedChainOver(wordRef: Expr[Long], highRef: Expr[Long])(index: Int): Term =
        if index == arity then Literal(IntConstant(-1))
        else packedNames(index) match
          case None => packedChainOver(wordRef, highRef)(index + 1)

          case Some((low, highWord)) =>
            If
              ( '{$wordRef == ${Expr(low)} && $highRef == ${Expr(highWord)}}.asTerm,
                Literal(IntConstant(index)),
                packedChainOver(wordRef, highRef)(index + 1) )

      def namedChain(name: Expr[String], index: Int): Expr[Int] =
        if index == arity then Expr(-1)
        else
          ' {
              if ${Expr(fieldNames(index))} == $name then ${Expr(index)}
              else ${namedChain(name, index + 1)}
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
          ' {
              val name = Json.Reader.keyNameOf($reader.asInstanceOf[AnyRef])
              if name == null then -2 else ${namedChain('{name.nn}, 0)}
            }

        val resolveStep: Term =
          If
            ( '{$wordRef == Json.Reader.KeyOpaque}.asTerm,
              opaque.asTerm,
              Block
                ( List(ValDef(high, Some('{$parser.directKeyWordHigh}.asTerm))),
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
              ( '{$wordRef == Json.Reader.KeyEnd}.asTerm,
                Assign(Ref(run), Literal(BooleanConstant(false))),
                Block
                  ( List(ValDef(found, Some(resolveStep))),
                    Match(Ref(found), arms ::: List(ended, fallthrough)) ) ) )

      // The first key step is unrolled ahead of the loop, so the steady-state
      // step consults no per-key seen/comma state.
      val loop: List[Statement] =
        readDefDefs :::
          List
            ( '{$parser.directOpenObject()(using $ptactic)}.asTerm,
              ValDef(run, Some(Literal(BooleanConstant(true)))),
              step('{$parser.directKeyWordFirst()}),
              While(Ref(run), step('{$parser.directKeyWordNext()})) )

      val absents: List[Term] = List.range(0, arity).map: index =>
        fieldTypes(index).asType match
          case '[fieldType] =>
            val keyText: Expr[Text] = '{${Expr(fieldNames(index))}.tt}

            val resolveAbsent: Term =
              Assign
                ( Ref(slots(index)),
                  ' {
                      val declared =
                        wisteria.internal.default[product, fieldType](${Expr(index)})

                      if !declared.absent then declared.asInstanceOf[fieldType]
                      else Json.Parsable.focusing($foci, $keyText)
                        ( ${fieldCode(index)(1).asExprOf[fieldType]} )
                    }

                  . asTerm )

            If('{!${Ref(seens(index)).asExprOf[Boolean]}}.asTerm, resolveAbsent, unit)

      val construct: Term =
        Apply(Select(New(Inferred(tpe)), ctor), slots.map { slot => Ref(slot) })

      Block(slotDefs ::: seenDefs ::: loop ::: absents, construct).asExprOf[product]

    ' {
        val foci = infer[Foci[Json.Focus]]
        val focused = foci.active
        val tactic = infer[Tactic[Json.Error]]
        val parser = $reader.rawParser.asInstanceOf[Parser]
        val ptactic = $reader.rawTactic.asInstanceOf[Tactic[Parse.Error]]
        ${body('foci, 'focused, 'tactic, 'parser, 'ptactic)}
      }

  // ── The printing side ──────────────────────────────────────────────────
  // A generated printer writes compact JSON text exactly as the derived encoding renders it: a
  // record is an object with its fields in order, leaving out a field whose encoding is
  // absent, and a collection an array. A field with no printer of its own is encoded through
  // its `Encodable in Json` and its tree written in place.

  // How a record field is printed: through an instance that writes it in place, or through its
  // `Encodable`, encoded first so that an absent field can be left out.
  private enum Printing:
    case Generated(instance: Inlinable)
    case Seam(encodable: Expr[Any])

  private def printingFor[fieldType: Type](cache: Cache)(using Quotes): Printing =
    import quotes.reflect.*

    def seam: Printing =
      Implicits.search(TypeRepr.of[fieldType is anticipation.Encodable in Json]) match
        case success: ImplicitSearchSuccess => Printing.Seam(success.tree.asExpr)

        case _ =>
          val name = TypeRepr.of[fieldType].show
          report.errorAndAbort(s"jacinta: no Printable or Encodable in Json for $name")

    val textEncoded = textual(TypeRepr.of[fieldType])

    resolve[fieldType](cache) match
      case Some(seam: EncodedPrinter[?])                         => Printing.Seam(seam.encodable)
      case Some(_: Inlinable.ProductInlinable[?]) if textEncoded => seam
      case Some(instance) if instance.prints                     => Printing.Generated(instance)
      case _                                                     => seam

  private[jacinta] def productWrite[product: Type](value: Expr[product], out: Expr[AnyRef])
    ( using Quotes )
  :   Expr[Unit] =

    import quotes.reflect.*

    val cache = continued()
    val tpe = TypeRepr.of[product].dealias
    val name = tpe.show
    cache.active += name

    try
      val classSymbol = tpe.classSymbol.get
      val fields = classSymbol.caseFields
      val fieldTypes = fields.map(tpe.memberType(_).dealias)

      val printings: List[Printing] = fieldTypes.map: fieldType =>
        fieldType.asType.absolve match
          case '[fieldType] => printingFor[fieldType](cache)

      def field(index: Int): Term = Select.unique(value.asTerm, fields(index).name)

      // The separator before every field after the first written, and the key.
      def opening(index: Int, written: Expr[Boolean]): Expr[Unit] =
        val key = Expr(fields(index).name)

        ' {
            if $written then Json.Printable.writeAscii($out, ",")
            Json.Printable.writeString($out, $key)
            Json.Printable.writeAscii($out, ":")
          }

      def entries(index: Int, written: Expr[Boolean]): Expr[Unit] =
        if index == fields.length then '{()}
        else fieldTypes(index).asType.absolve match
          case '[fieldType] =>
            val typed = field(index).asExprOf[fieldType]

            printings(index) match
              case Printing.Seam(encodable) =>
                ' {
                    val codec = $encodable.asInstanceOf[fieldType is anticipation.Encodable in Json]
                    val tree = Json.unseal(codec.encoded($typed))

                    if Json.Printable.present(tree) then
                      ${opening(index, written)}
                      Json.Printable.writeAst($out, tree)
                      ${entries(index + 1, '{true})}
                    else
                      ${entries(index + 1, written)}
                  }

              case Printing.Generated(instance) =>
                val typedInstance = instance.asInstanceOf[Inlinable { type Self = fieldType }]

                ' {
                    ${opening(index, written)}
                    ${typedInstance.write(typed, out)}
                    ${entries(index + 1, '{true})}
                  }

      ' {
          Json.Printable.writeAscii($out, "{")
          ${entries(0, '{false})}
          Json.Printable.writeAscii($out, "}")
        }

    finally cache.active -= name

  private[jacinta] def iterableWrite[collection: Type]
    ( value: Expr[collection], out: Expr[AnyRef], element0: Inlinable )
    ( using Quotes )
  :   Expr[Unit] =

    import quotes.reflect.*

    TypeRepr.of[collection].dealias match
      case AppliedType(_, arguments) =>
        arguments.last.asType.absolve match
          case '[element] =>
            val instance = element0.asInstanceOf[Inlinable { type Self = element }]

            ' {
                // The opaque prelude collections erase to their stdlib counterparts.
                val elements = $value.asInstanceOf[Iterable[element]].iterator
                Json.Printable.writeAscii($out, "[")
                def write(element: element): Unit = ${instance.write('element, out)}

                if elements.hasNext then write(elements.next())

                while elements.hasNext do
                  Json.Printable.writeAscii($out, ",")
                  write(elements.next())

                Json.Printable.writeAscii($out, "]")
              }

      case _ =>
        report.errorAndAbort("jacinta: an inlinable collection requires an applied collection type")

  // Whether the generator builds a type's codec structurally, as a record that does not encode
  // as text: the condition on `import codecs.direct`'s generated instances. A cheap shape test,
  // so a search under the mode stays cheap; anything else keeps the derived path.
  def generates[value: Type](using Quotes): Expr[Inlinable.Generates[value]] =
    import quotes.reflect.*

    val tpe = formless(TypeRepr.of[value])

    if productSupported(tpe) && !textual(tpe) then '{Inlinable.Generates[value]()}
    else report.errorAndAbort(s"jacinta: ${tpe.show} has no generated codec")

  def inlinablePrintable[value: Type](using Quotes): Expr[value is Json.Printable] =
    import quotes.reflect.*

    val tpe = formless(TypeRepr.of[value])

    if tpe =:= TypeRepr.of[value].dealias then generatedPrintable[value]
    else tpe.asType.absolve match
      case '[formless] =>
        '{${generatedPrintable[formless]}.asInstanceOf[value is Json.Printable]}

  private def generatedPrintable[value: Type](using Quotes): Expr[value is Json.Printable] =
    import quotes.reflect.*

    val cache: Cache = PrintingCache()

    val root: Inlinable = within(cache)(resolve[value](cache)).getOrElse:
      report.errorAndAbort
        ( s"jacinta: no Inlinable instance for ${TypeRepr.of[value].show}, and it is not an " +
          "inlinable product" )

    val runtime = root.isInstanceOf[NominalPrinter[?]] || root.isInstanceOf[EncodedPrinter[?]]

    if !root.prints || runtime then
      report.errorAndAbort
        ( s"jacinta: ${TypeRepr.of[value].show} has no generated printer; use its " +
          "`Encodable in Json`" )

    val instance = root.asInstanceOf[Inlinable { type Self = value }]
    val name = TypeRepr.of[value].dealias.show

    ' {
        lazy val self: Json.Printable.Direct[value] =
          new Json.Printable.Direct[value]:
            protected def writeCarrier(value0: value, out0: AnyRef): Unit =
              $ {
                  cache.roots(name) = 'self
                  within(cache)(instance.write('value0, 'out0))
                }

        // Sealed per the codec-thunk pattern, as `inlinableParsable`.
        // [quote-wall] generated Printable sealed inside quote
        caps.unsafe.unsafeAssumePure(self)
      }

  // ── The entry macro ────────────────────────────────────────────────────
  def inlinableParsable[value: Type](using Quotes): Expr[value is Json.Parsable] =
    import quotes.reflect.*

    val tpe = formless(TypeRepr.of[value])

    if tpe =:= TypeRepr.of[value].dealias then generatedParsable[value]
    else tpe.asType.absolve match
      case '[formless] =>
        '{${generatedParsable[formless]}.asInstanceOf[value is Json.Parsable]}

  private def generatedParsable[value: Type](using Quotes): Expr[value is Json.Parsable] =
    import quotes.reflect.*

    val cache: Cache = Cache()

    val root: Inlinable = resolve[value](cache).getOrElse:
      report.errorAndAbort
        ( s"jacinta: no Inlinable instance for ${TypeRepr.of[value].show}, and it is not an " +
          "inlinable product; use `Json.Parsable.staged` or `derived`" )

    // A runtime-tier root would find the very given under definition — an
    // infinite self-call — or add nothing over the instance it wraps.
    if root.isInstanceOf[RuntimeInlinable[?]] then
      report.errorAndAbort
        ( s"jacinta: ${TypeRepr.of[value].show} has no generator of its own; use " +
          "`Json.Parsable.staged` or `derived` rather than `Inlinable.parsable`" )

    val instance = root.asInstanceOf[Inlinable { type Self = value }]
    val name = TypeRepr.of[value].dealias.show

    ' {
        lazy val self: Json.Parsable.Direct[value] =
          new Json.Parsable.Direct[value]:
            protected def parseCarrier(reader0: AnyRef): value =
              // A capability class cannot be quoted into a pure hole, so
              // every use casts from the neutral carrier afresh.
              $ {
                  cache.roots(name) = 'self
                  within(cache)(instance.parse('{reader0.asInstanceOf[Json.Reader]}))
                }

        // Sealed per the codec-thunk pattern, like the staged instances: the
        // generated body resolves its capabilities where it is spliced.
        // [quote-wall] staged Parsable generated inside quote
        caps.unsafe.unsafeAssumePure(self)
      }
