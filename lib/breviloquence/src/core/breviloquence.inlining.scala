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
package breviloquence

import scala.caps
import scala.collection.immutable.Vector
import scala.collection.immutable.{List, Nil}
import scala.collection.mutable as scm
import scala.quoted.*

import anticipation.*
import contingency.*
import distillate.*
import prepositional.*
import vacuous.*

// The machinery behind `Inlinable`: expansion-time instance resolution, the
// structural generators for products, collections and sums, and the entry
// macros. A user-written `Inlinable`, and a sum's `Discriminable in Cbor`, can
// only be evaluated at expansion time by an in-macro staging compiler, which
// `breviloquence.staged` contributes as an `Inlinable.Stager`; without it, such
// a field takes the runtime seam, and a sum is not generated.
object inlining:

  // The runtime seam for a field type without an `Inlinable`: a nominal
  // `Parsable` (a custom instance) is called directly; anything else bridges
  // through its `Decodable in Cbor`, materializing just that field's
  // subtree. An inline method because `summonFrom` may only live in one; it
  // expands where the generated parser is spliced.
  // The reader travels as a neutral carrier and is reasserted here, at the audited point.
  inline def fieldSeam[fieldType](reader: AnyRef): fieldType =
    scala.compiletime.summonFrom:
      case parsable: (`fieldType` is Cbor.Parsable) => parsable.parse(Cbor.Reader.of(reader))

      case _ =>
        Cbor.Parsable
        . fromDecodable(scala.compiletime.summonInline[fieldType is Decodable in Cbor])
        . parse(Cbor.Reader.of(reader))

  // The seam's absent counterpart, preserving the AST path's semantics for
  // a missing key: `decoded(Cbor(Ast(Unset)))` through the bridge.
  inline def fieldSeamAbsent[fieldType](tactic: Tactic[Cbor.Error]): fieldType =
    scala.compiletime.summonFrom:
      case parsable: (`fieldType` is Cbor.Parsable) => parsable.absent()(using tactic)

      case _ =>
        Cbor.Parsable
        . fromDecodable(scala.compiletime.summonInline[fieldType is Decodable in Cbor])
        . absent()(using tactic)

  // ── The resolution ladder ──────────────────────────────────────────────
  // builtin → staged summon → structural collection/product/sum →
  // (caller's) runtime seam. The cache spans one entry expansion, so a
  // type's staging summon runs at most once however often it recurs in the
  // graph.

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
          List(TypeRepr.of[Cbor.Parsable2].typeSymbol, TypeRepr.of[Cbor.Printable2].typeSymbol)

        symbol.name == "generated" && owners.contains(symbol.owner)

  // A type with any `Form` refinement removed: a read of `Foo in Cbor` infers the instance's
  // type as `Foo { type Form = Cbor }`, whose codec is `Foo`'s.
  private def formless(using Quotes)(tpe: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    tpe.dealias match
      case Refinement(parent, "Form", _) => formless(parent)
      case other                         => other

  private def resolve[field: Type](cache: Cache)(using Quotes): Option[Inlinable] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[field].dealias

    builtinFor(tpe).orElse:
      cache.instances.getOrElseUpdate(tpe.show, Inlinable.Stager.summon[field].map(unwrap))
      . orElse(structuralFor[field](cache))
      . orElse(runtimeFor[field])

  // A `derives`-clause carrier delegates to a structural instance; the
  // ladder works with the delegate so generator identities stay
  // recognizable.
  private def unwrap(instance: Inlinable): Inlinable = instance match
    case derived: Inlinable.ForCbor[?] => derived.delegate.asInstanceOf[Inlinable]
    case other                         => other

  // The runtime tiers, resolved with a reflection-level implicit search at
  // expansion time and called through neutral-carrier helpers (the erasing
  // cast crosses the capability boundary, as wisteria's field-instance
  // engine does): a nominal `Parsable` — the recursion tie, since a
  // recursive record's own alias given (a lazy val) is found from inside
  // its own definition — then the `Decodable` bridge. Neither
  // `summonInline` nor a `using` parameter works here: inside the generated
  // parser's inline context, the former fails to expand the blanket
  // `summonFrom` given for recursively-derived instances, and honest
  // capability-typed codecs fail to conform to the evidence query.
  private def runtimeFor[field: Type](using Quotes): Option[Inlinable] =
    import quotes.reflect.*

    def searched(target: TypeRepr): Option[Expr[Any]] =
      Implicits.search(target) match
        case success: ImplicitSearchSuccess if !blanket(success.tree) => Some(success.tree.asExpr)
        case _                                                        => None

    ongoing.value match
      case Some(_: PrintingCache) =>
        rootFor[field].orElse(searched(TypeRepr.of[field is Cbor.Printable]))
        . map(NominalPrinter[field](_))
        . orElse(searched(TypeRepr.of[field is Encodable in Cbor]).map(EncodedPrinter[field](_)))

      case _ =>
        rootFor[field].orElse(searched(TypeRepr.of[field is Cbor.Parsable]))
        . map(RuntimeInlinable[field](_))
        . orElse(searched(TypeRepr.of[field is Decodable in Cbor]).map(DecodedInlinable[field](_)))

  // The printing counterparts of the runtime tiers: a nominal `Printable` (the recursion tie,
  // or a hand-written one), called in place, and the `Encodable` seam, whose tree is written
  // in place (an element encoding to `Unset` is written as `undefined`, as the derivation
  // writes it; a record field is handled by `productWrite`, which leaves it out).
  private final class NominalPrinter[value](printable: Expr[Any]) extends Inlinable:
    type Self = value

    def parse(reader: Expr[Cbor.Reader])(using Quotes, Type[value]): Expr[value] =
      quotes.reflect.report.errorAndAbort("breviloquence: a printer cannot parse")

    override def prints: Boolean = true

    override def write(value: Expr[value], out: Expr[AnyRef])(using Quotes, Type[value])
    :   Expr[Unit] =

      '{Cbor.Printable.writeField[value]($printable.asInstanceOf[AnyRef], $value, $out)}

  private final class EncodedPrinter[value](val encodable: Expr[Any]) extends Inlinable:
    type Self = value

    def parse(reader: Expr[Cbor.Reader])(using Quotes, Type[value]): Expr[value] =
      quotes.reflect.report.errorAndAbort("breviloquence: a printer cannot parse")

    override def prints: Boolean = true

    override def write(value: Expr[value], out: Expr[AnyRef])(using Quotes, Type[value])
    :   Expr[Unit] =

      ' {
          val typed = $encodable.asInstanceOf[value is Encodable in Cbor]
          Cbor.Printable.writeAst($out, Cbor.unseal(typed.encoded($value)))
        }

  private final class RuntimeInlinable[value](parsable: Expr[Any]) extends Inlinable:
    type Self = value

    def parse(reader: Expr[Cbor.Reader])(using Quotes, Type[value]): Expr[value] =
      ' {
          Cbor.Parsable.parseField[value]
            ( $parsable.asInstanceOf[AnyRef], $reader.asInstanceOf[AnyRef] )
        }

    override def absent(tactic: Expr[Tactic[Cbor.Error]])(using Quotes, Type[value])
    :   Expr[value] =

      '{Cbor.Parsable.absentField[value]($parsable.asInstanceOf[AnyRef])(using $tactic)}

  private final class DecodedInlinable[value](decodable: Expr[Any]) extends Inlinable:
    type Self = value

    def parse(reader: Expr[Cbor.Reader])(using Quotes, Type[value]): Expr[value] =
      '{$decodable.asInstanceOf[value is Decodable in Cbor].decoded($reader.value())}

    override def absent(tactic: Expr[Tactic[Cbor.Error]])(using Quotes, Type[value])
    :   Expr[value] =

      ' {
          $decodable.asInstanceOf[value is Decodable in Cbor]
          . decoded(Cbor.ast(Cbor.Ast(vacuous.Unset)))
        }

  // Composition points become local defs rather than textual inlining: a
  // fully-flattened parser exceeds HotSpot's huge-method bytecode limit and
  // runs interpreted. A local def keeps each generated method
  // JIT-compilable while the call stays monomorphic and direct. Leaf
  // generators stay inline.
  private def nested[fieldType: Type]
    ( instance: Inlinable { type Self = fieldType }, reader: Expr[Cbor.Reader] )
    ( using Quotes )
  :   Expr[fieldType] =

    instance match
      case _: Inlinable.ProductInlinable[?] | _: Inlinable.IterableInlinable[?]
        | _: Inlinable.SumInlinable[?] =>
        ' {
            def parseNested(): fieldType = ${instance.parse(reader)}
            parseNested()
          }

      case _ =>
        instance.parse(reader)

  // `Data`, read off the result of `Cbor.Reader.byteString` rather than quoted as
  // `Type.of[Data]`: under capture checking, a quoted `Array[Byte]^{}` unpickles as
  // `Array[Byte]^{any.rd}`, which no longer conforms to the type it was quoted from.
  private def dataType(using Quotes): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    val reader = TypeRepr.of[Cbor.Reader]

    reader.memberType(reader.typeSymbol.methodMember("byteString").head).absolve match
      case MethodType(_, _, result) => result

  private def builtinFor(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[Inlinable] =
    import quotes.reflect.*

    if tpe =:= TypeRepr.of[Int] then Some(Inlinable.int)
    else if tpe =:= TypeRepr.of[Long] then Some(Inlinable.long)
    else if tpe =:= TypeRepr.of[Double] then Some(Inlinable.double)
    else if tpe =:= TypeRepr.of[Float] then Some(Inlinable.float)
    else if tpe =:= TypeRepr.of[Boolean] then Some(Inlinable.boolean)
    else if tpe =:= TypeRepr.of[Text] then Some(Inlinable.text)
    else if tpe =:= TypeRepr.of[String] then Some(Inlinable.string)
    else if tpe =:= dataType then Some(Inlinable.byteString)
    else if tpe =:= TypeRepr.of[Cbor] then Some(Inlinable.cbor)
    else None

  // The opaque prelude `List`/`Set`/`Sequence` map to their underlying stdlib
  // collection (used to summon a `Factory`, since the opaque companion exposes
  // only a `Conversion`, not a direct instance).
  private def aliasCollectionUnderlying(using Quotes)(tpe: quotes.reflect.TypeRepr)
  :   Option[quotes.reflect.TypeRepr] =

    import quotes.reflect.*
    val listSym   = TypeRepr.of[proscenium.List[Any]].typeSymbol
    val setSym    = TypeRepr.of[proscenium.Set[Any]].typeSymbol
    val seriesSym = TypeRepr.of[proscenium.Sequence[Any]].typeSymbol

    tpe.dealias match
      case AppliedType(constructor, List(element)) if constructor.typeSymbol == listSym =>
        Some(TypeRepr.of[scala.collection.immutable.List].appliedTo(element))

      case AppliedType(constructor, List(element)) if constructor.typeSymbol == setSym =>
        Some(TypeRepr.of[scala.collection.immutable.Set].appliedTo(element))

      case AppliedType(constructor, List(element)) if constructor.typeSymbol == seriesSym =>
        Some(TypeRepr.of[Vector].appliedTo(element))

      case _ =>
        None

  private def structuralFor[field: Type](cache: Cache)(using Quotes): Option[Inlinable] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[field].dealias

    // A `Map` is an `Iterable` of pairs, but its wire form is a CBOR map,
    // not an array — it stays on the seam (the AST `mapDecodable`).
    val isMap = tpe.derivesFrom(Symbol.requiredClass("scala.collection.Map"))

    // The opaque prelude `List`/`Set`/`Sequence` don't conform to `Iterable`, so
    // recognize their type constructors by symbol; each stages as an array
    // exactly like a stdlib collection (the element becomes the recursion
    // point, so `List[Tree]` inside `Tree` degrades `Tree`, not the whole list).
    val listSym   = TypeRepr.of[proscenium.List[Any]].typeSymbol
    val setSym    = TypeRepr.of[proscenium.Set[Any]].typeSymbol
    val seriesSym = TypeRepr.of[proscenium.Sequence[Any]].typeSymbol

    val isAliasCollection = tpe match
      case AppliedType(constructor, _) =>
        val sym = constructor.typeSymbol
        sym == listSym || sym == setSym || sym == seriesSym

      case _ =>
        false

    if !isMap && (tpe <:< TypeRepr.of[Iterable[Any]] || isAliasCollection) then tpe match
      case AppliedType(_, arguments) =>
        arguments.last.asType match
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
      else Inlinable.Stager.discriminantKey[field].map: key =>
        Inlinable.SumInlinable[field](key, sumPrints(quotes.reflect.TypeRepr.of[field], key))

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
        !applied &&
          classSymbol.flags.is(Flags.Sealed) &&
          children.nonEmpty &&
          children.forall: child =>
            child.isClassDef && child.flags.is(Flags.Case) && !hasRenames(child)

      if supported then Some(children.map { child => (child.name, child.typeRef) }) else None

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
  // generator does not replicate; annotated records stay on the AST path.
  private def hasRenames(using Quotes)(classSymbol: quotes.reflect.Symbol): Boolean =
    import quotes.reflect.*

    val annotated =
      classSymbol.primaryConstructor.paramSymss.flatten.filterNot(_.isTypeParam)
        . flatMap(_.annotations) ++
        classSymbol.caseFields.flatMap(_.annotations)

    annotated.exists: annotation => annotation.tpe <:< TypeRepr.of[adversaria.name[?]]

  // ── The collection generator ───────────────────────────────────────────
  // Mirrors the AST `collectionDecodable` exactly: a definite array is
  // count-driven, an indefinite one Break-terminated — with the element's
  // generated code inlined into the loop.
  private[breviloquence] def iterableBody[collection: Type]
    ( reader: Expr[Cbor.Reader], element0: Inlinable )
    ( using Quotes )
  :   Expr[collection] =

    import quotes.reflect.*

    TypeRepr.of[collection].dealias match
      case AppliedType(_, arguments) =>
        // The opaque prelude collections have no direct `Factory`, so build at
        // the underlying stdlib type and cast the result (a no-op at erasure).
        val factoryType =
          aliasCollectionUnderlying(TypeRepr.of[collection]).getOrElse(TypeRepr.of[collection])

        (arguments.last.asType, factoryType.asType) match
          case ('[element], '[stdlib]) =>
            val instance = element0.asInstanceOf[Inlinable { type Self = element }]

            ' {
                def parseElement(): element = ${instance.parse(reader)}
                val factory = infer[scala.collection.Factory[element, stdlib]]
                val builder = factory.newBuilder
                val tactic = infer[Tactic[Cbor.Error]]
                var remaining = CborParser.of($reader.rawParser).directOpenArray()(using tactic)
                var run = remaining != 0

                while run do
                  if remaining < 0 && CborParser.of($reader.rawParser).directBreak()(using tactic)
                  then run = false
                  else
                    builder += parseElement()
                    remaining -= 1
                    if remaining == 0 then run = false

                builder.result().asInstanceOf[collection]
              }

      case _ =>
        report.errorAndAbort
          ( "breviloquence: an inlinable collection requires an applied collection type" )

  // ── The product generator ──────────────────────────────────────────────
  // Self-contained monomorphic record parsing, mirroring the AST record
  // decoder's semantics: typed local slots and seen flags, literal
  // packed-word key dispatch (an opaque key resolves through the generally
  // consumed name against literal strings; unknown keys and non-text-keyed
  // entries are skipped), first occurrence read per key, declared defaults
  // then absent semantics for missing fields, direct construction.
  private[breviloquence] def productBody[product: Type](reader: Expr[Cbor.Reader])(using Quotes)
  :   Expr[product] =

    productBody[product](reader, continued())

  private def productBody[product: Type](reader: Expr[Cbor.Reader], cache: Cache)(using Quotes)
  :   Expr[product] =

    import quotes.reflect.*

    cache.active += TypeRepr.of[product].dealias.show

    try productBody0[product](reader, cache)
    finally cache.active -= TypeRepr.of[product].dealias.show

  private def productBody0[product: Type](reader: Expr[Cbor.Reader], cache: Cache)(using Quotes)
  :   Expr[product] =

    import quotes.reflect.*

    val tpe = TypeRepr.of[product].dealias

    if !productSupported(tpe) then
      report.errorAndAbort
        ( s"breviloquence: ${tpe.show} is not an inlinable product (a non-generic, top-level " +
          "or object-nested case class with a single parameter list and no `@name` renames); " +
          "use a `Decodable in Cbor`" )

    val classSymbol = tpe.classSymbol.get
    val ctor = classSymbol.primaryConstructor
    val fields = classSymbol.caseFields
    val arity = fields.length
    val fieldNames: List[String] = fields.map(_.name)
    val fieldTypes: List[TypeRepr] = fields.map: field => tpe.memberType(field).dealias

    // Field names pack exactly as `directKeyWord` packs wire keys: 1-16
    // bytes, all 7-bit ASCII, little-endian into a low and high word.
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
      ( tactic: Expr[Tactic[Cbor.Error]],
        parser: Expr[CborParser] )
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

      // Builtins read straight off the parser bound once per record,
      // skipping the Cbor.Reader rim entirely.
      def builtinDirect(tpe: TypeRepr): Option[Expr[Any]] =
        if tpe =:= TypeRepr.of[Int] then Some('{$parser.directLong()(using $tactic).toInt})
        else if tpe =:= TypeRepr.of[Long] then Some('{$parser.directLong()(using $tactic)})
        else if tpe =:= TypeRepr.of[Double] then
          Some('{$parser.directDouble()(using $tactic)})
        else if tpe =:= TypeRepr.of[Float] then
          Some('{$parser.directDouble()(using $tactic).toFloat})
        else if tpe =:= TypeRepr.of[Boolean] then
          Some('{$parser.directBoolean()(using $tactic)})
        else if tpe =:= TypeRepr.of[Text] then
          Some('{$parser.directString()(using $tactic).tt})
        else if tpe =:= TypeRepr.of[String] then
          Some('{$parser.directString()(using $tactic)})
        else if tpe =:= dataType then
          Some('{$parser.directBytes()(using $tactic)})
        else if tpe =:= TypeRepr.of[Cbor] then
          Some('{Cbor.ast($parser.value()(using $tactic))})
        else
          None

      // The absent expression per field, through the ladder.
      def fieldAbsent(index: Int): Expr[Any] =
        fieldTypes(index).asType match
          case '[fieldType] =>
            if builtinDirect(fieldTypes(index)).isDefined then
              '{Cbor.Parsable.missing[fieldType]()(using $tactic)}
            else
              resolve[fieldType](cache) match
                case Some(instance0) =>
                  instance0.asInstanceOf[Inlinable { type Self = fieldType }].absent(tactic)

                case None =>
                  '{inlining.fieldSeamAbsent[fieldType]($tactic)}

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
                      '{inlining.fieldSeam[fieldType]($reader.asInstanceOf[AnyRef])}

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
            ( '{!${Ref(seens(index)).asExprOf[Boolean]}}.asTerm,
              readAndMark,
              '{$parser.directSkipValue()(using $tactic)}.asTerm )

        CaseDef(Literal(IntConstant(index)), None, rhs)

      def fallthrough =
        CaseDef(Wildcard(), None, '{$parser.directSkipValue()(using $tactic)}.asTerm)

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
          ' {
              val name = $parser.directKeyName()(using $tactic)
              if name == null then -1 else ${namedChain('{name.nn}, 0)}
            }

        val resolveStep: Term =
          If
            ( '{$wordRef == Cbor.Reader.KeyOpaque}.asTerm,
              opaque.asTerm,
              Block
                ( List(ValDef(high, Some('{$parser.directKeyHigh}.asTerm))),
                  packedChainOver(wordRef, highRef)(0) ) )

        val entry: Term =
          Block
            ( List
                ( ValDef(word, Some('{$parser.directKeyWord()}.asTerm)),
                  Block
                    ( List(ValDef(found, Some(resolveStep))),
                      Match(Ref(found), arms ::: List(fallthrough)) ),
                  Assign(Ref(remaining), '{$remainingRef - 1}.asTerm) ),
              If
                ( '{$remainingRef == 0}.asTerm,
                  Assign(Ref(run), Literal(BooleanConstant(false))),
                  unit ) )

        If
          ( '{$remainingRef < 0 && $parser.directBreak()(using $tactic)}.asTerm,
            Assign(Ref(run), Literal(BooleanConstant(false))),
            entry )

      val loop: List[Statement] =
        readDefDefs :::
          List
            ( ValDef(remaining, Some('{$parser.directOpenMap()(using $tactic)}.asTerm)),
              ValDef(run, Some('{$remainingRef != 0}.asTerm)),
              While(runRef.asTerm, step) )

      val absents: List[Term] = List.range(0, arity).map: index =>
        fieldTypes(index).asType match
          case '[fieldType] =>
            val resolveAbsent: Term =
              Assign
                ( Ref(slots(index)),
                  ' {
                      val declared =
                        wisteria.internal.default[product, fieldType](${Expr(index)})

                      if !declared.absent then declared.asInstanceOf[fieldType]
                      else ${fieldAbsent(index).asExprOf[fieldType]}
                    }.asTerm )

            If('{!${Ref(seens(index)).asExprOf[Boolean]}}.asTerm, resolveAbsent, unit)

      val construct: Term =
        Apply(Select(New(Inferred(tpe)), ctor), slots.map { slot => Ref(slot) })

      Block(slotDefs ::: seenDefs ::: loop ::: absents, construct).asExprOf[product]

    // The parser is not bound to a local: every rim call reaches it through the reader's
    // carrier and an inline cast, the one form that is exclusive in every checking mode
    // (see `CborParser.of`).
    ' {
        val tactic = infer[Tactic[Cbor.Error]]
        ${body('tactic, '{CborParser.of($reader.rawParser)})}
      }

  // ── The sum generator ──────────────────────────────────────────────────
  // The variant is chosen by a scan-ahead of the discriminant entry (the
  // parser is left where it started), then parsed from the start of the
  // map as its product — the discriminant entry skips as an unknown key —
  // exactly the AST disjunction's `discriminate` + `delegate` semantics.
  private[breviloquence] def sumBody[sum: Type](reader: Expr[Cbor.Reader], key: String)
    ( using Quotes )
  :   Expr[sum] =

    sumBody[sum](reader, key, continued())

  private def sumBody[sum: Type](reader: Expr[Cbor.Reader], key: String, cache: Cache)
    ( using Quotes )
  :   Expr[sum] =

    import quotes.reflect.*

    cache.active += TypeRepr.of[sum].dealias.show

    try sumBody0[sum](reader, key, cache) finally cache.active -= TypeRepr.of[sum].dealias.show

  private def sumBody0[sum: Type](reader: Expr[Cbor.Reader], key: String, cache: Cache)
    ( using Quotes )
  :   Expr[sum] =

    import quotes.reflect.*

    val variants = sumVariants(TypeRepr.of[sum].dealias).getOrElse:
      report.errorAndAbort
        ( s"breviloquence: ${TypeRepr.of[sum].show} is not an inlinable sum (a non-generic " +
          "sealed type whose variants are all case classes without `@name` renames)" )

    val arity = variants.length

    def dispatch(index: Int, tag: Expr[String]): Expr[sum] =
      if index == arity then
        ' {
            provide[Tactic[wisteria.Variant.Error]]:
              abort(wisteria.Variant.Error[sum]($tag.tt))
          }
      else
        variants(index)(1).asType match
          case '[type variantType <: sum; variantType] =>
            val instance = resolve[variantType](cache).getOrElse:
              report.errorAndAbort
                ( s"breviloquence: no Inlinable for variant ${variants(index)(0)}" )

            . asInstanceOf[Inlinable { type Self = variantType }]

            ' {
                if $tag == ${Expr(variants(index)(0))} then
                  def parseVariant(): variantType = ${instance.parse(reader)}
                  parseVariant()
                else
                  ${dispatch(index + 1, tag)}
              }

    ' {
        val tactic = infer[Tactic[Cbor.Error]]
        val tag = CborParser.of($reader.rawParser).directDiscriminant(${Expr(key)})(using tactic)

        if tag == null then abort(Cbor.Error(Cbor.Error.Reason.Absent))(using tactic)
        else
          val tagValue: String = tag.nn
          ${dispatch(0, 'tagValue)}
      }

  // ── The printing side ──────────────────────────────────────────────────
  // A generated printer writes into a `Producer.Bytes` exactly the bytes the derived encoding
  // writes: a record is a definite-length map with text keys in field order, leaving out a
  // field whose encoding is `Unset`; a collection a definite-length array; a sum its variant's
  // map with the discriminant entry added at the end. A field with no printer of its own is
  // encoded through its `Encodable in Cbor` and its tree written in place.

  // How a record field is printed: through an instance that writes it in place, or through its
  // `Encodable`, encoded ahead of the map's head so that a field encoding to `Unset` can be
  // left out of the count.
  private enum Printing:
    case Generated(instance: Inlinable)
    case Seam(encodable: Expr[Any])

  private def printingFor[fieldType: Type](cache: Cache)(using Quotes): Printing =
    import quotes.reflect.*

    resolve[fieldType](cache) match
      case Some(seam: EncodedPrinter[?])     => Printing.Seam(seam.encodable)
      case Some(instance) if instance.prints => Printing.Generated(instance)

      case _ =>
        val name = TypeRepr.of[fieldType].show
        report.errorAndAbort(s"breviloquence: no Printable or Encodable in Cbor for $name")

  // Writes one value through its printing instance.
  private def printed[fieldType: Type]
    ( printing: Printing, value: Expr[fieldType], out: Expr[AnyRef] )
    ( using Quotes )
  :   Expr[Unit] =

    printing match
      case Printing.Generated(instance) =>
        instance.asInstanceOf[Inlinable { type Self = fieldType }].write(value, out)

      // A seam field's tree is encoded ahead of the map's head, and written by the caller.
      case Printing.Seam(encodable) =>
        ' {
            val typed = $encodable.asInstanceOf[fieldType is Encodable in Cbor]
            Cbor.Printable.writeAst($out, Cbor.unseal(typed.encoded($value)))
          }

  private[breviloquence] def productWrite[product: Type](value: Expr[product], out: Expr[AnyRef])
    ( using Quotes )
  :   Expr[Unit] =

    productWrite[product](value, out, None, continued())

  // `discriminant`: a sum variant's key and tag, written as the map's last entry.
  private def productWrite[product: Type]
    ( value:        Expr[product],
      out:          Expr[AnyRef],
      discriminant: Option[(String, String)],
      cache:        Cache )
    ( using Quotes )
  :   Expr[Unit] =

    import quotes.reflect.*

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

      // Each seam field's tree, encoded before the map's head so that its count is known.
      def withSeams(index: Int, encoded: List[Option[Expr[Cbor.Ast]]]): Expr[Unit] =
        if index == fields.length then writeMap(encoded.reverse)
        else fieldTypes(index).asType.absolve match
          case '[fieldType] => printings(index) match
            case Printing.Seam(encodable) =>
              val typed = field(index).asExprOf[fieldType]

              ' {
                  val tree: Cbor.Ast =
                    Cbor.unseal
                      ( $encodable.asInstanceOf[fieldType is Encodable in Cbor].encoded($typed) )

                  ${withSeams(index + 1, Some('tree) :: encoded)}
                }

            case _ =>
              withSeams(index + 1, None :: encoded)

      def writeMap(encoded: List[Option[Expr[Cbor.Ast]]]): Expr[Unit] =
        val fixed = encoded.count(_.isEmpty) + (if discriminant.isDefined then 1 else 0)

        val count: Expr[Int] = encoded.flatten.foldLeft(Expr(fixed)): (total, tree) =>
          '{$total + (if Cbor.Printable.present($tree) then 1 else 0)}

        val entries: List[Expr[Unit]] = List.range(0, fields.length).map: index =>
          fieldTypes(index).asType.absolve match
            case '[fieldType] =>
              val key = Expr(fields(index).name)
              val typed = field(index).asExprOf[fieldType]

              encoded(index) match
                case Some(tree) =>
                  ' {
                      if Cbor.Printable.present($tree) then
                        Cbor.Printable.writeText($out, $key)
                        Cbor.Printable.writeAst($out, $tree)
                    }

                case None =>
                  ' {
                      Cbor.Printable.writeText($out, $key)
                      ${printed[fieldType](printings(index), typed, out)}
                    }

        val tail: List[Expr[Unit]] = discriminant.toList.map: (key, tag) =>
          ' {
              Cbor.Printable.writeText($out, ${Expr(key)})
              Cbor.Printable.writeText($out, ${Expr(tag)})
            }

        Expr.block('{Cbor.Printable.writeHead($out, 5, $count)} :: entries ++ tail, '{()})

      withSeams(0, Nil)

    finally cache.active -= name

  private[breviloquence] def iterableWrite[collection: Type]
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
                val elements = $value.asInstanceOf[Iterable[element]]
                Cbor.Printable.writeHead($out, 4, elements.size)
                def write(element: element): Unit = ${instance.write('element, out)}
                elements.foreach(write)
              }

      case _ =>
        report.errorAndAbort
          ( "breviloquence: an inlinable collection requires an applied collection type" )

  private[breviloquence] def sumWrite[sum: Type](value: Expr[sum], out: Expr[AnyRef], key: String)
    ( using Quotes )
  :   Expr[Unit] =

    import quotes.reflect.*

    val cache = continued()
    val variants = sumVariants(TypeRepr.of[sum].dealias).get

    def dispatch(index: Int): Expr[Unit] =
      if index == variants.length then '{()}
      else
        variants(index)(1).asType.absolve match
          case '[type variantType <: sum; variantType] =>
            val (tag, _) = variants(index)
            val variant = '{$value.asInstanceOf[variantType]}
            val written = productWrite[variantType](variant, out, Some((key, tag)), cache)

            '{if $value.isInstanceOf[variantType] then $written else ${dispatch(index + 1)}}

    dispatch(0)

  // Whether a sum's variants can all be printed with its discriminant appended: each a record,
  // none with a field of the discriminant's name, which the derived encoding would overwrite in
  // place rather than append.
  private[breviloquence] def sumPrints(using Quotes)(tpe: quotes.reflect.TypeRepr, key: String)
  :   Boolean =

    sumVariants(tpe.dealias).exists: variants =>
      variants.forall: (_, variant) =>
        productSupported(variant) && !variant.classSymbol.get.caseFields.exists(_.name == key)

  // Whether the generator builds a type's codec structurally, as a record: the condition on
  // `import codecs.direct`'s generated instances. A cheap shape test, so a search under the mode
  // stays cheap; a sum's discriminant is only known to the staging compiler, so a sum keeps the
  // derived path unless it has an instance of its own.
  def generates[value: Type](using Quotes): Expr[Inlinable.Generates[value]] =
    import quotes.reflect.*

    val tpe = formless(TypeRepr.of[value])

    if productSupported(tpe) then '{Inlinable.Generates[value]()}
    else report.errorAndAbort(s"breviloquence: ${tpe.show} has no generated codec")

  def inlinablePrintable[value: Type](using Quotes): Expr[value is Cbor.Printable] =
    import quotes.reflect.*

    val tpe = formless(TypeRepr.of[value])

    if tpe =:= TypeRepr.of[value].dealias then generatedPrintable[value]
    else tpe.asType.absolve match
      case '[formless] =>
        '{${generatedPrintable[formless]}.asInstanceOf[value is Cbor.Printable]}

  private def generatedPrintable[value: Type](using Quotes): Expr[value is Cbor.Printable] =
    import quotes.reflect.*

    val cache: Cache = PrintingCache()

    val root: Inlinable = within(cache)(resolve[value](cache)).getOrElse:
      report.errorAndAbort
        ( s"breviloquence: no Inlinable instance for ${TypeRepr.of[value].show}, and it is not " +
          "an inlinable product, collection or key-discriminated sum; use an `Encodable in Cbor`" )

    val runtime = root.isInstanceOf[NominalPrinter[?]] || root.isInstanceOf[EncodedPrinter[?]]

    if !root.prints || runtime then
      report.errorAndAbort
        ( s"breviloquence: ${TypeRepr.of[value].show} has no generated printer; use its " +
          "`Encodable in Cbor`" )

    val instance = root.asInstanceOf[Inlinable { type Self = value }]
    val name = TypeRepr.of[value].dealias.show

    ' {
        lazy val self: Cbor.Printable.Direct[value] =
          new Cbor.Printable.Direct[value]:
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
  def inlinableParsable[value: Type](using Quotes): Expr[value is Cbor.Parsable] =
    import quotes.reflect.*

    val tpe = formless(TypeRepr.of[value])

    if tpe =:= TypeRepr.of[value].dealias then generatedParsable[value]
    else tpe.asType.absolve match
      case '[formless] =>
        '{${generatedParsable[formless]}.asInstanceOf[value is Cbor.Parsable]}

  private def generatedParsable[value: Type](using Quotes): Expr[value is Cbor.Parsable] =
    import quotes.reflect.*

    val cache: Cache = Cache()

    val root: Inlinable = resolve[value](cache).getOrElse:
      report.errorAndAbort
        ( s"breviloquence: no Inlinable instance for ${TypeRepr.of[value].show}, and it is not " +
          "an inlinable product, collection or key-discriminated sum; use a `Decodable in Cbor`" )

    // A runtime-tier root would find the very given under definition — an
    // infinite self-call — or add nothing over the instance it wraps.
    if root.isInstanceOf[RuntimeInlinable[?]] || root.isInstanceOf[DecodedInlinable[?]] then
      report.errorAndAbort
        ( s"breviloquence: ${TypeRepr.of[value].show} has no generator of its own; use its " +
          "existing instance directly rather than `Inlinable.parsable`" )

    val instance = root.asInstanceOf[Inlinable { type Self = value }]
    val name = TypeRepr.of[value].dealias.show

    ' {
        lazy val self: Cbor.Parsable.Direct[value] =
          new Cbor.Parsable.Direct[value]:
            protected def parseCarrier(reader0: AnyRef): value =
              // A capability class cannot be quoted into a pure hole, so
              // every use casts from the neutral carrier afresh.
              $ {
                  cache.roots(name) = 'self
                  within(cache)(instance.parse('{reader0.asInstanceOf[Cbor.Reader]}))
                }

        // Sealed per the codec-thunk pattern: the generated body resolves its
        // capabilities where it is spliced.
        // [quote-wall] generated Parsable sealed inside quote
        caps.unsafe.unsafeAssumePure(self)
      }
