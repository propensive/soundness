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

// The machinery behind `Inlinable`: expansion-time instance resolution, the
// structural generators for messages, repeated fields and oneof sums, and the
// entry macros behind `Inlinable.parsable`, `Inlinable.printable` and the
// `Protobuf.Parsable`/`Protobuf.Printable` generators. A user-written
// `Inlinable` can only be evaluated at expansion time by an in-macro staging
// compiler, which `locomotion.staged` contributes as an `Inlinable.Stager`;
// without it, such a field takes the runtime seam.
object inlining:

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
          case '{$found: any} =>
            '{$found.asInstanceOf[fieldType is Decodable in Protobuf].decoded($wire)}

      case failure: ImplicitSearchFailure =>
        report.errorAndAbort
          ( s"locomotion: no Decodable in Protobuf for ${TypeRepr.of[fieldType].show}: " +
            failure.explanation )

  // A nominal `Protobuf.Parsable` for a field type, resolved at expansion
  // time — the recursion tie: a recursive message's own alias given (a lazy
  // val) is found from inside its own definition, so nested occurrences
  // parse through the very instance being generated. Also the composition
  // point for hand-written `Parsable`s in field positions.
  private def parsableFor[fieldType: Type](using Quotes): Option[Expr[Any]] =
    import quotes.reflect.*

    rootFor[fieldType].orElse:
      Implicits.search(TypeRepr.of[fieldType is Protobuf.Parsable]) match
        case success: ImplicitSearchSuccess if !blanket(success.tree) => Some(success.tree.asExpr)
        case _                                                        => None

  // ── The resolution ladder and field plans ──────────────────────────────
  // Every field of a message compiles to one of four plans: a builtin
  // scalar `Leaf` (read in place, last occurrence wins); a `Nested`
  // instance (a message, sum or custom leaf, parsed within its field
  // window); a `Gather` (a repeated field, appending per occurrence —
  // packed or not — into a builder); or the runtime `Seam` (occurrences
  // gathered as wire values and decoded once through the field's
  // `Decodable in Protobuf`).

  // The per-entry expansion cache: memoized staging summons, the set of
  // types whose generators are currently on the expansion stack — a
  // recursive occurrence (`Tree` with `children: List[Tree]`) must not
  // expand forever — and the instance being generated, named by its root
  // type, which a recursive occurrence of that type reads through.
  private final class Cache:
    val instances: scm.HashMap[String, Option[Inlinable]] = scm.HashMap()
    val active: scm.Set[String] = scm.Set()
    val roots: scm.HashMap[String, Expr[Any]] = scm.HashMap()

  // The cache of the expansion in progress. A structural instance reached
  // through `Inlinable.parse` (or `measure`, `write`) carries no cache, so it
  // continues this one rather than starting afresh: the active set then spans
  // the whole expansion, and a mutually recursive message ties back to a
  // generator already on the stack instead of expanding forever.
  private val ongoing: scala.util.DynamicVariable[Option[Cache]] =
    scala.util.DynamicVariable(None)

  private def continued(): Cache = ongoing.value.getOrElse(Cache())

  private def within[result](cache: Cache)(generate: => result): result =
    ongoing.withValue(Some(cache))(generate)

  // A recursive occurrence of the root type reads through the instance being generated.
  private def rootFor[fieldType: Type](using Quotes): Option[Expr[Any]] =
    import quotes.reflect.*

    val name = TypeRepr.of[fieldType].dealias.show

    ongoing.value.flatMap(_.roots.get(name))

  // Whether an implicit-search result is the `import codecs.direct` blanket given, which a
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
          List
            ( TypeRepr.of[Protobuf.Parsable2].typeSymbol,
              TypeRepr.of[Protobuf.Printable2].typeSymbol )

        symbol.name == "generated" && owners.contains(symbol.owner)

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

  // `Data`, read off the result of `ProtobufReader.data` rather than quoted as `Type.of[Data]`:
  // under capture checking, a quoted `Array[Byte]^{}` unpickles as `Array[Byte]^{any.rd}`,
  // which no longer conforms to the type it was quoted from.
  private def dataType(using Quotes): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    val reader = TypeRepr.of[ProtobufReader]

    reader.memberType(reader.typeSymbol.methodMember("data").head).absolve match
      case MethodType(_, _, result) => result

  private def builtinKind(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[Int] =
    import quotes.reflect.*

    if tpe =:= TypeRepr.of[Int] then Some(KInt)
    else if tpe =:= TypeRepr.of[Long] then Some(KLong)
    else if tpe =:= TypeRepr.of[Boolean] then Some(KBoolean)
    else if tpe =:= TypeRepr.of[Double] then Some(KDouble)
    else if tpe =:= TypeRepr.of[Float] then Some(KFloat)
    else if tpe =:= TypeRepr.of[Text] then Some(KText)
    else if tpe =:= dataType then Some(KData)
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
    // A map: each occurrence an entry message `{ key = 1; value = 2 }`, gathered into the
    // map's builder — the key a scalar, the value a scalar or a printing/parsing message.
    case MapGather(key: Elem, keyType: Any, value: Elem, valueType: Any)
    case Seam

  private def resolve[field: Type](cache: Cache)(using Quotes): Option[Inlinable] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[field].dealias

    if builtinKind(tpe).isDefined then None else
      cache.instances.getOrElseUpdate(tpe.show, Inlinable.Stager.summon[field].map(unwrap))
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
    case other                             => other

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

  // The opaque prelude `List`/`Set`/`Sequence` do not conform to `Iterable`
  // (so the `Iterable` test misses them) and their companions expose no
  // `Factory`, only a `Conversion`: each maps to its underlying stdlib
  // collection, at which a builder is summoned and whose result is cast back
  // — a no-op at erasure — mirroring breviloquence's generator.
  // A type with its capture annotations stripped: a frozen array field is `Array[T]^{}`,
  // which reflection presents as the applied type under an annotation.
  private def unannotated(using Quotes)(tpe: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    tpe.dealias match
      case AnnotatedType(inner, _) => unannotated(inner)
      case other                   => other

  // Whether a field is a frozen array, `Array[T]^{}`: a repeated field in the representation
  // with no cell per element — gathered into the stdlib's array builder for its element, and
  // walked by index when printed. (`Array[Byte]^{}` is `Data`, a scalar, and never reaches
  // here.)
  private def isFrozenArray(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean =
    import quotes.reflect.*

    val arraySym = TypeRepr.of[proscenium.Array[Any]].typeSymbol

    unannotated(tpe) match
      case AppliedType(constructor, List(element)) =>
        constructor.typeSymbol == arraySym && !(element.dealias =:= TypeRepr.of[Byte])

      case _ =>
        false

  // A frozen array of packable scalars, whose builder is the unboxed one.
  private def isArrayType(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean =
    import quotes.reflect.*

    isFrozenArray(tpe) && (unannotated(tpe) match
      case AppliedType(_, List(element)) => builtinKind(element.dealias).exists(_ <= KFloat)
      case _                             => false)

  private def aliasCollectionUnderlying(using Quotes)(tpe: quotes.reflect.TypeRepr)
  :   Option[quotes.reflect.TypeRepr] =

    import quotes.reflect.*

    val listSym     = TypeRepr.of[proscenium.List[Any]].typeSymbol
    val setSym      = TypeRepr.of[proscenium.Set[Any]].typeSymbol
    val sequenceSym = TypeRepr.of[proscenium.Sequence[Any]].typeSymbol

    unannotated(tpe) match
      case AppliedType(constructor, List(element)) if isFrozenArray(tpe) =>
        Some(TypeRepr.of[scala.Array].appliedTo(element))

      case AppliedType(constructor, List(element)) if constructor.typeSymbol == listSym =>
        Some(TypeRepr.of[scala.collection.immutable.List].appliedTo(element))

      case AppliedType(constructor, List(element)) if constructor.typeSymbol == setSym =>
        Some(TypeRepr.of[scala.collection.immutable.Set].appliedTo(element))

      case AppliedType(constructor, List(element)) if constructor.typeSymbol == sequenceSym =>
        Some(TypeRepr.of[scala.collection.immutable.Vector].appliedTo(element))

      case AppliedType(constructor, List(key, value))
        if constructor.typeSymbol == TypeRepr.of[proscenium.Map[Any, Any]].typeSymbol =>
        Some(TypeRepr.of[scala.collection.immutable.Map].appliedTo(List(key, value)))

      case _ =>
        None

  // A fresh builder for a gathered field, typed at the field's own type so
  // the per-occurrence appends and the finalizer need no cast: summoned at
  // the field type, or — for an opaque prelude alias, which has no
  // `Factory` — at its underlying stdlib collection and cast once here.
  private def newBuilder[element: Type, fieldType: Type](using Quotes)
  :   Expr[scm.Builder[element, fieldType]] =

    import quotes.reflect.*

    val mapSym = TypeRepr.of[scala.collection.immutable.Map[Any, Any]].typeSymbol

    aliasCollectionUnderlying(TypeRepr.of[fieldType]) match
      // A map's own builder stages up through the small maps before a hash map; the hash
      // map's builder, taken directly, measured faster for a map of any size a message
      // carries. The result is a `Map`.
      case Some(AppliedType(constructor, List(key, value))) if constructor.typeSymbol == mapSym =>
        (key.asType, value.asType) match
          case ('[k], '[v]) =>
            ' {
                scala.collection.immutable.HashMap.newBuilder[k, v]
                . asInstanceOf[scm.Builder[element, fieldType]]
              }

      case Some(underlying) =>
        underlying.asType match
          case '[stdlib] =>
            ' {
                infer[scala.collection.Factory[element, stdlib]].newBuilder
                . asInstanceOf[scm.Builder[element, fieldType]]
              }

      case None =>
        '{infer[scala.collection.Factory[element, fieldType]].newBuilder}

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
            // An optional collection has no inlined form on either path:
            // it stays on the seam, prelude alias or not.
            val innerIterable =
              inner <:< TypeRepr.of[Iterable[Any]] || aliasCollectionUnderlying(inner).isDefined

            if innerIterable then Plan.Seam else inner.asType match
              case '[innerType] =>
                resolve[innerType](cache) match
                  case Some(instance)
                    if !instance.isInstanceOf[Inlinable.IterableInlinable[?]] &&
                      !cache.active.contains(inner.show) =>
                      Plan.OptionalNested(instance, inner)

                  case _ =>
                    Plan.Seam

      case None =>
        // A `Map` — stdlib or the opaque prelude alias — is an `Iterable` of
        // pairs, but its wire form is a repeated entry message: it stays on
        // the seam (`mapDecodable`). The opaque prelude `List`/`Set`/`Sequence`
        // do not conform to `Iterable`, so they are recognised by their
        // constructor.
        val isMap =
          tpe.derivesFrom(Symbol.requiredClass("scala.collection.Map")) ||
            tpe.typeSymbol == TypeRepr.of[proscenium.Map[Any, Any]].typeSymbol

        val isAliasCollection = aliasCollectionUnderlying(tpe).isDefined

        // A map entry's key is a scalar; its value a scalar or a message with an instance.
        def entryElem(entryType: TypeRepr, message: Boolean): Option[Elem] =
          builtinKind(entryType) match
            case Some(kind) if kind <= KFloat => Some(Elem.Scalar(kind))
            case Some(kind)                   => Some(Elem.Chunk(kind))

            case None if message =>
              entryType.asType match
                case '[entry] =>
                  resolve[entry](cache) match
                    case Some(instance)
                      if !instance.isInstanceOf[Inlinable.IterableInlinable[?]] &&
                        !cache.active.contains(entryType.show) =>
                        Some(Elem.Nested(instance, entryType))

                    case _ =>
                      None

            case None =>
              None

        if isMap then unannotated(tpe) match
          case AppliedType(_, List(keyType0, valueType0)) =>
            val keyType = keyType0.dealias
            val valueType = valueType0.dealias

            (entryElem(keyType, false), entryElem(valueType, true)) match
              case (Some(key), Some(value)) => Plan.MapGather(key, keyType, value, valueType)
              case _                        => Plan.Seam

          case _ =>
            Plan.Seam

        else if tpe <:< TypeRepr.of[Iterable[Any]] || isAliasCollection
        then unannotated(tpe) match
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
                        if !instance.isInstanceOf[Inlinable.IterableInlinable[?]] &&
                          !cache.active.contains(elementType.show) =>
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
        !applied &&
          classSymbol.flags.is(Flags.Sealed) &&
          children.nonEmpty &&
          children.forall: child => child.isClassDef && child.flags.is(Flags.Case)

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
    productShaped(tpe) && tpe.classSymbol.exists(fieldNumbersOf(_).isDefined)

  // The shape of an inlinable message, short of its field numbers: a `@field` annotation is
  // not yet a literal application while the caller is still being typed, as it is when a
  // `Generates` is searched for, so only the generator itself reads the numbers.
  private def productShaped(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean =
    import quotes.reflect.*

    tpe.classSymbol.exists: classSymbol =>
      classSymbol.flags.is(Flags.Case) &&
        !classSymbol.owner.isTerm &&
        (tpe match { case AppliedType(_, _) => false case _ => true }) &&
        classSymbol.primaryConstructor.paramSymss
          . filterNot(_.exists(_.isTypeParam)).length == 1

  // ── The product generator ──────────────────────────────────────────────
  // Self-contained monomorphic message parsing, mirroring the AST record
  // decoder's semantics: typed local slots and seen flags, a literal
  // field-number dispatch, last-occurrence-wins scalars, per-occurrence
  // gathering (packed or unpacked) for repeated fields, declared defaults
  // then proto3 absent semantics for missing fields, direct construction.
  private[locomotion] def productBody[product: Type](reader: Expr[ProtobufReader])
    ( using Quotes )
  :   Expr[product] =

    productBody[product](reader, continued())

  private def productBody[product: Type](reader: Expr[ProtobufReader], cache: Cache)
    ( using Quotes )
  :   Expr[product] =

    import quotes.reflect.*

    val tpe = TypeRepr.of[product].dealias
    cache.active += tpe.show

    try productBody0[product](reader, cache) finally cache.active -= tpe.show

  private def productBody0[product: Type](reader: Expr[ProtobufReader], cache: Cache)
    ( using Quotes )
  :   Expr[product] =

    import quotes.reflect.*

    val tpe = TypeRepr.of[product].dealias

    if !productSupported(tpe) then
      report.errorAndAbort
        ( s"locomotion: ${tpe.show} is not an inlinable message (a non-generic, top-level or " +
          "object-nested case class with a single parameter list and distinct, statically " +
          "readable field numbers); use a `Decodable in Protobuf`" )

    val classSymbol = tpe.classSymbol.get
    val ctor = classSymbol.primaryConstructor
    val fields = classSymbol.caseFields
    val arity = fields.length
    val fieldTypes: List[TypeRepr] = fields.map: field => tpe.memberType(field).dealias
    val numbers: List[Int] = fieldNumbersOf(classSymbol).get
    val plans: List[Plan] = fieldTypes.map(planFor(_, cache))

    def body
      ( tactic: Expr[Tactic[Protobuf.Error]],
        parser: Expr[ProtobufParser] )
    :   Expr[product] =

      val owner = Symbol.spliceOwner

      val slots = List.range(0, arity).map: index =>
        Symbol.newVal(owner, "slot"+index, fieldTypes(index), Flags.Mutable, Symbol.noSymbol)

      val seens = List.range(0, arity).map: index =>
        Symbol.newVal(owner, "seen"+index, TypeRepr.of[Boolean], Flags.Mutable, Symbol.noSymbol)

      // A numeric leaf with no declared default needs no `seen` flag: its slot starts at the
      // proto3 zero an absent field decodes to, so the record loop can skip the store and
      // the finalizer the test. (A declared default is read at expansion from the
      // companion's default-argument method.)
      val companion = classSymbol.companionModule

      def declaresDefault(index: Int): Boolean =
        companion.methodMember("$lessinit$greater$default$" + (index + 1)).nonEmpty

      def needsSeen(index: Int): Boolean = plans(index) match
        case Plan.Leaf(kind) => kind > KFloat || declaresDefault(index)
        case _               => true

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

      // The proto3 zero value of a builtin scalar — what an absent field
      // decodes to on the AST path (`decoded(Protobuf.Absent)`).
      def zeroValue(kind: Int): Expr[Any] = kind match
        case KInt     => Expr(0)
        case KLong    => Expr(0L)
        case KBoolean => Expr(false)
        case KDouble  => Expr(0.0)
        case KFloat   => Expr(0.0f)
        case KText    => '{Text("")}
        case KData    => '{Array.empty[Byte]}

      // One scalar field read, dispatching on the tag's wire code — the
      // parser handles the natural fast path and payload-window fallback.
      def scalarRead(kind: Int, code: Expr[Int]): Expr[Any] = kind match
        case KInt     => '{$parser.directLong($code)(using $tactic).toInt}
        case KLong    => '{$parser.directLong($code)(using $tactic)}
        case KBoolean => '{$parser.directLong($code)(using $tactic) != 0L}
        case KDouble  => '{$parser.directDouble($code)(using $tactic)}
        case KFloat   => '{$parser.directFloat($code)(using $tactic)}
        case KText    => '{Text($parser.directString($code)(using $tactic))}
        case KData    => '{$parser.directData($code)(using $tactic)}

      // One packed element read, in place within the packed run's window.
      def packedRead(kind: Int): Expr[Any] = kind match
        case KInt     => '{$parser.directVarint()(using $tactic).toInt}
        case KLong    => '{$parser.directVarint()(using $tactic)}
        case KBoolean => '{$parser.directVarint()(using $tactic) != 0L}
        case KDouble  => '{java.lang.Double.longBitsToDouble($parser.directFixed64()(using $tactic))}
        case KFloat   => '{java.lang.Float.intBitsToFloat($parser.directFixed32()(using $tactic))}

      // Per-field gathering state: a typed builder for `Gather` fields, a
      // wire-value buffer for `Seam` fields.
      // The element a gathered field's builder takes: the element, or a map's entry pair.
      def gatheredType(index: Int): Option[TypeRepr] = plans(index) match
        case Plan.Gather(_, elementType0) =>
          Some(elementType0.asInstanceOf[TypeRepr])

        case Plan.MapGather(_, keyType0, _, valueType0) =>
          Some(TypeRepr.of[Tuple2].appliedTo
            ( List(keyType0.asInstanceOf[TypeRepr], valueType0.asInstanceOf[TypeRepr]) ))

        case _ =>
          None

      val builders: scala.collection.immutable.Map[Int, Symbol] =
        List.range(0, arity).flatMap: index =>
          gatheredType(index).map: elementType =>
            val builderType =
              TypeRepr.of[scm.Builder].appliedTo(List(elementType, fieldTypes(index)))

            index -> Symbol.newVal
              ( owner, "builder"+index, builderType, Flags.EmptyFlags, Symbol.noSymbol )

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
        gatheredType(index).map: elementType =>
          (elementType.asType, fieldTypes(index).asType) match
            case ('[element], '[fieldType]) =>
              ValDef(builders(index), Some(newBuilder[element, fieldType].asTerm))

      val bufferDefs: List[Statement] = List.range(0, arity).flatMap: index =>
        plans(index) match
          case Plan.Seam =>
            Some(ValDef(buffers(index), Some('{scm.ListBuffer[Protobuf]()}.asTerm)))

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

            case Plan.Gather(_, _) | Plan.MapGather(_, _, _, _) =>
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
                    ' {
                        val saved = $parser.directEnterField($code)(using $tactic)

                        val result: fieldType =
                          ${ instance.asInstanceOf[Inlinable { type Self = fieldType }]
                               . parse(reader) }

                        $parser.directLeaveField(saved)(using $tactic)
                        result
                      }

              case Plan.NestedRuntime(parsable) =>
                fieldTypes(index).asType match
                  case '[fieldType] =>
                    val instance = parsable.asInstanceOf[Expr[Any]]

                    ' {
                        val saved = $parser.directEnterField($code)(using $tactic)

                        val result: fieldType =
                          Protobuf.Parsable.parseField[fieldType]
                            ( $instance.asInstanceOf[AnyRef], $reader.asInstanceOf[AnyRef] )

                        $parser.directLeaveField(saved)(using $tactic)
                        result
                      }

              case Plan.OptionalNested(instance, innerType0) =>
                innerType0.asInstanceOf[TypeRepr].asType match
                  case '[innerType] =>
                    ' {
                        val saved = $parser.directEnterField($code)(using $tactic)

                        val result: innerType =
                          ${ instance.asInstanceOf[Inlinable { type Self = innerType }]
                               . parse(reader) }

                        $parser.directLeaveField(saved)(using $tactic)
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

                        // An array field's builder is the stdlib's unboxed one for the
                        // element: appended through its own class, not the `Builder`
                        // interface, so no element is boxed on the way in.
                        def append(item: Expr[element]): Expr[Unit] =
                          if isArrayType(fieldTypes(index)) then kind match
                            case KInt =>
                              '{$builder.asInstanceOf[scm.ArrayBuilder.ofInt].addOne(${item.asExprOf[Int]})}

                            case KLong =>
                              '{$builder.asInstanceOf[scm.ArrayBuilder.ofLong].addOne(${item.asExprOf[Long]})}

                            case KBoolean =>
                              '{$builder.asInstanceOf[scm.ArrayBuilder.ofBoolean].addOne(${item.asExprOf[Boolean]})}

                            case KDouble =>
                              '{$builder.asInstanceOf[scm.ArrayBuilder.ofDouble].addOne(${item.asExprOf[Double]})}

                            case _ =>
                              '{$builder.asInstanceOf[scm.ArrayBuilder.ofFloat].addOne(${item.asExprOf[Float]})}
                          else '{$builder += $item}

                        ' {
                            if $code == 2 && ${Expr(natural)} != 2 then
                              val saved = $parser.directEnterField(2)(using $tactic)

                              while !$parser.directAtLimit do
                                ${append(packedRead(kind).asExprOf[element])}

                              $parser.directLeaveField(saved)(using $tactic)
                            else
                              ${append(scalarRead(kind, code).asExprOf[element])}
                          }

                      case Elem.Chunk(kind) =>
                        '{$builder += ${scalarRead(kind, code).asExprOf[element]}}

                      case Elem.Nested(instance0, _) =>
                        val instance =
                          instance0.asInstanceOf[Inlinable { type Self = element }]

                        ' {
                            val saved = $parser.directEnterField($code)(using $tactic)
                            val result: element = ${instance.parse(reader)}
                            $parser.directLeaveField(saved)(using $tactic)
                            $builder += result
                          }

                      case Elem.Runtime(parsable) =>
                        val instance = parsable.asInstanceOf[Expr[Any]]

                        ' {
                            val saved = $parser.directEnterField($code)(using $tactic)

                            val result: element =
                              Protobuf.Parsable.parseField[element]
                                ( $instance.asInstanceOf[AnyRef], $reader.asInstanceOf[AnyRef] )

                            $parser.directLeaveField(saved)(using $tactic)
                            $builder += result
                          }

                      case Elem.Seam =>
                        val occurrence =
                          seamDecode[element]('{$parser.directWire($code)(using $tactic)})

                        '{$builder += $occurrence}

              case Plan.MapGather(key, keyType0, value, valueType0) =>
                val keyType = keyType0.asInstanceOf[TypeRepr]
                val valueType = valueType0.asInstanceOf[TypeRepr]

                // One entry's field as its decoder receives it, and the value it takes when
                // the entry omits it — the zero of a scalar, the empty message of a nested.
                def entryRead(elem: Elem, tpe: TypeRepr, code: Expr[Int]): Expr[Any] =
                  elem match
                    case Elem.Scalar(kind) => scalarRead(kind, code)
                    case Elem.Chunk(kind)  => scalarRead(kind, code)

                    case Elem.Nested(instance0, _) =>
                      tpe.asType match
                        case '[entry] =>
                          val instance = instance0.asInstanceOf[Inlinable { type Self = entry }]

                          ' {
                              val saved = $parser.directEnterField($code)(using $tactic)
                              val result: entry = ${instance.parse(reader)}
                              $parser.directLeaveField(saved)(using $tactic)
                              result
                            }

                    case _ =>
                      report.errorAndAbort("locomotion: an unreadable map entry")

                def entryAbsent(elem: Elem, tpe: TypeRepr): Expr[Any] = elem match
                  case Elem.Scalar(kind) => zeroValue(kind)
                  case Elem.Chunk(kind)  => zeroValue(kind)

                  case Elem.Nested(instance0, _) =>
                    tpe.asType match
                      case '[entry] =>
                        instance0.asInstanceOf[Inlinable { type Self = entry }].absent(tactic)

                  case _ =>
                    report.errorAndAbort("locomotion: an unreadable map entry")

                (keyType.asType, valueType.asType, fieldTypes(index).asType) match
                  case ('[keyT], '[valueT], '[fieldType]) =>
                    val builder =
                      Ref(builders(index)).asExprOf[scm.Builder[(keyT, valueT), fieldType]]

                    ' {
                        val saved = $parser.directEnterField($code)(using $tactic)
                        var entryKey: keyT = ${entryAbsent(key, keyType).asExprOf[keyT]}
                        var entryValue: valueT = ${entryAbsent(value, valueType).asExprOf[valueT]}

                        // The entry's fields, last occurrence winning, as `mapDecodable` reads
                        // them from its field table.
                        while !$parser.directAtLimit do
                          val tag = $parser.directTag()(using $tactic)
                          val entryCode = tag & 7

                          (tag >>> 3) match
                            case 1 => entryKey = ${entryRead(key, keyType, 'entryCode).asExprOf[keyT]}
                            case 2 => entryValue = ${entryRead(value, valueType, 'entryCode).asExprOf[valueT]}
                            case _ => $parser.directSkipField(entryCode)(using $tactic)

                        $parser.directLeaveField(saved)(using $tactic)
                        $builder += ((entryKey, entryValue))
                      }

              case Plan.Seam =>
                '{()}

            Some(rhs.asTerm.changeOwner(method)))

      // The dispatch arms, one per field number. Scalars and nested values
      // overwrite their slot (last occurrence wins, as the AST accessors'
      // `single`); gathered fields append.
      def arms(code: Expr[Int]): List[CaseDef] = List.range(0, arity).map: index =>
        val body: Term = plans(index) match
          case Plan.Leaf(_) | Plan.Nested(_) | Plan.NestedRuntime(_) | Plan.OptionalLeaf(_)
            | Plan.OptionalNested(_, _) =>
            Block
              ( Assign(Ref(slots(index)), Apply(Ref(readDefs(index)), List(code.asTerm))) ::
                  (if needsSeen(index)
                   then List(Assign(Ref(seens(index)), Literal(BooleanConstant(true))))
                   else Nil),
                unit )

          case Plan.Gather(_, _) | Plan.MapGather(_, _, _, _) =>
            Block
              ( List
                  ( Apply(Ref(readDefs(index)), List(code.asTerm)),
                    Assign(Ref(seens(index)), Literal(BooleanConstant(true))) ),
                unit )

          case Plan.Seam =>
            val buffer = Ref(buffers(index)).asExprOf[scm.ListBuffer[Protobuf]]

            Block
              ( List
                  ( '{$buffer += $parser.directWire($code)(using $tactic)}.asTerm,
                    Assign(Ref(seens(index)), Literal(BooleanConstant(true))) ),
                unit )

        CaseDef(Literal(IntConstant(numbers(index))), None, body)

      def fallthrough(code: Expr[Int]) =
        CaseDef(Wildcard(), None, '{$parser.directSkipField($code)(using $tactic)}.asTerm)

      // The wire codes a field is expected with: its natural encoding, and for a packable
      // scalar both the packed run and the bare value. A field read with another code, a
      // seam field, and an unknown number all take the general dispatch.
      def naturalCodes(index: Int): List[Int] = plans(index) match
        case Plan.Leaf(kind)                          => List(naturalCode(kind))
        case Plan.OptionalLeaf(kind)                  => List(naturalCode(kind))
        case Plan.Gather(Elem.Scalar(kind), _)        => List(2, naturalCode(kind)).distinct
        case Plan.Gather(Elem.Chunk(kind), _)         => List(naturalCode(kind))
        case Plan.Gather(_, _)                        => List(2)
        case Plan.Nested(_) | Plan.NestedRuntime(_)   => List(2)
        case Plan.OptionalNested(_, _)                => List(2)
        case Plan.MapGather(_, _, _, _)               => List(2)
        case Plan.Seam                                => Nil

      // The record loop: one tag per iteration. The whole tag is matched first — one dense
      // switch over the constants a well-formed message carries, each arm handing its field
      // reader the wire code it was declared with — and only another code, a seam field or
      // an unknown number reaches the dispatch on the field number.
      val step: Term =
        val tag = Symbol.newVal(owner, "tag", TypeRepr.of[Int], Flags.EmptyFlags, Symbol.noSymbol)
        val code = Symbol.newVal(owner, "code", TypeRepr.of[Int], Flags.EmptyFlags, Symbol.noSymbol)
        val tagRef = Ref(tag).asExprOf[Int]
        val codeRef = Ref(code).asExprOf[Int]

        val general: Term =
          Block
            ( List(ValDef(code, Some('{$tagRef & 7}.asTerm))),
              Match('{$tagRef >>> 3}.asTerm, arms(codeRef) ::: List(fallthrough(codeRef))) )

        val expected: List[CaseDef] = List.range(0, arity).flatMap: index =>
          naturalCodes(index).map: wireCode =>
            val literal = Expr(wireCode)

            val body: Term = plans(index) match
              case Plan.Leaf(_) | Plan.Nested(_) | Plan.NestedRuntime(_) | Plan.OptionalLeaf(_)
                | Plan.OptionalNested(_, _) =>
                Block
                  ( Assign(Ref(slots(index)), Apply(Ref(readDefs(index)), List(literal.asTerm)))
                      :: (if needsSeen(index)
                          then List(Assign(Ref(seens(index)), Literal(BooleanConstant(true))))
                          else Nil),
                    unit )

              case _ =>
                Block
                  ( List
                      ( Apply(Ref(readDefs(index)), List(literal.asTerm)),
                        Assign(Ref(seens(index)), Literal(BooleanConstant(true))) ),
                    unit )

            CaseDef(Literal(IntConstant((numbers(index) << 3) | wireCode)), None, body)

        Block
          ( List(ValDef(tag, Some('{$parser.directTag()(using $tactic)}.asTerm))),
            Match(tagRef.asTerm, expected ::: List(CaseDef(Wildcard(), None, general))) )

      val loop: List[Statement] =
        List(While('{!$parser.directAtLimit}.asTerm, step))

      // The declared-default-then-absent expression per field. A frozen array field takes
      // its absent value directly: the default probe cannot be typed over `Array[T]^{}`
      // under capture checking (its `Optionality` loses the capture set), and the ADT path
      // decodes an absent packed field as empty regardless.
      def defaultOr(index: Int, absent: Expr[Any]): Expr[Any] =
        fieldTypes(index).asType match
          case '[fieldType] =>
            if isFrozenArray(fieldTypes(index)) then absent.asExprOf[fieldType] else
              ' {
                  val declared = wisteria.internal.default[product, fieldType](${Expr(index)})

                  if !declared.absent then declared.asInstanceOf[fieldType]
                  else ${absent.asExprOf[fieldType]}
                }

      def planAbsent(index: Int, tactic: Expr[Tactic[Protobuf.Error]]): Expr[Any] =
        plans(index) match
          case Plan.Leaf(kind) =>
            zeroValue(kind)

          case Plan.OptionalLeaf(_) | Plan.OptionalNested(_, _) =>
            '{vacuous.Unset}

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

                ' {
                    val window = $parser.directEmptyWindow()

                    val result: fieldType =
                      Protobuf.Parsable.parseField[fieldType]
                        ( $instance.asInstanceOf[AnyRef], $reader.asInstanceOf[AnyRef] )

                    $parser.directRestore(window)
                    result
                  }

          case Plan.Gather(_, _) | Plan.MapGather(_, _, _, _) =>
            fieldTypes(index).asType match
              case '[fieldType] =>
                '{ ${Ref(builders(index)).asExpr}
                    . asInstanceOf[scm.Builder[?, fieldType]].result() }

          case Plan.Seam =>
            fieldTypes(index).asType match
              case '[fieldType] =>
                seamDecode[fieldType]('{Protobuf.Absent})

      // Finalize each slot: missing fields take their declared default or
      // proto3 absent value; gathered fields collect their builder; seam
      // fields decode their gathered occurrences exactly as the AST path
      // decodes `Repeated(values)`.
      val finalizers: List[Term] = List.range(0, arity).map: index =>
        val seen = Ref(seens(index)).asExprOf[Boolean]

        plans(index) match
          case Plan.Leaf(_) if !needsSeen(index) =>
            unit

          case Plan.Leaf(_) | Plan.Nested(_) | Plan.NestedRuntime(_) | Plan.OptionalLeaf(_)
            | Plan.OptionalNested(_, _) =>
            If
              ( '{!$seen}.asTerm,
                Assign
                  ( Ref(slots(index)),
                    defaultOr(index, planAbsent(index, tactic)).asTerm ),
                unit )

          case Plan.Gather(_, _) | Plan.MapGather(_, _, _, _) =>
            fieldTypes(index).asType match
              case '[fieldType] =>
                val collected =
                  '{ ${Ref(builders(index)).asExpr}
                      . asInstanceOf[scm.Builder[?, fieldType]].result() }

                Assign
                  ( Ref(slots(index)),
                    ' {
                        if $seen then $collected
                        else ${defaultOr(index, collected).asExprOf[fieldType]}
                      }.asTerm )

          case Plan.Seam =>
            fieldTypes(index).asType match
              case '[fieldType] =>
                val buffer = Ref(buffers(index)).asExprOf[scm.ListBuffer[Protobuf]]

                val absent: Expr[fieldType] =
                  defaultOr(index, seamDecode[fieldType]('{Protobuf.Absent}))
                  . asExprOf[fieldType]

                Assign
                  ( Ref(slots(index)),
                    ' {
                        if $seen then
                          ${seamDecode[fieldType]('{Protobuf.Repeated(($buffer.toList).to(proscenium.List))})}
                        else
                          $absent
                      }.asTerm )

      // A frozen array slot reads back without its capture set, so it is reasserted at the
      // one place it leaves the parser: the record's constructor.
      val construct: Term =
        Apply
          ( Select(New(Inferred(tpe)), ctor),
            List.range(0, arity).map: index =>
              if isFrozenArray(fieldTypes(index)) then fieldTypes(index).asType match
                case '[fieldType] => '{${Ref(slots(index)).asExpr}.asInstanceOf[fieldType]}.asTerm
              else Ref(slots(index)) )

      Block
        ( slotDefs ::: seenDefs ::: builderDefs ::: bufferDefs ::: readDefDefs ::: loop :::
          finalizers,
          construct )

      . asExprOf[product]

    // The parser is not bound to a local: a `val` holding the exclusive parser is an alias of
    // the reader it came from, which the separation checker then holds read-only wherever the
    // reader is used again (a nested or runtime-parsed field), and a capture-checked-only
    // module admits no `update` call through such a binding at all. Each rim call reaches the
    // parser through the reader's accessor instead — a field read the JIT hoists.
    ' {
        val tactic = infer[Tactic[Protobuf.Error]]
        ${body('tactic, '{ProtobufParser.of($reader.rawParser)})}
      }

  // A wholly-absent message — what `decoded(Protobuf.Absent)` produces on
  // the AST path: every field takes its declared default or proto3 absent
  // value.
  private[locomotion] def productAbsent[product: Type]
    ( tactic: Expr[Tactic[Protobuf.Error]] )
    ( using Quotes )
  :   Expr[product] =

    import quotes.reflect.*

    val cache: Cache = Cache()
    val tpe = TypeRepr.of[product].dealias
    cache.active += tpe.show
    val classSymbol = tpe.classSymbol.get
    val ctor = classSymbol.primaryConstructor
    val fields = classSymbol.caseFields
    val arity = fields.length
    val fieldTypes: List[TypeRepr] = fields.map: field => tpe.memberType(field).dealias
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
              case KText    => '{Text("")}
              case KData    => '{Array.empty[Byte]}

            case Plan.Nested(instance) =>
              instance.asInstanceOf[Inlinable { type Self = fieldType }].absent(tactic)

            case Plan.OptionalLeaf(_) | Plan.OptionalNested(_, _) =>
              '{vacuous.Unset}

            case Plan.Gather(_, elementType0) =>
              val elementType = elementType0.asInstanceOf[TypeRepr]

              elementType.asType match
                case '[element] => '{${newBuilder[element, fieldType]}.result()}

            case Plan.MapGather(_, keyType0, _, valueType0) =>
              ( keyType0.asInstanceOf[TypeRepr].asType,
                valueType0.asInstanceOf[TypeRepr].asType ) match
                case ('[keyT], '[valueT]) => '{${newBuilder[(keyT, valueT), fieldType]}.result()}

            case Plan.Seam =>
              seamDecode[fieldType]('{Protobuf.Absent})

          ' {
              val declared = wisteria.internal.default[product, fieldType](${Expr(index)})

              if !declared.absent then declared.asInstanceOf[fieldType]
              else ${absent.asExprOf[fieldType]}
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

    sumBody[sum](reader, continued())

  private def sumBody[sum: Type](reader: Expr[ProtobufReader], cache: Cache)(using Quotes)
  :   Expr[sum] =

    import quotes.reflect.*

    cache.active += TypeRepr.of[sum].dealias.show

    try sumBody0[sum](reader, cache) finally cache.active -= TypeRepr.of[sum].dealias.show

  private def sumBody0[sum: Type](reader: Expr[ProtobufReader], cache: Cache)(using Quotes)
  :   Expr[sum] =

    import quotes.reflect.*

    val variants = sumVariants(TypeRepr.of[sum].dealias).getOrElse:
      report.errorAndAbort
        ( s"locomotion: ${TypeRepr.of[sum].show} is not an inlinable oneof (a non-generic " +
          "sealed type whose variants are all case classes)" )

    val arity = variants.length

    def dispatch(index: Int, chosen: Expr[Int], tactic: Expr[Tactic[Protobuf.Error]])
    :   Expr[sum] =

      if index == arity then
        '{abort(Protobuf.Error(Protobuf.Error.Reason.MissingField(0)))(using $tactic)}
      else
        variants(index)(1).asType match
          case '[type variantType <: sum; variantType] =>
            val instance = resolve[variantType](cache).getOrElse:
              report.errorAndAbort
                ( s"locomotion: no Inlinable for variant ${variants(index)(0)}" )

            . asInstanceOf[Inlinable { type Self = variantType }]

            ' {
                if $chosen == ${Expr(index + 1)} then
                  val parsed: variantType = ${instance.parse(reader)}
                  parsed
                else
                  ${dispatch(index + 1, chosen, tactic)}
              }

    ' {
        val tactic = infer[Tactic[Protobuf.Error]]
        var chosen = Int.MaxValue
        var chosenStart = 0
        var chosenEnd = 0

        // The message is buffered whole before the scan, so the chosen extent is still in
        // the buffer when the scan returns to it.
        ProtobufParser.of($reader.rawParser).directBufferWindow()(using tactic)

        while !ProtobufParser.of($reader.rawParser).directAtLimit do
          val tag = ProtobufParser.of($reader.rawParser).directTag()(using tactic)
          val number = tag >>> 3
          val saved = ProtobufParser.of($reader.rawParser).directEnterField(tag & 7)(using tactic)

          if number >= 1 && number <= ${Expr(arity)} && number <= chosen then
            chosen = number
            chosenStart = ProtobufParser.of($reader.rawParser).directMark
            chosenEnd = ProtobufParser.of($reader.rawParser).directBoundary

          ProtobufParser.of($reader.rawParser).directLeaveField(saved)(using tactic)

        if chosen == Int.MaxValue
        then abort(Protobuf.Error(Protobuf.Error.Reason.MissingField(0)))(using tactic)
        else
          val outer = ProtobufParser.of($reader.rawParser).directWindow(chosenStart, chosenEnd)
          val result: sum = ${dispatch(0, 'chosen, 'tactic)}
          ProtobufParser.of($reader.rawParser).directRestore(outer)
          result
      }

  // ── The printing generators ────────────────────────────────────────────
  // The product printer follows the product parser's plans, so a message
  // prints exactly the fields it parses: a builtin scalar `Leaf` is sized
  // and written in place; a `Nested` instance that prints is measured into
  // a slot of the size table reserved before its children and written after
  // its length prefix; a `Gather` is a packed run (numeric scalars, one
  // `Len` field whose size is recorded even when empty, so the write pass
  // needs no second emptiness test), one field per element (strings, bytes,
  // printing messages), or the whole-field seam; and the `Seam` encodes the
  // field once, through its `Encodable in Protobuf`, stashing the wire value
  // between the passes. Both passes visit the fields in declaration order,
  // which is the order the derived `Encodable` writes them, so the bytes are
  // the derivation's.
  //
  // The printer and the size table are neutral carriers, reasserted at every
  // use (`ProtobufPrinter.of`, `ProtobufSizes.of`) and never bound to a
  // local, for the reason `productBody` gives for the parser.

  private def seamEncode[fieldType: Type](value: Expr[fieldType])(using Quotes)
  :   Expr[Protobuf] =

    import quotes.reflect.*

    Implicits.search(TypeRepr.of[fieldType is Encodable in Protobuf]).absolve match
      case success: ImplicitSearchSuccess =>
        success.tree.asExpr.absolve match
          case '{$found: any} =>
            '{$found.asInstanceOf[fieldType is Encodable in Protobuf].encoded($value)}

      case failure: ImplicitSearchFailure =>
        report.errorAndAbort
          ( s"locomotion: no Encodable in Protobuf for ${TypeRepr.of[fieldType].show}: " +
            failure.explanation )

  // A nominal `Protobuf.Printable` for a field type — the recursion tie and
  // the composition point for hand-written printers, as `parsableFor`.
  private def printableFor[fieldType: Type](using Quotes): Option[Expr[Any]] =
    import quotes.reflect.*

    rootFor[fieldType].orElse:
      Implicits.search(TypeRepr.of[fieldType is Protobuf.Printable]) match
        case success: ImplicitSearchSuccess if !blanket(success.tree) => Some(success.tree.asExpr)
        case _                                                        => None

  // A field's tag, known at expansion: a single literal byte for a field number up to 15 —
  // nearly every field — and a varint beyond.
  private def tagWrite(number: Int, code: Int, printer: Expr[AnyRef])(using Quotes)
  :   Expr[Unit] =

    val tag = (number << 3) | code

    if tag < 0x80 then '{ProtobufPrinter.of($printer).byte(${Expr(tag)})}
    else '{ProtobufPrinter.of($printer).varint(${Expr(tag.toLong)})}

  private def wireCode(kind: Int): Int = kind match
    case KDouble        => 1
    case KFloat         => 5
    case KText | KData  => 2
    case _              => 0

  // The encoded length of a builtin scalar's payload, recording a string's.
  private def scalarSize(kind: Int, value: Expr[Any], sizes: Expr[AnyRef])(using Quotes)
  :   Expr[Int] =

    kind match
      case KInt     => '{ProtobufPrinter.varintSize(${value.asExprOf[Int]}.toLong)}
      case KLong    => '{ProtobufPrinter.varintSize(${value.asExprOf[Long]})}
      case KBoolean => Expr(1)
      case KDouble  => Expr(8)
      case KFloat   => Expr(4)

      case KText =>
        ' {
            val length = ProtobufSizes.of($sizes).stashText(${value.asExprOf[Text]}.s)
            ProtobufPrinter.varintSize(length.toLong) + length
          }

      case KData =>
        ' {
            val length = ${value.asInstanceOf[Expr[Data]]}.length
            ProtobufPrinter.varintSize(length.toLong) + length
          }

  // The payload of a builtin scalar, after its tag.
  private def scalarWrite
    ( kind: Int, value: Expr[Any], printer: Expr[AnyRef], sizes: Expr[AnyRef] )
    ( using Quotes )
  :   Expr[Unit] =

    kind match
      case KInt  => '{ProtobufPrinter.of($printer).varint(${value.asExprOf[Int]}.toLong)}
      case KLong => '{ProtobufPrinter.of($printer).varint(${value.asExprOf[Long]})}

      case KBoolean =>
        '{ProtobufPrinter.of($printer).varint(if ${value.asExprOf[Boolean]} then 1L else 0L)}

      case KDouble =>
        ' {
            ProtobufPrinter.of($printer).fixed64
              ( java.lang.Double.doubleToLongBits(${value.asExprOf[Double]}) )
          }

      case KFloat =>
        ' {
            ProtobufPrinter.of($printer).fixed32
              ( java.lang.Float.floatToIntBits(${value.asExprOf[Float]}) )
          }

      case KText =>
        '{ProtobufPrinter.of($printer).stashedText(ProtobufSizes.of($sizes))}

      case KData =>
        '{ProtobufPrinter.of($printer).chunk(${value.asInstanceOf[Expr[Data]]})}

  // The elements of a gathered field, visited through an iterator by a local recursive
  // function: no lambda, so nothing is boxed and no `var` is captured.
  private def summed[element: Type](iterable: Expr[Iterable[element]])
    ( size: Expr[element] => Expr[Int] )
    ( using Quotes )
  :   Expr[Int] =

    ' {
        val iterator = $iterable.iterator

        def recur(total: Int): Int =
          if iterator.hasNext then
            val item = iterator.next()
            recur(total + ${size('item)})
          else
            total

        recur(0)
      }

  private def visited[element: Type](iterable: Expr[Iterable[element]])
    ( write: Expr[element] => Expr[Unit] )
    ( using Quotes )
  :   Expr[Unit] =

    ' {
        val iterator = $iterable.iterator

        def recur(): Unit =
          if iterator.hasNext then
            val item = iterator.next()
            ${write('item)}
            recur()

        recur()
      }

  // The same two walks over a frozen array, by index: a `readUnchecked` on a concrete element
  // type is an unboxed array load.
  private def summedArray[element: Type](array: Expr[Array[element]])
    ( size: Expr[element] => Expr[Int] )
    ( using Quotes )
  :   Expr[Int] =

    ' {
        def recur(index: Int, total: Int): Int =
          if index < $array.length
          then recur(index + 1, total + ${size('{$array.readUnchecked(index)})})
          else total

        recur(0, 0)
      }

  private def visitedArray[element: Type](array: Expr[Array[element]])
    ( write: Expr[element] => Expr[Unit] )
    ( using Quotes )
  :   Expr[Unit] =

    ' {
        def recur(index: Int): Unit =
          if index < $array.length then
            ${write('{$array.readUnchecked(index)})}
            recur(index + 1)

        recur(0)
      }

  // Whether a map entry's side has a generated printer: a scalar always, a message when its
  // instance prints.
  private def entryPrints(elem: Elem): Boolean = elem match
    case Elem.Scalar(_)           => true
    case Elem.Chunk(_)            => true
    case Elem.Nested(instance, _) => instance.prints
    case _                        => false

  // A message in a field position: its size goes into a slot reserved before its children
  // are measured, and the field is its tag, the length prefix and the body.
  private def framedSize(number: Int, body: Expr[Int], sizes: Expr[AnyRef])(using Quotes)
  :   Expr[Int] =

    ' {
        val slot = ProtobufSizes.of($sizes).reserve()
        val size = $body
        ProtobufSizes.of($sizes).set(slot, size)
        ${Expr(ProtobufPrinter.tagSize(number))} + ProtobufPrinter.varintSize(size.toLong) + size
      }

  private def framedWrite
    ( number: Int, body: Expr[Unit], printer: Expr[AnyRef], sizes: Expr[AnyRef] )
    ( using Quotes )
  :   Expr[Unit] =

    ' {
        ${tagWrite(number, 2, printer)}
        ProtobufPrinter.of($printer).varint(ProtobufSizes.of($sizes).next().toLong)
        $body
      }

  // The product's reflected shape, shared by both passes: its fields, their types and
  // numbers, and their plans.
  private def productShape[product: Type](cache: Cache)(using Quotes)
  :   (List[quotes.reflect.Symbol], List[quotes.reflect.TypeRepr], List[Int], List[Plan]) =

    import quotes.reflect.*

    val tpe = TypeRepr.of[product].dealias

    if !productSupported(tpe) then
      report.errorAndAbort
        ( s"locomotion: ${tpe.show} is not an inlinable message (a non-generic, top-level or " +
          "object-nested case class with a single parameter list and distinct, statically " +
          "readable field numbers); use an `Encodable in Protobuf`" )

    val classSymbol = tpe.classSymbol.get
    val fields = classSymbol.caseFields
    val fieldTypes = fields.map: field => tpe.memberType(field).dealias

    (fields, fieldTypes, fieldNumbersOf(classSymbol).get, fieldTypes.map(planFor(_, cache)))

  // Runs a generator with the product on the expansion stack, as `productBody` does.
  private def active[product: Type, result](cache: Cache)(generate: => result)(using Quotes)
  :   result =

    val name = quotes.reflect.TypeRepr.of[product].dealias.show
    cache.active += name
    try generate finally cache.active -= name

  private[locomotion] def productMeasure[product: Type](value: Expr[product], sizes: Expr[AnyRef])
    ( using Quotes )
  :   Expr[Int] =

    productMeasure[product](value, sizes, continued())

  private def productMeasure[product: Type]
    ( value: Expr[product], sizes: Expr[AnyRef], cache: Cache )
    ( using Quotes )
  :   Expr[Int] =

    import quotes.reflect.*

    active[product, Expr[Int]](cache):
      val (fields, fieldTypes, numbers, plans) = productShape[product](cache)

      val sizesOf: List[Expr[Int]] = List.range(0, fields.length).map: index =>
        val number = numbers(index)
        val tag = ProtobufPrinter.tagSize(number)
        val field = Select(value.asTerm, fields(index)).asExpr

        fieldTypes(index).asType match
          case '[fieldType] =>
            val typed = field.asExprOf[fieldType]

            def seam: Expr[Int] =
              ' {
                  val wire = ${seamEncode[fieldType](typed)}
                  ProtobufSizes.of($sizes).stash(wire)
                  ProtobufPrinter.fieldSize(${Expr(number)}, wire)
                }

            def nominal: Expr[Int] = printableFor[fieldType] match
              case Some(printable) =>
                framedSize
                  ( number,
                    ' {
                        Protobuf.Printable.measureField[fieldType]
                          ( $printable.asInstanceOf[AnyRef], $typed, $sizes )
                      },
                    sizes )

              case None =>
                seam

            plans(index) match
              case Plan.Leaf(kind) =>
                '{${Expr(tag)} + ${scalarSize(kind, typed, sizes)}}

              case Plan.OptionalLeaf(kind) =>
                optionalInner(fieldTypes(index)).get.asType match
                  case '[inner] =>
                    ' {
                        if ${typed.asExprOf[Optional[inner]]}.absent then 0
                        else
                          ${Expr(tag)} + ${scalarSize(kind, '{$typed.asInstanceOf[inner]}, sizes)}
                      }

              case Plan.Nested(instance) if instance.prints =>
                framedSize
                  ( number,
                    instance.asInstanceOf[Inlinable { type Self = fieldType }]
                    . measure(typed, sizes),
                    sizes )

              case Plan.Nested(_) | Plan.NestedRuntime(_) =>
                nominal

              case Plan.OptionalNested(instance, innerType0) if instance.prints =>
                innerType0.asInstanceOf[TypeRepr].asType match
                  case '[inner] =>
                    val present: Expr[inner] = '{$typed.asInstanceOf[inner]}

                    val framed =
                      framedSize
                        ( number,
                          instance.asInstanceOf[Inlinable { type Self = inner }]
                          . measure(present, sizes),
                          sizes )

                    '{if ${typed.asExprOf[Optional[inner]]}.absent then 0 else $framed}

              case Plan.Gather(element, elementType0) =>
                elementType0.asInstanceOf[TypeRepr].asType match
                  case '[element] =>
                    val iterable = '{$typed.asInstanceOf[Iterable[element]]}

                    // The walk over the field's elements: by index for a frozen array,
                    // through an iterator otherwise.
                    val sumOver: (Expr[element] => Expr[Int]) => Expr[Int] =
                      if isFrozenArray(fieldTypes(index))
                      then summedArray('{$typed.asInstanceOf[Array[element]]})
                      else summed(iterable)

                    element match
                      case Elem.Scalar(kind) =>
                        def each(item: Expr[element]): Expr[Int] = scalarSize(kind, item, sizes)
                        val runSize: Expr[Int] = sumOver(each)

                        ' {
                            val run = $runSize
                            ProtobufSizes.of($sizes).record(run)

                            if run == 0 then 0
                            else ${Expr(tag)} + ProtobufPrinter.varintSize(run.toLong) + run
                          }

                      case Elem.Chunk(kind) =>
                        def each(item: Expr[element]): Expr[Int] =
                          '{${Expr(tag)} + ${scalarSize(kind, item, sizes)}}

                        sumOver(each)

                      case Elem.Nested(instance0, _) if instance0.prints =>
                        val instance = instance0.asInstanceOf[Inlinable { type Self = element }]

                        def framed(item: Expr[element]): Expr[Int] =
                          framedSize(number, instance.measure(item, sizes), sizes)

                        sumOver(framed)

                      case Elem.Nested(_, _) | Elem.Runtime(_) | Elem.Seam =>
                        printableFor[element] match
                          case Some(printable) =>
                            def framed(item: Expr[element]): Expr[Int] =
                              framedSize
                                ( number,
                                  ' {
                                      Protobuf.Printable.measureField[element]
                                        ( $printable.asInstanceOf[AnyRef], $item, $sizes )
                                    },
                                  sizes )

                            sumOver(framed)

                          case None =>
                            seam

              case Plan.MapGather(key, keyType0, value, valueType0) =>
                val keyType = keyType0.asInstanceOf[TypeRepr]
                val valueType = valueType0.asInstanceOf[TypeRepr]

                // One entry field's size with its tag: a scalar in place, a message framed.
                def entrySize(elem: Elem, tpe: TypeRepr, number: Int, item: Expr[Any])
                :   Option[Expr[Int]] =

                  elem match
                    case Elem.Scalar(kind) =>
                      Some('{${Expr(ProtobufPrinter.tagSize(number))} + ${scalarSize(kind, item, sizes)}})

                    case Elem.Chunk(kind) =>
                      Some('{${Expr(ProtobufPrinter.tagSize(number))} + ${scalarSize(kind, item, sizes)}})

                    case Elem.Nested(instance0, _) if instance0.prints =>
                      tpe.asType match
                        case '[entry] =>
                          val instance = instance0.asInstanceOf[Inlinable { type Self = entry }]
                          Some(framedSize(number, instance.measure(item.asExprOf[entry], sizes), sizes))

                    case _ =>
                      None

                (keyType.asType, valueType.asType) match
                  case ('[keyT], '[valueT]) =>
                    val entries = '{$typed.asInstanceOf[Iterable[(keyT, valueT)]]}

                    def entry(item: Expr[(keyT, valueT)]): Option[Expr[Int]] =
                      for
                        keySize   <- entrySize(key, keyType, 1, '{$item._1})
                        valueSize <- entrySize(value, valueType, 2, '{$item._2})
                      yield framedSize(number, '{$keySize + $valueSize}, sizes)

                    // Both sides print, or the whole map takes the seam.
                    if entryPrints(key) && entryPrints(value)
                    then summed(entries): item => entry(item).get
                    else seam

              // A recursive message field (`Plan.Seam` here, its parser being on the
              // stack) prints through its own nominal instance; the rest of the seam —
              // maps, typed integers, optional collections, custom types — through their
              // `Encodable`.
              case _ =>
                nominal

      sizesOf.foldLeft[Expr[Int]](Expr(0)): (total, size) => '{$total + $size}

  private[locomotion] def productWrite[product: Type]
    ( value: Expr[product], printer: Expr[AnyRef], sizes: Expr[AnyRef] )
    ( using Quotes )
  :   Expr[Unit] =

    productWrite[product](value, printer, sizes, continued())

  private def productWrite[product: Type]
    ( value: Expr[product], printer: Expr[AnyRef], sizes: Expr[AnyRef], cache: Cache )
    ( using Quotes )
  :   Expr[Unit] =

    import quotes.reflect.*

    active[product, Expr[Unit]](cache):
      val (fields, fieldTypes, numbers, plans) = productShape[product](cache)

      val writes: List[Expr[Unit]] = List.range(0, fields.length).map: index =>
        val number = numbers(index)
        val field = Select(value.asTerm, fields(index)).asExpr

        fieldTypes(index).asType match
          case '[fieldType] =>
            val typed = field.asExprOf[fieldType]

            def seam: Expr[Unit] =
              ' {
                  ProtobufPrinter.of($printer).field
                    ( ${Expr(number)}, ProtobufSizes.of($sizes).unstash().asInstanceOf[Protobuf] )
                }

            def tagged(kind: Int, item: Expr[Any]): Expr[Unit] =
              ' {
                  ${tagWrite(number, wireCode(kind), printer)}
                  ${scalarWrite(kind, item, printer, sizes)}
                }

            def nominal: Expr[Unit] = printableFor[fieldType] match
              case Some(printable) =>
                framedWrite
                  ( number,
                    ' {
                        Protobuf.Printable.writeField[fieldType]
                          ( $printable.asInstanceOf[AnyRef], $typed, $printer, $sizes )
                      },
                    printer, sizes )

              case None =>
                seam

            plans(index) match
              case Plan.Leaf(kind) =>
                tagged(kind, typed)

              case Plan.OptionalLeaf(kind) =>
                optionalInner(fieldTypes(index)).get.asType match
                  case '[inner] =>
                    ' {
                        if !${typed.asExprOf[Optional[inner]]}.absent
                        then ${tagged(kind, '{$typed.asInstanceOf[inner]})}
                      }

              case Plan.Nested(instance) if instance.prints =>
                framedWrite
                  ( number,
                    instance.asInstanceOf[Inlinable { type Self = fieldType }]
                    . write(typed, printer, sizes),
                    printer, sizes )

              case Plan.Nested(_) | Plan.NestedRuntime(_) =>
                nominal

              case Plan.OptionalNested(instance, innerType0) if instance.prints =>
                innerType0.asInstanceOf[TypeRepr].asType match
                  case '[inner] =>
                    val present: Expr[inner] = '{$typed.asInstanceOf[inner]}

                    val framed =
                      framedWrite
                        ( number,
                          instance.asInstanceOf[Inlinable { type Self = inner }]
                          . write(present, printer, sizes),
                          printer, sizes )

                    '{if !${typed.asExprOf[Optional[inner]]}.absent then $framed}

              case Plan.Gather(element, elementType0) =>
                elementType0.asInstanceOf[TypeRepr].asType match
                  case '[element] =>
                    val iterable = '{$typed.asInstanceOf[Iterable[element]]}

                    val eachOver: (Expr[element] => Expr[Unit]) => Expr[Unit] =
                      if isFrozenArray(fieldTypes(index))
                      then visitedArray('{$typed.asInstanceOf[Array[element]]})
                      else visited(iterable)

                    element match
                      case Elem.Scalar(kind) =>
                        def each(item: Expr[element]): Expr[Unit] =
                          scalarWrite(kind, item, printer, sizes)

                        val walk: Expr[Unit] = eachOver(each)

                        ' {
                            val run = ProtobufSizes.of($sizes).next()

                            if run > 0 then
                              ${tagWrite(number, 2, printer)}
                              ProtobufPrinter.of($printer).varint(run.toLong)
                              $walk
                          }

                      case Elem.Chunk(kind) =>
                        eachOver: item => tagged(kind, item)

                      case Elem.Nested(instance0, _) if instance0.prints =>
                        val instance = instance0.asInstanceOf[Inlinable { type Self = element }]

                        def framed(item: Expr[element]): Expr[Unit] =
                          framedWrite(number, instance.write(item, printer, sizes), printer, sizes)

                        eachOver(framed)

                      case Elem.Nested(_, _) | Elem.Runtime(_) | Elem.Seam =>
                        printableFor[element] match
                          case Some(printable) =>
                            def framed(item: Expr[element]): Expr[Unit] =
                              framedWrite
                                ( number,
                                  ' {
                                      Protobuf.Printable.writeField[element]
                                        ($printable.asInstanceOf[AnyRef], $item, $printer, $sizes)
                                    },
                                  printer, sizes )

                            eachOver(framed)

                          case None =>
                            seam

              case Plan.MapGather(key, keyType0, value, valueType0) =>
                val keyType = keyType0.asInstanceOf[TypeRepr]
                val valueType = valueType0.asInstanceOf[TypeRepr]

                def entryWrite(elem: Elem, tpe: TypeRepr, number: Int, item: Expr[Any])
                :   Option[Expr[Unit]] =

                  elem match
                    case Elem.Scalar(kind) =>
                      Some
                        ( ' {
                              ${tagWrite(number, wireCode(kind), printer)}
                              ${scalarWrite(kind, item, printer, sizes)}
                            } )

                    case Elem.Chunk(kind) =>
                      Some
                        ( ' {
                              ${tagWrite(number, wireCode(kind), printer)}
                              ${scalarWrite(kind, item, printer, sizes)}
                            } )

                    case Elem.Nested(instance0, _) if instance0.prints =>
                      tpe.asType match
                        case '[entry] =>
                          val instance = instance0.asInstanceOf[Inlinable { type Self = entry }]

                          Some
                            ( framedWrite
                                ( number, instance.write(item.asExprOf[entry], printer, sizes),
                                  printer, sizes ) )

                    case _ =>
                      None

                (keyType.asType, valueType.asType) match
                  case ('[keyT], '[valueT]) =>
                    val entries = '{$typed.asInstanceOf[Iterable[(keyT, valueT)]]}

                    def entry(item: Expr[(keyT, valueT)]): Option[Expr[Unit]] =
                      for
                        keyWrite   <- entryWrite(key, keyType, 1, '{$item._1})
                        valueWrite <- entryWrite(value, valueType, 2, '{$item._2})
                      yield framedWrite(number, '{$keyWrite; $valueWrite}, printer, sizes)

                    if entryPrints(key) && entryPrints(value)
                    then visited(entries): item => entry(item).get
                    else seam

              // A recursive message field (`Plan.Seam` here, its parser being on the
              // stack) prints through its own nominal instance; the rest of the seam —
              // maps, typed integers, optional collections, custom types — through their
              // `Encodable`.
              case _ =>
                nominal

      Expr.block(writes, '{()})

  // A oneof is a message holding one field, numbered by the variant's
  // declaration order — `EncodableDerivation.disjunction`'s bytes. The
  // variants are tried in order; the last needs no test, the sum being
  // sealed.
  private[locomotion] def sumMeasure[sum: Type](value: Expr[sum], sizes: Expr[AnyRef])
    ( using Quotes )
  :   Expr[Int] =

    sumDispatch[sum, Int](value, continued()):
      [variant] => (index: Int, instance: Inlinable { type Self = variant }, item: Expr[variant]) =>
        (tpe: Type[variant]) ?=> framedSize(index + 1, instance.measure(item, sizes), sizes)

  private[locomotion] def sumWrite[sum: Type]
    ( value: Expr[sum], printer: Expr[AnyRef], sizes: Expr[AnyRef] )
    ( using Quotes )
  :   Expr[Unit] =

    sumDispatch[sum, Unit](value, continued()):
      [variant] => (index: Int, instance: Inlinable { type Self = variant }, item: Expr[variant]) =>
        (tpe: Type[variant]) ?=>
          framedWrite(index + 1, instance.write(item, printer, sizes), printer, sizes)

  private def sumDispatch[sum: Type, result: Type](value: Expr[sum], cache: Cache)
    ( body: [variant] => (Int, Inlinable { type Self = variant }, Expr[variant])
                         -> Type[variant] ?-> Expr[result] )
    ( using Quotes )
  :   Expr[result] =

    import quotes.reflect.*

    active[sum, Expr[result]](cache):
      val variants = sumVariants(TypeRepr.of[sum].dealias).getOrElse:
        report.errorAndAbort
          ( s"locomotion: ${TypeRepr.of[sum].show} is not an inlinable oneof (a non-generic " +
            "sealed type whose variants are all case classes)" )

      def dispatch(index: Int): Expr[result] =
        variants(index)(1).asType match
          case '[type variantType <: sum; variantType] =>
            val instance = resolve[variantType](cache).getOrElse:
              report.errorAndAbort(s"locomotion: no Inlinable for variant ${variants(index)(0)}")

            if !instance.prints then
              report.errorAndAbort
                ( s"locomotion: the Inlinable for variant ${variants(index)(0)} generates no " +
                  "printer" )

            val typed = instance.asInstanceOf[Inlinable { type Self = variantType }]

            if index == variants.length - 1 then
              ' {
                  val variant = $value.asInstanceOf[variantType]
                  ${body[variantType](index, typed, 'variant)}
                }
            else
              ' {
                  if $value.isInstanceOf[variantType] then
                    val variant = $value.asInstanceOf[variantType]
                    ${body[variantType](index, typed, 'variant)}
                  else
                    ${dispatch(index + 1)}
                }

      dispatch(0)

  // Whether the generator builds a type's parser and printer structurally — a message or a
  // oneof of messages — which is what `import codecs.direct` offers a generated codec for. A
  // cheap shape test, so that a search under the mode stays cheap; anything else fails the
  // search, and the type takes the derived path. (A type with a user-written `Inlinable` is
  // reached through `Inlinable.parsable` and `Inlinable.printable`, as before.)
  def generates[value: Type](using Quotes): Expr[Inlinable.Generates[value]] =
    import quotes.reflect.*

    val tpe = formless(TypeRepr.of[value])

    val structural =
      productShaped(tpe) || sumVariants(tpe).exists: variants =>
        variants.map(_(1)).forall(productShaped(_))

    if structural then '{Inlinable.Generates[value]()}
    else report.errorAndAbort(s"locomotion: ${tpe.show} has no generated codec")

  // A type with any `Form` refinement removed: a read of `Foo in Protobuf` infers the
  // instance's type as `Foo { type Form = Protobuf }`, whose codec is `Foo`'s.
  private def formless(using Quotes)(tpe: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    tpe.dealias match
      case Refinement(parent, "Form", _) => formless(parent)
      case other                         => other

  // ── The entry macros ───────────────────────────────────────────────────
  // Each generated instance is bound to a lazy `self`, so that a recursive
  // occurrence of its own type reads through it, in place.
  def inlinablePrintable[value: Type](using Quotes): Expr[value is Protobuf.Printable] =
    import quotes.reflect.*

    val tpe = formless(TypeRepr.of[value])

    if tpe =:= TypeRepr.of[value].dealias then generatedPrintable[value]
    else tpe.asType.absolve match
      case '[formless] =>
        '{${generatedPrintable[formless]}.asInstanceOf[value is Protobuf.Printable]}

  private def generatedPrintable[value: Type](using Quotes): Expr[value is Protobuf.Printable] =
    import quotes.reflect.*

    val cache: Cache = Cache()

    val root: Inlinable = resolve[value](cache).getOrElse:
      report.errorAndAbort
        ( s"locomotion: no Inlinable instance for ${TypeRepr.of[value].show}, and it is not " +
          "an inlinable message or oneof; use an `Encodable in Protobuf`" )

    if root.isInstanceOf[Inlinable.IterableInlinable[?]] || !root.prints then
      report.errorAndAbort
        ( s"locomotion: ${TypeRepr.of[value].show} has no generated printer; a Protobuf " +
          "message is the unit of direct printing" )

    val instance = root.asInstanceOf[Inlinable { type Self = value }]
    val name = TypeRepr.of[value].dealias.show

    ' {
        lazy val self: Protobuf.Printable.Direct[value] =
          new Protobuf.Printable.Direct[value]:
            protected def measureCarrier(value0: value, sizes0: AnyRef): Int =
              $ {
                  cache.roots(name) = 'self
                  within(cache)(instance.measure('value0, 'sizes0))
                }

            protected def writeCarrier(value0: value, printer0: AnyRef, sizes0: AnyRef): Unit =
              $ {
                  cache.roots(name) = 'self
                  within(cache)(instance.write('value0, 'printer0, 'sizes0))
                }

        // Sealed per the codec-thunk pattern, as `inlinableParsable`.
        // [quote-wall] staged Printable generated inside quote
        caps.unsafe.unsafeAssumePure(self)
      }

  def inlinableParsable[value: Type](using Quotes): Expr[value is Protobuf.Parsable] =
    import quotes.reflect.*

    val tpe = formless(TypeRepr.of[value])

    if tpe =:= TypeRepr.of[value].dealias then generatedParsable[value]
    else tpe.asType.absolve match
      case '[formless] =>
        '{${generatedParsable[formless]}.asInstanceOf[value is Protobuf.Parsable]}

  private def generatedParsable[value: Type](using Quotes): Expr[value is Protobuf.Parsable] =
    import quotes.reflect.*

    val cache: Cache = Cache()

    val root: Inlinable = resolve[value](cache).getOrElse:
      report.errorAndAbort
        ( s"locomotion: no Inlinable instance for ${TypeRepr.of[value].show}, and it is not " +
          "an inlinable message or oneof; use a `Decodable in Protobuf`" )

    if root.isInstanceOf[Inlinable.IterableInlinable[?]] then
      report.errorAndAbort
        ( s"locomotion: ${TypeRepr.of[value].show} is a collection; a Protobuf message is the " +
          "unit of direct parsing" )

    val instance = root.asInstanceOf[Inlinable { type Self = value }]
    val name = TypeRepr.of[value].dealias.show

    ' {
        lazy val self: Protobuf.Parsable.Direct[value] =
          new Protobuf.Parsable.Direct[value]:
            protected def parseCarrier(reader0: AnyRef): value =
              // The reader is never bound as a typed local: a capability
              // class cannot be quoted into a pure hole, so every use casts
              // from the neutral carrier afresh — the rim-reassertion
              // pattern, at each use.
              $ {
                  cache.roots(name) = 'self
                  within(cache)(instance.parse('{reader0.asInstanceOf[ProtobufReader]}))
                }

        // Sealed per the codec-thunk pattern: the generated body resolves its
        // capabilities where it is spliced.
        // [quote-wall] staged Parsable generated inside quote
        caps.unsafe.unsafeAssumePure(self)
      }
