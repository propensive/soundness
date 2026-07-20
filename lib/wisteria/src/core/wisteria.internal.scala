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
package wisteria

import scala.collection.immutable as sci
import scala.quoted.*
import scala.reflect.ClassTag

import proscenium.compat.*

import anticipation.*
import denominative.*
import gigantism.*
import rudiments.*
import vacuous.*

object internal:
  inline def default[product, field](index: Int): Optional[field] =
    ${getDefault[product, field]('index)}

  // Inline accessors over the symbol-inspection macros, for use outside this object.
  inline def sumName[derivation]: Text = ${typeName[derivation]}
  inline def variantLabelList[derivation]: List[Text] = ${variantLabels[derivation]}
  inline def fieldLabelList[derivation]: List[Text] = ${fieldLabels[derivation]}

  // Resolves a field/variant instance via the real implicit search while *ignoring the typeclass's
  // wrapper givens* (the derivation's own `derived`, which route data types into a fresh graph).
  // This makes a field summon land on a *sibling* synthetic given (a recursive or shared type in
  // the same derivation block), a codec (`List[T]`, …), or a leaf — never re-entering the
  // derivation. Deferred to the splice site (a sibling val's nested `Quotes`) so the siblings
  // emitted alongside it are in scope.
  inline def field[typeclass[_], elem]: typeclass[elem] = ${fieldInstance[typeclass, elem]}

  // ---- vicarious.Specific integration ----

  // Inline accessor (for tests / tooling): the dotted paths overridden by an in-scope
  // `derivation is Specific over typeclass`, or `Nil` if there is none.
  inline def overridePaths[typeclass[_], derivation]: List[Text] =
    ${overridePathsMacro[typeclass, derivation]}

  def overridePathsMacro[typeclass[_]: Type, derivation: Type]: Macro[List[Text]] =
    import quotes.reflect.*

    val paths = specificOverridesRepr(TypeRepr.of[typeclass], TypeRepr.of[derivation]) match
      case Some((_, keys)) => keys.toList
      case None            => Nil

    '{List.of(${Expr.ofList(paths.stdlib.map { k => '{${Expr(k)}.tt} })})}

  // The override set for `root` and the typeclass `typeclassConstructor`: a reference to the
  // in-scope `root is Specific over typeclass` given and the dotted paths it overrides — or `None`
  // if no such `Specific` is in scope. A `Specific` whose `Transport` is for a different typeclass
  // (`Transport { type Self = X }` is not `typeclass[X]`) is ignored. Only the override paths
  // (string constants) are read from the given's tree; the override *values* are taken at runtime
  // from the given's `instances` map, never by re-splicing their trees (which would move lambdas
  // across owners and crash the inliner).
  private def specificOverridesRepr(using Quotes)
    ( typeclassConstructor: quotes.reflect.TypeRepr, root: quotes.reflect.TypeRepr )
  :   Option[(quotes.reflect.Term, Set[String])] =

    import quotes.reflect.*

    val specificType = Refinement(TypeRepr.of[vicarious.Specific], "Self", TypeBounds(root, root))

    Implicits.search(specificType) match
      case success: ImplicitSearchSuccess =>
        if transportMatches(typeclassConstructor, success.tree.tpe)
        then Some((success.tree, readKeys(success.tree)))
        else None

      case _ =>
        None

  // The value of refinement type member `name` in `repr` (walking refinement layers), if present.
  private def refinementMember(using Quotes)(repr: quotes.reflect.TypeRepr, name: String)
  :   Option[quotes.reflect.TypeRepr] =

    import quotes.reflect.*

    repr match
      case Refinement(_, `name`, TypeBounds(low, _)) =>
        Some(low)

      case Refinement(parent, _, _) =>
        refinementMember(parent, name)

      case AndType(left, right) =>
        refinementMember(left, name).orElse(refinementMember(right, name))

      // Capture checking wraps inferred types in `@scala.caps.internal.inferred` (and `@retains`
      // for captures); these are transparent to structural type inspection, so see through them —
      // otherwise the phantom `VRoot`/`VPath` carrier is invisible and per-path overrides silently
      // fall back to the default derivation.
      case AnnotatedType(underlying, _) =>
        refinementMember(underlying, name)

      case _ =>
        None

  // Whether the `Specific`'s `Transport`, applied at a sample type, yields `typeclass[sample]` — so
  // the override targets this typeclass and not some other.
  private def transportMatches(using Quotes)
    ( typeclassConstructor: quotes.reflect.TypeRepr, specific: quotes.reflect.TypeRepr )
  :   Boolean =

    import quotes.reflect.*
    val sample = TypeRepr.of[Any]

    refinementMember(specific.widen.dealias, "Transport") match
      case Some(transport) =>
        Refinement(transport, "Self", TypeBounds(sample, sample)) =:=
          typeclassConstructor.appliedTo(sample)

      case None =>
        false

  // ---- Path-specialised field resolution (the "spine") ----

  // A path-carrier type `field { type VRoot = root; type VPath = path }` — a phantom refinement
  // threading the override root and dotted path down the derivation so type-keyed field resolution
  // can distinguish, say, the `cto` employee from the `ceo` employee.
  private def carrierType(using Quotes)
    ( field: quotes.reflect.TypeRepr, root: quotes.reflect.TypeRepr, path: String )
  :   quotes.reflect.TypeRepr =

    import quotes.reflect.*
    val pathType = ConstantType(StringConstant(path))

    val withRoot = Refinement(field, "VRoot", TypeBounds(root, root))
    Refinement(withRoot, "VPath", TypeBounds(pathType, pathType))

  // The override context for deriving `derivation`: the root type, the dotted path so far, a
  // reference to the in-scope `Specific` given, and the overridden paths — from a `VRoot`/`VPath`
  // carrier if present (a deeper spine type), else from an in-scope `Specific` for `derivation`
  // itself (root, path ""). `None` when no override applies.
  private def overrideContext(using Quotes)
    ( typeclassConstructor: quotes.reflect.TypeRepr, derivation: quotes.reflect.TypeRepr )
  :   Option[(quotes.reflect.TypeRepr, String, quotes.reflect.Term, Set[String])] =

    import quotes.reflect.*

    (refinementMember(derivation, "VRoot"), refinementMember(derivation, "VPath")) match
      case (Some(root), Some(ConstantType(StringConstant(path)))) =>
        specificOverridesRepr(typeclassConstructor, root).map: (given_, keys) =>
          (root, path, given_, keys)

      case _ =>
        // `derivation` may be `T & Product` (added by `derivedOne`); strip to the bare type so the
        // `Specific` search keys on `T`.
        val base = productType(derivation)

        specificOverridesRepr(typeclassConstructor, base).map: (given_, keys) =>
          (base, "", given_, keys)

  // Reads the override paths (the string-literal keys) from an in-scope `Specific` given's expanded
  // `instances = Map(("path", instance), …)` body. Available for a given defined in the current
  // compilation run (the usual case: the override is defined where the derivation is requested).
  // Only the keys are read; the values stay in the runtime map.
  private def readKeys(using Quotes)(tree: quotes.reflect.Term): Set[String] =
    import quotes.reflect.*

    def givenSymbol(term: Term): Symbol = term match
      case Inlined(_, _, body) => givenSymbol(body)
      case Typed(expr, _)      => givenSymbol(expr)
      case Block(_, expr)      => givenSymbol(expr)
      case other               => other.symbol

    val rhs = givenSymbol(tree).tree match
      case valDef: ValDef => valDef.rhs
      case _              => None

    val keys = scala.collection.mutable.LinkedHashSet[String]()

    // Inline expansion wraps each tuple element in `Inlined`/`Typed`; strip those to reach the
    // string-literal path key.
    def unwrap(tree: Tree): Tree = tree match
      case Inlined(_, _, body) => unwrap(body)
      case Typed(expr, _)      => unwrap(expr)
      case Block(Nil, expr)    => unwrap(expr)
      case other               => other

    val accumulator = new TreeAccumulator[Unit]:
      def foldTree(state: Unit, tree: Tree)(owner: Symbol): Unit =
        tree match
          case Apply(_, sci.List(key, _)) => unwrap(key) match
            case Literal(StringConstant(path)) => keys += path
            case _                             => ()

          case _ =>
            ()

        foldOverTree(state, tree)(owner)

    rhs.foreach(accumulator.foldTree((), _)(Symbol.spliceOwner))
    keys.pipe(Set.from(_))

  // A marker that `resolvableNonStructural` injects (as a synthetic `given`) into a probe's
  // implicit scope, so the derivation macro can detect that implicit search has re-entered it for
  // this exact instance type. Never instantiated at runtime.
  trait Reentrant[carrier]

  // What `deriveGraph` returns when reached only as a re-entry probe (a `Reentrant` for its own
  // instance is in scope). `resolvableNonStructural` detects this tree; it is never evaluated and
  // never reaches generated code (probe results are discarded).
  def reentrySentinel[instance]: instance = null.asInstanceOf[instance]

  // Whether `derivation` is a sum — for `derivedOne` to pick `disjunction` vs `conjunction`.
  inline def isSum[derivation]: Boolean = ${isSumMacro[derivation]}

  def isSumMacro[derivation: Type]: Macro[Boolean] =
    import quotes.reflect.*
    Expr(isSumType(TypeRepr.of[derivation]))

  def fieldLabels[derivation: Type]: Macro[List[Text]] =
    import quotes.reflect.*

    val labels =
      productType(TypeRepr.of[derivation]).typeSymbol.caseFields.map: field =>
        '{${Expr(field.name)}.tt}

    '{List.of(${Expr.ofList(labels)})}


  // Whether `tpe` is a sum: a sealed trait / enum with `children`. A `Variant & Parent`
  // intersection (a sum's variant) is the variant — a product. A singleton (case object /
  // parameterless enum case) has a *term* symbol whose *type* symbol widens to the sum, so the term
  // test comes first.
  def isSumType(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean =
    import quotes.reflect.*

    tpe.dealias match
      case AndType(_, _)          => false
      case Refinement(base, _, _) => isSumType(base)

      case other =>
        other.termSymbol.isNoSymbol && other.typeSymbol.children.nonEmpty

  // Whether `tpe` is a (non-sum) product wisteria can build structurally.
  def isProductType(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean =
    import quotes.reflect.*

    !isSumType(tpe) &&
      (tpe <:< TypeRepr.of[Product] || tpe <:< TypeRepr.of[scala.Singleton] ||
        tpe.typeSymbol.caseFields.nonEmpty)

  // The type of a variant: a `TypeRef` for a case-class variant, a `TermRef` for a singleton.
  private def variantType(using Quotes)(child: quotes.reflect.Symbol): quotes.reflect.TypeRepr =
    if child.isType then child.typeRef else child.termRef

  // Whether every variant of the sum is a singleton (case object / parameterless enum case) — i.e.
  // it is a "choice". Computed here, at macro time, by symbol inspection rather than via an inline
  // `<:< Singleton` fold over `MirroredElemTypes`: that fold mis-reduces deep inside
  // `deriveGraph`'s nested inlining (a non-singleton variant is wrongly accepted), so a typeclass
  // that rejects non-choice sums would not surface its `compiletime.error`.
  def isChoice[derivation: Type]: Macro[Boolean] =
    import quotes.reflect.*
    val children = TypeRepr.of[derivation].typeSymbol.children
    val singleton = children.forall(variantType(_) <:< TypeRepr.of[scala.Singleton])
    Expr(children.nonEmpty && singleton)

  // The variant type intersected with the sum parent (`variant & derivation`): keeps a
  // parameterless enum case distinct (its bare type widens to the sum) while conforming to
  // `variant <: derivation`.
  private def variantWith(using Quotes)
    ( child: quotes.reflect.Symbol, parent: quotes.reflect.TypeRepr )
  :   quotes.reflect.TypeRepr =

    import quotes.reflect.*
    AndType(variantType(child), parent)

  // A "wrapper" given supplies `typeclass[T]` at a bare type parameter `T` — the derivation's own
  // `derived`, or a `summonFrom` catch-all routing data types in — unlike a structural codec like
  // `List[T] is Encodable` whose argument is an applied type. These are ignored during
  // field/variant resolution and reachability probing so neither re-enters the in-progress
  // derivation.
  private def isWrapper(using Quotes)
    ( typeclassConstructor: quotes.reflect.TypeRepr, method: quotes.reflect.Symbol )
  :   Boolean =

    import quotes.reflect.*

    method.info match
      case poly: PolyType if poly.paramNames.size == 1 =>
        def strip(tpe: TypeRepr): TypeRepr = tpe match
          case method: MethodType => strip(method.resType)
          case other              => other

        strip(poly.resType) =:= typeclassConstructor.appliedTo(poly.param(0))

      case _ => false

  // The typeclass's wrapper givens (its `derived` etc.), to be ignored during resolution/probing.
  private def wrappersOf[typeclass[_]: Type](using Quotes): List[quotes.reflect.Symbol] =
    import quotes.reflect.*
    val typeclassConstructor = TypeRepr.of[typeclass]
    val owner = typeclassConstructor.appliedTo(TypeRepr.of[Any]).typeSymbol.maybeOwner

    if owner.isNoSymbol then Nil
    else
      List.of:
        (owner.methodMembers ++ owner.fieldMembers)
          . filter(isWrapper(typeclassConstructor, _)).distinct

  // Resolves a field/variant instance via the real implicit search, ignoring the typeclass's
  // wrapper givens so it lands on a sibling synthetic given (a recursive/shared type), a codec, or
  // a leaf — never re-entering the in-progress derivation. Deferred to the use site (a sibling
  // val's nested `Quotes`) so the siblings generated alongside it are in scope.
  def fieldInstance[typeclass[_]: Type, elem: Type]: Macro[typeclass[elem]] =
    import quotes.reflect.*

    val wrappers = wrappersOf[typeclass]
    val instance = TypeRepr.of[typeclass[elem]]

    // Prefer a resolution that avoids the wrappers — a sibling, a codec, or a concrete leaf. Only
    // if none exists is `elem` something a wrapper handles specially, so fall back to the ordinary
    // search.
    val result = Implicits.searchIgnoring(instance)(wrappers*) match
      case success: ImplicitSearchSuccess => success
      case _                              => Implicits.search(instance)

    // The found instance crosses the derivation boundary through a single erasing
    // cast: resolution readily finds honest capability-typed codecs (every given
    // that includes a tactic is a capability), but their `^{tactic, …}` types do
    // not conform to the bare expected form. The derived instance consuming the
    // field codec is itself honestly typed at its own given, so the engine narrows
    // here, once, rather than requiring a seal at every derivation site.
    result.absolve match
      case success: ImplicitSearchSuccess =>
        success.tree.asExpr match
          case '{$found: any} => '{$found.asInstanceOf[typeclass[elem]]}

      case failure: ImplicitSearchFailure => report.errorAndAbort(failure.explanation)

  // `self.derivedOne[elem]`, built in the given nested `Quotes` (a sibling val's scope).
  private def derivedOneInstance(using nested: Quotes)(self: Expr[Any], elem: Type[?]): Expr[Any] =
    import nested.reflect.*
    val selfTerm = self.asTerm
    val method = selfTerm.tpe.typeSymbol.methodMember("derivedOne").head

    Select(selfTerm, method).appliedToType(TypeRepr.of(using elem)).asExpr

  // The symbol the resolved given ultimately refers to — the head of the application/inlining the
  // implicit search produced. Unwraps the layers an implicit resolution may add.
  private def rootSymbol(using Quotes)(tree: quotes.reflect.Term): quotes.reflect.Symbol =
    import quotes.reflect.*

    def loop(term: Term): Symbol = term match
      case Inlined(_, _, body) => loop(body)
      case TypeApply(fun, _)   => loop(fun)
      case Apply(fun, _)       => loop(fun)
      case Typed(expr, _)      => loop(expr)
      case Block(_, expr)      => loop(expr)
      case other               => other.symbol

    loop(tree)

  // Whether `tree` (the result of an implicit search) is the re-entry sentinel — i.e. the search
  // fell through to the derivation macro, which bailed.
  private def isSentinel(using Quotes)(tree: quotes.reflect.Term): Boolean =
    import quotes.reflect.*
    rootSymbol(tree) == Symbol.requiredMethod("wisteria.internal.reentrySentinel")

  // Does `typeclass[tpe]` resolve to a *non-derivation* instance — a handwritten codec, a leaf, or
  // a bridge resolution such as `Encodable in Text` → JSON? We run the compiler's own implicit
  // search in a context augmented with a synthetic `given Reentrant[typeclass[tpe]]`. If nothing
  // better matches, search falls through to the derivation macro (`deriveGraph`), which finds that
  // marker and returns `reentrySentinel` rather than recursing — so a sentinel result means
  // "structural, derive it" and any other success means "already resolvable elsewhere, don't make
  // it a sibling". This is the one place wisteria reaches into `dotty.tools.dotc`: there is no
  // public API to search with an extra given injected (a context-function search target is *not*
  // synthesised by `Implicits.search`).
  private def resolvableNonStructural(using Quotes)
    ( typeclassConstructor: quotes.reflect.TypeRepr, tpe: quotes.reflect.TypeRepr )
  :   Boolean =

    inferWith(typeclassConstructor.appliedTo(tpe), Nil).let(!isSentinel(_)).or(false)

  // Runs the compiler's implicit search for `instance` in a context augmented with synthetic
  // `given`s: always a `Reentrant[instance]` (so the derivation macro, reached only as a last
  // resort, self-bails to `reentrySentinel` rather than recursing), plus one `given` for each type
  // in `extraGivens`. Returns the resolved tree on success (including a sentinel tree), `Unset`
  // otherwise (no match, or an ambiguity, which `inferImplicit` reports as a non-success result).
  // This is the one place wisteria reaches into `dotty.tools.dotc`: there is no public API to
  // search with extra givens injected.
  private def inferWith(using Quotes)
    ( instance: quotes.reflect.TypeRepr, extraGivens: List[quotes.reflect.TypeRepr] )
  :   Optional[quotes.reflect.Term] =

    import quotes.reflect.*
    import dotty.tools.dotc as dotc

    given context: dotc.core.Contexts.Context =
      quotes.asInstanceOf[runtime.impl.QuotesImpl].ctx

    def syntheticGiven(name: String, info: TypeRepr): dotc.core.Symbols.Symbol =
      dotc.core.Symbols.newSymbol
        ( context.owner,
          dotc.core.Names.termName(name),
          dotc.core.Flags.Given,
          info.asInstanceOf[dotc.core.Types.Type],
          dotc.core.Symbols.NoSymbol,
          dotc.util.Spans.NoCoord )

    val reentrant = TypeRepr.of[Reentrant].appliedTo(instance)

    val elementGivens =
      extraGivens.zipWithIndex.map: (tpe, index) =>
        syntheticGiven("$wisteriaGiven$"+index, tpe)

    val markers =
      syntheticGiven("$wisteriaReentrant", reentrant) ::
        (elementGivens: List[dotc.core.Symbols.Symbol])

    val augmented = context.fresh.setScope(dotc.core.Scopes.newScopeWith(markers*))

    val result =
      new dotc.typer.Typer(0).inferImplicit
        ( instance.asInstanceOf[dotc.core.Types.Type],
          dotc.ast.tpd.EmptyTree,
          context.owner.span )
        ( using augmented )

    result match
      case success: dotc.typer.Implicits.SearchSuccess => success.tree.asInstanceOf[Term]
      case _                                           => Unset

  // Derives `typeclass[derivation]` by emitting a block of mutually-recursive synthetic
  // `given lazy val`s — one per distinct product/sum type reachable from `derivation` that a
  // structural derivation would otherwise build. Each instance is built once via `self.derivedOne`,
  // its fields/variants resolving through `field` (a real `searchIgnoring`) onto a sibling given, a
  // codec, or a leaf. Sharing deduplicates repeated subtrees (so a large graph stays within JVM
  // class limits) and ties recursive types safely (a lazy val may reference itself by name). The
  // lazy vals sit in this block at the use site, so a `summonInline[Mirror]` in a body captures the
  // surrounding frame — letting method-local and object-nested types construct via `fromProduct`.
  def deriveGraph[typeclass[_]: Type, derivation: Type](self: Expr[Any])
  :   Macro[typeclass[derivation]] =

    import quotes.reflect.*

    if self.asTerm.tpe.typeSymbol.methodMember("derivedOne").isEmpty then
      report.errorAndAbort("wisteria: the derivation defines no `derivedOne`")

    val typeclassConstructor = TypeRepr.of[typeclass]
    val wrappers = wrappersOf[typeclass]
    val instanceTpe = typeclassConstructor.appliedTo(TypeRepr.of[derivation])

    def instanceOf(tpe: TypeRepr): TypeRepr = typeclassConstructor.appliedTo(tpe)

    def codecFunction(tpe: TypeRepr, args: List[TypeRepr]): TypeRepr =
      defn.FunctionClass(args.length, isContextual = true).typeRef
        . appliedTo(args.stdlib.map(instanceOf) :+ instanceOf(tpe))

    def isCodec(tpe: TypeRepr, args: List[TypeRepr]): Boolean =
      args.nonEmpty &&
        (Implicits.searchIgnoring(codecFunction(tpe, args))(wrappers*) match
              case _: ImplicitSearchSuccess => true
              case _                        => false)

    // A fallback for codecs whose given carries capability parameters beyond the element instances
    // (a decoder's `Factory`/`Tactic`/`Foci`, …), which `isCodec`'s single-shape `codecFunction`
    // search cannot express. We resolve the *applied* instance (`typeclass[F[args]]`) with a
    // synthetic `given typeclass[arg]` injected per type-arg — so the codec's (by-name) element
    // parameter is satisfiable without deriving the element (re-entering this very type would
    // diverge) — while its other capabilities resolve from the ambient scope. A `Reentrant` marker
    // makes the catch-all derivation self-bail to the sentinel; a genuine codec resolves to a more
    // specific given. It is a codec iff the search lands on a non-sentinel given that is not one of
    // the derivation's own wrappers. Used only in `visit` (never the root `.apply` branch, which
    // needs the contextual-function shape).
    def codecProbe(tpe: TypeRepr, args: List[TypeRepr]): Boolean =
      args.nonEmpty && inferWith(instanceOf(tpe), args.map(instanceOf)).let: tree =>

        !isSentinel(tree) && !wrappers.stdlib.contains(rootSymbol(tree))

      . or(false)

    def fullGraph: Expr[typeclass[derivation]] =
      val reachable = scala.collection.mutable.LinkedHashMap[String, TypeRepr]()
      val seen = scala.collection.mutable.HashSet[String]()

      // A codec (`List[T]`, …) is matched first and its element types followed, so a structural
      // element wrapped in a codec still becomes a shared sibling. Only then do we ask whether a
      // non-codec type already resolves elsewhere (skip it) or must be derived structurally (a
      // sibling). The root is always derived structurally — never probed (it would otherwise find
      // its own `derives` companion given and make the block self-reference, a lazy-val cycle).
      def visit(raw: TypeRepr, isRoot: Boolean): Unit =
        val tpe = raw.dealias
        val key = tpe.show

        if !seen.contains(key) then
          seen += key
          val args = List.of(tpe.typeArgs)

          // A codec is recognised by the fast `isCodec` (single contextual-function shape) or, for
          // codecs with extra capability parameters (decoders' `Factory`/`Tactic`/`Foci`), the
          // `codecProbe` fallback; either way its element types are followed so a structural
          // element wrapped in a collection still becomes a shared sibling (the recursion-through-
          // a-collection case). Only a non-codec product/sum reaches `resolvableNonStructural`,
          // which bails at the type's own `deriveGraph` (its `Reentrant` marker is injected) so it
          // never recurses into fields. A codec the probe still cannot recognise (e.g. `Map[K, V]`
          // whose key uses a different typeclass) is simply not made a sibling here; it resolves at
          // the field site.
          if isCodec(tpe, args) || codecProbe(tpe, args) then args.each(visit(_, false))
          else if isSumType(tpe) then
            if isRoot || !resolvableNonStructural(typeclassConstructor, tpe) then
              reachable(key) = tpe

              tpe.typeSymbol.children.each: child =>
                visit(variantWith(child, tpe), false)
          else if isProductType(tpe) then
            // A path-carrier (a specialised spine type) is always derived; else probe as before.
            val carrier = refinementMember(tpe, "VRoot").isDefined

            if isRoot || carrier || !resolvableNonStructural(typeclassConstructor, tpe) then
              reachable(key) = tpe
              val product = productType(tpe)

              // Detection is per-type: any in-scope `Specific` for this exact type specialises its
              // spine, whether `tpe` is the root or a component reached deeper in the graph.
              overrideContext(typeclassConstructor, tpe) match
                case Some((root, parentPath, _, keys)) =>
                  // Spine field → specialised carrier sibling; overridden leaf → no sibling (the
                  // value comes from the given's runtime map); else a shared default sibling.
                  product.typeSymbol.caseFields.each: field =>
                    val fieldType = product.memberType(field)

                    val childPath =
                      if parentPath.isEmpty then field.name else parentPath+"."+field.name

                    if keys.contains(childPath) then
                      ()
                    else if keys.stdlib.exists(_.startsWith(childPath+".")) then
                      visit(carrierType(fieldType, root, childPath), false)
                    else
                      visit(fieldType, false)

                case None =>
                  product.typeSymbol.caseFields.each: field =>
                    visit(product.memberType(field), false)
          else tpe match
            // An alias like `Optional[P]` dealiases to a union (`Unset | P`) with no type
            // arguments, so the codec probes above cannot follow its elements, and a union is
            // neither a sum nor a product. Visit each branch so a structural element inside a
            // union field (`Optional`-of-product, #1600) still becomes a shared sibling: the
            // element codec then resolves to the sibling's lazy val — never a fresh capturing
            // expansion inside `optional`'s by-name slot, which capture checking rejects.
            // Singleton members (`Unset`, `None.type`) are sentinels handled by the union-level
            // codec itself: they can never need a sibling, and probing their resolvability
            // expands capability givens at macro time, which perturbs capture-checking state.
            case OrType(left, right) =>
              def branch(part: TypeRepr): Unit =
                if !(part.dealias <:< TypeRepr.of[scala.Singleton]) then visit(part, false)

              branch(left)
              branch(right)

            case _ =>
              ()

      val rootType = TypeRepr.of[derivation].dealias
      visit(rootType, isRoot = true)

      val owner = Symbol.spliceOwner

      val bindings0 =
        reachable.toList.zipWithIndex.map: (entry, index) =>
          val (key, tpe) = entry
          val flags = Flags.Given | Flags.Lazy

          val symbol =
            Symbol.newVal(owner, "wisteria$"+index, instanceOf(tpe), flags, Symbol.noSymbol)

          (key, tpe, symbol)

      val bindings: List[(String, TypeRepr, Symbol)] = List.of(bindings0)

      val symbolByKey = bindings.map { (key, _, symbol) => key -> symbol }.toMap

      // Each body is built in the val's *own* nested `Quotes` (`symbol.asQuotes`), so the `field`
      // resolutions inside `derivedOne` resolve in that val's scope — where the sibling givens are.
      val definitions: List[ValDef] = bindings.map: (_, tpe, symbol) =>
        ValDef(symbol, Some(derivedOneInstance(using symbol.asQuotes)(self, tpe.asType).asTerm))

      def resolveDirect(tpe: TypeRepr): Term =
        Implicits.searchIgnoring(instanceOf(tpe))(wrappers*).absolve match
          case success: ImplicitSearchSuccess => success.tree
          case failure: ImplicitSearchFailure => report.errorAndAbort(failure.explanation)

      def elementInstance(tpe: TypeRepr): Term = symbolByKey.get(tpe.dealias.show) match
        case Some(symbol) => Ref(symbol)
        case None         => resolveDirect(tpe)

      val rootArgs = List.of(rootType.typeArgs)

      val root: Term = symbolByKey.get(rootType.show) match
        case Some(symbol) => Ref(symbol)

        case None =>
          if !isCodec(rootType, rootArgs) then resolveDirect(rootType)
          else Implicits.searchIgnoring(codecFunction(rootType, rootArgs))(wrappers*).absolve.match
            case success: ImplicitSearchSuccess =>
              Select.unique(success.tree, "apply")
              . appliedToArgs(rootArgs.stdlib.map(elementInstance))

            case failure: ImplicitSearchFailure => report.errorAndAbort(failure.explanation)

      Block(definitions.stdlib, root.changeOwner(owner)).asExprOf[typeclass[derivation]]

    // If reached only as a re-entry probe (a `Reentrant` marker for this instance is in scope),
    // bail with the sentinel the prober detects — do not build the graph.
    Implicits.search(TypeRepr.of[Reentrant].appliedTo(instanceTpe)) match
      case _: ImplicitSearchSuccess =>
        Ref(Symbol.requiredMethod("wisteria.internal.reentrySentinel")).appliedToType(instanceTpe)
        . asExprOf[typeclass[derivation]]

      case _ =>
        fullGraph


  // Wraps a homogeneous list of field results into an `IArray`, summoning the `ClassTag` at the
  // expansion site (where `result` is concrete).
  private def immutableArray[result: Type](results: sci.List[Expr[result]])(using Quotes)
  :   Expr[IArray[result]] =

    import quotes.reflect.*

    val classTag = Expr.summon[ClassTag[result]].getOrElse:
      report.errorAndAbort("wisteria: no ClassTag available for the result type")

    '{Array[result](${Varargs(results)}*)(using $classTag).immutable(using Unsafe)}

  // The derivation type may be an intersection `Variant & Parent` (when deriving a sum's
  // variant); resolve to the variant side. The parent of `Variant & Parent` is the sum, which has
  // no term symbol and does have `children`; everything else is the variant. (A parameterless enum
  // case is a `TermRef` whose *type* symbol widens to the sum, so the term test comes first.)
  private def productType(using Quotes)(repr: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    def parent(repr: TypeRepr): Boolean =
      repr.termSymbol.isNoSymbol && repr.typeSymbol.children.nonEmpty

    repr.dealias match
      case AndType(left, right)     => if parent(left) then productType(right) else productType(left)
      case Refinement(base, _, _)   => productType(base)
      case AnnotatedType(under, _)  => productType(under)
      case other                    => other

  // Construct a product value from its field-value terms via the primary constructor — no
  // `Mirror`, no `fromProduct`. The constructed type is `productType(derivation)` (stripping any
  // `over` carrier refinement). For an inner class the type carries its prefix (`Outer.Inner` /
  // `outer.Inner`), so `New` of that type tree resolves the outer pointer automatically; type
  // arguments (generic case classes) are reapplied to the constructor. A singleton (case object /
  // parameterless enum case) is referenced directly.
  private def constructProduct[derivation: Type](using Quotes)
    ( arguments: List[quotes.reflect.Term] )
  :   quotes.reflect.Term =

    import quotes.reflect.*
    val tpe = productType(TypeRepr.of[derivation])
    val symbol = tpe.typeSymbol

    if !tpe.termSymbol.isNoSymbol then Ref(tpe.termSymbol)
    else
      // `TypeTree.ref(symbol)` carries the class's *defining* prefix (e.g. the enclosing object for
      // an inner class), giving a stable outer reference that resolves even within a nested
      // context-bound implicit search — unlike `tpe.asType`, whose prefix can be the
      // context-relative `this`.
      val typeTree = tpe.typeArgs match
        case Nil  => TypeTree.ref(symbol)

        case args =>
          Applied(TypeTree.ref(symbol), args.map { arg => TypeTree.of(using arg.asType) })

      Select(New(typeTree), symbol.primaryConstructor).appliedToArgs(arguments.stdlib)


  // Retypes the constructed plain product `Term` to `derivation`. When `derivation` is a carrier
  // refinement (`Product over Format` = `Product { type Transport = Format }`) the plain product is
  // a *supertype*, so a runtime `asInstanceOf` downcast is needed (sound: `Transport` is phantom).
  // When `derivation` is the plain product, `asExprOf` suffices and — crucially — avoids wrapping
  // the `New` in a cast node, which makes erasure crash with "missing outer accessor" for a class
  // nested in an object/method (the `New` carries an outer pointer the cast node mishandles).
  private def castConstructed[derivation: Type](using Quotes)(constructed: quotes.reflect.Term)
  :   Expr[derivation] =

    import quotes.reflect.*
    val derivationType = TypeRepr.of[derivation]

    if productType(derivationType) =:= derivationType then constructed.asExprOf[derivation]
    else
      TypeApply(Select.unique(constructed, "asInstanceOf"), sci.List(TypeTree.of[derivation]))
      . asExprOf[derivation]


  // The declared name of the resolved variant/product — the term symbol for a singleton case
  // object or parameterless enum case, otherwise the type symbol.
  private def caseName(using Quotes)(repr: quotes.reflect.TypeRepr): String =
    val resolved = productType(repr)
    if resolved.termSymbol.isNoSymbol then resolved.typeSymbol.name else resolved.termSymbol.name

  // Applies the polymorphic-function `lambda` at type `field`, passing `value` as the value
  // argument and `contextArgs` as the trailing context-function arguments. The applications are
  // beta-reduced — substituting `field` and inlining the body with concrete types, exactly as the
  // inliner does for an `inline` lambda parameter — so no residual abstract `apply` call survives
  // (whose path-dependent `#Self` parameter types would otherwise fail to reduce).
  private def applyLambda[field: Type]
    ( using Quotes )
    ( lambda: quotes.reflect.Term, value: Expr[Any], contextArgs: List[Expr[Any]] )
  :   quotes.reflect.Term =

    import quotes.reflect.*

    def reduce(term: Term): Term = Term.betaReduce(term).getOrElse(term)

    val typeApplied = TypeApply(Select.unique(lambda, "apply"), sci.List(TypeTree.of[field]))
    val applied = reduce(Apply(typeApplied, sci.List(value.asTerm)))

    reduce(Apply(Select.unique(applied, "apply"), contextArgs.stdlib.map(_.asTerm)))

  // Produces the tuple of field values (each = `lambda` applied to that field with its instance,
  // default, label and index). This macro is mirror-free and does NOT construct — `build` does the
  // construction *inline* via `reflection.fromProduct(…)` using the `Mirror` threaded from the
  // caller's `summonFrom`. Keeping construction inline (and out of macro output) is what lets a
  // method-local / object-nested case class derive: `fromProduct` captures the outer pointer from
  // the surrounding frame, which a macro-emitted `New` cannot reach (erasure "missing outer
  // accessor"); and because the macro never embeds the `Mirror`, it never escapes its scope.
  def buildProduct[typeclass[_]: Type, derivation <: Product: Type]
    ( lambda: Expr[Any] )
  :   Macro[Tuple] =

    import quotes.reflect.*

    val tpe = productType(TypeRepr.of[derivation])
    val symbol = tpe.typeSymbol
    val lambdaTerm = lambda.asTerm
    val context = overrideContext(TypeRepr.of[typeclass], TypeRepr.of[derivation])

    val arguments = symbol.caseFields.zipWithIndex.map: (field, index) =>
      val fieldName = field.name

      tpe.memberType(field).asType.absolve match
        case '[field] =>
          val indexExpr = Expr(index)
          val nameExpr = Expr(fieldName)
          val default = '{Default(wisteria.internal.default[derivation, field]($indexExpr))}
          val label = '{$nameExpr.tt.aka["label"]}
          val fieldIndex = '{$indexExpr.asInstanceOf[Int & FieldIndex[field]].aka["index"]}
          val instance = specificFieldInstance[typeclass, field](context, fieldName)

          sharedInstance[typeclass, field](index, instance): (reference, contextual) =>
            applyLambda[field](lambdaTerm, reference, List(contextual, default, label, fieldIndex))

          . asExpr

    Expr.ofTupleFromSeq(arguments)

  def contextsProduct[typeclass[_]: Type, derivation: Type, result: Type]
    ( lambda: Expr[Any] )
  :   Macro[IArray[result]] =

    import quotes.reflect.*

    val tpe = productType(TypeRepr.of[derivation])
    val lambdaTerm = lambda.asTerm
    val context = overrideContext(TypeRepr.of[typeclass], TypeRepr.of[derivation])

    val results = tpe.typeSymbol.caseFields.zipWithIndex.map: (field, index) =>
      val fieldName = field.name

      tpe.memberType(field).asType.absolve match
        case '[field] =>
          val indexExpr = Expr(index)
          val nameExpr = Expr(fieldName)
          val default = '{Default(wisteria.internal.default[derivation, field]($indexExpr))}
          val label = '{$nameExpr.tt.aka["label"]}

          val accessor: Expr[derivation -> field] =
            '{value => value.asInstanceOf[Product].productElement($indexExpr).asInstanceOf[field]}

          val dereference = '{$accessor.aka["dereference"]}
          val fieldIndex = '{$indexExpr.asInstanceOf[Int & FieldIndex[field]].aka["index"]}
          val instance = specificFieldInstance[typeclass, field](context, fieldName)

          sharedInstance[typeclass, field](index, instance): (reference, contextual) =>
            val arguments = List(contextual, default, label, dereference, fieldIndex)
            applyLambda[field](lambdaTerm, reference, arguments)

          . asExprOf[result]

    immutableArray[result](results)

  def fieldsProduct[typeclass[_]: Type, derivation: Type, result: Type]
    ( product: Expr[derivation], lambda: Expr[Any] )
  :   Macro[IArray[result]] =

    import quotes.reflect.*

    val tpe = productType(TypeRepr.of[derivation])
    val lambdaTerm = lambda.asTerm
    val context = overrideContext(TypeRepr.of[typeclass], TypeRepr.of[derivation])

    val results = tpe.typeSymbol.caseFields.zipWithIndex.map: (field, index) =>
      val fieldName = field.name

      tpe.memberType(field).asType.absolve match
        case '[field] =>
          val indexExpr = Expr(index)
          val nameExpr = Expr(fieldName)

          val fieldValue: Expr[Any] =
            '{$product.asInstanceOf[Product].productElement($indexExpr).asInstanceOf[field]}

          val instance = specificFieldInstance[typeclass, field](context, fieldName)
          val contextual = '{$instance.aka["contextual"]}
          val default = '{Default(wisteria.internal.default[derivation, field]($indexExpr))}
          val label = '{$nameExpr.tt.aka["label"]}
          val fieldIndex = '{$indexExpr.asInstanceOf[Int & FieldIndex[field]].aka["index"]}
          val arguments = List(contextual, default, label, fieldIndex)

          applyLambda[field](lambdaTerm, fieldValue, arguments).asExprOf[result]

    immutableArray[result](results)

  def constructMonadic[typeclass[_]: Type, constructor[_]: Type, derivation <: Product: Type]
    ( bind: Expr[Any], pure: Expr[Any], lambda: Expr[Any] )
  :   Macro[constructor[derivation]] =

    import quotes.reflect.*

    val tpe = productType(TypeRepr.of[derivation])
    val symbol = tpe.typeSymbol
    val bindTerm = bind.asTerm
    val pureTerm = pure.asTerm
    val lambdaTerm = lambda.asTerm
    val tupleType = TypeRepr.of[Tuple]
    val constructorTuple = TypeRepr.of[constructor[Tuple]]
    val derivationType = TypeRepr.of[derivation]
    val context = overrideContext(TypeRepr.of[typeclass], derivationType)

    // `pure.apply[monadic](value)`
    def applyPure(monadic: TypeRepr, value: Term): Term =
      Apply
        ( TypeApply(Select.unique(pureTerm, "apply"), sci.List(TypeTree.of(using monadic.asType))),
          sci.List(value) )

    // `bind.apply[input, output](monad)(function)` — the poly `apply` is curried, so the function
    // argument is applied to the returned `Function1` via its own `apply`.
    def applyBind(input: TypeRepr, output: TypeRepr, monad: Term, function: Term): Term =
      val typed =
        TypeApply
          ( Select.unique(bindTerm, "apply"),
            sci.List(TypeTree.of(using input.asType), TypeTree.of(using output.asType)) )

      Apply(Select.unique(Apply(typed, sci.List(monad)), "apply"), sci.List(function))

    // Accumulate each field monadically into a (reversed) tuple, threading through `bind`:
    //   bind(acc) { tuple => bind(lambda[field](ctx)) { value => pure(value *: tuple) } }
    var accumulator: Term = applyPure(tupleType, '{EmptyTuple}.asTerm)

    val tupleFunction =
      MethodType(sci.List("tuple"))(_ => sci.List(tupleType), _ => constructorTuple)

    symbol.caseFields.zipWithIndex.each: (field, index) =>
      val fieldName = field.name

      tpe.memberType(field).asType.absolve match
        case '[field] =>
          val indexExpr = Expr(index)
          val nameExpr = Expr(fieldName)
          val fieldType = TypeRepr.of[field]

          val valueFunction =
            MethodType(sci.List("value"))(_ => sci.List(fieldType), _ => constructorTuple)

          val outer = Lambda(Symbol.spliceOwner, tupleFunction, { (owner, parameters) =>
            val tupleRef = parameters.head.asInstanceOf[Term]
            val instance = specificFieldInstance[typeclass, field](context, fieldName)
            val contextValue: Expr[Any] = instance
            val contextual = '{$instance.aka["contextual"]}
            val default = '{Default(wisteria.internal.default[derivation, field]($indexExpr))}
            val label = '{$nameExpr.tt.aka["label"]}
            val fieldIndex = '{$indexExpr.asInstanceOf[Int & FieldIndex[field]].aka["index"]}
            val arguments = List(contextual, default, label, fieldIndex)
            val monad = applyLambda[field](lambdaTerm, contextValue, arguments)

            val inner = Lambda(owner, valueFunction, { (innerOwner, innerParameters) =>
              val valueRef = innerParameters.head.asInstanceOf[Term]
              val prepended = '{${valueRef.asExprOf[field]} *: ${tupleRef.asExprOf[Tuple]}}.asTerm
              applyPure(tupleType, prepended).changeOwner(innerOwner)
            })

            applyBind(fieldType, tupleType, monad, inner).changeOwner(owner)
          })

          accumulator = applyBind(tupleType, tupleType, accumulator, outer)

    // Finally reverse the accumulated tuple and construct the product from its elements.
    val derivationFunction =
      val paramNames = sci.List("tuple")
      MethodType(paramNames)(_ => sci.List(tupleType), _ => TypeRepr.of[constructor[derivation]])

    val product = productType(derivationType)

    val finalize = Lambda(Symbol.spliceOwner, derivationFunction, { (finalOwner, parameters) =>
      val reversed = '{${parameters.head.asInstanceOf[Term].asExprOf[Tuple]}.reverse}

      // Extract each field from the (reversed) accumulated tuple and pass it to the constructor.
      val arguments = product.typeSymbol.caseFields.zipWithIndex.map: (field, index) =>
        product.memberType(field).asType.absolve match
          case '[field] => '{$reversed.productElement(${Expr(index)}).asInstanceOf[field]}.asTerm

      // Cast to `derivation` (possibly a carrier refinement) — see `castConstructed`.
      val constructed0 = constructProduct[derivation](List.of(arguments))
      val constructed = castConstructed[derivation](constructed0).asTerm
      applyPure(derivationType, constructed).changeOwner(finalOwner)
    })

    applyBind(tupleType, derivationType, accumulator, finalize).asExprOf[constructor[derivation]]

  def typeName[derivation: Type]: Macro[Text] =
    import quotes.reflect.*
    val name: String = caseName(TypeRepr.of[derivation])
    '{${Expr(name)}.tt}

  def variantLabels[derivation: Type]: Macro[List[Text]] =
    import quotes.reflect.*

    val labels = TypeRepr.of[derivation].typeSymbol.children.map: child =>
      '{${Expr(child.name)}.tt}

    '{List.of(${Expr.ofList(labels)})}

  // Resolves a field's typeclass instance via an inline `summonInline` at the use site. Deferring
  // resolution to the splice site (rather than `Expr.summon` here) is what lets a recursive or
  // repeated field find a sibling synthetic given generated alongside it, instead of re-deriving.
  private def resolveField[typeclass[_]: Type, field: Type](using Quotes): Expr[typeclass[field]] =
    '{wisteria.internal.field[typeclass, field]}

  // The typeclass instance for a field, honoring `context` (the product's override context — read
  // once per product: root type, dotted path so far, the in-scope `Specific` given reference, and
  // its keys): an override value (from the given's runtime `instances` map), a path-specialised
  // sibling (resolved at a carrier type), or — with no `Specific` — exactly `resolveField`, so the
  // common path is unchanged.
  private def specificFieldInstance[typeclass[_]: Type, field: Type](using Quotes)
    ( context: Option[(quotes.reflect.TypeRepr, String, quotes.reflect.Term, Set[String])],
      fieldName: String )
  :   Expr[typeclass[field]] =

    import quotes.reflect.*

    context match
      case None =>
        resolveField[typeclass, field]

      case Some((root, parentPath, given_, keys)) =>
        val childPath = if parentPath.isEmpty then fieldName else parentPath+"."+fieldName

        if keys.contains(childPath) then
          // Take the override's value from the given's runtime `instances` map (cast to the field's
          // type) — never re-splice its tree.
          val specific = given_.asExprOf[vicarious.Specific]
          '{$specific.instances(${Expr(childPath)}).asInstanceOf[typeclass[field]]}
        else if keys.stdlib.exists(_.startsWith(childPath+".")) then
          carrierType(TypeRepr.of[field], root, childPath).asType.absolve match
            case '[carrier] =>
              '{wisteria.internal.field[typeclass, carrier].asInstanceOf[typeclass[field]]}
        else
          resolveField[typeclass, field]

  // Binds a field/variant's resolved instance to a single val, so the lambda's value argument and
  // its `"contextual"` context argument share one instance instead of resolving it twice. Without
  // that sharing each use re-emits — and, absent dedup, re-derives — the whole sub-instance,
  // doubling the generated code (and anonymous classes) for every decoder- or `contexts`-style
  // derivation. `build(ref, contextual)` produces the applied term given the shared ref and its
  // `"contextual"`-tagged view; the result is wrapped in a block that introduces the val.
  private def sharedInstance[typeclass[_]: Type, field: Type](using Quotes)
    ( index: Int, instance: Expr[typeclass[field]] )
    ( build: (Expr[typeclass[field]], Expr[Any]) => quotes.reflect.Term )
  :   quotes.reflect.Term =

    import quotes.reflect.*

    val symbol =
      Symbol.newVal
        ( Symbol.spliceOwner,
          "wisteria$field$"+index,
          TypeRepr.of[typeclass[field]],
          Flags.EmptyFlags,
          Symbol.noSymbol )

    val reference = Ref(symbol).asExprOf[typeclass[field]]
    val applied = build(reference, '{$reference.aka["contextual"]})

    Block(sci.List(ValDef(symbol, Some(instance.asTerm))), applied)

  def getDefault[product: Type, field: Type](index: Expr[Int]): Macro[Optional[field]] =
    import quotes.reflect.*

    val methodName: String = "$lessinit$greater$default$"+(index.valueOrAbort + 1)
    val productSymbol = TypeRepr.of[product].classSymbol

    productSymbol.flatMap: symbol =>
      symbol.companionClass.declaredMethod(methodName).headOption.map: method =>
        Ref(symbol.companionModule).select(method)

    . map: selection =>
        TypeRepr.of[product].typeArgs match
          case Nil       => selection
          case arguments => selection.appliedToTypes(arguments)

    . map(_.asExprOf[field]).getOrElse('{Unset})
