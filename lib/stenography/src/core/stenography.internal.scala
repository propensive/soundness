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
package stenography

import scala.collection.immutable as sci
import scala.collection.mutable as scm
import scala.collection.immutable.{List, Nil}
import scala.quoted.*

import anticipation.*
import gigantism.*
import rudiments.*
import vacuous.*

object internal:
  import dotty.tools.dotc.*

  def designator[designator <: AnyKind: Type]: Macro[Text] = Expr(name[designator])

  def name[designator <: AnyKind: Type](using Quotes): Text =
    import quotes.reflect.*
    name(TypeRepr.of[designator])

  // What stays the same between calls to `name` in one compilation run: the trees of the
  // types already rendered, which depend on nothing but the type (`Bindings`); each scope's
  // summary (`scopeInfo`), which depends only on the scope's symbol; and the union of the
  // summaries of each list of scopes a call site sees, which, within one macro expansion, is the
  // same for every name. Without them, every call re-walked the declarations of every wildcard
  // import in scope, so that explaining a missing given in a large scope, which names thousands
  // of types, took hours (#2318). A type is only valid in the run which made it, so none of this
  // is carried into the next run.
  private final class RunCache(val id: Int):
    val bindings: Bindings = Bindings()

    val scopes: scm.HashMap[core.Symbols.Symbol, (List[Designator], List[(String, Text)])] =
      scm.HashMap()

    val summaries
    :   scm.HashMap
          [ List[core.Symbols.Symbol], (sci.Set[Designator], sci.Map[String, Text]) ] =
      scm.HashMap()

  // One cache for each compiler, keyed by its `ContextBase` together with the run id: a run id
  // alone is not enough, because a warm session which replaces its `Compiler` after a crash
  // starts counting runs again. Only the latest run of each base is kept. The values are held
  // softly because they reach their own key (types hold symbols, and symbols not yet completed
  // hold the context which will complete them), so a strongly-held value would keep a
  // discarded compiler alive; a soft one is cleared once it goes unused, and before memory runs
  // out.
  private val runCaches
  :   java.util.WeakHashMap
        [ core.Contexts.ContextBase, java.lang.ref.SoftReference[RunCache] ] =
    java.util.WeakHashMap()

  private def runCache(using context: core.Contexts.Context): RunCache = runCaches.synchronized:
    val id = context.runId
    val cached = runCaches.get(context.base)
    val current = if cached == null then null else cached.get()

    if current != null && current.id == id then current else
      val fresh = RunCache(id)
      runCaches.put(context.base, java.lang.ref.SoftReference(fresh))
      fresh

  def name(using Quotes)(typeRepr: quotes.reflect.TypeRepr): Text =
    // Deliberate stdlib opt-out: this file's import-graph bookkeeping is stdlib throughout
    // (`sci.Set`/`sci.Map`).
    val importsInScope = metaprogramming.imports.stdlib

    quotes.absolve match
      case quotes: runtime.impl.QuotesImpl =>
        given context: core.Contexts.Context = quotes.ctx
        val cache = runCache
        given Bindings = cache.bindings

        // Whether `A => B` desugars to `ImpureFunctionN` at the use site, and hence whether a
        // bare `FunctionN` should be rendered `->` or `=>`.
        val pureFuns: Boolean =
          try config.Feature.pureFunsEnabled catch case _: Exception => true

        val imports: sci.Set[Designator] = importsInScope.map(_.term).map(Syntax.term(_)).toSet

        val outer: List[Designator] = context.compilationUnit.tpdTree.absolve match
          case ast.tpd.PackageDef(root, statements) =>
            Designator(root.show) :: statements.collect:
              case ast.tpd.Import(name, _) => Designator(name.show)

          case _ =>
            Nil

        // Drill into every scope whose members are reachable unqualified from here — each
        // wildcard import that's in scope (including the REPL-accumulated ones across earlier
        // lines, captured by `metaprogramming.imports`) and this unit's own package — and
        // harvest two things from a single pass over its declarations. (`Imports.exports` is the
        // same harvest of the first, for a scope named from outside any macro.)
        //
        // The `direct` set holds type aliases carrying the `Exported` flag. Those aliases'
        // *target* types are reachable via just their leaf in the current scope, so we render
        // them that way.
        //
        // The `aliases` map holds the infix type aliases which refine a single type member,
        // keyed by that member's name, so `Foo { type Form = Bar }` can be written back as
        // `Foo in Bar`. Scanning scopes rather than the whole classpath is what keeps the
        // rendering honest: an alias is only worth preferring where it is in scope to be written.
        val imported = importsInScope.filter(_.wildcard).map: imp =>
          imp.term.asInstanceOf[core.Types.Type].termSymbol

        // This unit's own package, so that a module's aliases apply within the module which
        // declares them, without it having to import itself.
        val own = context.compilationUnit.tpdTree.absolve match
          case ast.tpd.PackageDef(root, _) => List(root.symbol)
          case _                           => Nil

        val scopes = (imported ++ own).filter(_.exists)

        val (direct, aliases) = cache.summaries.establish(scopes):
          val harvested = scopes.map: scope =>
            cache.scopes.establish(scope)(scopeInfo(scope))

          // Where two aliases claim the same member, choose by name, so that the rendering
          // does not depend on the order the scopes happened to be walked in.
          val aliases = harvested.flatMap(_(1)).groupBy(_(0)).view.mapValues: candidates =>
            candidates.map(_(1).s).min.tt

          (harvested.flatMap(_(0)).toSet, aliases.toMap)

        val designators =
          sci.Set(Designator("scala"), Designator("scala.Predef")) ++ imports ++ outer

        Syntax(typeRepr).text(using Imports(designators, direct, aliases, pureFuns))

      case _ =>
        given Bindings = Bindings()
        val imports = importsInScope.map(_.term).map(Syntax.term(_)).toSet
        val designators = sci.Set(Designator("scala"), Designator("scala.Predef")) ++ imports
        Syntax(typeRepr).text(using Imports(designators, sci.Set()))

  // Everything worth knowing about one scope whose members are reachable unqualified: the
  // targets of its `Exported` aliases, and the infix type aliases it declares which refine a
  // single type member.
  private[stenography] def scopeInfo(using Quotes, dotty.tools.dotc.core.Contexts.Context)
    ( rootSym: dotty.tools.dotc.core.Symbols.Symbol )
  :   (List[Designator], List[(String, Text)]) =

    import dotty.tools.dotc.core.Flags

    // Top-level definitions in a package are stored inside synthetic
    // `<filename>$package` classes; export forwarders for types live there.
    val directDecls = rootSym.info.decls.toList
    val packageClasses = directDecls.filter(_.name.toString.endsWith("$package"))
    val nestedDecls = packageClasses.flatMap(_.info.decls.toList)

    // A type alias can never be a *direct* member of a package — a top-level one lives in the
    // `$package` class above — so a package's own declarations cannot hold an infix alias and
    // are skipped. That is not just an optimisation: `java.lang` is a root import, and reading
    // the `Infix` flag off each of its members would complete every class in it, which on a JDK
    // whose classfiles this compiler cannot fully read fails outright.
    val aliasDecls = if rootSym.is(Flags.Package) then nestedDecls else directDecls ++ nestedDecls

    val exported = (directDecls ++ nestedDecls).filter(_.is(Flags.Exported))

    (exported.flatMap(exportedTarget), aliasDecls.filter(_.is(Flags.Infix)).flatMap(refiningAlias))


  private def exportedTarget(using Quotes, dotty.tools.dotc.core.Contexts.Context)
    ( decl: dotty.tools.dotc.core.Symbols.Symbol )
  :   List[Designator] =

    import dotty.tools.dotc.core.Types
    import quotes.reflect.TypeRepr

    // A re-export's target, when the forwarder is a plain renaming of it: a simple alias of a
    // `TypeRef`, or a polymorphic one whose body applies a `TypeRef` to exactly its own
    // parameters in order (an exported `infix type on[a, b]`, say). Other polymorphic aliases
    // (tuple types, `Some`, `<:<`, …) reshape their parameters, so their target is not
    // reachable by the forwarder's leaf, and they are skipped.
    def target(tpe: Types.Type): Option[Types.TypeRef] = tpe match
      case ref: Types.TypeRef => Some(ref)

      case lambda: Types.HKTypeLambda => lambda.resType match
        case Types.AppliedType(ref: Types.TypeRef, arguments)
        if arguments.length == lambda.paramNames.length &&
          arguments.zipWithIndex.forall:
            case (param: Types.TypeParamRef, index) =>
              param.binder == lambda && param.paramNum == index
            case _                                  => false
        =>  Some(ref)

        case _ => None

      case _ => None

    decl.info match
      case alias: Types.TypeAlias => target(alias.alias) match
        case Some(ref) =>
          Syntax(ref.asInstanceOf[TypeRepr]) match
            // Add both forms so the same import path can shorten references
            // to either the type itself or its companion (e.g. `Textual` and
            // `Textual.foo` both resolve via `import soundness.*`).
            case Syntax.Simple(designator) => List(designator, designator.companionObject)
            case _                         => Nil

        case None =>
          Nil

      case _ =>
        Nil


  // Recognises an alias of the shape
  //
  //     infix type in [refined, form] = refined { type Form = form }
  //
  // and pairs the refined member's name with the operator's, `"Form" -> "in"`. The two type
  // parameters have to appear in exactly those positions: the first as the refinement's parent
  // and the second as the sole member's alias. Anything else — a fixed parent, a second member,
  // an abstract member, a different arity — has no two-operand infix form and is skipped, which
  // is what excludes `transcribes` and every infix alias that is not a refinement at all.
  private def refiningAlias(using Quotes, dotty.tools.dotc.core.Contexts.Context)
    ( decl: dotty.tools.dotc.core.Symbols.Symbol )
  :   List[(String, Text)] =

    import dotty.tools.dotc.core.Types

    def parameter(repr: Types.Type, binder: Types.HKTypeLambda, index: Int): Boolean =
      repr match
        case ref: Types.TypeParamRef => ref.binder == binder && ref.paramNum == index
        case _                       => false

    val lambda: Optional[Types.HKTypeLambda] = decl.info match
      case alias: Types.TypeAlias => alias.alias match
        case lambda: Types.HKTypeLambda => lambda
        case _                          => Unset

      case lambda: Types.HKTypeLambda => lambda
      case _                          => Unset

    lambda.lay(Nil): lambda =>
      if lambda.paramNames.length != 2 then Nil else
        // An `export`ed alias forwards to the original as an applied type, so dealiasing the
        // body is what lets an alias reached through a prelude be recognised too.
        lambda.resType.dealias match
          case Types.RefinedType(parent, name, Types.TypeAlias(rhs))
          if parameter(parent, lambda, 0) && parameter(rhs, lambda, 1) =>
            List(name.toString -> decl.name.toString.tt)

          case _ =>
            Nil
