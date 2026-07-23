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
package prescience

import scala.collection.immutable.{List, Nil, ::}
import scala.quoted.*

import prepositional.*

object internal:

  // ── Tier A: static-path instance evaluation ───────────────────────────────
  // Obtains the live value behind a summoned instance `Expr` when — and only
  // when — its tree is a reference to a precompiled static object, or a
  // parameterless member of one. This mirrors what the compiler's own splice
  // interpreter does to run a macro implementation (`Interpreter.loadModule`
  // reads `MODULE$` through the macro classloader and invokes methods on it
  // reflectively), applied to a summoned given instead of the splice entry
  // point. The load and the cast are sound because the instance's class and
  // this macro's classes come from the same classloader.
  //
  // Anything else — a conditional given (an `Apply`), a lexically-scoped or
  // same-run instance (whose classfile does not exist yet) — yields `None`,
  // and the caller degrades to the next tier.
  private[prescience] def evaluate(using Quotes)(term: quotes.reflect.Term): Option[AnyRef] =
    import quotes.reflect.*

    def loadModule(binaryName: String): Option[AnyRef] =
      try
        val loaded = Class.forName(binaryName, true, macroClassloader).nn
        Option(loaded.getField("MODULE$").nn.get(null))
      catch
        case _: ReflectiveOperationException => None
        case _: LinkageError                 => None

    def invokeMember(target: AnyRef, name: String): Option[AnyRef] =
      try
        val method = target.getClass.nn.getMethods.nn.find: method =>
          method.nn.getName == name && method.nn.getParameterCount == 0

        method.flatMap { method => Option(method.nn.invoke(target)) }
      catch
        case _: ReflectiveOperationException => None
        case _: LinkageError                 => None

    def moduleBinaryName(symbol: Symbol): String =
      val name = symbol.fullName
      if name.endsWith("$") then name else name+"$"

    def eval(term: Term): Option[AnyRef] = term match
      case Inlined(_, Nil, inner) => eval(inner)
      case Typed(inner, _)        => eval(inner)
      case Block(Nil, inner)      => eval(inner)

      case select @ Select(qualifier, name) =>
        if definedInCurrentRun(select.symbol) then None
        else if select.symbol.flags.is(Flags.Module) then
          loadModule(moduleBinaryName(select.symbol))
        else eval(qualifier).flatMap(invokeMember(_, name))

      case ref: Ref =>
        val symbol = ref.symbol

        if definedInCurrentRun(symbol) then None
        else if symbol.flags.is(Flags.Module) then loadModule(moduleBinaryName(symbol))
        else if symbol.owner.flags.is(Flags.Module) then
          loadModule(moduleBinaryName(symbol.owner)).flatMap(invokeMember(_, symbol.name))
        else None

      case _ =>
        None

    eval(term)

  private def macroClassloader: ClassLoader =
    // The Splicer installs the macro classloader as the thread context
    // classloader for the duration of the expansion.
    val contextual = Thread.currentThread.nn.getContextClassLoader
    if contextual != null then contextual else getClass.getClassLoader.nn

  // The current compilation's output directory, through the compiler's own
  // context (reachable because `scala3-compiler` is on this module's
  // classpath). `None` when the output is virtual or the internals shift.
  private def currentOutputDirectory(using Quotes): Option[String] =
    try
      val ctx = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
      given dotty.tools.dotc.core.Contexts.Context = ctx
      import dotty.tools.dotc.config.Settings.Setting.value

      Option(ctx.settings.outputDir.value.file).map(_.nn.getCanonicalPath.nn)
    catch case _: Exception => None

  // Whether a symbol's definition belongs to the compilation run in
  // progress: its classfile either does not exist yet or — worse — exists
  // stale from a previous incremental compile, so neither evaluation tier
  // may touch it and the caller must degrade to the runtime tier.
  private def definedInCurrentRun(using Quotes)(symbol: quotes.reflect.Symbol): Boolean =
    try
      val ctx = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
      val run = ctx.run

      if run == null then false else
        val sources = run.nn.units.map(_.source.file.path).toSet
        val position = symbol.pos
        position.exists { position => sources.contains(position.sourceFile.path) }
    catch case _: Exception => false

  // The macro classpath minus the current output directory, so the staging
  // tier's inner compiler can never resolve a same-run definition against a
  // stale classfile.
  private def innerClasspath(using Quotes): String =
    val separator = java.io.File.pathSeparator.nn
    val full = dotty.tools.dotc.util.ClasspathFromClassloader(macroClassloader).nn

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

  // ── Tier B: implicit search under an in-macro staging compiler ────────────
  // Evaluates any instance that implicit search can resolve from *global*
  // scope (companions and package-level givens on the classpath): a fresh
  // `staging.Compiler` is constructed over the macro classloader — fresh, so
  // its nested-`run` guard cannot trip — and the search runs inside
  // `staging.run`, whose result is the live instance. The outer macro's
  // `Type` cannot cross into the inner context, so the field type travels by
  // `java.lang.Class`, rebuilt inside via `TypeRepr.typeConstructorOf` —
  // which restricts this tier to non-generic class types. Lexically-scoped
  // givens at the use site are invisible to the inner search, and each call
  // costs a full (in-memory) compiler run.
  private[prescience] def summonStaged[field: Type](using Quotes): Option[Inlinable] =
    import quotes.reflect.*
    import scala.quoted.staging

    TypeRepr.of[field].classSymbol.flatMap: classSymbol =>
      if definedInCurrentRun(classSymbol) then None else try
        val clazz = Class.forName(classSymbol.fullName, false, macroClassloader).nn

        // The compiled snippet lands in a real directory, and the inner
        // compiler's classpath excludes the current output directory, so a
        // same-run instance can never resolve against a stale classfile from
        // a previous incremental compile. `-experimental` because the repo
        // compiles under experimental language features, so its symbols are
        // `@experimental`-tagged.
        val outputDir: String =
          java.nio.file.Files.createTempDirectory("prescience").nn.toString

        given settings: staging.Compiler.Settings =
          staging.Compiler.Settings.make
            (Some(outputDir), List("-experimental", "-classpath", innerClasspath))

        given staging.Compiler = staging.Compiler.make(macroClassloader)
        val started = System.nanoTime

        val result: Any = staging.run:
          val quotes2 = summon[Quotes]
          import quotes2.reflect as r2

          val fieldType = r2.TypeRepr.typeConstructorOf(clazz)

          val target =
            r2.Refinement
              (r2.TypeRepr.of[Inlinable], "Self", r2.TypeBounds(fieldType, fieldType))

          // The summon is embedded in the snippet rather than performed here
          // eagerly: the run's expression builder executes post-typer (phase
          // `quotedFrontend`), where a failing implicit search asserts, while
          // an embedded `summonInline` resolves during the inner compiler's
          // own inlining phase — an ordinary search context.
          target.asType match
            case '[target] => '{ scala.compiletime.summonInline[target] }

        // The snippet's classes are now ordinary files in `outputDir`; a new
        // classloader over that directory (parented by the macro classloader,
        // which supplies the instance's own classes) can reconstruct the
        // instance independently of `staging.run`'s internal loader — the
        // basis for caching a compiled summon across expansions.
        val reloaded: Any =
          try
            val url = java.io.File(outputDir).toURI.nn.toURL.nn
            val loader = java.net.URLClassLoader(Array(url), macroClassloader)
            val generated = loader.loadClass("Generated$Code$From$Quoted").nn
            val instance = generated.getConstructor().nn.newInstance()
            generated.getMethod("apply").nn.invoke(instance)
          catch
            case _: ReflectiveOperationException => result
            case _: LinkageError                 => result

        val duration = (System.nanoTime - started)/1000000L

        report.info
          (s"prescience: staged summon for ${classSymbol.fullName} took ${duration}ms; "+
            s"classes in $outputDir")

        reloaded match
          case instance: Inlinable => Some(instance)
          case _                   => None
      catch
        case _: ReflectiveOperationException => None
        case _: LinkageError                 => None
        case _: AssertionError               => None
        case _: Exception                    => None

  // ── The deriving macro ─────────────────────────────────────────────────────
  // One flat reader per case class: the input splits once, and each field's
  // code comes from its `Inlinable` instance — inlined when the instance is
  // evaluable at expansion time (tier A, then optionally tier B), a spliced
  // runtime call otherwise.
  def readStaged[value: Type](input: Expr[String], staging: Expr[Boolean])(using Quotes)
  :   Expr[value] =

    import quotes.reflect.*

    val useStaging: Boolean = staging.valueOrAbort
    val tpe = TypeRepr.of[value].dealias

    val classSymbol = tpe.classSymbol.getOrElse:
      report.errorAndAbort("prescience: reading requires a case class")

    if !classSymbol.flags.is(Flags.Case) then
      report.errorAndAbort("prescience: reading requires a case class")

    tpe match
      case AppliedType(_, _) =>
        report.errorAndAbort("prescience: generic case classes are out of scope for the PoC")

      case _ =>
        ()

    val ctor = classSymbol.primaryConstructor
    val fields = classSymbol.caseFields
    val arity = fields.length
    val fieldTypes: List[TypeRepr] = fields.map { field => tpe.memberType(field).dealias }

    def readField(index: Int, parts: Expr[Array[String]]): Term =
      fieldTypes(index).asType match
        case '[fieldType] =>
          val fieldExpr: Expr[String] = '{ $parts(${Expr(index)}) }

          val instanceExpr: Expr[fieldType is Inlinable] =
            Expr.summon[fieldType is Inlinable].getOrElse:
              report.errorAndAbort
                (s"prescience: no Inlinable instance for field ${fields(index).name}: "+
                  fieldTypes(index).show)

          def runtimeCall: Term = '{ $instanceExpr.readRuntime($fieldExpr) }.asTerm

          evaluate(instanceExpr.asTerm) match
            case Some(instance) =>
              instance.asInstanceOf[Inlinable].read(fieldExpr).asExprOf[fieldType].asTerm

            case None =>
              if useStaging then
                summonStaged[fieldType] match
                  case Some(instance) => instance.read(fieldExpr).asExprOf[fieldType].asTerm
                  case None           => runtimeCall
              else runtimeCall

    def construct(parts: Expr[Array[String]]): Expr[value] =
      Apply(Select(New(Inferred(tpe)), ctor), List.range(0, arity).map(readField(_, parts)))
      . asExprOf[value]

    '{
      val parts: Array[String] = $input.split(",").nn.map(_.nn)
      ${ construct('parts) }
    }
