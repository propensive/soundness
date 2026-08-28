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
package larceny

import scala.language.adhocExtensions

import java.io.{File, FileInputStream}
import java.util.Properties
import java.util.jar.JarFile

import scala.collection.mutable as scm
import scala.util.control.NonFatal

import dotty.tools.*, dotc.*, util.*, ast.Trees.*, core.*

import Constants.Constant, Contexts.*, Decorators.*, StdNames.*

import plugins.*

object LarcenyTransformer:
  // Read the `plugin.properties` manifest from a `-Xplugin` path and tell
  // whether it identifies this larceny plugin. Path may be a jar (read via
  // JarFile) or a class directory (read as a plain file). Any I/O error
  // means "not larceny" — the worst that happens is larceny propagates
  // itself and the sub-compilation explodes, which surfaces as a test
  // failure, not silent corruption.
  def isLarceny(path: String): Boolean =
    try
      val file = new File(path)

      val pluginClass =
        if file.isDirectory then
          val propsFile = new File(file, "plugin.properties")

          if !propsFile.exists then null
          else
            val stream = new FileInputStream(propsFile)

            try
              val props = new Properties()
              props.load(stream)
              props.getProperty("pluginClass")
            finally stream.close()
        else
          val jar = new JarFile(file)

          try
            val entry = jar.getEntry("plugin.properties")

            if entry == null then null
            else
              val stream = jar.getInputStream(entry).nn

              try
                val props = new Properties()
                props.load(stream)
                props.getProperty("pluginClass")
              finally stream.close()
          finally jar.close()

      pluginClass == "larceny.LarcenyPlugin"
    catch case NonFatal(_) => false

class LarcenyTransformer() extends PluginPhase:
  val phaseName = "errorcap"
  override val runsAfter = Set("parser")
  override val runsBefore = Set("typer")

  // The default MiniPhase.run wraps this phase in a singleton MegaPhase whose run does
  // `tpdTree = atPhase(this.next)(transformUnit(tpdTree))`. At pretyper position tpdTree
  // is EmptyTree, but the MegaPhase machinery still walks it with the per-node
  // transformXxx pipeline under a typer-phase context, and re-assigns the result. In
  // combination with macro expansion in a class body, that interferes with the namer and
  // produces spurious "X is already defined as class X" errors on code that doesn't even
  // call demilitarize. We do all our work in runOn instead — see issue #452.
  override def run(using Context): Unit = ()

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    val processed = super.runOn(units)
    processed.foreach(transformCompilationUnit)
    processed

  private def transformCompilationUnit(unit: CompilationUnit)(using Context): Unit =
    import ast.untpd.*

    val classpath = ctx.settings.classpath.value
    val language = ctx.settings.language.value

    // `-Ycc-new` selects the capture checker but is not a `-language` setting, so
    // it must be propagated separately or sub-compilations of capture-checked
    // sources would be checked by the old scheme (which misses level violations
    // such as stashing a local capability in an outer mutable variable).
    val ccNew = ctx.settings.YccNew.value

    // The prelude is part of the language environment too: without the parent's
    // `-Yimports`/`-Yno-predef`, helper code outside any `demilitarize` region can
    // fail to resolve in the sub-compilation, and those global errors suppress
    // inlining -- so the regions' own `compiletime.error`s never fire and every
    // capture comes back empty.
    val yimports = ctx.settings.Yimports.value
    val noPredef = ctx.settings.YnoPredef.value

    // Warnings used to be invisible to `demilitarize` in all but one case. The sub-compilation
    // starts from `initCtx.fresh` -- a compiler-default context, not the parent's -- so none of
    // the parent's warning flags applied, and every flag-gated warning (deprecation and unused
    // above all) simply never fired, while default-on warnings leaked in unmarked. Propagate the
    // flags that decide *which* warnings exist.
    //
    // `-Wconf` is the load-bearing one: without the parent's `:s` rules the sub-compilation would
    // report warnings the outer build deliberately silences, so `demilitarize` blocks would start
    // capturing diagnostics that the surrounding file is configured never to see.
    //
    // `-Werror`/`-Xfatal-warnings` is deliberately NOT propagated: promoting warnings to errors
    // would stop compilation at the first one and defeat the point of capturing them as warnings.
    //
    // `Wunused` is `private[config]`, so it is read back through the `WunusedHas` predicates and
    // restated as an explicit choice list. That is equivalent in effect, though not always
    // textually identical to what the parent was given (`-Wunused:all` comes back expanded).
    val unusedChoices: List[(String, Boolean)] =
      List
        ( "imports"   -> ctx.settings.WunusedHas.imports,
          "privates"  -> ctx.settings.WunusedHas.privates,
          "locals"    -> ctx.settings.WunusedHas.locals,
          "explicits" -> ctx.settings.WunusedHas.explicits,
          "implicits" -> ctx.settings.WunusedHas.implicits,
          "params"    -> ctx.settings.WunusedHas.params,
          "patvars"   -> ctx.settings.WunusedHas.patvars,
          "linted"    -> ctx.settings.WunusedHas.linted )

    val unused: List[String] = unusedChoices.collect:
      case (choice, true) => choice

    val warnings: List[String] =
      (if ctx.settings.deprecation.value then List("-deprecation") else Nil) ++
        (if ctx.settings.feature.value then List("-feature") else Nil) ++
        (if ctx.settings.unchecked.value then List("-unchecked") else Nil) ++
        (if ctx.settings.Wall.value then List("-Wall") else Nil) ++
        (if unused.isEmpty then Nil else List("-Wunused:"+unused.mkString(","))) ++
        ctx.settings.Wconf.value.map("-Wconf:"+_)

    object collector extends UntypedTreeMap:
      val regions: scm.ListBuffer[(Int, Int)] = scm.ListBuffer()

      override def transform(tree: Tree)(using Context): Tree = tree match
        case Apply(Ident(name), List(body)) if name.toString == "demilitarize" =>
          try regions += (body.span.start -> body.span.end) catch case error: AssertionError => ()
          tree

        case _ =>
          super.transform(tree)

    collector.transform(unit.untpdTree)
    val regions = collector.regions.to(Set)
    val source = String(unit.source.content)

    // Propagate the outer compilation's `-Xplugin` flags to the
    // sub-compilation, except for larceny itself — otherwise every
    // `demilitarize(...)` in the sub-source would re-fire the larceny
    // transformer and recurse forever. Larceny is identified by reading the
    // `plugin.properties` manifest entry of each plugin path (jar or
    // directory) and matching `pluginClass=larceny.LarcenyPlugin`.
    val plugins: List[String] =
      ctx.settings.plugin.value.filterNot(LarcenyTransformer.isLarceny)

    val errors: List[CompileError] =
      Subcompiler.compile
        ( language, classpath, source, regions, plugins, ccNew, yimports, noPredef, warnings )

    object transformer extends UntypedTreeMap:
      override def transform(tree: Tree)(using Context): Tree = tree match
        case Apply(Ident(name), List(content)) if name.toString == "procrastinate" =>
          val source2 = source.substring(content.span.start, content.span.end).nn
          val javaClasspath = System.getProperty("java.class.path").nn

          Apply
            ( Select
                ( Select
                    ( Select(Ident(nme.ROOTPKG), "larceny".toTermName),
                      "Subcompiler".toTermName ),
                  "compile".toTermName ),
              List
                ( Literal(Constant(javaClasspath+":"+ctx.settings.classpath.value)),
                  Literal(Constant(source2)) ) )

        case Apply(Ident(name), List(content)) if name.toString == "demilitarize" =>
          val captured = errors.filter: error =>
            try error.point >= content.span.start && error.point <= content.span.end
            catch case error: AssertionError => false

          val msgs = captured.map: error =>
            Apply
              ( Select(Select(Ident(nme.ROOTPKG), "larceny".toTermName), "CompileError".toTermName),
                List
                  ( Literal(Constant(error.reason.ordinal)),
                    Literal(Constant(error.message)),
                    Literal(Constant(error.focus)),
                    Literal(Constant(error.start)),
                    Literal(Constant(error.offset)),
                    Literal(Constant(error.importance.ordinal)) ) )

          Apply
            ( Ident(name),
              List
                ( Block
                    ( Nil,
                      Apply(Select(Select(Ident(nme.ROOTPKG), nme.scala), nme.List), msgs) ) ) )

        case _ =>
          super.transform(tree)

    unit.untpdTree = transformer.transform(unit.untpdTree)
