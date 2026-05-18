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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import language.adhocExtensions

import java.io.{File, FileInputStream}
import java.util.Properties
import java.util.jar.JarFile

import scala.collection.mutable as scm
import scala.util.control.NonFatal

import dotty.tools.*, dotc.*, util.*, ast.Trees.*, ast.tpd, core.*,
    Constants.Constant, Contexts.*, Decorators.*, StdNames.*, plugins.*

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
  import tpd.*

  val phaseName = "errorcap"
  override val runsAfter = Set("parser")
  override val runsBefore = Set("typer")

  override def transformUnit(tree: Tree)(using context: Context): Tree =
    import ast.untpd.*

    val classpath = context.settings.classpath.value
    val language = context.settings.language.value

    object collector extends UntypedTreeMap:
      val regions: scm.ListBuffer[(Int, Int)] = scm.ListBuffer()

      override def transform(tree: Tree)(using Context): Tree = tree match
        case Apply(Ident(name), List(body)) if name.toString == "demilitarize" =>
          try regions += (body.span.start -> body.span.end) catch case error: AssertionError => ()
          tree

        case _ =>
          super.transform(tree)

    collector.transform(context.compilationUnit.untpdTree)
    val regions = collector.regions.to(Set)
    val source = String(context.compilationUnit.source.content)

    // Propagate the outer compilation's `-Xplugin` flags to the
    // sub-compilation, except for larceny itself — otherwise every
    // `demilitarize(...)` in the sub-source would re-fire the larceny
    // transformer and recurse forever. Larceny is identified by reading the
    // `plugin.properties` manifest entry of each plugin path (jar or
    // directory) and matching `pluginClass=larceny.LarcenyPlugin`.
    val plugins: List[String] =
      context.settings.plugin.value.filterNot(LarcenyTransformer.isLarceny)

    val errors: List[CompileError] =
      Subcompiler.compile(language, classpath, source, regions, plugins)

    object transformer extends UntypedTreeMap:
      override def transform(tree: Tree)(using Context): Tree = tree match
        case Apply(Ident(name), List(content)) if name.toString == "procrastinate" =>
          val source2 = source.substring(content.span.start, content.span.end)
          val javaClasspath = System.getProperty("java.class.path").nn
          Apply
            ( Select
                ( Select
                    ( Select(Ident(nme.ROOTPKG), "larceny".toTermName),
                      "Subcompiler".toTermName ),
                  "compile".toTermName ),
              List
                ( Literal(Constant(javaClasspath+":"+context.settings.classpath.value)),
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
                    Literal(Constant(error.offset)) ) )

          Apply
            ( Ident(name),
              List
                ( Block
                    ( Nil,
                      Apply(Select(Select(Ident(nme.ROOTPKG), nme.scala), nme.List), msgs) ) ) )

        case _ =>
          super.transform(tree)

    context.compilationUnit.untpdTree = transformer.transform(context.compilationUnit.untpdTree)
    super.transformUnit(tree)
