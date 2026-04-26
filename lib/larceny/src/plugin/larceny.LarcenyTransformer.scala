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

import scala.collection.mutable as scm

import dotty.tools.*, dotc.*, util.*, ast.Trees.*, ast.tpd, core.*,
    Constants.Constant, Contexts.*, Decorators.*, StdNames.*, plugins.*

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

    val errors: List[CompileError] =
      Subcompiler.compile(language, classpath, source, regions)

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
                    Literal(Constant(error.code)),
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
