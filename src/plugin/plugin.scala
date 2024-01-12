/*
    Larceny, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package larceny

import dotty.tools.*, dotc.*, util.*, reporting.*, ast.Trees.*, ast.tpd, core.*, Constants.Constant, Contexts.*,
    Decorators.*, StdNames.*, plugins.*

import scala.util.chaining.*
import scala.collection.mutable as scm

import language.adhocExtensions

class LarcenyPlugin() extends StandardPlugin:
  val name: String = "larceny"
  override val description: String = "capture errors"
  def init(options: List[String]): List[PluginPhase] = List(LarcenyTransformer())

class LarcenyTransformer() extends PluginPhase:
  import tpd.*

  val phaseName = "errorcap"
  override val runsAfter = Set("parser")
  override val runsBefore = Set("typer")

  override def transformUnit(tree: Tree)(using Context): Tree =
    import ast.untpd.*
    val classpath = ctx.settings.classpath.value
    
    object collector extends UntypedTreeMap:
      val regions: scm.ListBuffer[(Int, Int)] = scm.ListBuffer()
      
      override def transform(tree: Tree)(using Context): Tree = tree match
        case Apply(Ident(name), List(body)) if name.toString == "demilitarize" =>
          try regions += (body.span.start -> body.span.end) catch case err: AssertionError => ()
          tree
        
        case _ =>
          super.transform(tree)
          
    collector.transform(ctx.compilationUnit.untpdTree)
    val regions = collector.regions.to(Set)

    val source = String(ctx.compilationUnit.source.content)

    val errors: List[CompileError] =
      Subcompiler.compile(ctx.settings.classpath.value, source, regions)


    object transformer extends UntypedTreeMap:

      override def transform(tree: Tree)(using Context): Tree = tree match
        case Apply(Ident(name), List(content)) if name.toString == "deferCompilation" =>
          val source2 = source.substring(content.span.start, content.span.end)
          val javaClasspath = System.getProperty("java.class.path").nn
          Apply(Select(Select(Select(Ident(nme.ROOTPKG), "larceny".toTermName),
              "Subcompiler".toTermName), "compile".toTermName), List(
            Literal(Constant(javaClasspath+":"+ctx.settings.classpath.value)),
            Literal(Constant(source2))
          ))

        case Apply(Ident(name), List(content)) if name.toString == "demilitarize" =>
          
          val captured = errors.filter: error =>
            try error.point >= content.span.start && error.point <= content.span.end
            catch case err: AssertionError => false
          
          val msgs = captured.map: error =>
            Apply(Select(Select(Ident(nme.ROOTPKG), "larceny".toTermName), "CompileError".toTermName), List(
              Literal(Constant(error.id)),
              Literal(Constant(error.message)),
              Literal(Constant(error.code)),
              Literal(Constant(error.start)),
              Literal(Constant(error.offset))
            ))
          
          Apply(Ident(name), List(Block(List(), Apply(Select(Select(Ident(nme.ROOTPKG), nme.scala),
              nme.List), msgs))))
        
        case _ =>
          super.transform(tree)
    
    ctx.compilationUnit.untpdTree = transformer.transform(ctx.compilationUnit.untpdTree)
    super.transformUnit(tree)

object Subcompiler:
  val Scala3: Compiler = new Compiler()

  class CustomReporter() extends Reporter, UniqueMessagePositions, HideNonSensicalMessages:
    val errors: scm.ListBuffer[CompileError] = scm.ListBuffer()
    
    def doReport(diagnostic: Diagnostic)(using Context): Unit =
      try
        val pos = diagnostic.pos
        val code = String(ctx.compilationUnit.source.content.slice(pos.start, pos.end))
        val offset = pos.point - pos.start
        
        errors += CompileError(diagnostic.msg.errorId.ordinal, diagnostic.msg.message, code,
            pos.start, offset)
      
      catch case err: Throwable => ()

  def compile(classpath: String, source: String): List[CompileError] =
    compile(classpath, source, Set((0, source.length)))

  def compile(classpath: String, source: String, regions: Set[(Int, Int)]): List[CompileError] =
    
    object driver extends Driver:
      val currentCtx: Context =
        val ctx = initCtx.fresh
        val ctx2 = ctx.setSetting(ctx.settings.classpath, classpath)
        setup(Array[String](""), ctx2).map(_(1)).get
      
      def run
          (source: String, regions: Set[(Int, Int)], errors: List[CompileError])
          : List[CompileError] =
        if regions.isEmpty then errors else
          val reporter: CustomReporter = CustomReporter()
          val sourceFile: SourceFile = SourceFile.virtual("<subcompilation>", source)
          val ctx = currentCtx.fresh
          
          val ctx2 = ctx.setReporter(reporter)
              .setSetting(ctx.settings.classpath, classpath)
              .setSetting(ctx.settings.YstopBefore, List("genSJSIR"))
              .setSetting(ctx.settings.color, "never")
          
          Scala3.newRun(using ctx2).tap: run =>
            run.compileSources(List(sourceFile))
            if !reporter.hasErrors then finish(Scala3, run)(using ctx2)
          
          val newErrors = reporter.errors.to(List)

          def recompile
              (todo: List[CompileError], done: Set[(Int, Int)], source: String)
              : List[CompileError] =
            todo match
              case Nil =>
                if done.isEmpty then errors ::: newErrors
                else run(source, regions -- done, errors ::: newErrors)
              case error :: tail =>
                regions.find: (start, end) =>
                  error.point >= start && error.point <= end
                .match
                  case None =>
                    recompile(tail, done, source)
                  
                  case Some(region@(from, to)) =>
                    if done.contains(region) then recompile(tail, done, source) else
                      val newSource = source.take(from)+"{}"+(" "*(to - from - 2))+source.drop(to)
                      recompile(tail, done + region, newSource)
    
          recompile(newErrors, Set(), source)
      
    driver.run(source, regions, Nil)
