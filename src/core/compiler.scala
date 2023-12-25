/*
    Anthology, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package anthology

import anticipation.*
import galilei.*, filesystemOptions.dereferenceSymlinks
import gossamer.*
import perforate.*
import fulminate.*
import ambience.*
import rudiments.*
import spectacular.*

import scala.collection.mutable as scm

import dotty.tools.*, dotc.*, reporting.*, interfaces as dtdi, util as dtdu, core.*

import language.adhocExtensions

object Compiler:
  private var Scala3 = new dotty.tools.dotc.Compiler()

case class CompilationError() extends Error(msg"there was a compilation error")

object Compilation:
  var Scala3: Compiler = new Compiler()

case class Compilation
    [CompilerType <: Double]
    (sources: Map[Text, Text],
        classpath: List[Path],
        out: Path):
  def apply()(using SystemProperties): List[Diagnostic] raises CompilationError =
    object reporter extends Reporter, UniqueMessagePositions, HideNonSensicalMessages:
      val errors: scm.ListBuffer[Diagnostic] = scm.ListBuffer()
      def doReport(diagnostic: Diagnostic)(using core.Contexts.Context): Unit = errors += diagnostic
    
    mitigate:
      case SystemPropertyError(_) => CompilationError()
      case IoError(_)             => CompilationError()
    .within:
      val separator: Text = Properties.path.separator().show
      
      val classpathText: Text = classpath.map: path =>
        if path.is[Directory] then t"${path.fullname}/" else path.fullname
      .join(separator)
    
      val callbackApi = new dtdi.CompilerCallback:
        override def onClassGenerated
            (source: dtdi.SourceFile, generatedClass: dtdi.AbstractFile, className: String)
            : Unit =
          ()
        
        override def onSourceCompiled(source: dtdi.SourceFile): Unit = ()
      
      object driver extends dotc.Driver:
        val currentCtx =
          val ctx = initCtx.fresh
          //val pluginParams = plugins
          //val jsParams = 

          setup(Array[String]("-d", out.fullname.s, "-deprecation", "-feature", "-Wunused:all",
              "-new-syntax", "-Yrequire-targetName", "-Ysafe-init", "-Yexplicit-nulls", "-Xmax-inlines", "64",
              "-Ycheck-all-patmat", "-classpath", classpathText.s, ""), ctx).map(_(1)).get
        
        def run(classpath: Text): List[Diagnostic] =
          val ctx = currentCtx.fresh
          
          val ctx2 = ctx
            .setReporter(reporter)
            .setCompilerCallback(callbackApi)
            .setSetting(ctx.settings.language, Nil)
            .setSetting(ctx.settings.classpath, classpath.s)
          
          val sourceFiles: List[dtdu.SourceFile] = sources.to(List).map: (name, content) =>
            dtdu.SourceFile.virtual(name.s, content.s)
          
          Compilation.Scala3.newRun(using ctx2).tap: run =>
            run.compileSources(sourceFiles)
            if !reporter.hasErrors then finish(Compilation.Scala3, run)(using ctx2)
          
          reporter.errors.to(List)
      
      driver.run(classpathText)
          
            
enum CompileResult:
  case Success(warnings: List[Text])
  case Failure(errors: List[Text])
  case Crash(details: Text)
  case Aborted

enum Syntax:
  case Old, New

enum WarningFlag:
  case Deprecation, Feature

enum CompileFlag:
  case Explain

enum LanguageFeatures:
  case ClauseInterleaving
  case FewerBraces
  case RelaxedExtensionImports
  case ErasedDefinitions
  case SaferExceptions
  case PostfixOps
  case ImplicitConversions
  case Dynamics
  case ReflectiveCalls
  case CaptureChecking
  case GenericNumberLiterals
  case NamedTypeArguments
  case PureFunctions
  case Into

enum JavaVersion:
  case Java8, Java9, Java10, Java11, Java12, Java13, Java14, Java15, Java16, Java17, Java18, Java19, Java20,
      Java21
