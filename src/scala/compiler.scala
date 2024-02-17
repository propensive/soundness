/*
    Anthology, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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
import galilei.*
import contingency.*
import fulminate.*
import ambience.*
import gossamer.*
import rudiments.*
import spectacular.*
import hellenism.*

import scala.collection.mutable as scm

import dotty.tools.*, dotc.*, reporting.*, interfaces as dtdi, util as dtdu, core.*

import language.adhocExtensions

object Compiler:
  private var Scala3 = new dotty.tools.dotc.Compiler()

case class ScalacError() extends Error(msg"there was a compilation error")

type ScalacVersions = 3.0 | 3.1 | 3.2 | 3.3 | 3.4 | 3.5

case class CompileOption[CompilerType <: ScalacVersions](flags: Text*)

enum Unused[CompilerType]:
  case All extends Unused[3.1 | 3.2 | 3.3]
  case None extends Unused[3.1 | 3.2 | 3.3]
  case Subset[CompilerType <: 3.3](features: List[UnusedFeature[CompilerType]]) extends Unused[CompilerType]

enum UnusedFeature[CompilerType](val name: Text):
  case Imports(strict: Boolean) extends UnusedFeature[3.3](t"imports")
  case Privates extends UnusedFeature[3.3](t"privates")
  case Locals extends UnusedFeature[3.3](t"locals")
  case Explicits extends UnusedFeature[3.3](t"explicits")
  case Implicits extends UnusedFeature[3.3](t"implicits")
  case Params extends UnusedFeature[3.3](t"params")
  case Linted extends UnusedFeature[3.3](t"linted")

package scalacOptions:
  val newSyntax = CompileOption[ScalacVersions](t"-new-syntax")

  package warnings:
    val feature = CompileOption[ScalacVersions](t"-feature")
    val deprecation = CompileOption[ScalacVersions](t"-deprecation")
    val implausiblePatterns = CompileOption[3.3](t"-Wimplausible-patterns")
    
    def unused[CompilerType <: ScalacVersions](selection: Unused[CompilerType]) =
      val option = (selection: @unchecked) match
        case Unused.All              => t"-Wunused:all"
        case Unused.None             => t"-Wunused:none"
        case Unused.Subset(features) => features.map(_.name).join(t"-Wunused:", t",", t"")

      CompileOption[CompilerType](option)

  package internal:
    val requireTargetName = CompileOption[ScalacVersions](t"-Yrequire-targetName")
    val safeInit = CompileOption[ScalacVersions](t"-Ysafe-init")
    val explicitNulls = CompileOption[ScalacVersions](t"-Yexplicit-nulls")
    val checkPatterns = CompileOption[ScalacVersions](t"-Ycheck-all-patmat")

  package advanced:
    def maxInlines(n: Int): CompileOption[ScalacVersions] = CompileOption(t"-Xmax-inlines", n.show)

  package language:
    package experimental:
      val clauseInterleaving = CompileOption[3.3 | 3.4](t"-language:experimental.clauseInterleaving")
      val givenLoopPrevention = CompileOption[3.4](t"-language:experimental.givenLoopPrevention")
      val fewerBraces = CompileOption[3.1 | 3.2 | 3.3 | 3.4](t"-language:experimental.fewerBraces")
      val into = CompileOption[3.4](t"-language:experimental.into")
      val relaxedExtensionImports = CompileOption[3.3](t"-language:experimental.relaxedExtensionImports")
      val erasedDefinitions = CompileOption[ScalacVersions](t"-language:experimental.erasedDefinitions")
      val saferExceptions = CompileOption[3.2 | 3.3 | 3.4](t"-language:experimental.saferExceptions")
      val namedTypeArguments = CompileOption[ScalacVersions](t"-language:experimental.namedTypeArguments")
      val pureFunctions = CompileOption[3.3 | 3.4](t"-language:experimental.pureFunctions")
      val captureChecking = CompileOption[3.3 | 3.4](t"-language:experimental.captureChecking")

object Scalac:
  var Scala3: Compiler = new Compiler()

case class Scalac[CompilerType <: ScalacVersions](options: List[CompileOption[CompilerType]]):
  def apply
      (classpath: LocalClasspath)
      (sources: Map[Text, Text], out: Path)
      (using SystemProperties)
      : List[Diagnostic] raises ScalacError =

    object reporter extends Reporter, UniqueMessagePositions, HideNonSensicalMessages:
      val errors: scm.ListBuffer[Diagnostic] = scm.ListBuffer()
      def doReport(diagnostic: Diagnostic)(using core.Contexts.Context): Unit = errors += diagnostic
    
    given (ScalacError fixes SystemPropertyError) =
      case SystemPropertyError(_) => ScalacError()

    given (ScalacError fixes IoError) =
      case IoError(_) => ScalacError()
    
    val separator: Text = Properties.path.separator().show
      
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
        val args: List[Text] = List(t"-d", out.fullname, t"-classpath", classpath()) ::: options.flatMap(_.flags) ::: List(t"")
        setup(args.map(_.s).to(Array), ctx).map(_(1)).get
        
      def run(classpath: LocalClasspath): List[Diagnostic] =
        val ctx = currentCtx.fresh
          
        val ctx2 = ctx
          .setReporter(reporter)
          .setCompilerCallback(callbackApi)
          .setSetting(ctx.settings.language, Nil)
          .setSetting(ctx.settings.classpath, classpath().s)
          
        val sourceFiles: List[dtdu.SourceFile] = sources.to(List).map: (name, content) =>
          dtdu.SourceFile.virtual(name.s, content.s)
          
        Scalac.Scala3.newRun(using ctx2).tap: run =>
          run.compileSources(sourceFiles)
          if !reporter.hasErrors then finish(Scalac.Scala3, run)(using ctx2)
          
        reporter.errors.to(List)
      
    driver.run(classpath)
          
            
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
