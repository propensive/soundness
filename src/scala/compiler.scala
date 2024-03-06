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
import parasite.*
import eucalyptus.*
import turbulence.*
import vacuous.*
import gossamer.*
import rudiments.*
import spectacular.*
import hellenism.*

given Realm = realm"anthology"

import dotty.tools.*, dotc.*, reporting.*, interfaces as dtdi, util as dtdu, core.*
import dotty.tools.dotc.sbt.interfaces as dtdsi

import language.adhocExtensions

object Compiler:
  private var Scala3 = new dotty.tools.dotc.Compiler()

case class ScalacError() extends Error(msg"there was a compilation error")

type ScalacVersions = 3.0 | 3.1 | 3.2 | 3.3 | 3.4 | 3.5

case class CompileOption[-CompilerType <: ScalacVersions](flags: Text*)

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
  def sourceFuture = CompileOption[ScalacVersions](t"-source", t"future")
  val experimental = CompileOption[3.4](t"-experimental")

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
      val clauseInterleaving =      CompileOption[3.3 | 3.4](t"-language:experimental.clauseInterleaving")
      val givenLoopPrevention =     CompileOption[3.4](t"-language:experimental.givenLoopPrevention")
      val fewerBraces =             CompileOption[3.1 | 3.2 | 3.3 | 3.4](t"-language:experimental.fewerBraces")
      val into =                    CompileOption[3.4](t"-language:experimental.into")
      val relaxedExtensionImports = CompileOption[3.3](t"-language:experimental.relaxedExtensionImports")
      val erasedDefinitions =       CompileOption[ScalacVersions](t"-language:experimental.erasedDefinitions")
      val genericNumberLiterals =   CompileOption[ScalacVersions](t"-language:experimental.genericNumberLiterals")
      val saferExceptions =         CompileOption[3.2 | 3.3 | 3.4](t"-language:experimental.saferExceptions")
      val namedTypeArguments =      CompileOption[ScalacVersions](t"-language:experimental.namedTypeArguments")
      val pureFunctions =           CompileOption[3.3 | 3.4](t"-language:experimental.pureFunctions")
      val captureChecking =         CompileOption[3.3 | 3.4](t"-language:experimental.captureChecking")

object Scalac:
  var Scala3: Compiler = new Compiler()

enum Importance:
  case Info, Warning, Error

object Notice:
  def apply(diagnostic: Diagnostic): Notice =
    val importance: Importance = Importance.fromOrdinal(diagnostic.level)
    val message: Text = diagnostic.message.tt
    val content: Text = diagnostic.position.map(_.nn.lineContent.nn.tt).nn.orElse(t"").nn
    
    Notice(importance, message, content)

case class Notice(importance: Importance, message: Text, code: Text)

case class Scalac[CompilerType <: ScalacVersions](options: List[CompileOption[CompilerType]]):

  def commandLineArguments: List[Text] = options.flatMap(_.flags)

  def apply(classpath: LocalClasspath)[PathType: GenericPath](sources: Map[Text, Text], out: PathType)
      (using SystemProperties, Log[Text], Monitor)
          : ScalacProcess raises ScalacError =
    
    val scalacProcess: ScalacProcess = ScalacProcess()

    object reporter extends Reporter, UniqueMessagePositions, HideNonSensicalMessages:
      def doReport(diagnostic: Diagnostic)(using core.Contexts.Context): Unit =
        Log.fine(Notice(diagnostic).debug)
        scalacProcess.put(Notice(diagnostic))
    
    given (ScalacError fixes SystemPropertyError) =
      case SystemPropertyError(_) => ScalacError()

    given (ScalacError fixes IoError) =
      case IoError(_) => ScalacError()
    
    val separator: Text = Properties.path.separator().show
      
    val callbackApi = new dtdi.CompilerCallback:
      override def onClassGenerated
          (source: dtdi.SourceFile, generatedClass: dtdi.AbstractFile, className: String)
              : Unit =

        return ()
        
      override def onSourceCompiled(source: dtdi.SourceFile): Unit = ()
    
    object ProgressApi extends dtdsi.ProgressCallback:
      private var last: Int = -1
      override def informUnitStarting(stage: String, unit: CompilationUnit): Unit = ()
      
      override def progress(current: Int, total: Int, currentStage: String, nextStage: String): Boolean =
        val int = (100.0*current/total).toInt
        
        if int > last then
          last = int
          scalacProcess.put(CompileProgress(last/100.0, currentStage.tt))
        
        scalacProcess.continue
      
    object driver extends dotc.Driver:
      val currentCtx =
        val ctx = initCtx.fresh
        //val pluginParams = plugins
        //val jsParams = 
        val args: List[Text] =
          List(t"-d", out.pathText, t"-classpath", classpath()) ::: commandLineArguments ::: List(t"")

        setup(args.map(_.s).to(Array), ctx).map(_(1)).get
        
      def run(): ScalacProcess =
        given Contexts.Context = currentCtx.fresh.pipe: ctx =>
          ctx.setReporter(reporter).setCompilerCallback(callbackApi).setProgressCallback(ProgressApi)
        
        val sourceFiles: List[dtdu.SourceFile] = sources.to(List).map: (name, content) =>
          dtdu.SourceFile.virtual(name.s, content.s)
          
        Async:
          Scalac.Scala3.newRun.tap: run =>
            run.compileSources(sourceFiles)
            if !reporter.hasErrors then finish(Scalac.Scala3, run)
            
            scalacProcess.put:
              if reporter.hasErrors then CompileResult.Failed else CompileResult.Succeeded
          .unit
  
        scalacProcess
      
    driver.run()

case class CompileProgress(complete: Double, stage: Text)

class ScalacProcess():
  private[anthology] var continue: Boolean = true
  private val completion: Promise[CompileResult] = Promise()
  private val noticesFunnel: Funnel[Notice] = Funnel()
  private val progressFunnel: Funnel[CompileProgress] = Funnel()

  def put(notice: Notice): Unit = noticesFunnel.put(notice)
  def put(progress: CompileProgress): Unit = progressFunnel.put(progress)
  def put(result: CompileResult): Unit = completion.offer(result)

  def complete()(using Log[Text]): CompileResult raises CancelError =
    completion.await().also:
      noticesFunnel.stop()
      progressFunnel.stop()
  
  def abort(): Unit = continue = false
  def cancelled: Boolean = continue == false

  lazy val progress: LazyList[CompileProgress] = progressFunnel.stream
  lazy val notices: LazyList[Notice] = noticesFunnel.stream

enum CompileResult:
  case Failed
  case Succeeded

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
  case Jdk(version: 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21)
