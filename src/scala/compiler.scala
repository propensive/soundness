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

import ambience.*
import anticipation.*
import contingency.*
import digression.*
import gossamer.*
import hellenism.*
import parasite.*
import rudiments.*
import spectacular.*
import vacuous.*

import dotty.tools.dotc as dtd, dtd.reporting.*, dtd.interfaces as dtdi, dtd.util as dtdu, dtd.core as dtdc
import dotty.tools.dotc.sbt.interfaces as dtdsi

import scala.util.control as suc

import language.adhocExtensions

case class ScalacOption[-VersionType <: Scalac.All](flags: Text*)

enum Unused[VersionType]:
  case All extends Unused[3.1 | 3.2 | 3.3 | 3.4 | 3.5]
  case None extends Unused[3.1 | 3.2 | 3.3 | 3.4 | 3.5]
  case Subset[VersionType <: 3.3](features: List[UnusedFeature[VersionType]]) extends Unused[VersionType]

enum UnusedFeature[VersionType](val name: Text):
  case Imports(strict: Boolean) extends UnusedFeature[3.3](t"imports")
  case Privates extends UnusedFeature[3.3 | 3.4](t"privates")
  case Locals extends UnusedFeature[3.3 | 3.4](t"locals")
  case Explicits extends UnusedFeature[3.3 | 3.4](t"explicits")
  case Implicits extends UnusedFeature[3.3 | 3.4](t"implicits")
  case Params extends UnusedFeature[3.3 | 3.4](t"params")
  case Linted extends UnusedFeature[3.3 | 3.4](t"linted")

package scalacOptions:
  val newSyntax = ScalacOption[Scalac.All](t"-new-syntax")
  def sourceFuture = ScalacOption[Scalac.All](t"-source", t"future")
  val experimental = ScalacOption[3.4](t"-experimental")

  package warnings:
    val feature = ScalacOption[Scalac.All](t"-feature")
    val deprecation = ScalacOption[Scalac.All](t"-deprecation")
    val implausiblePatterns = ScalacOption[3.3 | 3.4](t"-Wimplausible-patterns")
    val enumCommentDiscard = ScalacOption[3.4](t"-Wenum-comment-discard")
    val unstableInlineAccessors = ScalacOption[3.4](t"-WunstableInlineAccessors")
    val nonUnitStatement = ScalacOption[3.4](t"-Wnonunit-statement")
    val valueDiscard = ScalacOption[3.4](t"-Wvalue-discard")

    def unused[VersionType <: Scalac.All](selection: Unused[VersionType]) =
      val option = (selection: @unchecked) match
        case Unused.All              => t"-Wunused:all"
        case Unused.None             => t"-Wunused:none"
        case Unused.Subset(features) => features.map(_.name).join(t"-Wunused:", t",", t"")

      ScalacOption[VersionType](option)

    package lint:
      val privateShadow = ScalacOption[3.4](t"-Wshadow:private-shadow")
      val typeParameterShadow = ScalacOption[3.4](t"-Wshadow:type-parameter-shadow")

  package internal:
    val requireTargetName = ScalacOption[Scalac.All](t"-Yrequire-targetName")
    val safeInit = ScalacOption[Scalac.All](t"-Ysafe-init")
    val explicitNulls = ScalacOption[Scalac.All](t"-Yexplicit-nulls")
    val checkPatterns = ScalacOption[Scalac.All](t"-Ycheck-all-patmat")
    val ccNew = ScalacOption[3.4](t"-Ycc-new")

  package advanced:
    def maxInlines(n: Int): ScalacOption[Scalac.All] = ScalacOption(t"-Xmax-inlines", n.show)
  package language:
    package experimental:
      val clauseInterleaving =      ScalacOption[3.3 | 3.4](t"-language:experimental.clauseInterleaving")
      val givenLoopPrevention =     ScalacOption[3.4](t"-language:experimental.givenLoopPrevention")
      val fewerBraces =             ScalacOption[3.1 | 3.2 | 3.3 | 3.4](t"-language:experimental.fewerBraces")
      val into =                    ScalacOption[3.4](t"-language:experimental.into")
      val relaxedExtensionImports = ScalacOption[3.3](t"-language:experimental.relaxedExtensionImports")
      val erasedDefinitions =       ScalacOption[Scalac.All](t"-language:experimental.erasedDefinitions")
      val genericNumberLiterals =   ScalacOption[Scalac.All](t"-language:experimental.genericNumberLiterals")
      val saferExceptions =         ScalacOption[3.2 | 3.3 | 3.4](t"-language:experimental.saferExceptions")
      val namedTypeArguments =      ScalacOption[Scalac.All](t"-language:experimental.namedTypeArguments")
      val pureFunctions =           ScalacOption[3.3 | 3.4](t"-language:experimental.pureFunctions")
      val captureChecking =         ScalacOption[3.3 | 3.4](t"-language:experimental.captureChecking")


object Scalac:
  type All = 3.0 | 3.1 | 3.2 | 3.3 | 3.4 | 3.5
  private var Scala3: dtd.Compiler = new dtd.Compiler()
  def refresh(): Unit = Scala3 = new dtd.Compiler()
  def compiler(): dtd.Compiler = Scala3

extension (companion: Notice.type)
  def apply(diagnostic: Diagnostic): Notice =
    val importance: Importance = Importance.fromOrdinal(diagnostic.level)
    val file: Text = diagnostic.position.map(_.nn.source.nn.name.nn.tt).nn.orElse(t"unknown").nn
    val message: Text = diagnostic.message.tt

    diagnostic.position.map: position =>
      position.nn.pipe: position =>
        val codeRange =
          CodeRange(position.startLine.nn, position.startColumn.nn, position.endLine.nn, position.endColumn.nn)

        Notice(importance, file, message, codeRange)

    . nn.orElse(Notice(importance, file, message, Unset)).nn

case class Scalac[VersionType <: Scalac.All](options: List[ScalacOption[VersionType]]):

  def commandLineArguments: List[Text] = options.flatMap(_.flags)

  def apply(classpath: LocalClasspath)[PathType: GenericPath](sources: Map[Text, Text], out: PathType)
     (using SystemProperties, Monitor, Codicil)
          : CompileProcess logs CompileEvent raises CompilerError =

    val scalacProcess: CompileProcess = CompileProcess()

    object reporter extends Reporter, UniqueMessagePositions, HideNonSensicalMessages:
      def doReport(diagnostic: Diagnostic)(using dtdc.Contexts.Context): Unit =
        Log.fine(CompileEvent.Notice(diagnostic.toString.tt))
        scalacProcess.put(Notice(diagnostic))

    val callbackApi = new dtdi.CompilerCallback {}

    object ProgressApi extends dtdsi.ProgressCallback:
      private var last: Int = -1
      override def informUnitStarting(stage: String, unit: dtd.CompilationUnit): Unit = ()

      override def progress(current: Int, total: Int, currentStage: String, nextStage: String): Boolean =
        val int = (100.0*current/total).toInt

        if int > last then
          last = int
          scalacProcess.put(CompileProgress(last/100.0, currentStage.tt))

        scalacProcess.continue

    object driver extends dtd.Driver:
      val currentCtx =
        val ctx = initCtx.fresh
        //val pluginParams = plugins
        //val jsParams =
        val args: List[Text] =
          List(t"-d", out.pathText, t"-classpath", classpath()) ::: commandLineArguments ::: List(t"")

        Log.info(CompileEvent.Running(args))
        setup(args.map(_.s).to(Array), ctx).map(_(1)).get

      def run(): CompileProcess =
        given dtdc.Contexts.Context = currentCtx.fresh.pipe: ctx =>
          ctx.setReporter(reporter).setCompilerCallback(callbackApi).setProgressCallback(ProgressApi)

        val sourceFiles: List[dtdu.SourceFile] = sources.to(List).map: (name, content) =>
          dtdu.SourceFile.virtual(name.s, content.s)

        scalacProcess.put:
          task(t"scalac"):
            try
              Scalac.compiler().newRun.tap: run =>
                run.compileSources(sourceFiles)
                if !reporter.hasErrors then finish(Scalac.Scala3, run)

              scalacProcess.put(if reporter.hasErrors then CompileResult.Failure else CompileResult.Success)

            catch case suc.NonFatal(error) =>
              scalacProcess.put(CompileResult.Crash(error.stackTrace))
              Scalac.refresh()

        scalacProcess

    driver.run()

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
  case Jdk(version: 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22)
  case Jre(version: 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22)
