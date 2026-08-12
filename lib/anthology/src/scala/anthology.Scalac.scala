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
package anthology

import proscenium.compat.*

import scala.language.adhocExtensions

import scala.annotation.targetName
import scala.util.control as suc

import dotty.tools.dotc as dtd
import dotty.tools.dotc.core as dtdc
import dotty.tools.dotc.interfaces as dtdi
import dotty.tools.dotc.util as dtdu

import ambience.*
import anticipation.*
import contingency.*
import digression.*
import gossamer.*
import hellenism.*
import nomenclature.n
import parasite.*, Async.nominative
import prepositional.*
import rudiments.*

object Scalac:
  type Versions = 3.0 | 3.1 | 3.2 | 3.3 | 3.4 | 3.5 | 3.6 | 3.7 | 3.8 | 3.9

  case class Option[-version <: Versions](flags: Text*)

  private val mutex: Mutex = Mutex()
  @scala.caps.unsafe.untrackedCaptures
  private var Scala3: dtd.Compiler = new dtd.Compiler()

  def refresh(): Unit = mutex { Scala3 = new dtd.Compiler() }
  def compiler(): dtd.Compiler = Scala3

  // Preserves the single-type-argument call form, `Scalac[3.6](options)`, which targets the
  // classfile universe.
  @targetName("applyClassfile")
  def apply[version <: Versions](options: List[Option[version]])
  :   Scalac[version, Universe.Classfile] =

    new Scalac(options)

  object Setup:
    given sessional: [version <: Versions, universe <: Universe]
    =>  ( system:   System,
          emission: Universe.Emission[universe],
          tactic:   Tactic[Compiler.Error],
          loggable: (CompileEvent is Loggable)^ )
    =>  ( ScalacSessional[version, universe]^{tactic, loggable, scala.caps.any} ) =

      ScalacSessional()

  // A compiler configuration bound to a classpath: the target of a warm compiler session,
  // `scalac.on(classpath).session`, which retains the classpath's loaded symbol table
  // across the session's compiles.
  case class Setup[version <: Versions, universe <: Universe]
    ( scalac: Scalac[version, universe], classpath: LocalClasspath )

case class Scalac[version <: Scalac.Versions, universe <: Universe] private
  ( options: List[Scalac.Option[version]] ):

  def commandLineArguments: List[Text] = options.bind(_.flags)

  def targeting[universe2 <: Universe]: Scalac[version, universe2] = new Scalac(options)

  def on(classpath: LocalClasspath): Scalac.Setup[version, universe] =
    Scalac.Setup(this, classpath)


  def apply
    ( classpath: LocalClasspath )
    [ path: Abstractable across Paths to Text ]
    ( sources: Map[Text, Text], out: path )
    ( using System, Monitor, Probate, Universe.Emission[universe] )
    ( using Tactic[Compiler.Error], (CompileEvent is Loggable)^ )
  :   CompileProcess =

    val scalacProcess: CompileProcess = CompileProcess()
    val reporter = processReporter(scalacProcess)

    val arguments: List[Text] =
      summon[Universe.Emission[universe]].flags :::
        List(t"-d", out.generic, t"-classpath", classpath()) :::
        commandLineArguments :::
        List(t"")

    val driver = ScalacDriver()
    val currentContext = driver.baseContext(arguments).get

    given dtdc.Contexts.Context = currentContext.fresh.pipe: context =>
      context
      . setReporter(reporter)
      . setCompilerCallback(new dtdi.CompilerCallback {})
      . setProgressCallback(progressCallback(scalacProcess))

    val sourceFiles: scala.collection.immutable.List[dtdu.SourceFile] =
      sources.stdlib.toList.map: (name, content) =>
        dtdu.SourceFile.virtual(name.s, content.s)

    scalacProcess.put:
      // The run compiles under this process's own compiler and reporter; no aliased
      // writer.
      scala.caps.unsafe.unsafeAssumeSeparate:
       task(n"scalac"):
        try
          Scalac.compiler().newRun.tap: run =>
            run.compileSources(sourceFiles)
            if !reporter.hasErrors then driver.finishRun(Scalac.Scala3, run)

          scalacProcess.put
            ( if reporter.hasErrors then CompileResult.Failure else CompileResult.Success )

        catch case suc.NonFatal(error) =>
          scalacProcess.put(CompileResult.Crash(error.stackTrace))
          Scalac.refresh()

    scalacProcess
