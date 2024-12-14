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
import fulminate.*
import gossamer.*
import hellenism.*
import parasite.*
import rudiments.*
import turbulence.*
import vacuous.*

given realm: Realm = realm"anthology"

enum Importance:
  case Info, Warning, Error

case class CodeRange(startLine: Int, startColumn: Int, endLine: Int, endColumn: Int)
case class Notice(importance: Importance, file: Text, message: Text, codeRange: Optional[CodeRange])

case class CompilerError()(using Diagnostics)
extends Error(m"there was a problem with the compiler configuration")

enum CompileResult:
  case Failure
  case Success
  case Crash(error: StackTrace)

case class CompileProgress(complete: Double, stage: Text)

trait Compiler:
  def apply(classpath: LocalClasspath)[PathType: GenericPath]
     (sources: Map[Text, Text], out: PathType)
     (using SystemProperties, Monitor)
          : CompileProcess logs CompileEvent raises CompilerError

class CompileProcess():
  private[anthology] var continue: Boolean = true
  private val completion: Promise[CompileResult] = Promise()
  private val noticesSpool: Spool[Notice] = Spool()
  private val progressSpool: Spool[CompileProgress] = Spool()
  private var compilation: Optional[Task[Unit]] = Unset
  private var errorCount: Int = 0
  private var warningCount: Int = 0

  def put(notice: Notice): Unit =
    noticesSpool.put(notice)

    notice.importance match
      case Importance.Error   => errorCount += 1
      case Importance.Warning => warningCount += 1
      case _                  => ()

  def put(progress: CompileProgress): Unit = progressSpool.put(progress)
  def put(result: CompileResult): Unit = completion.offer(result)
  def put(task: Task[Unit]): Unit = compilation = task

  def complete()(using Monitor): CompileResult logs CompileEvent raises ConcurrencyError =
    try completion.await() finally
      safely(compilation.let(_.await()))
      safely(noticesSpool.stop())
      safely(progressSpool.stop())

  def abort(): Unit = continue = false
  def cancelled: Boolean = !continue

  lazy val progress: LazyList[CompileProgress] = progressSpool.stream
  lazy val notices: LazyList[Notice] = noticesSpool.stream

enum CompileEvent:
  case Start
  case CompilerCrash
  case Notice(diagnostic: Text)
  case Running(args: List[Text])

object CompileEvent:
  given CompileEvent is Communicable =
    case Start              => m"Starting compilation"
    case CompilerCrash      => m"Compiler crashed"
    case Notice(diagnostic) => m"The compiler emitted a diagnostic message: $diagnostic"
    case Running(args)      => m"Running compiler with arguments ${args.join(t" ")}"
