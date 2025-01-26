/*
    Anthology, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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
import contingency.*
import parasite.*
import proscenium.*
import turbulence.*
import vacuous.*

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

  def complete()(using Monitor): CompileResult logs CompileEvent raises AsyncError =
    try completion.await() finally
      safely(compilation.let(_.await()))
      safely(noticesSpool.stop())
      safely(progressSpool.stop())

  def abort(): Unit = continue = false
  def cancelled: Boolean = !continue

  lazy val progress: Stream[CompileProgress] = progressSpool.stream
  lazy val notices: Stream[Notice] = noticesSpool.stream
