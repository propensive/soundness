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

import scala.caps

import anticipation.*
import murmuration.fold
import rudiments.each
import rudiments.reverse
import contingency.*
import parasite.*
import prepositional.*
import turbulence.*
import zephyrine.*
import vacuous.*

object CompileProcess:
  // One element of the live update feed: a diagnostic or a progress tick, in
  // arrival order across the compilation.
  enum Update:
    case Noticed(notice: Notice)
    case Progressed(progress: CompileProgress)

class CompileProcess():
  @scala.caps.unsafe.untrackedCaptures
  private[anthology] var continue: Boolean = true

  private val completion: Promise[CompileResult] = Promise()
  private val relay: Relay[CompileProcess.Update] = Relay()

  @scala.caps.unsafe.untrackedCaptures
  private var compilation: Optional[Task[Unit]] = Unset
  @scala.caps.unsafe.untrackedCaptures
  private var errorCount: Int = 0
  @scala.caps.unsafe.untrackedCaptures
  private var warningCount: Int = 0
  // Newest-first; reversed by the `notices` accessor.
  @scala.caps.unsafe.untrackedCaptures
  private var noticeList: List[Notice] = Nil

  def put(notice: Notice): Unit =
    noticeList = notice :: noticeList
    relay.put(CompileProcess.Update.Noticed(notice))

    notice.importance match
      case Importance.Error   => errorCount += 1
      case Importance.Warning => warningCount += 1
      case _                  => ()

  def put(progress: CompileProgress): Unit =
    relay.put(CompileProcess.Update.Progressed(progress))

  def put(result: CompileResult): Unit = completion.offer(result)
  def put(task: Task[Unit]): Unit = compilation = task
  def errors: Int = errorCount
  def warnings: Int = warningCount

  def complete()(using Monitor)
    ( using Tactic[Async.Error], (CompileEvent is Loggable)^ )
  :   CompileResult =
    try completion.await() finally
      safely(compilation.let(_.await()))
      safely(relay.stop())

  def abort(): Unit = continue = false
  def cancelled: Boolean = !continue

  // The live update feed: a single-owner, separation-checked pull endpoint over the
  // relay, whose refill blocks for the first update and then drains whatever else has
  // arrived into the same window. Claim it once; consume element-wise with
  // `updates.records`. It ends when the compilation completes (`complete` stops the
  // relay), so a claim after completion replays the whole feed and then ends.
  def updates(using Buffering)
  :   (Stream[Array[CompileProcess.Update]^{}] over Credit)^ =
    relay.stream

  // The diagnostics reported so far, oldest first: a strict snapshot for post-hoc
  // inspection, complete once `complete()` has returned. Live consumers should prefer
  // `updates`.
  def notices: List[Notice] = noticeList.reverse
