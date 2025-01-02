/*
    Turbulence, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package turbulence

import language.experimental.captureChecking

import java.util.concurrent.atomic as juca

import java.util.concurrent as juc

object Tap:
  enum Regulation:
    case Start, Stop

class Tap(initial: Boolean = true):
  private val flowing: juca.AtomicBoolean = juca.AtomicBoolean(initial)
  private val spool: Spool[Tap.Regulation] = Spool()

  def resume(): Unit = if !flowing.getAndSet(true) then spool.put(Tap.Regulation.Start)
  def pause(): Unit = if flowing.getAndSet(false) then spool.put(Tap.Regulation.Stop)
  def stop(): Unit = spool.stop()
  def state(): Boolean = flowing.get
  def stream: LazyList[Tap.Regulation] = spool.stream
