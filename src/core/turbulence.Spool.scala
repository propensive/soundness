/*
    Turbulence, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import java.util.concurrent as juc

import rudiments.*

object Spool:
  private object Termination

class Spool[ItemType]():
  private val queue: juc.LinkedBlockingQueue[ItemType | Spool.Termination.type] =
    juc.LinkedBlockingQueue()

  def put(item: ItemType): Unit = queue.put(item)
  def stop(): Unit = queue.put(Spool.Termination)

  def stream: Stream[ItemType] =
    Stream.continually(queue.take().nn).takeWhile(_ != Spool.Termination)
    . asInstanceOf[Stream[ItemType]]
