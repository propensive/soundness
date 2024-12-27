/*
    Turbulence, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import language.experimental.pureFunctions

import java.util.concurrent as juc

import anticipation.*
import parasite.*, orphanDisposal.await
import rudiments.*

object Multiplexer:
  private object Termination

case class Multiplexer[KeyType, ElementType]()(using Monitor):
  private val tasks: TrieMap[KeyType, Task[Unit]] = TrieMap()

  private val queue: juc.LinkedBlockingQueue[ElementType | Multiplexer.Termination.type] =
    juc.LinkedBlockingQueue()

  def close(): Unit = tasks.keys.each(remove(_))

  @tailrec
  private def pump(key: KeyType, stream: LazyList[ElementType])(using Worker): Unit =
    if stream.isEmpty then remove(key) else
      relent()
      queue.put(stream.head)
      pump(key, stream.tail)

  def add(key: KeyType, stream: LazyList[ElementType]): Unit = tasks(key) = async(pump(key, stream))

  private def remove(key: KeyType): Unit = synchronized:
    tasks -= key
    if tasks.isEmpty then queue.put(Multiplexer.Termination)

  def stream: LazyList[ElementType] =
    LazyList.continually(queue.take().nn).takeWhile(_ != Multiplexer.Termination)
    . asInstanceOf[LazyList[ElementType]]
