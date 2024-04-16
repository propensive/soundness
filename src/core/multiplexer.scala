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

import parasite.*, asyncOptions.waitForOrphans
import rudiments.*
import vacuous.*
import anticipation.*
import contingency.*

import java.util.concurrent as juc

import language.experimental.pureFunctions

object Multiplexer:
  private object Termination

case class Multiplexer[KeyType, ElementType]()(using Monitor):
  private val tasks: TrieMap[KeyType, Task[Unit]] = TrieMap()
  
  private val queue: juc.LinkedBlockingQueue[ElementType | Multiplexer.Termination.type] =
    juc.LinkedBlockingQueue()

  def close(): Unit = tasks.keys.each(remove(_))

  @tailrec
  private def pump(key: KeyType, stream: LazyList[ElementType])(using Subordinate): Unit =
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
     .asInstanceOf[LazyList[ElementType]]

extension [ElementType](stream: LazyList[ElementType])

  def deduplicate: LazyList[ElementType] =
    def recur(last: ElementType, stream: LazyList[ElementType]): LazyList[ElementType] =
      stream match
        case head #:: tail => if last == head then recur(last, tail) else head #:: recur(head, tail)
        case _             => LazyList()

    stream match
      case head #:: tail => head #:: recur(head, tail)
      case _             => LazyList()

  def rate[DurationType: GenericDuration: SpecificDuration](duration: DurationType)
      (using Monitor, Errant[ConcurrencyError])
          : LazyList[ElementType] =
    
    def recur(stream: LazyList[ElementType], last: Long): LazyList[ElementType] = stream match
      case head #:: tail =>
        val delay = SpecificDuration(duration.milliseconds - (System.currentTimeMillis - last))
        if delay.milliseconds > 0 then sleep(delay)
        stream
      
      case _ =>
        LazyList()

    async(recur(stream, System.currentTimeMillis)).await()

  def multiplexWith(that: LazyList[ElementType])(using Monitor): LazyList[ElementType] =
    unsafely(LazyList.multiplex(stream, that))

  def regulate(tap: Tap)(using Monitor): LazyList[ElementType] =
    def defer
        (active: Boolean,
         stream: LazyList[Some[ElementType] | Tap.Regulation],
         buffer: List[ElementType])
            : LazyList[ElementType] =

      recur(active, stream, buffer)

    @tailrec
    def recur
        (active: Boolean,
         stream: LazyList[Some[ElementType] | Tap.Regulation],
         buffer: List[ElementType])
            : LazyList[ElementType] =
      
      if active && buffer.nonEmpty then buffer.head #:: defer(true, stream, buffer.tail)
      else if stream.isEmpty then LazyList()
      else stream.head match
        case Tap.Regulation.Start =>
          recur(true, stream.tail, buffer)
        
        case Tap.Regulation.Stop =>
          recur(false, stream.tail, Nil)
        
        case Some(other) =>
          if active then other.nn #:: defer(true, stream.tail, Nil)
          else recur(false, stream.tail, other.nn :: buffer)

    LazyList.defer(recur(true, stream.map(Some(_)).multiplexWith(tap.stream), Nil))

  def cluster[DurationType: GenericDuration](duration: DurationType, maxSize: Optional[Int] = Unset)
      (using Monitor)
          : LazyList[List[ElementType]] =

    val Limit = maxSize.or(Int.MaxValue)
    
    def recur(stream: LazyList[ElementType], list: List[ElementType], count: Int)
            : LazyList[List[ElementType]] =

      count match
        case 0 => safely(async(stream.isEmpty).await()) match
          case Unset => recur(stream, Nil, 0)
          case false => recur(stream.tail, stream.head :: list, count + 1)
          case true  => LazyList()
        
        case Limit =>
          list.reverse #:: recur(stream, Nil, 0)
        
        case _ => safely(async(stream.isEmpty).await(duration)) match
          case Unset => list.reverse #:: recur(stream, Nil, 0)
          case false => recur(stream.tail, stream.head :: list, count + 1)
          case true  => LazyList(list.reverse)
    
    LazyList.defer(recur(stream, Nil, 0))

  def parallelMap[ElementType2](lambda: ElementType => ElementType2)(using Monitor)
          : LazyList[ElementType2] =

    val out: Funnel[ElementType2] = Funnel()
    
    async:
      stream.map: elem =>
        async(out.put(lambda(elem)))
    
    out.stream
