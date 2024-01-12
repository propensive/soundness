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

import rudiments.*
import vacuous.*
import parasite.*
import capricious.*
import anticipation.*

import scala.collection.mutable as scm

import language.experimental.captureChecking

extension (lazyList: LazyList[Bytes])
  def slurp(): Bytes =
    val bld: scm.ArrayBuilder[Byte] = scm.ArrayBuilder.ofByte()
    
    lazyList.each: bs =>
      bld.addAll(bs.mutable(using Unsafe))
    
    bld.result().immutable(using Unsafe)

  def drop(byteSize: ByteSize): LazyList[Bytes] =
    def recur(stream: LazyList[Bytes], skip: ByteSize): LazyList[Bytes] = stream match
      case head #:: tail =>
        if head.byteSize < skip then recur(tail, skip - head.byteSize) else head.drop(skip.long.toInt) #:: tail
      
      case _ =>
        LazyList()
      
    recur(lazyList, byteSize)

  def shred(mean: Double, variance: Double)(using RandomNumberGenerator): LazyList[Bytes] = stochastic:
    given Distribution = Gamma.approximate(mean, variance)
    
    def newArray(): Array[Byte] = new Array[Byte](arbitrary[Double]().toInt.max(1))
    
    def recur(stream: LazyList[Bytes], sourcePos: Int, dest: Array[Byte], destPos: Int): LazyList[Bytes] =
      stream match
        case source #:: more =>
          val ready = source.length - sourcePos
          val free = dest.length - destPos

          if ready < free then
            System.arraycopy(source, sourcePos, dest, destPos, ready)
            recur(more, 0, dest, destPos + ready)
          else if free < ready then
            System.arraycopy(source, sourcePos, dest, destPos, free)
            dest.immutable(using Unsafe) #:: recur(stream, sourcePos + free, newArray(), 0)
          else // free == ready
            System.arraycopy(source, sourcePos, dest, destPos, free)
            dest.immutable(using Unsafe) #:: recur(more, 0, newArray(), 0)
        
        case _ =>
          if destPos == 0 then LazyList() else LazyList(dest.slice(0, destPos).immutable(using Unsafe))
        
      

    recur(lazyList, 0, newArray(), 0)

  def take(byteSize: ByteSize): LazyList[Bytes] =
    def recur(stream: LazyList[Bytes], count: ByteSize): LazyList[Bytes] = stream match
      case head #:: tail =>
        if head.byteSize < count then head #:: recur(tail, count - head.byteSize)
        else LazyList(head.take(count.long.toInt))
      
      case _ =>
        LazyList()
      
    recur(lazyList, byteSize)

extension (obj: LazyList.type)
  def multiplex[ElemType](streams: LazyList[ElemType]*)(using Monitor): LazyList[ElemType] =
    multiplexer(streams*).stream
  
  def multiplexer[ElemType](streams: LazyList[ElemType]*)(using Monitor): Multiplexer[Any, ElemType] =
    val multiplexer = Multiplexer[Any, ElemType]()
    streams.zipWithIndex.map(_.swap).each(multiplexer.add)
    multiplexer

  def defer[ElemType](lazyList: => LazyList[ElemType]): LazyList[ElemType] =
    (null.asInstanceOf[ElemType] #:: lazyList).tail

  def pulsar[DurationType: GenericDuration](duration: DurationType)(using Monitor): LazyList[Unit] =
    val startTime: Long = System.currentTimeMillis
    
    def recur(iteration: Int): LazyList[Unit] =
      try
        sleepUntil(startTime + duration.milliseconds*iteration)
        () #:: pulsar(duration)
      catch case err: CancelError => LazyList()
    
    recur(0)
