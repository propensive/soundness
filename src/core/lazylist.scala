package turbulence

import rudiments.*
import parasite.*
import anticipation.*

import scala.collection.mutable as scm

extension (value: LazyList[Bytes])
  def slurp(): Bytes =
    val bld: scm.ArrayBuilder[Byte] = scm.ArrayBuilder.ofByte()
    value.foreach { bs => bld.addAll(bs.mutable(using Unsafe)) }
    
    bld.result().immutable(using Unsafe)

extension (obj: LazyList.type)
  def multiplex[ElemType](streams: LazyList[ElemType]*)(using Monitor): LazyList[ElemType] =
    multiplexer(streams*).stream
  
  def multiplexer[ElemType](streams: LazyList[ElemType]*)(using Monitor): Multiplexer[Any, ElemType] =
    val multiplexer = Multiplexer[Any, ElemType]()
    streams.zipWithIndex.map(_.swap).foreach(multiplexer.add)
    multiplexer
  
  def pulsar[DurationType: GenericDuration](duration: DurationType)(using Monitor): LazyList[Unit] =
    val startTime: Long = System.currentTimeMillis
    
    def recur(iteration: Int): LazyList[Unit] =
      try
        sleepUntil(startTime + duration.milliseconds*iteration)
        () #:: pulsar(duration)
      catch case err: CancelError => LazyList()
    
    recur(0)
