package turbulence

import java.util.concurrent as juc

object Funnel:
  private object Termination

class Funnel[ItemType]():
  private val queue: juc.LinkedBlockingQueue[ItemType | Funnel.Termination.type] = juc.LinkedBlockingQueue()
  
  def put(item: ItemType): Unit = queue.put(item)
  def stop(): Unit = queue.put(Funnel.Termination)
  def stream: LazyList[ItemType] = LazyList.continually(queue.take().nn).takeWhile(_ != Funnel.Termination)

class Gun() extends Funnel[Unit]():
  def fire(): Unit = put(())
