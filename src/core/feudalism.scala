package feudalism

import scala.reflect.*

import language.experimental.captureChecking

class MutexRef[ValueType](value: ValueType, makeSnapshot: ValueType => ValueType):
  def apply(): ValueType = value
  def snapshot(): ValueType = makeSnapshot(value)

class Mutex[ValueType](initial: ValueType):
  private var count: Int = 0
  private var value: ValueType = initial

  def read
      [ResultType, ImmutableType]
      (using immutable: Immutable[ValueType, ImmutableType])
      (fn: (ref: MutexRef[ImmutableType]) => ResultType)
      : ResultType =

    synchronized:
      while count == -1 do wait()
      count += 1
    
    val result = fn(MutexRef(immutable.make(value), immutable.snapshot(_)))
    
    synchronized:
      count -= 1
      notify()

    result
  
  def mutate(fn: ValueType => Unit): Unit =
    synchronized:
      while count != 0 do wait()
      count = -1

    fn(value)
    
    synchronized:
      count = 0
      notify()
  
  def replace(fn: ValueType => ValueType): Unit =
    synchronized:
      while count != 0 do wait()
      count = -1
    
    value = fn(value)
    
    synchronized:
      count = 0
      notify()

trait Immutable[MutableType, ImmutableType]:
  def snapshot(ref: ImmutableType): ImmutableType = ref
  def make(ref: MutableType): ImmutableType

object Immutable:
  given array[ElementType: ClassTag]: Immutable[Array[ElementType], IArray[ElementType]] with
    def make(value: Array[ElementType]): IArray[ElementType] = value.asInstanceOf[IArray[ElementType]]
    
    override def snapshot(value: IArray[ElementType]): IArray[ElementType] =
      val array = new Array[ElementType](value.length)
      System.arraycopy(value, 0, array, 0, value.length)
      array.asInstanceOf[IArray[ElementType]]
  
  given stringBuilder: Immutable[StringBuilder, String] with
    def make(value: StringBuilder): String = value.toString
  
  given any[ValueType](using DummyImplicit): Immutable[ValueType, ValueType] with
    def make(value: ValueType): ValueType = value
