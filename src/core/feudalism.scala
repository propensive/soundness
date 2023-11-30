package feudalism

import language.experimental.captureChecking

class Ref[ValueType](get: ValueType):
  def apply(): ValueType = get
  def copy(): ValueType^{this} = get

class Mutex[ValueType](initial: ValueType):
  private var count: Int = 0
  private var value: ValueType = initial

  def read
      [ResultType, ImmutableType]
      (using immutable: Immutable[ValueType, ImmutableType])
      (fn: (ref: Ref[ImmutableType]^) => ResultType)
      : ResultType^{fn*} =

    synchronized:
      while count == -1 do wait()
      count += 1
    
    val result = fn(Ref(immutable(value)))
    
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
  def apply(ref: MutableType): ImmutableType

object Immutable:
  given [ElementType]: Immutable[Array[ElementType], IArray[ElementType]] with
    def apply(ref: Array[ElementType]): IArray[ElementType] = ref.asInstanceOf[IArray[ElementType]]
  
  given Immutable[StringBuilder, String] with
    def apply(ref: StringBuilder): String = ref.toString
  
  given [ValueType](using DummyImplicit): Immutable[ValueType, ValueType] with
    def apply(ref: ValueType): ValueType = ref