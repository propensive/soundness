package rudiments

import language.experimental.captureChecking

extension [FunctorType[+_], ValueType](value: FunctorType[ValueType]^)(using functor: Functor[FunctorType])
  def map[ValueType2](fn: ValueType => ValueType2): FunctorType[ValueType2]^{value, fn} =
    functor.map(value, fn)

extension [CofunctorType[-_], ValueType](value: CofunctorType[ValueType]^)
          (using cofunctor: Cofunctor[CofunctorType])
  def contraMap[ValueType2](fn: ValueType2 => ValueType): CofunctorType[ValueType2]^{value, fn} =
    cofunctor.contraMap(value, fn)

trait Functor[FunctorType[+_]]:
  def map[ValueType, ValueType2](value: FunctorType[ValueType]^, fn: ValueType => ValueType2)
          : FunctorType[ValueType2]^{value, fn}
    
object Functor:
  given list: Functor[List] with
    def map[ElemType, ElemType2](list: List[ElemType]^, fn: ElemType => ElemType2)
            : List[ElemType2]^{list, fn} =
      list.map(fn)
  
  given option: Functor[Option] with
    def map[ValueType, ValueType2](option: Option[ValueType]^, fn: ValueType => ValueType2)
            : Option[ValueType2]^{option, fn} =
      option.map(fn)

trait Cofunctor[CofunctorType[-_]]:
  def contraMap[ValueType, ValueType2](value: CofunctorType[ValueType]^, fn: ValueType2 => ValueType)
               : CofunctorType[ValueType2]^{value, fn}
