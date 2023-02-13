package rudiments

import language.experimental.captureChecking

extension [FunctorType[+_], ValueType](value: {*} FunctorType[ValueType])(using functor: Functor[FunctorType])
  def map[ValueType2](fn: ValueType => ValueType2): {value, fn} FunctorType[ValueType2] =
    functor.map(value, fn)

extension [CofunctorType[-_], ValueType](value: {*} CofunctorType[ValueType])
          (using cofunctor: Cofunctor[CofunctorType])
  def contraMap[ValueType2](fn: ValueType2 => ValueType): {value, fn} CofunctorType[ValueType2] =
    cofunctor.contraMap(value, fn)

trait Functor[FunctorType[+_]]:
  def map[ValueType, ValueType2](value: {*} FunctorType[ValueType], fn: ValueType => ValueType2)
          : {value, fn} FunctorType[ValueType2]
    
object Functor:
  given list: Functor[List] with
    def map[ElemType, ElemType2](list: {*} List[ElemType], fn: ElemType => ElemType2)
            : {list, fn} List[ElemType2] =
      list.map(fn)
  
  given option: Functor[Option] with
    def map[ValueType, ValueType2](option: {*} Option[ValueType], fn: ValueType => ValueType2)
            : {option, fn} Option[ValueType2] =
      option.map(fn)

trait Cofunctor[CofunctorType[-_]]:
  def contraMap[ValueType, ValueType2](value: {*} CofunctorType[ValueType], fn: ValueType2 => ValueType)
               : {value, fn} CofunctorType[ValueType2]
