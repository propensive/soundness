package rudiments

import anticipation.*

import scala.compiletime.*

object Default:
  given Default[Int](0)
  given [ValueType: ValueOf]: Default[ValueType](valueOf[ValueType])
  given Default[Long](0L)
  given Default[Text]("".tt)
  given Default[String]("")
  given [ElemType]: Default[List[ElemType]](Nil)
  given [ElemType]: Default[Set[ElemType]](Set())
  given [ElemType]: Default[Vector[ElemType]](Vector())

trait Default[+ValueType](default: ValueType):
  def apply(): ValueType = default

inline def default[ValueType]: ValueType = summonInline[Default[ValueType]]()

