package spectacular

import vacuous.*
import rudiments.*
import denominative.*
import anticipation.*

object Enumerable:
  inline given [EnumType <: reflect.Enum] => EnumType is Enumerable as derived =
    ${Spectacular.enumerable[EnumType]}

trait Enumerable:
  type Self <: reflect.Enum
  private lazy val valuesMap: Map[Text, Self] = values.indexBy(_.toString.tt)
  val values: IArray[Self]
  def value(name: Text): Optional[Self] = valuesMap.at(name)
  def name(value: Self): Text = value.toString.tt
  def index(value: Self): Int = value.ordinal

  def value(ordinal: Ordinal): Optional[Self] =
    if ordinal.n0 >= 0 && ordinal.n0 < values.length then values(ordinal.n0) else Unset
