package vicarious

import anticipation.*
import vacuous.*
import prepositional.*

import language.dynamics

case class Proxy[KeyType](label: Optional[Text] = Unset) extends Dynamic:
  def selectDynamic(key: String): Proxy[KeyType] =
    Proxy(label.lay(key.tt)(_+".".tt+key.tt))

  def apply[ValueType]()(using catalog: Catalog[KeyType, ValueType]): ValueType =
    catalog.values(label.or("".tt))

  def applyDynamic[ValueType](key: String)()(using Catalog[KeyType, ValueType]): ValueType =
    selectDynamic(key).apply[ValueType]()
