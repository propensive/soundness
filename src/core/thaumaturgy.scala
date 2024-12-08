package vicarious

import anticipation.*
import rudiments.*
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

case class Catalog[KeyType, ValueType](values: Map[Text, ValueType]):
  def apply(accessor: (`*`: Proxy[KeyType]) ?=> Proxy[KeyType]): ValueType =
    values(accessor(using Proxy[KeyType]()).label.vouch(using Unsafe))

  def map[ValueType2](lambda: ValueType => ValueType2): Catalog[KeyType, ValueType2] =
    Catalog(values.view.mapValues(lambda).to(Map))

  def revise(lambda: (`*`: Proxy[KeyType]) ?=> Proxy[?] ~> ValueType): Catalog[KeyType, ValueType] =
    val partialFunction = lambda(using Proxy())
    Catalog[KeyType, ValueType](values.map: (key, value) =>
      key -> partialFunction.applyOrElse(Proxy(key), _ => value))

  def place[ResultType]
     (lambda: (catalog: Catalog[KeyType, ValueType], `*`: Proxy[KeyType]) ?=> ResultType)
          : ResultType =
    lambda(using this, Proxy())
