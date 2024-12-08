package vicarious

import anticipation.*
import rudiments.*
import vacuous.*

import language.dynamics

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
