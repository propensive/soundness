package gnossienne

import scala.annotation.*

import anticipation.*
import contingency.*
import distillate.*
import prepositional.*

object Reference:
  given encodable: [entity: Resolvable] => (entity.Operand is Encodable in Text)
        =>  (Reference to entity) is Encodable in Text =
    _.key.encode

  given decodable: [entity: Resolvable] => (entity.Operand is Decodable in Text)
        =>  (Reference to entity) is Decodable in Text =
    value => Reference(value.decode)

  def apply[result: Resolvable](operand: result.Operand): Reference to result =
    new Reference(operand):
      type Result = result

trait Reference(private val rawKey: Any):
  type Result

  def key(using resolvable: Result is Resolvable): resolvable.Operand =
    rawKey.asInstanceOf[resolvable.Operand]

  def apply()(using resolvable: Result is Resolvable): Result raises ReferenceError =
    resolvable.resolve(key.asInstanceOf[resolvable.Operand])
