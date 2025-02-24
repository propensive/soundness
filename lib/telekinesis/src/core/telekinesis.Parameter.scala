package telekinesis

import anticipation.*
import contingency.*
import distillate.*
import prepositional.*
import vacuous.*

object Parameter:
  def apply[ValueType: {Decodable in Query, Encodable in Query}](name: Text)
  :     Parameter of ValueType =
    new Parameter(name):
      type Subject = ValueType
      def decode(query: Query): Subject = ValueType.decoded(query)
      def encode(value: Subject): Query = ValueType.encoded(value)

trait Parameter(val name: Text):
  type Subject
  def decode(query: Query): Subject
  def encode(value: Subject): Query

  def apply(value: Subject): Query = encode(value)

  inline def apply()(using request: Http.Request): Subject raises QueryError =
    decode(request.query(name))

  def unapply(scrutinee: Http.Request): Option[Subject] =
    safely(decode(scrutinee.query(name))).option
