package austronesian

import scala.collection.Factory

import anticipation.*
import contingency.*
import distillate.*
import prepositional.*
import rudiments.*
import wisteria.*

object Austronesian:
  opaque type Java =
  IArray[Any] | String | Boolean | Byte | Char | Short | Int | Long | Float | Double

  object Java extends Java2:
    given text: Text is Encodable in Java = _.s
    given string: String is Encodable in Java = identity(_)
    given int: Int is Encodable in Java = identity(_)
    given long: Long is Encodable in Java = identity(_)
    given float: Float is Encodable in Java = identity(_)
    given double: Double is Encodable in Java = identity(_)
    given char: Char is Encodable in Java = identity(_)
    given boolean: Boolean is Encodable in Java = identity(_)
    given byte: Byte is Encodable in Java = identity(_)

    given list: [CollectionType <: Iterable, ElementType: Encodable in Java]
    =>     CollectionType[ElementType] is Encodable in Java =
      iterable => IArray.from(iterable.map(_.encode))

    given text2: Tactic[JavaError] => Text is Decodable in Java =
      case string: String => string.tt
      case _              => raise(JavaError()) yet "".tt

    given string2: Tactic[JavaError] => String is Decodable in Java =
      case string: String => string
      case _              => raise(JavaError()) yet ""

    given int2: Tactic[JavaError] => Int is Decodable in Java =
      case int: Int => int
      case _        => raise(JavaError()) yet 0

    given long2: Tactic[JavaError] => Long is Decodable in Java =
      case long: Long => long
      case _          => raise(JavaError()) yet 0L

    given float2: Tactic[JavaError] => Float is Decodable in Java =
      case float: Float => float
      case _            => raise(JavaError()) yet 0.0f

    given double2: Tactic[JavaError] => Double is Decodable in Java =
      case double: Double => double
      case _              => raise(JavaError()) yet 0.0

    given char2: Tactic[JavaError] => Char is Decodable in Java =
      case char: Char => char
      case _          => raise(JavaError()) yet '\u0000'

    given boolean2: Tactic[JavaError] => Boolean is Decodable in Java =
      case boolean: Boolean => boolean
      case _                => raise(JavaError()) yet false

    given collection: [CollectionType <: Iterable, ElementType: Decodable in Java]
    =>    Tactic[JavaError]
    =>    (factory: Factory[ElementType, CollectionType[ElementType]])
    =>    CollectionType[ElementType] is Decodable in Java =

      case array: Array[Java] =>
        factory.newBuilder.pipe: builder =>
          array.each(builder += _.decode)
          builder.result()

      case other =>
        raise(JavaError()) yet factory.newBuilder.result()

  trait Java2:
    inline given encodable: [ValueType: Reflection] => ValueType is Encodable in Java =
      Austronesian2.EncodableDerivation.derived

    inline given decodable: [ValueType: Reflection] => ValueType is Decodable in Java =
      Austronesian2.DecodableDerivation.derived
