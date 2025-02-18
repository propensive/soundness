package austronesian

import scala.compiletime.*

import anticipation.*
import contingency.*
import distillate.*
import prepositional.*
import rudiments.*
import wisteria.*

object Austronesian2:
  object EncodableDerivation extends Derivation[[Type] =>> Type is Encodable in Java]:

    inline def join[DerivationType <: Product: ProductReflection]
    :     DerivationType is Encodable in _root_.austronesian.Austronesian.Java =

      fields(_):
        [FieldType] => _.encode
      .asInstanceOf[Java]

    inline def split[DerivationType: SumReflection]: DerivationType is Encodable in Java =
      variant(_):
        [VariantType <: DerivationType] => value =>
          IArray.create[Java](2): array =>
            array(0) = label.s.asInstanceOf[Java]
            array(1) = value.encode

          . asInstanceOf[Java]

  object DecodableDerivation extends Derivable[Decodable in Java]:
    inline def join[DerivationType <: Product: ProductReflection]
    :     DerivationType is Decodable in Java =

      case array: Array[Java] =>
        construct: [FieldType] =>
          _.decoded(array(index))

      case other =>
        summonInline[Tactic[JavaError]].give(abort(JavaError()))

    inline def split[DerivationType: SumReflection]: DerivationType is Decodable in Java =
      case Array(label: String, java: Java) =>
        delegate(label): [VariantType <: DerivationType] =>
          _.decoded(java)

      case other =>
        summonInline[Tactic[JavaError]].give(abort(JavaError()))
