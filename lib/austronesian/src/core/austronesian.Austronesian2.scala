package austronesian

import scala.compiletime.*

import anticipation.*
import contingency.*
import distillate.*
import prepositional.*
import rudiments.*
import wisteria.*

object Austronesian2:
  object EncodableDerivation extends Derivation[[Type] =>> Type is Encodable in Stdlib]:

    inline def join[DerivationType <: Product: ProductReflection]
    :     DerivationType is Encodable in _root_.austronesian.Austronesian.Stdlib =

      fields(_):
        [FieldType] => _.encode
      .asInstanceOf[Stdlib]

    inline def split[DerivationType: SumReflection]: DerivationType is Encodable in Stdlib =
      variant(_):
        [VariantType <: DerivationType] => value =>
          IArray.create[Stdlib](2): array =>
            array(0) = label.s.asInstanceOf[Stdlib]
            array(1) = value.encode

          . asInstanceOf[Stdlib]

  object DecodableDerivation extends Derivable[Decodable in Stdlib]:
    inline def join[DerivationType <: Product: ProductReflection]
    :     DerivationType is Decodable in Stdlib =

      case array: Array[Stdlib] =>
        construct: [FieldType] =>
          _.decoded(array(index))

      case other =>
        summonInline[Tactic[StdlibError]].give(abort(StdlibError()))

    inline def split[DerivationType: SumReflection]: DerivationType is Decodable in Stdlib =
      case Array(label: String, stdlib: Stdlib) =>
        delegate(label): [VariantType <: DerivationType] =>
          _.decoded(stdlib)

      case other =>
        summonInline[Tactic[StdlibError]].give(abort(StdlibError()))
