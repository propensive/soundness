package wisteria

object VariantIndex:
  inline def apply[VariantType](int: Int): Int & VariantIndex[VariantType] =
    int.asInstanceOf[Int & VariantIndex[VariantType]]

erased trait VariantIndex[VariantType]
