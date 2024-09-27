package nomenclature

import rudiments.*

import scala.quoted.*

export Nomenclature.Name

extension (inline context: StringContext)
  transparent inline def n: Any = ${Nomenclature2.extractor('context)}

class NExtractor[TextType <: Label]():
  inline def apply[PlatformType: Nominative](): Name[PlatformType] =
    ${Nomenclature2.parse[PlatformType, TextType]}

  inline def unapply[PlatformType](inline scrutinee: Name[PlatformType]): Boolean =
    ${Nomenclature2.parse2[PlatformType, TextType]('scrutinee)}

  // inline def unapply[PlatformType: Nominative](scrutinee: Name[PlatformType])
  //         : Option[Name[PlatformType]] =
  //   ${Nomenclature2.parse2[PlatformType, TextType]('scrutinee)}