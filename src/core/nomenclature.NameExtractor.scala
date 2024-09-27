package nomenclature

import rudiments.*

import scala.quoted.*

class NameExtractor[TextType <: Label]():
  inline def apply[PlatformType: Nominative](): Name[PlatformType] =
    ${Nomenclature2.parse[PlatformType, TextType]}

  inline def unapply[PlatformType](inline scrutinee: Name[PlatformType]): Boolean =
    ${Nomenclature2.parse2[PlatformType, TextType]('scrutinee)}
