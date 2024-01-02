package wisteria2

import fulminate.*
import gossamer.*
import perforate.*
import anticipation.*

import scala.deriving.*
import scala.compiletime.*

case class CoproductError(variant: Text, coproduct: Text, validVariants: List[Text])
extends Error(msg"""the specified $variant is not one of the valid variants (${validVariants.join(t", ")}) of
                    coproduct $coproduct""")

object Derivation:
  transparent inline def sumOf
      [DerivationType, TypeclassType[_]]
      (variant: String)
      [ResultType]
      (inline split: TypeclassType[DerivationType] => ResultType)
      (using mirror: Mirror.Of[DerivationType])
      : ResultType =

    inline mirror match
      case given Mirror.SumOf[DerivationType] =>
        sum[DerivationType, TypeclassType, mirror.MirroredElemTypes, mirror.MirroredElemLabels](variant)(split)

  transparent inline def sumOf
      [DerivationType, TypeclassType[_]]
      (value: DerivationType)
      [ResultType]
      (inline split: String => TypeclassType[DerivationType] => ResultType)
      (using mirror: Mirror.Of[DerivationType])
      : ResultType =

    inline mirror match
      case mirror: Mirror.SumOf[DerivationType] =>
        sum[DerivationType, TypeclassType, mirror.MirroredElemTypes, mirror.MirroredElemLabels](mirror.ordinal(value))(split)(using mirror)

  transparent inline def sum
    [DerivationType, TypeclassType[_], VariantsType <: Tuple, LabelsType <: Tuple]
    (ordinal: Int)
    [ResultType]
    (inline split: String => TypeclassType[DerivationType] => ResultType)
    (using mirror: Mirror.SumOf[DerivationType])
    : ResultType =

  inline erasedValue[VariantsType] match
    case _: (variantType *: variantsType) => inline erasedValue[LabelsType] match
      case _: (labelType *: labelsType) =>
        if ordinal == 0 then inline valueOf[labelType].asMatchable match
          case label: String =>
            split(label)(summonInline[TypeclassType[variantType]].asInstanceOf[TypeclassType[DerivationType]])
        else sum[DerivationType, TypeclassType, variantsType, labelsType](ordinal - 1)(split)
      case EmptyTuple => throw Mistake(msg"unreachable")
    case EmptyTuple => throw Mistake(msg"unreachable")
        

  transparent inline def sum
      [DerivationType, TypeclassType[_], VariantsType <: Tuple, LabelsType <: Tuple]
      (variant: String)
      [ResultType]
      (inline split: TypeclassType[DerivationType] => ResultType)
      (using mirror: Mirror.SumOf[DerivationType])
      : ResultType =

    inline erasedValue[VariantsType] match
      case _: (variantType *: variantsType) => inline erasedValue[LabelsType] match
        case _: (labelType *: labelsType) => inline valueOf[labelType].asMatchable match
          case label: String =>
            if label == variant
            then split(summonInline[TypeclassType[variantType]].asInstanceOf[TypeclassType[DerivationType]])
            else sum[DerivationType, TypeclassType, variantsType, labelsType](variant)(split)

        case EmptyTuple => throw Mistake(msg"unreachable")
      case EmptyTuple =>
        val raises = summonInline[Raises[CoproductError]]
        val variants = constValueTuple[mirror.MirroredElemLabels].toList.map(_.toString.tt)
        abort(CoproductError(variant.tt, constValue[mirror.MirroredLabel].tt, variants))(using raises)

