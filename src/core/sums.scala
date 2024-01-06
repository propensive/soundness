package wisteria

import anticipation.*
import rudiments.*
import fulminate.*
import perforate.*

import scala.deriving.*
import scala.compiletime.*

trait SumDerivationMethods[TypeclassType[_]]:
  protected transparent inline def sum
      [DerivationType]
      (variant: Text)
      (inline lambda: [VariantType <: DerivationType] => TypeclassType[VariantType] => (label: Text, ordinal: Int) ?=> VariantType)
      (using reflection: Reflection[DerivationType])
      : DerivationType =

    inline reflection match
      case given SumReflection[DerivationType] =>
        sumRecur[DerivationType, reflection.MirroredElemTypes, reflection.MirroredElemLabels](variant, 0)(lambda)

  private transparent inline def sumRecur
      [DerivationType, VariantsType <: Tuple, LabelsType <: Tuple]
      (variant: Text, index: Int)
      (inline lambda: [VariantType <: DerivationType] => TypeclassType[VariantType] => (label: Text, ordinal: Int) ?=> VariantType)
      (using reflection: SumReflection[DerivationType])
      : DerivationType =

    inline erasedValue[VariantsType] match
      case _: (variantType *: variantsType) => inline erasedValue[LabelsType] match
        case _: (labelType *: moreLabelsType) => inline valueOf[labelType].asMatchable match
          case label: String =>
            if label.tt == variant
            then lambda[variantType & DerivationType](summonInline[TypeclassType[variantType & DerivationType]])(using variant, index)
            else sumRecur[DerivationType, variantsType, moreLabelsType](variant, index + 1)(lambda)

        case EmptyTuple => throw Mistake(msg"unreachable")
      case EmptyTuple =>
        val raises = summonInline[Raises[VariantError]]
        val variants = constValueTuple[reflection.MirroredElemLabels].toList.map(_.toString.tt)
        abort(VariantError(variant, constValue[reflection.MirroredLabel].tt, variants))(using raises)
  
  protected transparent inline def variant
      [DerivationType]
      (sum: DerivationType)
      [ResultType]
      (inline lambda: [VariantType <: DerivationType] => VariantType => (typeclass: TypeclassType[VariantType], label: Text, ordinal: Int) ?=> ResultType)
      (using reflection: SumReflection[DerivationType])
      : ResultType =

    inline reflection match
      case reflection: SumReflection[DerivationType] =>
        val ordinal = reflection.ordinal(sum)
        
        findVariant[DerivationType, reflection.MirroredElemTypes, reflection.MirroredElemLabels](sum, ordinal, 0)(using reflection)(lambda)

  private transparent inline def findVariant
      [DerivationType, VariantsType <: Tuple, LabelsType <: Tuple]
      (sum: DerivationType, ordinal: Int, index: Int)
      [ResultType]
      (using reflection: SumReflection[DerivationType])
      (inline lambda: [VariantType <: DerivationType] => VariantType => (typeclass: TypeclassType[VariantType], label: Text, ordinal: Int) ?=> ResultType)
      : ResultType =

    inline erasedValue[VariantsType] match
      case _: (variantType *: variantsType) => inline erasedValue[LabelsType] match
        case _: (labelType *: moreLabelsType) =>
          type variantSubtype = variantType & DerivationType

          if ordinal == 0 then inline valueOf[labelType].asMatchable match
            case label: String =>
              val typeclass = summonInline[TypeclassType[variantSubtype]]
              
              lambda[variantType & DerivationType](sum.asInstanceOf[variantSubtype])
                  (using typeclass, label.tt, index)

          else findVariant[DerivationType, variantsType, moreLabelsType](sum, ordinal, index + 1)(lambda)
        case EmptyTuple => throw Mistake(msg"unreachable")
      case EmptyTuple => throw Mistake(msg"unreachable")

  inline def split[DerivationType: SumReflection]: TypeclassType[DerivationType]
  