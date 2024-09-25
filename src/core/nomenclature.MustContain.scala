package nomenclature

import rudiments.*
import anticipation.*
import fulminate.*
import gossamer.*

object MustContain:
  given [TextType <: Label: ValueOf] => MustContain[TextType] is Checkable =
    _.contains(valueOf[TextType])
  
  given [TextType <: Label: ValueOf] => MustContain[TextType] is Expressible = () =>
    m"must contain ${valueOf[TextType]}"

trait MustContain[TextType <: Label]