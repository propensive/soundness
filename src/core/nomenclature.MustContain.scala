package nomenclature

import rudiments.*
import anticipation.*
import fulminate.*
import gossamer.*

object MustContain extends Rule({ text => m"must contain $text" }):
  def check(text: Text, param: Text): Boolean = text.contains(param)
  // given [TextType <: Label: ValueOf] => MustContain[TextType] is Checkable =
  //   _.contains(valueOf[TextType])
  
  given [TextType <: Label: ValueOf] => MustContain[TextType] is Expressible = () =>
    m"must contain ${valueOf[TextType]}"

trait MustContain[TextType <: Label] extends Check[TextType]