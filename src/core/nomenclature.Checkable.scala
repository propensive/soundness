package nomenclature

import rudiments.*
import anticipation.*
import fulminate.*
import gossamer.*

trait Rule(describe0: Text => Message):
  def describe(value: Text): Message = describe0(value)
  def check(value: Text, parameter: Text): Boolean

trait Check[ParamType <: Label]

object MustStart extends Rule({ text => m"must start with $text" }):
  def check(value: Text, parameter: Text): Boolean = value.starts(parameter)

  given [TextType <: Label: ValueOf] => MustStart[TextType] is Expressible = () =>
    m"must start with ${valueOf[TextType]}"

erased trait MustStart[TextType <: Label] extends Check[TextType]

object MustEnd extends Rule({ text => m"must end with $text"}):
  def check(value: Text, parameter: Text): Boolean = value.ends(parameter)
  
  given [TextType <: Label: ValueOf] => MustEnd[TextType] is Expressible = () =>
    m"must end with ${valueOf[TextType]}"

erased trait MustEnd[TextType <: Label] extends Check[TextType]

object MustNotStart extends Rule({ text => m"must not start with $text"}):
  def check(value: Text, parameter: Text): Boolean = !value.starts(parameter)

  given [TextType <: Label: ValueOf] => MustNotStart[TextType] is Expressible = () =>
    m"must not start with ${valueOf[TextType]}"

trait MustNotStart[TextType <: Label] extends Check[TextType]

object MustNotEnd extends Rule({ text => m"must not end with $text"}):
  def check(value: Text, parameter: Text): Boolean = !value.ends(parameter)

  given [TextType <: Label: ValueOf] => MustNotEnd[TextType] is Expressible = () =>
    m"must not end with ${valueOf[TextType]}"

trait MustNotEnd[TextType <: Label] extends Check[TextType]

object MustNotContain extends Rule({ text => m"must not contain $text"}):
  def check(value: Text, parameter: Text): Boolean = !value.contains(parameter)
  
  given [TextType <: Label: ValueOf] => MustNotContain[TextType] is Expressible = () =>
    m"must not contain ${valueOf[TextType]}"

trait MustNotContain[TextType <: Label] extends Check[TextType]

object MustNotEqual extends Rule({ text => m"must not equal $text"}):
  def check(value: Text, parameter: Text): Boolean = value != parameter

  given [TextType <: Label: ValueOf] => MustNotEqual[TextType] is Expressible = () =>
    m"must not equal ${valueOf[TextType]}"

trait MustNotEqual[TextType <: Label] extends Check[TextType]