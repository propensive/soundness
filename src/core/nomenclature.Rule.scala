package nomenclature

import rudiments.*
import anticipation.*
import fulminate.*
import gossamer.*

trait Checkable:
  type Self
  def check(text: Text): Boolean

object MustStart:
  given [TextType <: Label: ValueOf] => MustStart[TextType] is Checkable =
    _.starts(valueOf[TextType])

  given [TextType <: Label: ValueOf] => MustStart[TextType] is Expressible = () =>
    m"must start with ${valueOf[TextType]}"

trait MustStart[TextType <: Label]

object MustEnd:
  given [TextType <: Label: ValueOf] => MustEnd[TextType] is Checkable =
    _.ends(valueOf[TextType])

  given [TextType <: Label: ValueOf] => MustEnd[TextType] is Expressible = () =>
    m"must end with ${valueOf[TextType]}"

trait MustEnd[TextType <: Label]

object MustNotStart:
  given [TextType <: Label: ValueOf] => MustNotStart[TextType] is Checkable =
    !_.starts(valueOf[TextType])

  given [TextType <: Label: ValueOf] => MustNotStart[TextType] is Expressible = () =>
    m"must not start with ${valueOf[TextType]}"

trait MustNotStart[TextType <: Label]

object MustNotEnd:
  given [TextType <: Label: ValueOf] => MustNotEnd[TextType] is Checkable =
    !_.ends(valueOf[TextType])

  given [TextType <: Label: ValueOf] => MustNotEnd[TextType] is Expressible = () =>
    m"must not end with ${valueOf[TextType]}"

trait MustNotEnd[TextType <: Label]

object MustNotContain:
  given [TextType <: Label: ValueOf] => MustNotContain[TextType] is Checkable =
    !_.contains(valueOf[TextType])
  
  given [TextType <: Label: ValueOf] => MustNotContain[TextType] is Expressible = () =>
    m"must not contain ${valueOf[TextType]}"

trait MustNotContain[TextType <: Label]

object MustNotEqual:
  given [TextType <: Label: ValueOf] => MustNotEqual[TextType] is Checkable =
    _.s != valueOf[TextType]

  given [TextType <: Label: ValueOf] => MustNotEqual[TextType] is Expressible = () =>
    m"must not equal ${valueOf[TextType]}"

trait MustNotEqual[TextType <: Label]