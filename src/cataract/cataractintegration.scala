package honeycomb

import rudiments.*
import cataract.*

package integration.cataract:
  given [T]: Attribute["style", Style, T] = _.toString
  
  given [A <: Label, B <: Label, C <: Label]: Selectable[Tag[A, B, C]] =
    tag => Selector.Element(tag.label)
  
  given [A <: Label, B <: Label]: Selectable[Element[A, B]] = elem =>
    val attributes = elem.attributes.map {
      case (k, v: String) => str"$k=$v"
      case (k, v: Boolean) => k
    }.join("[", "][", "]")

    Selector.Element(str"${elem.label}$attributes")
  
  given transTag[A <: Label, B <: Label, C <: Label]: Selectable[TransTag[A, B, C]] =
    tag => Selector.Element(tag.label)
  
  given Selectable[Cls] = cls => Selector.Class(cls.name)
  given Selectable[DomId] = id => Selector.Id(id.name)