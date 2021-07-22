package honeycomb

import rudiments.*
import cataract.*

package integration.cataract:
  // given [A <: Label, B <: Label]: simplistic.CssSelection[Element[A, B]] = elem =>
  //   val attributes = elem.attributes.map {
  //     case (k, v: String) => str"$k=$v"
  //     case (k, v: Boolean) => k
  //   }.join("[", "][", "]")

  //   str"${elem.label}$attributes"
  
  //given simplistic.CssSelection[Cls] = cls => s".${cls.name}"
  //given simplistic.CssSelection[DomId] = id => s"#${id.name}"