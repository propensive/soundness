package nomenclature

import rudiments.*
import anticipation.*
import fulminate.*
import gossamer.*

trait Rule(val describe: Text => Message, val check: (Text, Text) => Boolean)
trait Check[ParamType <: Label]

object MustStart extends Rule({ text => m"must start with $text" }, _.starts(_))
erased trait MustStart[TextType <: Label] extends Check[TextType]

object MustEnd extends Rule({ text => m"must end with $text"}, _.ends(_))
erased trait MustEnd[TextType <: Label] extends Check[TextType]

object MustNotStart extends Rule({ text => m"must not start with $text"}, !_.starts(_))
erased trait MustNotStart[TextType <: Label] extends Check[TextType]

object MustNotEnd extends Rule({ text => m"must not end with $text"}, !_.ends(_))
erased trait MustNotEnd[TextType <: Label] extends Check[TextType]

object MustNotContain extends Rule({ text => m"must not contain $text"}, !_.contains(_))
erased trait MustNotContain[TextType <: Label] extends Check[TextType]

object MustNotEqual extends Rule({ text => m"must not equal $text"}, _ != _)
erased trait MustNotEqual[TextType <: Label] extends Check[TextType]