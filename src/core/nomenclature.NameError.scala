package nomenclature

import anticipation.*
import fulminate.*

case class NameError(name: Text, reason: Message)(using Diagnostics)
extends Error(m"the name $name is not valid because it $reason")
