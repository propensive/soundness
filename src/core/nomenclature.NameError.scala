package nomenclature

import anticipation.*
import fulminate.*

case class NameError(name: Text, rule: Rule, parameter: Text)(using Diagnostics)
extends Error(m"the name $name is not valid because it ${rule.describe(parameter)}")
