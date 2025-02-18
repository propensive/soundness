package austronesian

import anticipation.*
import fulminate.*

case class JavaError()(using Diagnostics) extends Error(m"JavaError")
