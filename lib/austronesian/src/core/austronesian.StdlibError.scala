package austronesian

import anticipation.*
import fulminate.*

case class StdlibError()(using Diagnostics) extends Error(m"JavaError")
