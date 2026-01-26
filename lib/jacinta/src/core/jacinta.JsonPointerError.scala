package jacinta

import fulminate.*

case class JsonPointerError()(using Diagnostics) extends Error(m"could not resolve JSON pointer")
