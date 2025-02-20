package anamnesis

import fulminate.*

case class DataError()(using Diagnostics) extends Error(m"Database error")
