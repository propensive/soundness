package aviation

import anticipation.*
import fulminate.*

case class TimestampError(value: Text)(using Diagnostics)
extends Error(m"The time $value could not be parsed")
