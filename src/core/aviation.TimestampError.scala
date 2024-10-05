package aviation

import fulminate.*
import anticipation.*

case class TimestampError(value: Text)(using Diagnostics)
extends Error(m"The time $value could not be parsed")