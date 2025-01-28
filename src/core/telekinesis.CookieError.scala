package telekinesis

import anticipation.*
import fulminate.*

case class CookieError(value: Text)(using Diagnostics)
extends Error(m"the cookie value could not be parsed")
