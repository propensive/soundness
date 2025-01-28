package telekinesis

import anticipation.*
import fulminate.*

case class AuthError(value: Text)(using Diagnostics)
extends Error(m"the authentication value $value is not valid")
