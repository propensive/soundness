package gnossienne

import anticipation.*
import fulminate.*

case class ReferenceError(reference: Text)(using Diagnostics)
extends Error(m"reference $reference could not be resolved")
