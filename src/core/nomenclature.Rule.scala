package nomenclature

import anticipation.*
import fulminate.*

trait Rule(val describe: Text => Message, val check: (Text, Text) => Boolean)