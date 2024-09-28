package galilei

import anticipation.*
import nomenclature.*
import prepositional.*
import contingency.*
import serpentine.*
import gossamer.*

object UnixRoot:
  export Unix.navigable

abstract class UnixRoot() extends Root(t"/", t"/", Case.Sensitive):
  type Platform = Unix

object UnixRootSingleton extends UnixRoot()