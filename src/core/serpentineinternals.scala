package serpentine

import rudiments.*
import gossamer.*

import language.experimental.captureChecking

object SerpentineInternals:
  opaque type PathElement[ForbiddenType <: ForbiddenSet] = String

  object PathElement:
    inline def apply[ForbiddenType <: ForbiddenSet](value: Text): PathElement[ForbiddenType] =
      value.s

  given [ForbiddenType <: ForbiddenSet]: Show[PathElement[ForbiddenType]] = Text(_)
  
export SerpentineInternals.PathElement