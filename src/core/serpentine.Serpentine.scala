package serpentine

import rudiments.*
import prepositional.*
import contingency.*
import vacuous.*

object Serpentine:
  @targetName("Parent")
  object ^

  @targetName("RelativeRoot")
  val `?` = PathAscent(0)

  @targetName("Slash")
  object `/`:
    def unapply[RootType <: AnyRef, ElementType]
        (using RootType is Navigable by ElementType)
        (path: Path on RootType by ElementType)
            : Option[(RootType | Path, ElementType)] =
      path.descent match
        case Nil          => None
        case head :: Nil  => Some((path.root, head))
        case head :: tail => Some((unsafely(path.parent.vouch), head))
  