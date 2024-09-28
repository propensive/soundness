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
    def unapply[PlatformType <: AnyRef & Matchable, ElementType]
        (using PlatformType is Navigable by ElementType)
        (path: Path on PlatformType by ElementType)
            : Option[(Path on PlatformType, ElementType)] =
      path.descent match
        case Nil          => None
        case head :: Nil  => Some((path.root, head))
        case head :: tail => Some((unsafely(path.parent.vouch), head))
  