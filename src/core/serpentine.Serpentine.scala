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
        (using navigable: PlatformType is Navigable)
        (path: Path on PlatformType)
            : Option[(Path on PlatformType, navigable.Operand)] =
      path.descent match
        case Nil          => None
        case head :: Nil  => Some((navigable.root(path.root), navigable.element(head)))
        case head :: tail => Some((unsafely(path.parent.vouch), navigable.element(head)))
  