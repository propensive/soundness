package serpentine

import contingency.*
import prepositional.*
import rudiments.*
import vacuous.*

object Serpentine:
  @targetName("Parent")
  object ^

  @targetName("RelativeRoot")
  val `?` = PathAscent(0)

  @targetName("Slash")
  object `/`:
    def unapply[PlatformType <: AnyRef & Matchable: {Navigable, Radical}, ElementType]
       (path: Path on PlatformType)
            : Option[(Path on PlatformType, PlatformType.Operand)] =
      path.textDescent match
        case Nil          => None
        case head :: Nil  => Some((PlatformType.root(path.textRoot), PlatformType.element(head)))
        case head :: tail => Some((unsafely(path.parent.vouch), PlatformType.element(head)))
