package serpentine

import rudiments.*
import prepositional.*

extension [RootType <: AnyRef](root: RootType)
  def path(using RootType is Navigable): Path on RootType = Path(root)(Nil)

export Serpentine.{?, ^, /}