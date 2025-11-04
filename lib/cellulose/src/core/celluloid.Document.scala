package celluloid

import soundness.*

trait Loadable:
  type Metadata

case class Document[content <: Loadable](root: content, metadata: root.Metadata)

extension [readable: Readable by Bytes](entity: readable)
  def load[content <: Loadable]: Document[content] =
    val conduit = Conduit(entity.stream[Bytes])
