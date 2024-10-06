package serpentine

import anticipation.*
import prepositional.*

trait Radical:
  type Self
  type Source <: Root on Self

  def rootLength(path: Text): Int
  def root(path: Text): Source
  def rootText(root: Source): Text
