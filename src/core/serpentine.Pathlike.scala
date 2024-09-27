package serpentine

import rudiments.*

import scala.reflect.*

object Pathlike:
  erased given [PathType <: Pathlike] => TypeTest[Pathlike, PathType] = ###

transparent trait Pathlike:
  type Platform
  //def root: Root on Platform
  def descent: List[Any]

  override def toString(): String = descent.reverse.mkString("/", "/", "")
