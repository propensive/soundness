package serpentine

import rudiments.*
import anticipation.*

import scala.reflect.*

object Pathlike:
  erased given [PathType <: Pathlike] => TypeTest[Pathlike, PathType] = ###

transparent trait Pathlike:
  type Platform
  def descent: List[Text]

  override def toString(): String = descent.reverse.mkString("/", "/", "")
