package serpentine

import anticipation.*
import rudiments.*

import scala.reflect.*

object Pathlike:
  erased given [PathType <: Pathlike] => TypeTest[Pathlike, PathType] = ###

transparent trait Pathlike:
  type Platform
  def textDescent: List[Text]
  def separator: Text

  override def toString(): String = textDescent.reverse.mkString(separator.s)
