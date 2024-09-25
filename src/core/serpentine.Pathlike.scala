package serpentine

import rudiments.*

import scala.compiletime.*
import scala.reflect.*

object Pathlike:
  erased given [PathType <: Pathlike] => TypeTest[Pathlike, PathType] = ###

transparent trait Pathlike:
  protected def pathDescent: List[Any]
  protected def pathRoot: AnyRef
  override def toString(): String = pathDescent.reverse.mkString(pathRoot.toString, "/", "")

  override def equals(that: Any): Boolean =
    that.asMatchable match
      case that: Pathlike =>
        pathDescent == that.pathDescent && ((pathRoot eq that.pathRoot) || pathRoot == that.pathRoot)
      case _ =>
        false
    
  override def hashCode: Int = pathRoot.hashCode*31 + pathDescent.hashCode
