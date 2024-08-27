package serpentine2

import anticipation.*
import rudiments.*
import contingency.*
import fulminate.*
import prepositional.*

trait OperatingSystem:
  type Element

object Linux
trait Linux extends Unix

object Unix
trait Unix extends OperatingSystem

object Windows
trait Windows extends OperatingSystem

object MacOs
trait MacOs extends Unix

trait Root:
  type Platform <: OperatingSystem
  type Element

  @targetName("child")
  infix def / (element: Element): Path on Platform = Path(this)(List(element))

object Path:
  def apply(root0: Root)(elements: Seq[root0.Element]): Path on root0.Platform =
    new Path(root0)(elements)

class Path(tracked val root: Root)(val descent: Seq[root.Element]):
  type Platform = root.Platform

  @targetName("child")
  infix def / (child: root.Element): Path on Platform = Path(root)(child +: descent)

object Serpentine:
  @targetName("UnixRoot")
  object % extends Root:
    type Element = Text
    type Platform = Unix

export Serpentine.`%`

case class Drive(letter: Char) extends Root:
  type Element = Text
  type Platform = Windows

object PathError:
  object Reason:
    given Reason is Communicable =
      case Reason.RootParent => m"the root has no parent"

  enum Reason:
    case RootParent

case class PathError(reason: PathError.Reason)
extends Error(m"the path was invalid because $reason")

class Relative(val root: Root)(ascent: Int, descent: Seq[root.Element]):
  type Platform = root.Platform

  @targetName("plus")
  def + (relative: Relative on Platform): Relative on Platform raises PathError = ???
