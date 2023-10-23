package hellenism

import rudiments.*
import serpentine.*
import gossamer.*
import anticipation.*

import scala.quoted.*

import java.net as jn

sealed trait ClasspathEntry

object ClasspathEntry:
  case class Directory(path: Text) extends ClasspathEntry:
    def apply[DirectoryType: SpecificDirectory](): DirectoryType = SpecificDirectory(path)

  case class Jarfile(path: Text) extends ClasspathEntry:
    def apply[FileType: SpecificFile](): FileType = SpecificFile(path)

  case class Other(path: Text) extends ClasspathEntry
  case object JavaRuntime extends ClasspathEntry

object Classloader:
  inline def apply[ClassType <: AnyKind]: Classloader = ClassRef[ClassType].classloader

class Classloader(val java: ClassLoader):
  protected def urlClassloader: Maybe[jn.URLClassLoader] = java match
    case java: jn.URLClassLoader => java
    case _                       => Unset
  
  def classpath: List[ClasspathEntry] =
    urlClassloader.mm(_.getURLs.nn.to(List)).or(Nil).map: url =>
      url.nn.getProtocol.nn.tt match
        case t"jrt" =>
          ClasspathEntry.JavaRuntime
        
        case t"file" =>
          val path: Text = url.nn.getPath.nn.tt
          if path.ends(t"/") then ClasspathEntry.Directory(path) else ClasspathEntry.Jarfile(path)
        
        case _ => ClasspathEntry.Other(url.toString.tt)

object Hellenism extends Hellenism2:

  opaque type ClassRef = Class[?]

  object ClassRef:
    def apply(javaClass: Class[?]): ClassRef = javaClass
    inline def apply[ClassType <: AnyKind]: ClassRef = ${Hellenism.makeClass[ClassType]}

  extension (classRef: ClassRef)
    def classloader: Classloader = new Classloader(classRef.getClassLoader().nn)

export Hellenism.ClassRef

trait Hellenism2:
  def makeClass[ClassType <: AnyKind: Type](using Quotes): Expr[ClassRef] =
    import quotes.reflect.*
    '{ClassRef(Class.forName(${Expr(TypeRepr.of[ClassType].classSymbol.get.fullName)}).nn)}
