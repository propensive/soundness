package hellenism

import rudiments.*

import scala.quoted.*

import java.net as jn

object Classloader:
  inline def apply[ClassType <: AnyKind]: Classloader = ClassRef[ClassType].classloader

class Classloader(val java: ClassLoader):
  protected def urlClassloader: Maybe[jn.URLClassLoader] = java match
    case java: jn.URLClassLoader => java
    case _                       => Unset

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
