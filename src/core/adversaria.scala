/* Adversaria, version 0.2.0. Copyright 2018 Jon Pretty, Propensive Ltd.
 *
 * The primary distribution site is: http://propensive.com/
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 * License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language governing permissions
 * and limitations under the License.
 */
package adversaria

import annotation.StaticAnnotation
import annotation.implicitNotFound
import language.experimental.macros, language.implicitConversions

import scala.reflect._, reflect.macros._

/** macro implementations */
object Macros {

  /** implicit conversion to construct a dependently-typed `ContextExt` with extension methods */
  private[Macros] implicit def ext(c: blackbox.Context): ContextExt[c.type] = new ContextExt(c)

  /** value class of macro extension methods */
  case class ContextExt[Ctx <: blackbox.Context](context: Ctx) extends AnyVal {
    import context._, universe._
    
    /** the adversaria package */
    private def pkg: Tree = q"_root_.adversaria"
   
    /** construct a new instance of `FindMetadata` */
    private def makeFindMetadata(paramMetadata: Tree, param: TermSymbol, ann: Tree,
        annType: Type, typ: Type) =
      q"""new $pkg.FindMetadata[$annType, $typ]($ann, $paramMetadata) {
            type Return = ${param.info.finalResultType}
            def get(t: $typ): ${param.info.finalResultType} = t.${param.name}
            override def toString(): _root_.java.lang.String = ${Literal(Constant("_."+param.name))}
          }"""

    /** produces a new Tree of a `ParamMetadata` instance */
    private def makeParamMetadata(paramName: String, annotations: List[Tree]): Tree =
      q"new $pkg.ParamMetadata($paramName, $annotations)"

    /** gets the constructor parameters from the specified type */
    private def constParams(typ: Type): List[TermSymbol] =
      typ.typeSymbol.asClass.primaryConstructor.asMethod.typeSignature.paramLists.head.map(_.asTerm)

    /** */
    private def paramMetadata(name: String, annotations: List[Tree]): Tree =
      q"""$pkg.ParamMetadata($name, $annotations)"""

    /** aborts the macro, reporting the error `msg` */
    private def fail(msg: String): Nothing = abort(enclosingPosition, s"adversaria: $msg")

    /** gets the untyped annotations for the given symbol */
    private def annotations(sym: Symbol) = sym.annotations.map(_.tree).map(untypecheck(_))

    /** gets a short, decoded version of a symbol's name */
    private def shortName(sym: Symbol): String = sym.name.decodedName.toString

    /** implementation of the typeMetadata macro */
    private[Macros] def typeMetadata(typ: Type): Tree = {
      val sym = typ.typeSymbol
      val fullName = sym.fullName
      
      val paramAnnotations: List[Tree] =
        constParams(typ).map { p => paramMetadata(shortName(p), annotations(p)) }
      
      q"$pkg.TypeMetadata(${shortName(sym)}, $fullName, ${annotations(sym)}, $paramAnnotations)"
    }

    /** implementation of the findMetadata macro */
    private[Macros] def findMetadata(aType: Type, tType: Type): Tree = {
      val optParam = constParams(tType).find(_.annotations.map(_.tree).exists(_.tpe =:= aType))
      val param = optParam.getOrElse(fail("could not find matching annotation"))
      val annotations = param.annotations.map(_.tree)
      val found = untypecheck(annotations.find(_.tpe =:= aType).get)
      val name = param.name.decodedName.toString
      val paramMetadata = makeParamMetadata(name, annotations.map(untypecheck(_)))
      
      makeFindMetadata(paramMetadata, param, found, aType, tType)
    }

  }

  /** delegates to the macro implementation method */
  def typeMetadata[T: c.WeakTypeTag](c: blackbox.Context): c.Tree =
    c.typeMetadata(c.weakTypeOf[T])

  /** delegates to the macro implementation method */
  def findMetadata[A <: StaticAnnotation: c.WeakTypeTag, T: c.WeakTypeTag]
                  (c: whitebox.Context)
                  : c.Tree =
    c.findMetadata(c.weakTypeOf[A], c.weakTypeOf[T])
}

/** companion object to `TypeMetadata`, providing the implicit macro generator of `TypeMetadata`
 *  evidence */
object TypeMetadata { implicit def annotations[T]: TypeMetadata[T] = macro Macros.typeMetadata[T] }

/** companion object to `FindMetadata`, which provides implicit macro generator of `FindMetadata`
 *  evidence, if it is available */
object FindMetadata {
  implicit def findMetadata[A <: StaticAnnotation, T]: FindMetadata[A, T] =
    macro Macros.findMetadata[A, T]
}

/** representation of the static metadata available on a type, including the type's name and
 *  annotations on the type and its constructor parameters */
case class TypeMetadata[T](typeName: String,
                           fullTypeName: String,
                           annotations: List[StaticAnnotation],
                           parameters: List[ParamMetadata])

/** an implicitly-generated representation of a particular field in a case class found by searching
 *  for it at compile-time by annotation */
@implicitNotFound("adversaria: could not find a parameter annotated with type @${A}")
abstract class FindMetadata[A <: StaticAnnotation, T](val annotation: A,
                                                      val parameter: ParamMetadata) {
  type Return
  def get(t: T): Return
}

case class ParamMetadata(fieldName: String, annotations: List[StaticAnnotation])
