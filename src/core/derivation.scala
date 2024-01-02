/*
    Wisteria, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package wisteria

import rudiments.{is as _, *}
import fulminate.*

import scala.quoted.*
import scala.compiletime.*

object Wisteria:
  inline def isObject[T]: Boolean = ${isObject[T]}
  inline def anns[T]: List[Matchable] = ${anns[T]}
  inline def typeAnns[T]: List[Any] = ${typeAnns[T]}
  inline def paramAnns[T]: List[(String, List[Matchable])] = ${paramAnns[T]}
  inline def isValueClass[T]: Boolean = ${isValueClass[T]}
  inline def defaultValue[T]: List[(String, Option[Any])] = ${defaultValue[T]}
  inline def paramTypeAnns[T]: List[(String, List[Any])] = ${paramTypeAnns[T]}
  inline def repeated[T]: List[(String, Boolean)] = ${repeated[T]}
  inline def typeInfo[T]: TypeInfo = ${typeInfo[T]}
  inline def summonOption[T]: Option[T] = ${summonOption[T]}
  
  def summonOption[T: Type](using Quotes): Expr[Option[T]] =
    Expr.summon[T] match
      case None => Expr(None)
      case Some(e) => '{Some($e)}

  def isObject[T: Type](using Quotes): Expr[Boolean] =
    import quotes.reflect.*

    Expr(TypeRepr.of[T].typeSymbol.flags.is(Flags.Module))

  def paramAnns[T: Type](using Quotes): Expr[List[(String, List[Matchable])]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]

    def filterAnn(a: Term): Boolean =
      a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
        a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"

    Expr.ofList {
      val constructorAnns = tpe.typeSymbol.primaryConstructor.paramSymss.flatten.map { field =>
        field.name -> field.annotations.filter(filterAnn).map(_.asExpr)
      }

      val fieldAnns = tpe.typeSymbol.caseFields.collect {
        case field: Symbol if field.isValDef =>
          field.name -> field.annotations.filter(filterAnn).map(_.asExpr)
      }

      (constructorAnns ++ fieldAnns).filter(_(1).nonEmpty).groupBy(_(0)).to(List).map: (name, l) =>
        Expr(name) -> l.flatMap(_(1))
      .map: (name, anns) =>
        Expr.ofTuple(name, Expr.ofList(anns.map(_.asExprOf[Matchable])))
    }
  
  def anns[T: Type](using Quotes): Expr[List[Matchable]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    
    Expr.ofList {
      tpe.typeSymbol.annotations.filter { a =>
        a.tpe.typeSymbol.maybeOwner.isNoSymbol || a.tpe.typeSymbol.owner.fullName !=
            "scala.annotation.internal"
      }.map(_.asExprOf[Matchable])
    }
  
  def typeAnns[T: Type](using Quotes): Expr[List[Any]] =
    import quotes.reflect.*
    
    def getAnnotations(t: TypeRepr): List[Term] = t.asMatchable match
      case AnnotatedType(inner, ann) => ann :: getAnnotations(inner)
      case _                         => Nil
    
    val tpe = TypeRepr.of[T]

    tpe.typeSymbol.tree match
      case ClassDef(_, _, parents, _, _) =>
        Expr.ofList {
          parents.collect { case t: TypeTree => t.tpe }.flatMap(getAnnotations).filter { a =>
            a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
                a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"
          }.map(_.asExpr)
        }

      case _ =>
        Expr.ofList(List())
  
  def isValueClass[T: Type](using Quotes): Expr[Boolean] =
    import quotes.reflect.*
    
    Expr(TypeRepr.of[T].baseClasses.contains(Symbol.classSymbol("scala.AnyVal")))
  
  def defaultValue[T: Type](using Quotes): Expr[List[(String, Option[Any])]] =
    import quotes.reflect.*

    // TODO: Implement RHS
    Expr.ofList(TypeRepr.of[T].typeSymbol.caseFields.map { case s => Expr(s.name -> None) })

  def paramTypeAnns[T: Type](using Quotes): Expr[List[(String, List[Any])]] =
    import quotes.reflect.*

    def getAnnotations(t: TypeRepr): List[Term] = t.asMatchable match
      case AnnotatedType(inner, ann) => ann :: getAnnotations(inner)
      case _                         => Nil

    Expr.ofList {
      TypeRepr.of[T].typeSymbol.caseFields.map { field =>
        val tpeRepr = field.tree match
          case v: ValDef => v.tpt.tpe
          case d: DefDef => d.returnTpt.tpe
          case other     => throw Mistake(msg"field is not of the expected AST type")
        
        Expr(field.name) -> getAnnotations(tpeRepr).filter { a =>
          a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
              a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"
          }.map(_.asExpr)
      }.filter(_._2.nonEmpty).map { (name, annots) => Expr.ofTuple(name, Expr.ofList(annots)) }
    }
  
  def repeated[T: Type](using Quotes): Expr[List[(String, Boolean)]] =
    import quotes.reflect.*
    
    def isRepeated[T](tpeRepr: TypeRepr): Boolean = tpeRepr match
      case a: AnnotatedType =>
        a.annotation.tpe match
          case tr: TypeRef => tr.name == "Repeated"
          case _           => false
      case _ => false

    val tr = TypeRepr.of[T]
    val symbol = tr.typeSymbol
    val constr = symbol.primaryConstructor.tree.asInstanceOf[DefDef]
    
    val areRepeated = constr.paramss.flatMap(_.params.flatMap {
      case ValDef(name, tpeTree, _) => Some(name -> isRepeated(tpeTree.tpe))
      case _                        => None
    })
    
    Expr(areRepeated)

  def typeInfo[T: Type](using Quotes): Expr[TypeInfo] =
    import quotes.reflect.*
    
    def normalizedName(s: Symbol): String =
      if s.flags.is(Flags.Module) then s.name.replaceAll("\\$$", "").nn else s.name
    
    def name(tpe: TypeRepr) : Expr[String] = Expr(normalizedName(tpe.typeSymbol))

    def ownerNameChain(sym: Symbol): List[String] =
      if sym.isNoSymbol || sym == defn.EmptyPackageClass || sym == defn.RootPackage ||
          sym == defn.RootClass
      then Nil
      else ownerNameChain(sym.owner) :+ normalizedName(sym)

    def owner(tpe: TypeRepr): Expr[String] =
      Expr(ownerNameChain(tpe.typeSymbol.maybeOwner).mkString("."))

    def typeInfo(tpe: TypeRepr): Expr[TypeInfo] = tpe.asMatchable match
      case AppliedType(tpe, args) =>
        '{TypeInfo(${owner(tpe)}, ${name(tpe)}, ${Expr.ofList(args.map(typeInfo))})}
      case _ =>
        '{TypeInfo(${owner(tpe)}, ${name(tpe)}, Nil)}

    typeInfo(TypeRepr.of[T])
