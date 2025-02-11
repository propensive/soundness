/*
    Wisteria, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import vacuous.*

import scala.quoted.*

object Wisteria:
  inline def default[ProductType, FieldType](index: Int): Optional[FieldType] =
    ${getDefault[ProductType, FieldType]('index)}

  def getDefault[ProductType: Type, FieldType: Type](index: Expr[Int])(using Quotes)
  :     Expr[Optional[FieldType]] =

    import quotes.reflect.*

    val methodName: String = "$lessinit$greater$default$"+(index.valueOrAbort + 1)
    val productSymbol = TypeRepr.of[ProductType].classSymbol

    productSymbol.flatMap: symbol =>
      symbol.companionClass.declaredMethod(methodName).headOption.map: method =>
        Ref(symbol.companionModule).select(method)

    . map: selection =>
        TypeRepr.of[ProductType].typeArgs match
          case Nil  => selection
          case args => selection.appliedToTypes(args)

    . map(_.asExprOf[FieldType]).getOrElse('{Unset})
